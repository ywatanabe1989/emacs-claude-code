;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-14 15:10:08>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-dashboard/ecc-dashboard-ui.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; Commentary:
;;
;; Dashboard UI implementation for Claude agent dashboard.
;; This file provides the UI components and display logic for the dashboard.
;;

;;; Code:

(require 'ecc-buffer-uid)
(require 'ecc-buffer-registry-uid)
(require 'ecc-buffer-metadata)
(require 'ecc-dashboard-variables)
(require 'ecc-dashboard-actions)
(require 'ecc-dashboard-filters)

;; Dashboard UI Mode
;; ------------------------------

(defvar ecc-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'ecc-dashboard-next-line)
    (define-key map (kbd "p") 'ecc-dashboard-previous-line)
    (define-key map (kbd "RET") 'ecc-dashboard-visit-agent)
    (define-key map (kbd "c") 'ecc-dashboard-create-agent-interactive)
    (define-key map (kbd "d") 'ecc-dashboard-delete-agent-interactive)
    (define-key map (kbd "r") 'ecc-dashboard-rename-agent-interactive)
    (define-key map (kbd "s") 'ecc-dashboard-set-status-interactive)
    (define-key map (kbd "t") 'ecc-dashboard-add-tag-interactive)
    (define-key map (kbd "T") 'ecc-dashboard-remove-tag-interactive)
    (define-key map (kbd "P") 'ecc-dashboard-set-project-interactive)
    (define-key map (kbd "f") 'ecc-dashboard-filter-interactive)
    (define-key map (kbd "g") 'ecc-dashboard-refresh)
    (define-key map (kbd "C") 'ecc-dashboard-cancel-timer)
    (define-key map (kbd "S") 'ecc-dashboard-start-timer)
    (define-key map (kbd "q") 'ecc-dashboard-quit)
    map)
  "Keymap for Claude agent dashboard mode.")

(define-derived-mode ecc-dashboard-mode special-mode
  "Claude Dashboard"
  "Major mode for managing Claude agents.
\\{ecc-dashboard-mode-map}"
  :group 'emacs-claude-dashboard
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (setq-local line-spacing 0.2))

;; Dashboard Display Functions
;; ------------------------------

(defun ecc-dashboard-get-buffer ()
  "Get or create the dashboard buffer."
  (let ((buffer (get-buffer ecc-dashboard-buffer-name)))
    (unless buffer
      (setq buffer (get-buffer-create ecc-dashboard-buffer-name))
      (with-current-buffer buffer
        (ecc-dashboard-mode)))
    buffer))

;; Global handler for buffer killing to remove from dashboard

(defun ecc-dashboard-handle-killed-buffer ()
  "Handler for kill-buffer-hook to remove killed buffers from dashboard.
This function is added to the global kill-buffer-hook and will
automatically remove any Claude agent buffer from the dashboard
registry when it is killed."
  (let ((curr-buffer (current-buffer)))
    (when (and (derived-mode-p 'vterm-mode)
               (boundp 'ecc-buffer--buffer-to-uid))
      (let ((uid (and (hash-table-p ecc-buffer--buffer-to-uid)
                      (gethash curr-buffer ecc-buffer--buffer-to-uid))))
        (when uid
          ;; Clean up the UID registry
          (when
              (fboundp 'ecc-buffer-registry-unregister-buffer-with-uid)
            (ecc-buffer-registry-unregister-buffer-with-uid uid))

          ;; Also directly remove from hashtable for good measure
          (when (and (boundp 'ecc-buffer-registry-by-uid)
                     (hash-table-p ecc-buffer-registry-by-uid))
            (remhash uid ecc-buffer-registry-by-uid))

          ;; Update dashboard if visible
          (when (and (get-buffer ecc-dashboard-buffer-name)
                     (get-buffer-window ecc-dashboard-buffer-name
                                        'visible))
            (ecc-dashboard-refresh)))))))

;; Add the global hook during package initialization
(add-hook 'kill-buffer-hook #'ecc-dashboard-handle-killed-buffer)

(defun ecc-dashboard-show ()
  "Display the Claude agent dashboard."
  (interactive)
  (let ((buffer (ecc-dashboard-get-buffer)))
    ;; Ensure registry has latest buffer information before refreshing
    (when (fboundp 'ecc-dashboard-ensure-registry-updated)
      (ecc-dashboard-ensure-registry-updated))

    (ecc-dashboard-refresh)

    ;; Use display-buffer with specific display action to ensure persistence
    (let
        ((window
          (display-buffer buffer '(display-buffer-use-some-window))))
      (when window
        (select-window window)

        ;; Ensure this window isn't easily replaced by other buffers
        (set-window-dedicated-p window t)))

    ;; Start auto-update timer if enabled and no errors were reported
    (when (and ecc-dashboard-auto-update
               (null ecc-dashboard--update-timer))
      (condition-case err
          (progn
            (setq ecc-dashboard--update-timer
                  (run-with-timer ecc-dashboard-update-interval
                                  ecc-dashboard-update-interval
                                  #'ecc-dashboard-refresh-if-visible))
            (message "Dashboard auto-update timer started"))
        (error
         (message "Failed to start dashboard timer: %s"
                  (error-message-string err)))))

    ;; Mark as initialized
    (setq ecc-dashboard--initialized t)

    ;; Add dashboard buffer-kill hook to ensure proper cleanup of timer
    (with-current-buffer buffer
      (add-hook 'kill-buffer-hook
                (lambda ()
                  (when ecc-dashboard--update-timer
                    (cancel-timer ecc-dashboard--update-timer)
                    (setq ecc-dashboard--update-timer nil)))
                nil t))))

(defun ecc-dashboard-refresh-if-visible (&optional _arg)
  "Refresh the dashboard if it's visible.
Optional ARG is ignored, allowing this to be used as a timer function."
  (condition-case err
      (let ((dashboard-buffer (get-buffer ecc-dashboard-buffer-name)))
        (when (and dashboard-buffer
                   (get-buffer-window dashboard-buffer 'visible))
          (ecc-dashboard-refresh)))
    (error
     (message "Error in dashboard refresh timer: %s"
              (error-message-string err))
     (when ecc-dashboard--update-timer
       (cancel-timer ecc-dashboard--update-timer)
       (setq ecc-dashboard--update-timer nil)))))

(defun ecc-dashboard-refresh ()
  "Refresh the dashboard display.
Automatically cleans up stale buffer entries before refreshing."
  (interactive)
  ;; FIRST: Clean up stale buffers automatically and forcefully
  (when (fboundp 'ecc-dashboard-cleanup-stale-buffers)
    (message "Running stale buffer cleanup...")
    (let ((cleaned (ecc-dashboard-cleanup-stale-buffers)))
      (when (> cleaned 0)
        (message "Cleaned up %d stale entries" cleaned))))

  ;; THEN: Ensure registry has latest buffer information before refreshing
  (when (fboundp 'ecc-dashboard-ensure-registry-updated)
    (ecc-dashboard-ensure-registry-updated))

  ;; VERIFY: Double-check that we're not showing stale buffers
  (let ((verify-count 0))
    (maphash (lambda (uid metadata)
               (when metadata
                 (let ((buf (and metadata (gethash 'buffer metadata))))
                   (when (or (null buf)
                             (not (and (bufferp buf) (buffer-live-p buf))))
                     (remhash uid ecc-buffer-registry-by-uid)
                     (setq verify-count (1+ verify-count))))))
             ecc-buffer-registry-by-uid)
    (when (> verify-count 0)
      (message "Verification cleanup: removed additional %d entries"
               verify-count)))

  ;; FINALLY: Display the dashboard with clean data
  (let ((buffer (ecc-dashboard-get-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (line (line-number-at-pos (point))))
        (erase-buffer)
        (ecc-dashboard--insert-header)
        (ecc-dashboard--insert-agents)
        (ecc-dashboard--insert-footer)
        (goto-char (point-min))
        (forward-line (1- line)))
      (set-buffer-modified-p nil))))

;; Helper functions for Dashboard display
;; ------------------------------

(defun ecc-dashboard--get-filtered-agents ()
  "Get filtered and sorted list of agent UIDs.
Applies current filter if any, otherwise gets all agents."
  (let ((uids
         (if ecc-dashboard--filter
             (let
                 ((filter-type (plist-get ecc-dashboard--filter :type))
                  (filter-value
                   (plist-get ecc-dashboard--filter :value)))
               (cond
                ((eq filter-type 'status)
                 (ecc-dashboard-filter-by-status filter-value))
                ((eq filter-type 'project)
                 (ecc-dashboard-filter-by-project filter-value))
                ((eq filter-type 'tag)
                 (ecc-dashboard-filter-by-tag filter-value))
                (t (ecc-dashboard-filter-agents
                    (plist-get ecc-dashboard--filter :predicate)))))
           ;; No filter - get all UIDs
           (let ((all-uids nil))
             (maphash (lambda (uid _metadata) (push uid all-uids))
                      ecc-buffer-registry-by-uid)
             all-uids))))

    ;; Double check to include only UIDs with live buffers
    (setq uids (seq-filter
                (lambda (uid)
                  (let*
                      ((metadata
                        (ecc-buffer-registry-get-metadata uid))
                       (buffer
                        (and metadata (gethash 'buffer metadata))))
                    (if (and buffer (buffer-live-p buffer))
                        t
                      ;; If buffer is not live, remove it from registry immediately
                      (when
                          (and uid
                               (boundp 'ecc-buffer-registry-by-uid))
                        (remhash uid ecc-buffer-registry-by-uid)
                        (message
                         "Removed stale buffer from registry during filtering: %s"
                         uid))
                      nil)))
                uids))

    ;; Sort the UIDs
    (ecc-dashboard--sort-agents uids)))

(defun ecc-dashboard--sort-agents (uids)
  "Sort agent UIDs according to dashboard settings."
  (sort uids
        (lambda (uid1 uid2)
          (let ((metadata1 (ecc-buffer-registry-get-metadata uid1))
                (metadata2 (ecc-buffer-registry-get-metadata uid2)))
            (cond
             ;; Sort by last access (newest first)
             ((eq ecc-dashboard-sort-order 'last-access)
              (let ((time1 (gethash 'last-access metadata1))
                    (time2 (gethash 'last-access metadata2)))
                (and time1 time2 (time-less-p time2 time1))))

             ;; Sort by name (alphabetically)
             ((eq ecc-dashboard-sort-order 'name)
              (let ((name1 (gethash 'name metadata1))
                    (name2 (gethash 'name metadata2)))
                (and name1 name2 (string< name1 name2))))

             ;; Sort by status
             ((eq ecc-dashboard-sort-order 'status)
              (let ((status1 (gethash 'status metadata1))
                    (status2 (gethash 'status metadata2)))
                (and status1 status2 (string< (symbol-name status1)
                                              (symbol-name status2)))))

             ;; Sort by creation time (newest first)
             ((eq ecc-dashboard-sort-order 'creation-time)
              (let ((time1 (gethash 'creation-time metadata1))
                    (time2 (gethash 'creation-time metadata2)))
                (and time1 time2 (time-less-p time2 time1))))

             ;; Sort by project
             ((eq ecc-dashboard-sort-order 'project)
              (let ((proj1 (gethash 'project metadata1))
                    (proj2 (gethash 'project metadata2)))
                (and proj1 proj2 (string< (symbol-name proj1)
                                          (symbol-name proj2)))))

             ;; Default sort by last access
             (t
              (let ((time1 (gethash 'last-access metadata1))
                    (time2 (gethash 'last-access metadata2)))
                (and time1 time2 (time-less-p time2 time1)))))))))

(defun ecc-dashboard--count-live-agents ()
  "Count the number of agents with live buffers."
  (let ((live-count 0))
    (maphash (lambda (_uid metadata)
               (let ((buffer (gethash 'buffer metadata)))
                 (when (and buffer (buffer-live-p buffer))
                   (setq live-count (1+ live-count)))))
             ecc-buffer-registry-by-uid)
    live-count))

(defun ecc-dashboard--insert-header ()
  "Insert dashboard header."
  (insert
   (propertize "Claude Agents Dashboard\n" 'face
               'ecc-dashboard-header-face))

  ;; Count only live agents
  (let ((live-count (ecc-dashboard--count-live-agents))
        (total-count (hash-table-count ecc-buffer-registry-by-uid)))
    (insert (format "Live agents: %d" live-count))
    (when (< live-count total-count)
      (insert
       (format " (cleaning up %d stale entries)"
               (- total-count live-count))))
    (insert "\n\n"))

  ;; Insert filter info if active
  (when ecc-dashboard--filter
    (insert (format "Filter: %s\n\n"
                    (plist-get ecc-dashboard--filter :description))))

  ;; Insert column headers
  (let ((headers
         (list
          (cons "ID" 4)
          (cons "Name" 20))))

    ;; Add optional columns based on settings
    (when ecc-dashboard-show-status
      (push (cons "Status" 10) (cdr (last headers))))

    (when ecc-dashboard-show-projects
      (push (cons "Project" 12) (cdr (last headers))))

    (when ecc-dashboard-show-timestamps
      (push (cons "Last Access" 20) (cdr (last headers))))

    (when ecc-dashboard-show-tags
      (push (cons "Tags" 30) (cdr (last headers))))

    ;; Insert headers
    (let ((header-string ""))
      (dolist (header headers)
        (let ((name (car header))
              (width (cdr header)))
          (setq header-string
                (concat header-string
                        (format (format "%%-%ds " width) name)))))

      (insert (propertize header-string 'face 'font-lock-comment-face))))

  (insert "\n" (make-string (window-width) ?-) "\n"))

(defun ecc-dashboard--insert-agents ()
  "Insert agent information."
  (let ((agents (ecc-dashboard--get-filtered-agents))
        (counter 1))
    (setq ecc-dashboard--current-agents nil)

    (if (null agents)
        (insert
         "\n   No agents found. Press 'c' to create a new agent.\n")

      (dolist (agent-uid agents)
        (let* ((metadata (ecc-buffer-registry-get-metadata agent-uid))
               (buffer (gethash 'buffer metadata))
               (name (gethash 'name metadata (buffer-name buffer)))
               (status
                (ecc-buffer-metadata-get agent-uid 'status 'idle))
               (project
                (ecc-buffer-metadata-get agent-uid 'project "-"))
               (tags (ecc-buffer-metadata-get agent-uid 'tags nil))
               (last-access
                (ecc-buffer-metadata-get agent-uid 'last-access nil))
               (last-access-str (if last-access
                                    (format-time-string
                                     "%Y-%m-%d %H:%M:%S" last-access)
                                  "-"))
               (tags-str (if tags
                             (mapconcat #'symbol-name tags ", ")
                           "-"))
               (status-face
                (cdr (assq status ecc-buffer-status-types))))

          ;; Store UID in our list
          (push agent-uid ecc-dashboard--current-agents)

          ;; Create line start with ID and name
          (insert (propertize (format "%-4d " counter)
                              'ecc-dashboard-uid agent-uid
                              'ecc-dashboard-line-type 'agent))

          (insert (propertize (format "%-20s "
                                      (truncate-string-to-width name
                                                                20
                                                                0 nil
                                                                "..."))
                              'ecc-dashboard-uid agent-uid))

          ;; Add optional columns based on settings
          (when ecc-dashboard-show-status
            (insert (propertize (format "%-10s " status)
                                'face status-face
                                'ecc-dashboard-uid agent-uid)))

          (when ecc-dashboard-show-projects
            (insert (propertize (format "%-12s "
                                        (truncate-string-to-width
                                         (if (symbolp project)
                                             (symbol-name project)
                                           (format "%s" project))
                                         12 0 nil "..."))
                                'ecc-dashboard-uid agent-uid)))

          (when ecc-dashboard-show-timestamps
            (insert (propertize (format "%-20s " last-access-str)
                                'ecc-dashboard-uid agent-uid)))

          (when ecc-dashboard-show-tags
            (insert (propertize (format "%-30s"
                                        (truncate-string-to-width
                                         tags-str 30 0 nil "..."))
                                'ecc-dashboard-uid agent-uid)))

          (insert "\n")
          (setq counter (1+ counter)))))

    ;; Reverse the list to match display order
    (setq ecc-dashboard--current-agents
          (nreverse ecc-dashboard--current-agents))))

(defun ecc-dashboard--insert-footer ()
  "Insert dashboard footer with command legends."
  (insert (make-string (window-width) ?-) "\n")

  ;; Combined commands and descriptions in a more compact format
  (insert
   (propertize "Commands: " 'face '(:foreground "cyan" :weight bold)))

  ;; First row of commands with descriptions
  (insert "[")
  (insert-text-button "n/p" 'follow-link t 'help-echo
                      "Next/Previous line")
  (insert " Navigate] [")
  (insert-text-button "RET" 'follow-link t 'help-echo
                      "Visit selected agent")
  (insert " Visit agent] [")
  (insert-text-button "c" 'follow-link t 'help-echo "Create new agent")
  (insert " Create agent] [")
  (insert-text-button "d" 'follow-link t 'help-echo "Delete agent")
  (insert " Delete agent]\n")

  ;; Second row of commands
  (insert (make-string (1+ (length "Commands: ")) ? ))
  (insert "[")
  (insert-text-button "r" 'follow-link t 'help-echo "Rename agent")
  (insert " Rename agent] [")
  (insert-text-button "s" 'follow-link t 'help-echo "Set status")
  (insert " Set status] [")
  (insert-text-button "t/T" 'follow-link t 'help-echo "Add/Remove tag")
  (insert " Add/Remove tag] [")
  (insert-text-button "P" 'follow-link t 'help-echo "Set project")
  (insert " Set project]\n")

  ;; Third row of commands
  (insert (make-string (1+ (length "Commands: ")) ? ))
  (insert "[")
  (insert-text-button "f" 'follow-link t 'help-echo "Filter agents")
  (insert " Filter agents] [")
  (insert-text-button "g" 'follow-link t 'help-echo
                      "Refresh dashboard")
  (insert " Refresh] [")
  (insert-text-button "C/S" 'follow-link t 'help-echo
                      "Cancel/Start timer")
  (insert " Timer controls] [")
  (insert-text-button "q" 'follow-link t 'help-echo "Quit dashboard")
  (insert " Quit]\n")

  ;; Add timer status
  (insert (make-string (window-width) ?-) "\n")
  (insert (format "Timer status: %s"
                  (if ecc-dashboard--update-timer
                      (propertize "ACTIVE" 'face
                                  '(:foreground "green" :weight bold))
                    (propertize "INACTIVE" 'face '(:foreground "red")))))
  (insert "\n"))

;; Dashboard Navigation
;; ------------------------------

(defun ecc-dashboard-next-line ()
  "Move to next agent line."
  (interactive)
  (forward-line 1)
  (when (or (eobp)
            (not (get-text-property (point) 'ecc-dashboard-line-type)))
    (if (eobp)
        (forward-line -1)
      (while (and (not (bobp))
                  (not
                   (get-text-property (point) 'ecc-dashboard-line-type)))
        (forward-line -1)))))

(defun ecc-dashboard-previous-line ()
  "Move to previous agent line."
  (interactive)
  (forward-line -1)
  (when (or (bobp)
            (not (get-text-property (point) 'ecc-dashboard-line-type)))
    (if (bobp)
        (forward-line 1)
      (while (and (not (eobp))
                  (not
                   (get-text-property (point) 'ecc-dashboard-line-type)))
        (forward-line 1)))))

(defun ecc-dashboard-get-current-uid ()
  "Get the UID of the agent at current line."
  (get-text-property (point) 'ecc-dashboard-uid))

;; Interactive Commands
;; ------------------------------

(defun ecc-dashboard-visit-agent ()
  "Visit the selected agent's buffer."
  (interactive)
  (let ((uid (ecc-dashboard-get-current-uid)))
    (when uid
      (let ((buffer (ecc-buffer-get-buffer-by-uid uid)))
        (if (and buffer (buffer-live-p buffer))
            (progn
              (ecc-buffer-set-current-buffer-by-uid uid)
              (pop-to-buffer buffer)
              (ecc-buffer-registry-update-timestamp uid))
          ;; If buffer is not live, clean up and refresh dashboard
          (when
              (fboundp 'ecc-buffer-registry-unregister-buffer-with-uid)
            (ecc-buffer-registry-unregister-buffer-with-uid uid)
            (ecc-dashboard-refresh)
            (message "Buffer for agent not available. Entry removed.")))))))

(defun ecc-dashboard-delete-agent-interactive ()
  "Interactively delete the selected agent."
  (interactive)
  (let ((uid (ecc-dashboard-get-current-uid)))
    (when uid
      (let* ((metadata (and (fboundp 'ecc-buffer-registry-get-metadata)
                            (ecc-buffer-registry-get-metadata uid)))
             (name (and metadata (gethash 'name metadata)))
             (prompt (format "Really delete agent '%s'? " 
                             (or name "unknown"))))
        (when (yes-or-no-p prompt)
          (condition-case err
              (progn
                (ecc-dashboard-delete-agent uid)
                (ecc-dashboard-refresh)
                (message "Deleted agent '%s'" (or name "unknown")))
            (error 
             (message "Error deleting agent: %s" (error-message-string err))
             ;; Try direct registry removal as a last resort
             (when (boundp 'ecc-buffer-registry-by-uid)
               (remhash uid ecc-buffer-registry-by-uid)
               (ecc-dashboard-refresh)
               (message "Forcibly removed agent from registry")))))))))

(defun ecc-dashboard-rename-agent-interactive ()
  "Interactively rename the selected agent."
  (interactive)
  (let ((uid (ecc-dashboard-get-current-uid)))
    (when uid
      (let* ((metadata (ecc-buffer-registry-get-metadata uid))
             (current-name (gethash 'name metadata))
             (new-name (read-string "New name: " current-name)))
        (when (and new-name (not (string= new-name "")))
          (ecc-dashboard-rename-agent uid new-name)
          (ecc-dashboard-refresh)
          (message "Renamed agent to '%s'" new-name))))))

(defun ecc-dashboard-set-status-interactive ()
  "Interactively set status for the selected agent."
  (interactive)
  (let ((uid (ecc-dashboard-get-current-uid)))
    (when uid
      (let* ((status-options (mapcar #'symbol-name
                                     (mapcar #'car
                                             ecc-buffer-status-types)))
             (completion-ignore-case t)
             (status
              (completing-read "Set status: " status-options nil t)))
        (when status
          (ecc-dashboard-set-agent-status uid status)
          (ecc-dashboard-refresh)
          (message "Status set to '%s'" status))))))

(defun ecc-dashboard-set-project-interactive ()
  "Interactively set project for the selected agent."
  (interactive)
  (let ((uid (ecc-dashboard-get-current-uid)))
    (when uid
      (let* ((projects (ecc-dashboard-get-all-projects))
             (project (completing-read "Set project: " projects)))
        (when (and project (not (string= project "")))
          (ecc-dashboard-set-agent-project uid project)
          (ecc-dashboard-refresh)
          (message "Project set to '%s'" project))))))

(defun ecc-dashboard-add-tag-interactive ()
  "Interactively add a tag to the selected agent."
  (interactive)
  (let ((uid (ecc-dashboard-get-current-uid)))
    (when uid
      (let* ((existing-tags (ecc-dashboard-get-all-tags))
             (tag-string (completing-read "Add tag: " existing-tags)))
        (when (and tag-string (not (string= tag-string "")))
          (ecc-dashboard-add-agent-tag uid tag-string)
          (ecc-dashboard-refresh)
          (message "Added tag '%s'" tag-string))))))

(defun ecc-dashboard-remove-tag-interactive ()
  "Interactively remove a tag from the selected agent."
  (interactive)
  (let ((uid (ecc-dashboard-get-current-uid)))
    (when uid
      (let* ((current-tags (ecc-buffer-metadata-get uid 'tags nil))
             (tag-options (mapcar #'symbol-name current-tags))
             (tag-string
              (completing-read "Remove tag: " tag-options nil t)))
        (when (and tag-string (not (string= tag-string "")))
          (ecc-dashboard-remove-agent-tag uid tag-string)
          (ecc-dashboard-refresh)
          (message "Removed tag '%s'" tag-string))))))

(defun ecc-dashboard-filter-interactive ()
  "Interactively set a filter for the dashboard."
  (interactive)
  (let* ((filter-types '("None" "Status" "Project" "Tag" "Name"))
         (filter-type
          (completing-read "Filter by: " filter-types nil t)))
    (if (string= filter-type "None")
        (progn
          (setq ecc-dashboard--filter nil)
          (ecc-dashboard-refresh)
          (message "Filter cleared"))
      (let*
          ((prompt
            (format "Filter value (%s): " (downcase filter-type)))
           options
           filter-value)
        ;; Set options based on filter type
        (setq options
              (cond
               ((string= filter-type "Status")
                (mapcar #'symbol-name (ecc-dashboard-get-all-statuses)))
               ((string= filter-type "Project")
                (mapcar (lambda (proj)
                          (if (symbolp proj)
                              (symbol-name proj)
                            (format "%s" proj)))
                        (ecc-dashboard-get-all-projects)))
               ((string= filter-type "Tag")
                (mapcar #'symbol-name (ecc-dashboard-get-all-tags)))
               (t nil)))

        ;; Get filter value
        (setq filter-value
              (if options
                  (completing-read prompt options nil
                                   (string= filter-type "Tag"))
                (read-string prompt)))

        ;; Set the filter
        (when (and filter-value (not (string= filter-value "")))
          (setq ecc-dashboard--filter
                (cond
                 ((string= filter-type "Status")
                  (list :type 'status
                        :value (intern filter-value)
                        :description
                        (format "Status = %s" filter-value)))
                 ((string= filter-type "Project")
                  (list :type 'project
                        :value (intern filter-value)
                        :description
                        (format "Project = %s" filter-value)))
                 ((string= filter-type "Tag")
                  (list :type 'tag
                        :value (intern filter-value)
                        :description
                        (format "Has tag '%s'" filter-value)))
                 ((string= filter-type "Name")
                  (list :type 'name
                        :value filter-value
                        :predicate (lambda (_uid metadata)
                                     (let
                                         ((name
                                           (gethash 'name metadata)))
                                       (and name
                                            (string-match-p
                                             filter-value name))))
                        :description
                        (format "Name matches '%s'" filter-value)))
                 (t nil)))

          (ecc-dashboard-refresh)
          (message "Filter applied: %s"
                   (plist-get ecc-dashboard--filter :description)))))))

(defun ecc-dashboard-cancel-timer ()
  "Cancel the dashboard update timer."
  (interactive)
  (when ecc-dashboard--update-timer
    (cancel-timer ecc-dashboard--update-timer)
    (setq ecc-dashboard--update-timer nil)
    (message "Dashboard auto-update timer canceled")))

(defun ecc-dashboard-start-timer ()
  "Start the dashboard update timer."
  (interactive)
  (when (and (not ecc-dashboard--update-timer)
             (get-buffer ecc-dashboard-buffer-name))
    (setq ecc-dashboard--update-timer
          (run-with-timer ecc-dashboard-update-interval
                          ecc-dashboard-update-interval
                          #'ecc-dashboard-refresh-if-visible))
    (message "Dashboard auto-update timer started")))

(defun ecc-dashboard-quit ()
  "Quit the dashboard and properly clean up the buffer."
  (interactive)
  (ecc-dashboard-cancel-timer)
  (let ((buffer (get-buffer ecc-dashboard-buffer-name)))
    (when buffer
      (quit-window)
      (kill-buffer buffer))))

(defun ecc-dashboard-sort-toggle ()
  "Toggle dashboard sort order."
  (interactive)
  (let* ((orders '(last-access name status creation-time project))
         (current-pos (cl-position ecc-dashboard-sort-order orders))
         (next-pos (mod (1+ (or current-pos 0)) (length orders))))
    (setq ecc-dashboard-sort-order (nth next-pos orders))
    (ecc-dashboard-refresh)
    (message "Sort order: %s" ecc-dashboard-sort-order)))

(provide 'ecc-dashboard-ui)

(when (not load-file-name)
  (message "ecc-dashboard-ui.el loaded: %s"
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

;;; ecc-dashboard-ui.el ends here