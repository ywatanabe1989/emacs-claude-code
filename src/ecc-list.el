;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-28 06:02:30>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-list.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;; 1. Dependencies
;; ----------------------------------------
(require 'ecc-debug)
(require 'ecc-state-detection)
(require 'ecc-auto-response)


;; 2. Configuration
;; ----------------------------------------
(defcustom --ecc-buffer-list-auto-refresh t
  "Whether to automatically refresh the buffer list."
  :type 'boolean
  :group 'ecc)

(defcustom --ecc-buffer-list-refresh-interval 2.0
  "Interval in seconds for automatic buffer list refresh."
  :type 'number
  :group 'ecc)


;; 3. Variables
;; ----------------------------------------
(defvar --ecc-buffer-list--timer nil
  "Timer for automatic buffer list refresh.")

(defvar --ecc-buffer-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") '--ecc-buffer-list-select)
    (define-key map (kbd "SPC") '--ecc-buffer-list-select)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "g") 'ecc-list-buffers)
    (define-key map (kbd "a") '--ecc-buffer-list-toggle-auto-response)
    (define-key map (kbd "r") '--ecc-buffer-list-toggle-auto-refresh)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    map)
  "Keymap for --ecc-buffer-list-mode.")


;; 4. Main Entry Points
;; ----------------------------------------
;;;###autoload
(defun ecc-list-buffers ()
  "Display a list of vterm buffers with their auto-response status.
Shows all vterm-mode buffers with their auto-response status.

In the buffer list:
- RET/SPC: Jump to buffer
- a: Toggle auto-response for buffer
- g: Refresh list
- q: Quit"
  (interactive)
  (let ((vterm-buffers '())
        (registered-buffers (--ecc-auto-response-get-registered-buffers)))
    
    ;; Collect vterm buffers
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (derived-mode-p 'vterm-mode)
            (push buffer vterm-buffers)))))
    
    ;; Create or switch to the buffer list
    (let ((list-buffer (get-buffer-create "*ECC Buffers*")))
      (with-current-buffer list-buffer
        ;; Cancel any existing timer before recreating content
        (--ecc-buffer-list--cancel-refresh-timer)
        (let ((inhibit-read-only t)
              (current-line (line-number-at-pos)))
          (erase-buffer)
          
          (insert "ECC Claude Buffer List\n")
          (insert "=====================\n\n")
          
          (if vterm-buffers
              (progn
                (insert (format "%-30s %-15s %-12s %s\n"
                                "Buffer Name" "Auto-Response" "Last Sent" "State"))
                (insert (format "%-30s %-15s %-12s %s\n"
                                "─────────────────────────────" 
                                "──────────────"
                                "────────────"
                                "─────"))
                
                ;; Sort buffers by name
                (setq vterm-buffers (sort vterm-buffers
                                          (lambda (a b)
                                            (string< (buffer-name a)
                                                     (buffer-name b)))))
                
                (dolist (buffer vterm-buffers)
                  (let* ((name (buffer-name buffer))
                         (auto-enabled (with-current-buffer buffer
                                         --ecc-auto-response--enabled))
                         (current-state (with-current-buffer buffer
                                          (--ecc-state-detection-detect)))
                         (state-str (if current-state
                                        (--ecc-state-detection-get-name current-state)
                                      "None"))
                         (last-time (with-current-buffer buffer
                                      --ecc-auto-response--last-time))
                         (time-str (if (and last-time (> last-time 0))
                                       (format-time-string "%H:%M:%S" last-time)
                                     "Never")))
                    (insert (propertize
                             (format "%-30s %-15s %-12s %s\n"
                                     (if (> (length name) 29)
                                         (concat (substring name 0 26) "...")
                                       name)
                                     (if auto-enabled "Enabled" "Disabled")
                                     time-str
                                     state-str)
                             '--ecc-buffer buffer)))))
            (insert "No vterm buffers found.\n"))
          
          (insert "\nCommands:\n")
          (insert "  RET/SPC  - Jump to buffer\n")
          (insert "  a        - Toggle auto-response\n")
          (insert "  g        - Refresh list\n")
          (insert "  r        - Toggle auto-refresh\n")
          (insert "  q        - Quit\n")
          (insert "  n/p      - Next/previous line\n")
          
          (when --ecc-buffer-list-auto-refresh
            (insert (format "\nAuto-refresh: ON (every %.1fs)\n" 
                            --ecc-buffer-list-refresh-interval)))
          
          ;; Enable the mode
          (--ecc-buffer-list-mode)
          (goto-char (point-min))
          ;; Try to restore position
          (when (and (> current-line 0)
                     (<= current-line (line-number-at-pos (point-max))))
            (goto-line current-line)))
        
        ;; Set up auto-refresh timer if enabled
        (when --ecc-buffer-list-auto-refresh
          (--ecc-buffer-list--setup-refresh-timer)))
      
      ;; Display the buffer
      (pop-to-buffer list-buffer))))


;; 5. Core Functions
;; ----------------------------------------
(define-derived-mode --ecc-buffer-list-mode special-mode "--ECC-Buffers"
  "Major mode for browsing Claude vterm buffers with auto-response status.
\\{--ecc-buffer-list-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (add-hook 'kill-buffer-hook '--ecc-buffer-list--cleanup nil t))

(defun --ecc-buffer-list-select ()
  "Select the buffer on the current line."
  (interactive)
  (let ((buffer (--ecc-buffer-list-get-buffer-at-point)))
    (when buffer
      (if (buffer-live-p buffer)
          (progn
            (switch-to-buffer buffer)
            (--ecc-debug-message "Switched to buffer: %s" (buffer-name buffer)))
        (message "Buffer no longer exists")))))

(defun --ecc-buffer-list-toggle-auto-response ()
  "Toggle auto-response for the buffer on the current line."
  (interactive)
  (let ((buffer (--ecc-buffer-list-get-buffer-at-point)))
    (when buffer
      (if (buffer-live-p buffer)
          (progn
            (with-current-buffer buffer
              (--ecc-auto-response-toggle-buffer))
            (ecc-list-buffers)
            (--ecc-debug-message "Toggled auto-response for: %s" (buffer-name buffer)))
        (message "Buffer no longer exists")))))

(defun --ecc-buffer-list-toggle-auto-refresh ()
  "Toggle automatic refresh of the buffer list."
  (interactive)
  (setq --ecc-buffer-list-auto-refresh (not --ecc-buffer-list-auto-refresh))
  (if --ecc-buffer-list-auto-refresh
      (progn
        (--ecc-buffer-list--setup-refresh-timer)
        (message "Auto-refresh enabled (interval: %.1fs)" 
                 --ecc-buffer-list-refresh-interval))
    (--ecc-buffer-list--cancel-refresh-timer)
    (message "Auto-refresh disabled"))
  (ecc-list-buffers))


;; 6. Helper/Utility Functions
;; ----------------------------------------
(defun --ecc-buffer-list-get-buffer-at-point ()
  "Get the buffer object from the current line."
  (get-text-property (line-beginning-position) '--ecc-buffer))

(defun --ecc-buffer-list--setup-refresh-timer ()
  "Set up the automatic refresh timer for the buffer list."
  (--ecc-buffer-list--cancel-refresh-timer)
  (when (and --ecc-buffer-list-auto-refresh
             (eq major-mode '--ecc-buffer-list-mode))
    (setq --ecc-buffer-list--timer
          (run-with-timer --ecc-buffer-list-refresh-interval
                          --ecc-buffer-list-refresh-interval
                          '--ecc-buffer-list--refresh-if-visible
                          (current-buffer)))
    (--ecc-debug-message "Buffer list auto-refresh timer started")))

(defun --ecc-buffer-list--cancel-refresh-timer ()
  "Cancel the automatic refresh timer."
  (when --ecc-buffer-list--timer
    (cancel-timer --ecc-buffer-list--timer)
    (setq --ecc-buffer-list--timer nil)
    (--ecc-debug-message "Buffer list auto-refresh timer cancelled")))

(defun --ecc-buffer-list--refresh-if-visible (buffer)
  "Refresh BUFFER if it's still visible."
  (save-window-excursion
  (when (and (buffer-live-p buffer)
             (get-buffer-window buffer))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (pos (point)))
        (ecc-list-buffers)
        (goto-char pos))))))

(defun --ecc-buffer-list--cleanup ()
  "Clean up when the buffer list is killed."
  (--ecc-buffer-list--cancel-refresh-timer))


(provide 'ecc-list)

(when
    (not load-file-name)
  (message "ecc-list.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))