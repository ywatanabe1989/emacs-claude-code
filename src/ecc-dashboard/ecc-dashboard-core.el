;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-14 11:50:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-dashboard/ecc-dashboard-core.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:
;;
;; Core functionality for Claude agent dashboard.
;; This file provides the core agent creation and management functions.
;;

;;; Code:

(require 'ecc-buffer-uid)
(require 'ecc-buffer-registry-uid)
(require 'ecc-buffer-metadata)
(require 'ecc-dashboard-variables)

;; Core Agent Creation and Management
;; ------------------------------

(defun ecc-dashboard-ensure-registry-updated ()
  "Ensure the UID registry has all registered buffers.
This makes sure any buffers created with `ecc-buffer-create` are properly
reflected in the dashboard, even if they were created immediately before
opening the dashboard."
  (dolist (buffer-pair ecc-buffer-registered-buffers-alist)
    (let ((buffer (car buffer-pair)))
      (when (and buffer (buffer-live-p buffer))
        ;; Ensure the buffer has a UID and is registered in the UID-based system
        (unless (gethash buffer ecc-buffer--buffer-to-uid)
          (ecc-buffer-registry-register-buffer-with-uid buffer))))))

;;;###autoload
(defun ecc-dashboard-create-agent (&optional options)
  "Create a new Claude agent.
OPTIONS is an alist of initial metadata properties.
NOTE: Status should be set separately with `ecc-dashboard-set-agent-status`
to ensure proper validation."
  (interactive)
  (let* ((name (or (cdr (assq 'name options))
                  (format "*CLAUDE-CODE-%02d*" 
                         (1+ (hash-table-count ecc-buffer-registry-by-uid)))))
         (buffer (get-buffer-create name))
         (safe-options (when options (copy-alist options)))
         uid)
    
    ;; Ensure we don't have status in the options (it needs special handling)
    (when (and options (assq 'status options))
      (message "Warning: Status in options will be ignored. Use ecc-dashboard-set-agent-status instead.")
      (setq safe-options (assq-delete-all 'status safe-options)))
    
    ;; Initialize the buffer - only set vterm-mode if needed
    (with-current-buffer buffer
      ;; Check if this is already a vterm buffer before setting the mode
      (unless (eq major-mode 'vterm-mode)
        (when (fboundp 'vterm-mode)
          (vterm-mode)))
      ;; Set original name safely (without changing major mode)
      (unless (local-variable-p 'ecc-original-name)
        (set (make-local-variable 'ecc-original-name) name)))
    
    ;; Register with UID system
    (setq uid (ecc-buffer-registry-register-buffer-with-uid buffer))
    
    ;; Set provided metadata (without status)
    (when safe-options
      (maphash (lambda (key value)
                 (ecc-buffer-metadata-set uid key value))
               safe-options))
    
    ;; For alist options
    (when (and options (consp (car-safe options)))
      (dolist (pair options)
        (when (consp pair)
          (let ((key (car pair))
                (value (cdr pair)))
            (unless (eq key 'status) ; Skip status which needs special handling
              (ecc-buffer-metadata-set uid key value))))))
    
    ;; Set creation time if not provided
    (unless (ecc-buffer-metadata-get uid 'creation-time)
      (ecc-buffer-metadata-set uid 'creation-time (current-time)))
    
    ;; Ensure name is set
    (ecc-buffer-metadata-set uid 'name name)
    
    ;; Debug output
    (message "Created agent with UID %s, buffer %s" uid buffer)
    
    ;; Return the new buffer's UID
    uid))

(defun ecc-dashboard-create-agent-interactive ()
  "Interactively create a new Claude agent."
  (interactive)
  (let* ((name (read-string "Agent name: " (format "*CLAUDE-CODE-%02d*" 
                                                 (1+ (hash-table-count 
                                                      ecc-buffer-registry-by-uid)))))
         (project (completing-read "Project: " 
                                 (if (fboundp 'ecc-dashboard-get-all-projects)
                                     (ecc-dashboard-get-all-projects)
                                   '("-"))
                                 nil nil nil nil "-"))
         (status-options (mapcar #'symbol-name 
                               (mapcar #'car ecc-buffer-status-types)))
         (status (completing-read "Status: " status-options
                               nil t nil nil "active"))
         (tags-str (read-string "Tags (comma separated): "))
         (tags (when (not (string= tags-str ""))
                 (mapcar #'intern
                         (split-string tags-str "," t "\\s-+"))))
         (options `((name . ,name)
                    (project . ,(intern project))
                    ,@(when tags `((tags . ,tags)))))
         (uid (ecc-dashboard-create-agent options)))
    (when uid
      (ecc-buffer-metadata-set-status uid (intern status))
      (message "Created new agent '%s'" name)
      uid)))

(provide 'ecc-dashboard-core)

;;; ecc-dashboard-core.el ends here