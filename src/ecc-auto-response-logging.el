;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-06-04 07:00:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-auto-response-logging.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ecc-debug)
(require 'cl-lib)

;; 1. Configuration
;; ----------------------------------------
(defcustom ecc-auto-response-log-buffer-name "*ECC Auto-Response Log*"
  "Name of the buffer for auto-response logging."
  :type 'string
  :group 'ecc)

(defcustom ecc-auto-response-log-level 'info
  "Logging level for auto-response.
Available levels: debug, info, warn, error."
  :type '(choice (const :tag "Debug" debug)
                 (const :tag "Info" info)
                 (const :tag "Warning" warn)
                 (const :tag "Error" error))
  :group 'ecc)

(defcustom ecc-auto-response-log-max-entries 1000
  "Maximum number of log entries to keep."
  :type 'integer
  :group 'ecc)

;; 2. Variables
;; ----------------------------------------
(defvar --ecc-auto-response-log-entries nil
  "List of log entries.")

(defvar --ecc-auto-response-log-levels
  '((debug . 0)
    (info  . 1)
    (warn  . 2)
    (error . 3))
  "Log level hierarchy.")

;; 3. Main logging functions
;; ----------------------------------------
(defun ecc-auto-response-log (level message &rest args)
  "Log a MESSAGE at LEVEL with optional ARGS.

Example:
  (ecc-auto-response-log 'info \"State detected: %s\" state)"
  (when (>= (cdr (assq level --ecc-auto-response-log-levels))
            (cdr (assq ecc-auto-response-log-level --ecc-auto-response-log-levels)))
    (let* ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S.%3N"))
           (buffer-name (buffer-name))
           (formatted-message (apply #'format message args))
           (entry (list :timestamp timestamp
                       :level level
                       :buffer buffer-name
                       :message formatted-message)))
      ;; Add to entries
      (push entry --ecc-auto-response-log-entries)
      ;; Trim if too many
      (when (> (length --ecc-auto-response-log-entries) ecc-auto-response-log-max-entries)
        (setq --ecc-auto-response-log-entries
              (cl-subseq --ecc-auto-response-log-entries 0 ecc-auto-response-log-max-entries)))
      ;; Also log to debug if enabled
      (when --ecc-debug-enabled
        (--ecc-debug-message "[%s] %s: %s" level buffer-name formatted-message))
      ;; Update log buffer if visible
      (--ecc-auto-response-log-update-buffer))))

;; 4. Specialized logging functions
;; ----------------------------------------
(defun ecc-auto-response-log-state-detection (state buffer-content)
  "Log state detection details."
  (ecc-auto-response-log 'debug "State detection: %s" state)
  (when state
    (ecc-auto-response-log 'debug "Buffer end content: %s"
                          (if (> (length buffer-content) 100)
                              (concat (substring buffer-content 0 100) "...")
                            buffer-content))))

(defun ecc-auto-response-log-send-attempt (state response)
  "Log auto-response send attempt."
  (ecc-auto-response-log 'info "Sending response '%s' for state %s" response state))

(defun ecc-auto-response-log-send-success (state response)
  "Log successful auto-response send."
  (ecc-auto-response-log 'info "Successfully sent '%s' for state %s" response state))

(defun ecc-auto-response-log-send-failure (state reason)
  "Log failed auto-response send."
  (ecc-auto-response-log 'error "Failed to send response for state %s: %s" state reason))

(defun ecc-auto-response-log-throttle (state reason)
  "Log throttled auto-response."
  (ecc-auto-response-log 'warn "Response throttled for state %s: %s" state reason))

;; 5. Log buffer functions
;; ----------------------------------------
(defun ecc-auto-response-show-log ()
  "Show the auto-response log buffer."
  (interactive)
  (let ((buffer (get-buffer-create ecc-auto-response-log-buffer-name)))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert "ECC Auto-Response Log\n")
      (insert "====================\n\n")
      (dolist (entry (reverse --ecc-auto-response-log-entries))
        (let ((timestamp (plist-get entry :timestamp))
              (level (plist-get entry :level))
              (buffer-name (plist-get entry :buffer))
              (message (plist-get entry :message)))
          (insert (format "[%s] %s <%s> %s\n"
                         timestamp
                         (upcase (symbol-name level))
                         buffer-name
                         message))))
      (read-only-mode 1)
      (goto-char (point-min)))
    (display-buffer buffer)))

(defun --ecc-auto-response-log-update-buffer ()
  "Update log buffer if it's visible."
  (let ((buffer (get-buffer ecc-auto-response-log-buffer-name)))
    (when (and buffer (get-buffer-window buffer))
      (ecc-auto-response-show-log))))

(defun ecc-auto-response-clear-log ()
  "Clear the auto-response log."
  (interactive)
  (setq --ecc-auto-response-log-entries nil)
  (--ecc-auto-response-log-update-buffer)
  (message "Auto-response log cleared"))

;; 6. Export functions
;; ----------------------------------------
(defun ecc-auto-response-export-log (filename)
  "Export log to FILENAME."
  (interactive "FExport log to file: ")
  (with-temp-file filename
    (insert "ECC Auto-Response Log Export\n")
    (insert (format-time-string "Exported: %Y-%m-%d %H:%M:%S\n"))
    (insert "============================\n\n")
    (dolist (entry (reverse --ecc-auto-response-log-entries))
      (let ((timestamp (plist-get entry :timestamp))
            (level (plist-get entry :level))
            (buffer-name (plist-get entry :buffer))
            (message (plist-get entry :message)))
        (insert (format "[%s] %s <%s> %s\n"
                       timestamp
                       (upcase (symbol-name level))
                       buffer-name
                       message)))))
  (message "Log exported to %s" filename))

(provide 'ecc-auto-response-logging)

(when (not load-file-name)
  (message "ecc-auto-response-logging.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))