;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 20:00:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-debug-utils.el

;;; Commentary:
;;; Comprehensive debug utilities for Claude Code (Consolidated Version).
;;; 
;;; This module provides standardized debugging functions that work with
;;; both global and buffer-local debug settings. It centralizes all debug
;;; functionality to ensure consistent debug output across the codebase.
;;;
;;; Key features:
;;; - Support for both global and buffer-local debug settings
;;; - Consistent debug message formatting
;;; - Optional timestamp inclusion in debug messages
;;; - Factory functions for creating context-aware debug functions
;;; - Detailed state information display for debugging
;;; - Debug message categories with selective enabling
;;; - Conditional debugging based on module, severity, or category

(require 'ecc-variables)
(require 'cl-lib)

;;; Code:

;;;; Customization options

(defgroup ecc-debug nil
  "Settings for Claude Code debugging."
  :group 'emacs-claude-code
  :prefix "ecc-debug-")

(defcustom ecc-debug-timestamp t
  "Whether to include timestamps in debug messages."
  :type 'boolean
  :group 'ecc-debug)

(defcustom ecc-debug-prefix "[Claude]"
  "Prefix for debug messages. Set to empty string to disable."
  :type 'string
  :group 'ecc-debug)

(defcustom ecc-debug-categories '(core state auto-response buffer vterm)
  "List of debug categories that can be selectively enabled."
  :type '(repeat symbol)
  :group 'ecc-debug)

(defcustom ecc-debug-enabled-categories nil
  "List of debug categories that are currently enabled.
If nil, all categories are enabled when debugging is active.
If a list, only the specified categories will produce debug output."
  :type '(choice (const :tag "All categories" nil)
                (repeat :tag "Selected categories" symbol))
  :group 'ecc-debug)

(defcustom ecc-debug-log-buffer-name "*Claude Debug Log*"
  "Name of the buffer to use for logging debug messages.
If nil, logging to buffer is disabled."
  :type '(choice string (const :tag "Disabled" nil))
  :group 'ecc-debug)

(defcustom ecc-debug-log-max-lines 1000
  "Maximum number of lines to keep in the debug log buffer.
When the buffer exceeds this size, the oldest lines are trimmed."
  :type 'integer
  :group 'ecc-debug)

;;;; Global Variables

(defvar ecc-debug-enabled nil
  "Whether debugging is globally enabled.")

;;;; Internal variables

(defvar ecc-debug--log-buffer nil
  "Buffer object for the debug log.")

(defvar-local ecc-debug-buffer-enabled nil
  "Whether debugging is enabled for the current buffer.")

;;;; Core debug functions

;;;###autoload
(defun ecc-debug-message (format-string &rest args)
  "Output a debug message if debugging is enabled.
FORMAT-STRING and ARGS are passed to `format'."
  (when (and (boundp 'ecc-debug-enabled) ecc-debug-enabled)
    (ecc-debug--display-message nil nil format-string args)))

;;;###autoload
(defun ecc-debug-message-category (category format-string &rest args)
  "Output a debug message for CATEGORY if enabled.
FORMAT-STRING and ARGS are passed to `format'."
  (when (and (boundp 'ecc-debug-enabled) 
             ecc-debug-enabled
             (ecc-debug--category-enabled-p category))
    (ecc-debug--display-message category nil format-string args)))

;;;###autoload
(defun ecc-debug-buffer-message (buffer format-string &rest args)
  "Output a debug message for BUFFER if its debug is enabled.
FORMAT-STRING and ARGS are passed to `format'."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (boundp 'ecc-debug-buffer-enabled) 
                 ecc-debug-buffer-enabled)
        (ecc-debug--display-message nil buffer format-string args)))))

;;;###autoload
(defun ecc-debug-buffer-message-category (buffer category format-string &rest args)
  "Output a message for CATEGORY in BUFFER if its debug is enabled.
FORMAT-STRING and ARGS are passed to `format'."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (boundp 'ecc-debug-buffer-enabled) 
                 ecc-debug-buffer-enabled
                 (ecc-debug--category-enabled-p category))
        (ecc-debug--display-message category buffer format-string args)))))

(defun ecc-debug--display-message (category buffer format-string args)
  "Format and display a debug message.
If CATEGORY is non-nil, includes category name in the message.
If BUFFER is non-nil, includes buffer name in the message.
FORMAT-STRING and ARGS are passed to `format'."
  (let* ((timestamp (if ecc-debug-timestamp
                       (format-time-string "[%H:%M:%S.%3N] ")
                     ""))
         (prefix (if (string-empty-p ecc-debug-prefix)
                    ""
                  (concat ecc-debug-prefix " ")))
         (category-str (if category
                         (format "[%s] " category)
                       ""))
         (buffer-str (if buffer
                        (format "[%s] " (buffer-name buffer))
                      ""))
         (full-format (concat timestamp prefix category-str buffer-str format-string)))
    ;; Display in messages buffer
    (let ((msg (apply #'format full-format args)))
      ;; Display message without echoing to minibuffer
      (let ((inhibit-message t))
        (message "%s" msg))
      ;; Log to debug buffer if enabled
      (when ecc-debug-log-buffer-name
        (let ((log-buffer (get-buffer-create ecc-debug-log-buffer-name)))
          (with-current-buffer log-buffer
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert msg "\n")
              ;; Trim buffer if it gets too long
              (ecc-debug--trim-log-buffer))))))))

(defun ecc-debug--category-enabled-p (category)
  "Return t if debug CATEGORY is enabled, nil otherwise."
  (or (null ecc-debug-enabled-categories)
      (memq category ecc-debug-enabled-categories)))

;; This function is now inlined in ecc-debug--display-message for better reliability
(defun ecc-debug--log-to-buffer (message)
  "Append MESSAGE to the debug log buffer.
This function is deprecated and kept for backward compatibility."
  (when ecc-debug-log-buffer-name
    (let ((log-buffer (get-buffer-create ecc-debug-log-buffer-name)))
      (with-current-buffer log-buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert message "\n")
          ;; Trim buffer if it gets too long
          (ecc-debug--trim-log-buffer))))))

(defun ecc-debug--trim-log-buffer ()
  "Trim the debug log buffer to `ecc-debug-log-max-lines'."
  (let ((max-lines ecc-debug-log-max-lines)
        (inhibit-read-only t))
    (goto-char (point-max))
    (forward-line (- max-lines))
    (when (not (bobp))
      (delete-region (point-min) (point))
      (goto-char (point-min))
      (insert (format "--- Log trimmed at %s ---\n" 
                     (format-time-string "%Y-%m-%d %H:%M:%S"))))))

;;;; Factory functions

;;;###autoload
(defun ecc-debug-make-debug-fn (&optional buffer category)
  "Create a debug function for specific context.
If BUFFER is provided, the function checks buffer-local debug settings.
If CATEGORY is provided, the function checks category-specific settings."
  (cond 
   ;; Both buffer and category specified
   ((and buffer category)
    (lambda (format-string &rest args)
      (apply #'ecc-debug-buffer-message-category 
             buffer category format-string args)))
   
   ;; Only buffer specified
   (buffer
    (lambda (format-string &rest args)
      (apply #'ecc-debug-buffer-message buffer format-string args)))
   
   ;; Only category specified  
   (category
    (lambda (format-string &rest args)
      (apply #'ecc-debug-message-category category format-string args)))
   
   ;; Neither specified - use global debug
   (t
    (lambda (format-string &rest args)
      (apply #'ecc-debug-message format-string args)))))

;;;; State information and control

;;;###autoload
(defun ecc-debug-print-state-info (&optional buffer)
  "Print detailed debug state information for BUFFER or current buffer.
Shows global and buffer-local debug settings and their values."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      (let ((msg
             (concat
              "Claude Debug Status:\n"
              "  Global debug enabled: " (if (and (boundp 'ecc-debug-enabled)
                                               ecc-debug-enabled)
                                          "YES" "NO") "\n"
              "  Buffer-local debug enabled: " (if (and (boundp 'ecc-debug-buffer-enabled)
                                                     ecc-debug-buffer-enabled)
                                                "YES" "NO") "\n"
              "  Debug categories: " (format "%s" ecc-debug-categories) "\n"
              "  Enabled categories: " (format "%s" 
                                             (or ecc-debug-enabled-categories "ALL")) "\n"
              "  Debug logging: " (if ecc-debug-log-buffer-name
                                    (format "Enabled (%s)" ecc-debug-log-buffer-name)
                                  "Disabled") "\n"
              "  Buffer: " (buffer-name) "\n"
              "  Major mode: " (symbol-name major-mode) "\n"
              "  Auto-response enabled: " (if (and (boundp 'ecc-buffer-auto-response-enabled)
                                                ecc-buffer-auto-response-enabled)
                                           "YES" "NO"))))
        (message "%s" msg)))))

;;;###autoload
(defun ecc-debug-toggle-global ()
  "Toggle global debug output."
  (interactive)
  (setq ecc-debug-enabled (not ecc-debug-enabled))
  (message "Claude global debug %s" 
           (if ecc-debug-enabled "enabled" "disabled")))

;;;###autoload
(defun ecc-debug-toggle-buffer (&optional buffer)
  "Toggle buffer-local debug output for BUFFER or current buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (setq ecc-debug-buffer-enabled (not ecc-debug-buffer-enabled))
    (message "Claude buffer debug %s for %s" 
             (if ecc-debug-buffer-enabled "enabled" "disabled")
             (buffer-name))))

;;;###autoload
(defun ecc-debug-toggle-category (category)
  "Toggle enabling of specific debug CATEGORY."
  (interactive (list (intern (completing-read 
                              "Debug category: " 
                              (mapcar #'symbol-name ecc-debug-categories)))))
  
  (cond
   ;; If no categories enabled (meaning all are active), create list with all except this one
   ((null ecc-debug-enabled-categories)
    (setq ecc-debug-enabled-categories 
          (cl-remove category ecc-debug-categories)))
   
   ;; If category is in enabled list, remove it
   ((memq category ecc-debug-enabled-categories)
    (setq ecc-debug-enabled-categories
          (cl-remove category ecc-debug-enabled-categories)))
   
   ;; Otherwise add it to enabled list
   (t
    (push category ecc-debug-enabled-categories)))
  
  (message "Debug category '%s' is now %s" 
           category
           (if (ecc-debug--category-enabled-p category)
               "enabled" "disabled")))

;;;###autoload
(defun ecc-debug-enable-all-categories ()
  "Enable all debug categories."
  (interactive)
  (setq ecc-debug-enabled-categories nil)
  (message "All debug categories enabled"))

;;;###autoload
(defun ecc-debug-clear-log ()
  "Clear the debug log buffer."
  (interactive)
  (when ecc-debug-log-buffer-name
    (let ((log-buffer (get-buffer ecc-debug-log-buffer-name)))
      (when log-buffer
        (with-current-buffer log-buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "--- Log cleared at %s ---\n" 
                           (format-time-string "%Y-%m-%d %H:%M:%S")))))))))

;;;###autoload
(defun ecc-debug-view-log ()
  "View the debug log buffer."
  (interactive)
  (if (and ecc-debug-log-buffer-name
           (get-buffer ecc-debug-log-buffer-name))
      (switch-to-buffer-other-window (get-buffer ecc-debug-log-buffer-name))
    (message "Debug log buffer not created yet. Enable logging first.")))

;;;; Module-specific debug utilities

;;;###autoload
(defun ecc-debug-auto-response (format-string &rest args)
  "Output an auto-response debug message if debugging is enabled.
FORMAT-STRING and ARGS are passed to `format'."
  (apply #'ecc-debug-message-category 'auto-response format-string args))

;;;###autoload
(defun ecc-debug-state (format-string &rest args)
  "Output a state detection debug message if debugging is enabled.
FORMAT-STRING and ARGS are passed to `format'."
  (apply #'ecc-debug-message-category 'state format-string args))

;;;###autoload
(defun ecc-debug-core (format-string &rest args)
  "Output a core debug message if debugging is enabled.
FORMAT-STRING and ARGS are passed to `format'."
  (apply #'ecc-debug-message-category 'core format-string args))

;;;###autoload
(defun ecc-debug-buffer (format-string &rest args)
  "Output a buffer management debug message if debugging is enabled.
FORMAT-STRING and ARGS are passed to `format'."
  (apply #'ecc-debug-message-category 'buffer format-string args))

;;;###autoload
(defun ecc-debug-vterm (format-string &rest args)
  "Output a vterm debug message if debugging is enabled.
FORMAT-STRING and ARGS are passed to `format'."
  (apply #'ecc-debug-message-category 'vterm format-string args))

;;;; Backward compatibility

;; Compatibility with older code
(defalias 'ecc-debug-make-debug-fn 'ecc-debug-make-debug-fn
  "Compatibility function for older code.")

(defalias 'ecc-debug-message 'ecc-debug-message
  "Compatibility function for older code.")

(defalias 'ecc-debug-print-state-info 'ecc-debug-print-state-info
  "Compatibility function for older code.")

(defalias 'ecc-debug-toggle-global 'ecc-debug-toggle-global
  "Compatibility function for older code.")

;; Provide both the consolidated name (for new code) and the original name (for backward compatibility)
;; Provide both the consolidated name and the original name for backward compatibility
(provide 'ecc-debug-utils)


;;; ecc-debug-utils.el ends here
