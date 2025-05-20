;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 22:45:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-debug-utils.el

;;; Commentary:
;;; Debug utilities for Claude Code.
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

(require 'ecc-variables)

;;; Code:

;; Customization options
(defgroup ecc-debug-utils nil
  "Settings for Claude Code debugging."
  :group 'ecc
  :prefix "ecc-debug-utils-")

(defcustom ecc-debug-utils-timestamp nil
  "Whether to include timestamps in debug messages."
  :type 'boolean
  :group 'ecc-debug-utils)

(defcustom ecc-debug-utils-prefix "DEBUG"
  "Prefix for debug messages. Set to empty string to disable."
  :type 'string
  :group 'ecc-debug-utils)

;; Main debug functions

;;;###autoload
(defun ecc-debug-utils-make-debug-fn (&optional buffer)
  "Create a debug function for BUFFER or global debug.
If BUFFER is provided, the function checks buffer-local debug settings.
If BUFFER is nil, checks global debug settings."
  (if buffer
      ;; Buffer-local debug function
      (lambda (format-string &rest args)
        (with-current-buffer buffer
          (when (and (boundp 'ecc-buffer-debug-enabled)
                     ecc-buffer-debug-enabled)
            (ecc-debug-utils-message format-string args buffer))))
    
    ;; Global debug function  
    (lambda (format-string &rest args)
      (when (and (boundp 'ecc-debug-enabled)
                 ecc-debug-enabled)
        (ecc-debug-utils-message format-string args nil)))))

;;;###autoload
(defun ecc-debug-utils-message (format-string args &optional buffer)
  "Format and display a debug message.
FORMAT-STRING and ARGS are passed to `format'.
If BUFFER is non-nil, includes buffer name in the message."
  (let* ((timestamp (if ecc-debug-utils-timestamp
                        (format-time-string "[%H:%M:%S.%3N] ")
                      ""))
         (prefix (if (string-empty-p ecc-debug-utils-prefix)
                     ""
                   (concat ecc-debug-utils-prefix ": ")))
         (buffer-name (if buffer
                         (format "[%s] " (buffer-name buffer))
                       ""))
         (full-format (concat timestamp prefix buffer-name format-string)))
    (apply #'message full-format args)))

;;;###autoload
(defun ecc-debug-utils-print-state-info (&optional buffer)
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
              "  Buffer-local debug enabled: " (if (and (boundp 'ecc-buffer-debug-enabled)
                                                     ecc-buffer-debug-enabled)
                                                "YES" "NO") "\n"
              "  Buffer: " (buffer-name) "\n"
              "  Major mode: " (symbol-name major-mode) "\n"
              "  Background detection active: " (if (and (boundp 'ecc-background-detection-active)
                                                      ecc-background-detection-active)
                                                 "YES" "NO") "\n"
              "  Auto-response enabled: " (if (and (boundp 'ecc-buffer-auto-response-enabled)
                                                ecc-buffer-auto-response-enabled)
                                           "YES" "NO"))))
        (message "%s" msg)))))

;; Compatibility with older code

;;;###autoload
(defalias 'ecc-debug-message 'ecc-debug-utils-message
  "Compatibility function for older code.")

(provide 'ecc-debug-utils)

;;; ecc-debug-utils.el ends here