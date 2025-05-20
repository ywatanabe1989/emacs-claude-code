;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 23:10:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-auto-buffer.el

;;; Commentary:
;;; Buffer-local state and configuration management for Claude auto-response.
;;; This module provides a clean implementation of buffer-local auto-response
;;; functionality, consolidating and simplifying the buffer-local state tracking
;;; that was previously spread across multiple modules.

(require 'ecc-variables)
(require 'ecc-auto-detect)
(require 'ecc-auto-core)

;;; Code:

;; Customization options
(defgroup ecc-auto-buffer nil
  "Buffer-local settings for Claude auto-response functionality."
  :group 'ecc
  :prefix "ecc-auto-buffer-")

(defcustom ecc-auto-buffer-default-enabled nil
  "Whether buffer-local auto-response is enabled by default for new buffers."
  :type 'boolean
  :group 'ecc-auto-buffer)

(defcustom ecc-auto-buffer-notify t
  "Whether to show notifications for buffer-local auto-responses."
  :type 'boolean
  :group 'ecc-auto-buffer)

(defcustom ecc-auto-buffer-throttle-time 5.0
  "Minimum time between duplicate responses in the same buffer."
  :type 'number
  :group 'ecc-auto-buffer)

;; Buffer-local variables
(defvar-local ecc-auto-buffer-enabled nil
  "Whether auto-response is enabled for this buffer.")

(defvar-local ecc-auto-buffer-y/n nil
  "Response to send for Y/N prompts in this buffer.")

(defvar-local ecc-auto-buffer-y/y/n nil
  "Response to send for Y/Y/N prompts in this buffer.")

(defvar-local ecc-auto-buffer-waiting nil
  "Response to send for waiting prompts in this buffer.")

(defvar-local ecc-auto-buffer-initial-waiting nil
  "Response to send for initial waiting state in this buffer.")

(defvar-local ecc-auto-buffer-notify-enabled nil
  "Whether to notify when responses are sent in this buffer.")

(defvar-local ecc-auto-buffer-debug-enabled nil
  "Whether to enable debugging output for this buffer.")

;; State tracking variables
(defvar-local ecc-auto-buffer-state nil
  "The current prompt state detected in this buffer.")

(defvar-local ecc-auto-buffer-active-state nil
  "The prompt state currently being processed in this buffer.")

(defvar-local ecc-auto-buffer-last-detection-time 0.0
  "Timestamp of last prompt detection in this buffer.")

(defvar-local ecc-auto-buffer-last-response-times nil
  "Alist of last response times for each state type.")

;; Helper functions

(defun ecc-auto-buffer-debug (format-string &rest args)
  "Output a debug message if buffer debugging is enabled.
Only prints the message when `ecc-auto-buffer-debug-enabled' is non-nil.
Uses FORMAT-STRING and ARGS like `message'."
  (when ecc-auto-buffer-debug-enabled
    (apply #'message format-string args)))

;; Core buffer management functions

;;;###autoload
(defun ecc-auto-buffer-init (&optional buffer)
  "Initialize buffer-local auto-response state for BUFFER.
If BUFFER is nil, use current buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    ;; Initialize state tracking
    (setq-local ecc-auto-buffer-state nil)
    (setq-local ecc-auto-buffer-active-state nil)
    (setq-local ecc-auto-buffer-last-detection-time 0.0)
    (setq-local ecc-auto-buffer-last-response-times
                '((:y/n . 0.0)
                  (:y/y/n . 0.0)
                  (:waiting . 0.0)
                  (:initial-waiting . 0.0)))
    
    ;; Initialize with global defaults
    (setq-local ecc-auto-buffer-enabled 
                ecc-auto-buffer-default-enabled)
    
    (setq-local ecc-auto-buffer-y/n
                (and (boundp 'ecc-auto-response-y/n)
                     ecc-auto-response-y/n))
    
    (setq-local ecc-auto-buffer-y/y/n
                (and (boundp 'ecc-auto-response-y/y/n)
                     ecc-auto-response-y/y/n))
    
    (setq-local ecc-auto-buffer-waiting
                (and (boundp 'ecc-auto-response-waiting)
                     ecc-auto-response-waiting))
    
    (setq-local ecc-auto-buffer-initial-waiting
                (and (boundp 'ecc-auto-response-initial-waiting)
                     ecc-auto-response-initial-waiting))
    
    (setq-local ecc-auto-buffer-notify-enabled 
                ecc-auto-buffer-notify)
    
    (setq-local ecc-auto-buffer-debug-enabled 
                (and (boundp 'ecc-debug-enabled) ecc-debug-enabled))
    
    ;; Register with auto-core system
    (ecc-auto-core-register-buffer (current-buffer))
    
    (when (called-interactively-p 'any)
      (message "Buffer-local auto-response initialized for %s" 
               (buffer-name))))
  buffer)

;;;###autoload
(defun ecc-auto-buffer-detect-and-update (&optional buffer)
  "Detect prompt state and update buffer-local state for BUFFER.
If BUFFER is nil, use current buffer.
Returns the detected state or nil."
  (with-current-buffer (or buffer (current-buffer))
    (let ((state (ecc-auto-detect-prompt)))
      (when state
        (setq-local ecc-auto-buffer-state state)
        (setq-local ecc-auto-buffer-last-detection-time (float-time)))
      state)))

;; Throttling functions

;;;###autoload
(defun ecc-auto-buffer-throttled-p (state &optional buffer)
  "Check if auto-response for STATE should be throttled in BUFFER.
Returns t if we responded to this state recently and should wait.
If BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((now (float-time))
           (last-time (alist-get state ecc-auto-buffer-last-response-times 0.0))
           (elapsed (- now last-time)))
      (or
       ;; Check if this is a duplicate of the currently active state
       (eq state ecc-auto-buffer-active-state)
       ;; Check if we need to throttle based on time
       (< elapsed ecc-auto-buffer-throttle-time)))))

;;;###autoload
(defun ecc-auto-buffer-update-response-time (state &optional buffer)
  "Update the last response time for STATE in BUFFER.
If BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (setf (alist-get state ecc-auto-buffer-last-response-times)
          (float-time))))

;; Checking and response functions

;;;###autoload
(defun ecc-auto-buffer-check (buffer)
  "Check BUFFER for Claude prompt and update state.
Returns the detected state or nil."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (ecc-auto-buffer-detect-and-update))))

;;;###autoload
(defun ecc-auto-buffer-set-active (state &optional buffer)
  "Set STATE as the active state being processed in BUFFER.
If BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (setq-local ecc-auto-buffer-active-state state)))

;;;###autoload
(defun ecc-auto-buffer-clear-active (&optional buffer)
  "Clear the active state in BUFFER.
If BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (setq-local ecc-auto-buffer-active-state nil)))

;; Predicates for checking specific states

;;;###autoload
(defun ecc-auto-buffer-has-prompt-p (&optional buffer)
  "Return non-nil if BUFFER has any detected prompt state.
If BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    ecc-auto-buffer-state))

;;;###autoload
(defun ecc-auto-buffer-has-state-p (expected-state &optional buffer)
  "Check if BUFFER has EXPECTED-STATE as its prompt state.
If BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (eq ecc-auto-buffer-state expected-state)))

;; User-facing commands

;;;###autoload
(defun ecc-auto-buffer-enable (&optional buffer)
  "Enable auto-response for BUFFER or current buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    ;; Initialize if not already done
    (unless (boundp 'ecc-auto-buffer-enabled)
      (ecc-auto-buffer-init))
    
    ;; Enable for this buffer
    (setq-local ecc-auto-buffer-enabled t)
    
    ;; Make sure buffer is registered with the core system
    (ecc-auto-core-register-buffer (current-buffer))
    
    (message "Auto-response enabled for buffer %s" (buffer-name))))

;;;###autoload
(defun ecc-auto-buffer-disable (&optional buffer)
  "Disable auto-response for BUFFER or current buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    ;; Disable for this buffer
    (setq-local ecc-auto-buffer-enabled nil)
    
    (message "Auto-response disabled for buffer %s" (buffer-name))))

;;;###autoload
(defun ecc-auto-buffer-toggle (&optional buffer)
  "Toggle auto-response for BUFFER or current buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (if (and (boundp 'ecc-auto-buffer-enabled) ecc-auto-buffer-enabled)
        (ecc-auto-buffer-disable)
      (ecc-auto-buffer-enable))))

;;;###autoload
(defun ecc-auto-buffer-debug-toggle (&optional buffer)
  "Toggle debug output for BUFFER or current buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (setq-local ecc-auto-buffer-debug-enabled 
                (not ecc-auto-buffer-debug-enabled))
    (message "Debug output %s for buffer %s"
             (if ecc-auto-buffer-debug-enabled "enabled" "disabled")
             (buffer-name))))

;;;###autoload
(defun ecc-auto-buffer-status (&optional buffer)
  "Display status of buffer-local auto-response for BUFFER.
If BUFFER is nil, use current buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (let ((status (format "Buffer: %s
Auto-Response: %s
Current State: %s
Y/N Response: %s
Y/Y/N Response: %s
Waiting Response: %s
Initial Response: %s
Notifications: %s
Debug: %s"
                          (buffer-name)
                          (if ecc-auto-buffer-enabled "Enabled" "Disabled")
                          ecc-auto-buffer-state
                          ecc-auto-buffer-y/n
                          ecc-auto-buffer-y/y/n
                          ecc-auto-buffer-waiting
                          ecc-auto-buffer-initial-waiting
                          (if ecc-auto-buffer-notify-enabled "Enabled" "Disabled")
                          (if ecc-auto-buffer-debug-enabled "Enabled" "Disabled"))))
      (if (called-interactively-p 'any)
          (message "%s" status)
        status))))

;; Backward compatibility
;;;###autoload
(defalias 'ecc-buffer-local-init 'ecc-auto-buffer-init
  "Compatibility alias for `ecc-auto-buffer-init'.")

;;;###autoload
(defalias 'ecc-buffer-state-init 'ecc-auto-buffer-init
  "Compatibility alias for `ecc-auto-buffer-init'.")

;;;###autoload
(defalias 'ecc-buffer-state-detect-and-update 'ecc-auto-buffer-detect-and-update
  "Compatibility alias for `ecc-auto-buffer-detect-and-update'.")

;;;###autoload
(define-obsolete-function-alias 'ecc-buffer-register-with-local-config
  'ecc-auto-buffer-init "May 2025")

(provide 'ecc-auto-buffer)

;;; ecc-auto-buffer.el ends here