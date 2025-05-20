;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 18:45:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-buffer-local.el

;;; Commentary:
;;; Buffer-local configuration and state management for Claude buffers.
;;; This module provides the functionality to manage Claude states and configurations
;;; on a per-buffer basis, allowing multiple Claude instances to operate independently.

(require 'ecc-variables)

;;; Code:

;; Buffer-local variables declarations

(defvar-local ecc-buffer-state nil
  "The current Claude prompt state for this buffer.
This is a buffer-local variable that tracks the detected state.
Possible values: :y/y/n, :y/n, :waiting, :initial-waiting, or nil.")

(defvar-local ecc-buffer-last-state-time 0.0
  "The timestamp of the last state detection for this buffer.
This is a buffer-local variable used for tracking state changes.")

(defvar-local ecc-buffer-last-time-alist
  '((:y/n . 0.0)
    (:y/y/n . 0.0)
    (:waiting . 0.0)
    (:initial-waiting . 0.0))
  "Alist tracking last time each type of response was sent for this buffer.
This is a buffer-local variable used for throttling.")

(defvar-local ecc-buffer-active-state nil
  "The currently active Claude prompt state being processed in this buffer.
This is a buffer-local variable used to prevent duplicate responses.")

;; Buffer-local configuration variables

(defvar-local ecc-buffer-auto-response-enabled nil
  "Whether auto-response is enabled for this buffer.")

(defvar-local ecc-buffer-auto-response-y/n nil
  "Response to send for Y/N prompts in this buffer.")

(defvar-local ecc-buffer-auto-response-y/y/n nil
  "Response to send for Y/Y/N prompts in this buffer.")

(defvar-local ecc-buffer-auto-response-waiting nil
  "Response to send for waiting prompts in this buffer.")

(defvar-local ecc-buffer-auto-response-initial-waiting nil
  "Response to send for initial waiting state in this buffer.")

(defvar-local ecc-buffer-auto-notify-on-prompt nil
  "Whether to notify when claude asks for a response in this buffer.")

(defvar-local ecc-buffer-auto-notify-completions nil
  "Whether to notify when auto-response completes in this buffer.")

(defvar-local ecc-buffer-debug-enabled nil
  "Whether to enable debugging output for this buffer.")

;; Initialization functions

;;;###autoload
(defun ecc-buffer-local-init (&optional buffer)
  "Initialize buffer-local variables for BUFFER or current buffer.
This sets up the necessary buffer-local state tracking for Claude."
  (with-current-buffer (or buffer (current-buffer))
    ;; Initialize state tracking variables
    (setq-local ecc-buffer-state nil)
    (setq-local ecc-buffer-last-state-time 0.0)
    (setq-local ecc-buffer-last-time-alist
                '((:y/n . 0.0)
                  (:y/y/n . 0.0)
                  (:waiting . 0.0)
                  (:initial-waiting . 0.0)))
    (setq-local ecc-buffer-active-state nil)
    
    ;; Initialize with global configuration as defaults
    (setq-local ecc-buffer-auto-response-enabled 
                (and (boundp 'ecc-buffer-auto-response-enabled)
                     ecc-buffer-auto-response-enabled))
    
    (setq-local ecc-buffer-auto-response-y/n
                (and (boundp 'ecc-auto-response-y/n)
                     ecc-auto-response-y/n))
    
    (setq-local ecc-buffer-auto-response-y/y/n
                (and (boundp 'ecc-auto-response-y/y/n)
                     ecc-auto-response-y/y/n))
    
    (setq-local ecc-buffer-auto-response-waiting
                (and (boundp 'ecc-auto-response-waiting)
                     ecc-auto-response-waiting))
    
    (setq-local ecc-buffer-auto-response-initial-waiting
                (and (boundp 'ecc-auto-response-initial-waiting)
                     ecc-auto-response-initial-waiting))
    
    (setq-local ecc-buffer-auto-notify-on-prompt
                (and (boundp 'ecc-auto-notify-on-claude-prompt)
                     ecc-auto-notify-on-claude-prompt))
    
    (setq-local ecc-buffer-auto-notify-completions
                (and (boundp 'ecc-auto-notify-completions)
                     ecc-auto-notify-completions))
    
    (setq-local ecc-buffer-debug-enabled
                (and (boundp 'ecc-debug-enabled)
                     ecc-debug-enabled))))

;; Buffer registration with local configuration

;;;###autoload
(defun ecc-buffer-register-with-local-config (buffer)
  "Register BUFFER as a Claude buffer with buffer-local configuration.
Returns the buffer that was registered."
  (interactive "bBuffer to register: ")
  (let ((buf (get-buffer buffer)))
    (unless (buffer-live-p buf)
      (user-error "Buffer does not exist or is not alive"))
    
    ;; Register the buffer
    (unless (assoc buf ecc-buffer-registered-buffers-alist)
      (push (cons buf nil) ecc-buffer-registered-buffers-alist)
      (message "Buffer '%s' registered as Claude buffer" (buffer-name buf)))
    
    ;; Initialize buffer-local variables
    (ecc-buffer-local-init buf)
    
    ;; Set as current Claude buffer
    (setq ecc-buffer-current-buffer buf)
    buf))

;; State tracking with buffer-local values

;;;###autoload
(defun ecc-buffer-local-update-state (state &optional buffer)
  "Update the buffer-local state tracking for STATE in BUFFER.
If BUFFER is nil, use the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (setq-local ecc-buffer-state state)
    (setq-local ecc-buffer-last-state-time (float-time))))

;;;###autoload
(defun ecc-buffer-local-state (&optional buffer)
  "Get the current buffer-local state for BUFFER.
If BUFFER is nil, use the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    ecc-buffer-state))

;; Throttling with buffer-local tracking

;;;###autoload
(defun ecc-buffer-local-throttled-p (state &optional buffer)
  "Check if auto-response for STATE should be throttled in BUFFER.
Returns t if we responded to this state recently and should wait.
If BUFFER is nil, use the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((now (float-time))
           (last-time
            (alist-get state ecc-buffer-last-time-alist 0.0))
           (elapsed (- now last-time))
           (throttle-time (if (boundp 'ecc-auto-response-throttle-time)
                              ecc-auto-response-throttle-time
                            5.0)))
      (or
       ;; Check if this is a duplicate of the currently active state
       (eq state ecc-buffer-active-state)
       ;; Check if we need to throttle based on time
       (< elapsed throttle-time)))))

;;;###autoload
(defun ecc-buffer-local-update-time (state &optional buffer)
  "Update the last response time for STATE in BUFFER.
If BUFFER is nil, use the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (setf (alist-get state ecc-buffer-last-time-alist)
          (float-time))))

;; Debug messaging with buffer-local control

;;;###autoload
(defun ecc-buffer-local-debug-message (format-string &rest args)
  "Output a debug message if debugging is enabled for current buffer.
Only prints the message when `ecc-buffer-debug-enabled' is non-nil.
Accepts the same arguments as `message': FORMAT-STRING and ARGS."
  (when ecc-buffer-debug-enabled
    (apply #'message format-string args)))

(provide 'ecc-buffer-local)

;;; ecc-buffer-local.el ends here