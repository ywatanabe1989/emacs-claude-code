;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 21:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-auto-response-buffer-local.el

;;; Commentary:
;;; Buffer-local auto-response system for Claude prompts.
;;; This module provides a per-buffer auto-response system that works with
;;; the background detection engine to automatically respond to Claude prompts
;;; in multiple buffers concurrently, with each buffer maintaining its own
;;; independent state and configuration.

(require 'ecc-variables)
(require 'ecc-buffer-local)
(require 'ecc-buffer-state)
(require 'ecc-background-detection)
(require 'ecc-vterm-utils)
(require 'ecc-debug-utils)

;;; Code:

;; Customization options for buffer-local auto-response
(defgroup ecc-auto-response-buffer-local nil
  "Buffer-local automatic response settings for Claude."
  :group 'ecc
  :prefix "ecc-auto-response-buffer-local-")

(defcustom ecc-auto-response-buffer-local-default-enabled nil
  "Whether buffer-local auto-response is enabled by default for new buffers."
  :type 'boolean
  :group 'ecc-auto-response-buffer-local)

(defcustom ecc-auto-response-buffer-local-notify t
  "Whether to show notifications for buffer-local auto-responses."
  :type 'boolean
  :group 'ecc-auto-response-buffer-local)

;; Main auto-response handler for buffer-local operation

(defun ecc-auto-response-buffer-local-check (buffer)
  "Check BUFFER for Claude prompt and respond if appropriate.
Uses buffer-local configuration and state tracking.
Returns t if a response was sent, nil otherwise."
  (interactive (list (current-buffer)))
  
  ;; Skip if buffer is not live
  (unless (buffer-live-p buffer)
    (when (boundp 'ecc-buffer-debug-enabled)
      (with-current-buffer buffer
        (when ecc-buffer-debug-enabled
          (message "Buffer is not live, not sending response"))))
    (cl-return-from ecc-auto-response-buffer-local-check nil))
  
  ;; Check if auto-response is enabled for this buffer
  (with-current-buffer buffer
    ;; Skip if auto-response is disabled for this buffer
    (unless ecc-buffer-auto-response-enabled
      (when ecc-buffer-debug-enabled
        (message "Auto-response disabled for buffer %s" (buffer-name)))
      (cl-return-from ecc-auto-response-buffer-local-check nil))
    
    ;; Get current prompt state from buffer-local state storage
    (let ((current-state (ecc-buffer-state-get-prompt)))
      
      ;; Skip if no state detected
      (unless current-state
        (when ecc-buffer-debug-enabled
          (message "No state detected in buffer %s" (buffer-name)))
        (cl-return-from ecc-auto-response-buffer-local-check nil))
      
      ;; Check if we should throttle responses
      (when (ecc-buffer-state-throttled-p current-state)
        (when ecc-buffer-debug-enabled
          (message "Throttled response to %s in buffer %s"
                   current-state (buffer-name)))
        (cl-return-from ecc-auto-response-buffer-local-check nil))
      
      ;; Send response based on state type
      (cond
       ((eq current-state :y/y/n)
        (ecc-auto-response-buffer-local-send-message 
         buffer ecc-buffer-auto-response-y/y/n "Y/Y/N"))
       
       ((eq current-state :y/n)
        (ecc-auto-response-buffer-local-send-message 
         buffer ecc-buffer-auto-response-y/n "Y/N"))
       
       ((eq current-state :initial-waiting)
        (ecc-auto-response-buffer-local-send-message 
         buffer ecc-buffer-auto-response-initial-waiting "Initial-Waiting"))
       
       ((eq current-state :waiting)
        (ecc-auto-response-buffer-local-send-message 
         buffer ecc-buffer-auto-response-waiting "Continue"))
       
       (t ;; Unrecognized state
        (when ecc-buffer-debug-enabled
          (message "Unrecognized state %s in buffer %s"
                   current-state (buffer-name)))
        nil)))))

;; Buffer-local message sending

(defun ecc-auto-response-buffer-local-send-message (buffer response type)
  "Send RESPONSE to Claude in BUFFER.
TYPE is a descriptive string for notification messages.
Uses buffer-local configuration."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      ;; Set as active state to prevent duplicate responses
      (ecc-buffer-state-set 'active-state (ecc-buffer-state-get-prompt))
      
      ;; Check if we're in a terminal mode
      (cond
       ;; vterm mode
       ((derived-mode-p 'vterm-mode)
        (ecc-auto-response-buffer-local-send-to-vterm buffer response))
       
       ;; comint mode (e.g., shell)
       ((derived-mode-p 'comint-mode)
        (comint-send-string
         (get-buffer-process buffer)
         (concat response "\n")))
       
       ;; Default fallback
       (t
        (when ecc-buffer-debug-enabled
          (message "Cannot send response to buffer %s in %s mode"
                   (buffer-name) major-mode))
        (cl-return-from ecc-auto-response-buffer-local-send-message nil)))
      
      ;; Update time tracking
      (let ((state (ecc-buffer-state-get-prompt)))
        (ecc-buffer-state-set 
         (intern (format "last-%s-time" (symbol-name state)))
         (float-time)))
      
      ;; Notify user if enabled
      (when ecc-buffer-auto-notify-completions
        (ecc-auto-response-buffer-local-notify buffer type response))))
  
  ;; Return t to indicate success
  t)

(defun ecc-auto-response-buffer-local-send-to-vterm (buffer response)
  "Send RESPONSE to Claude in vterm BUFFER.
Uses buffer-local state tracking."
  ;; Use the shared utility function with debug support
  (ecc-vterm-utils-send-string 
   buffer response (ecc-debug-utils-make-debug-fn buffer)))

(defun ecc-auto-response-buffer-local-notify (buffer type response)
  "Display notification about auto-response in BUFFER of TYPE with RESPONSE.
Uses buffer-local notification settings."
  (with-current-buffer buffer
    (when ecc-buffer-auto-notify-completions
      (let ((msg (format "Auto-responded in %s: %s (\"%s\")"
                         (buffer-name) type response)))
        (message msg)))))

;; System initialization

;;;###autoload
(defun ecc-auto-response-buffer-local-init (&optional buffer)
  "Initialize buffer-local auto-response for BUFFER or current buffer.
Sets up the necessary buffer-local state tracking and configuration."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    ;; Initialize buffer state container
    (ecc-buffer-state-init)
    
    ;; Ensure buffer-local variables are set up
    (unless (boundp 'ecc-buffer-auto-response-enabled)
      (ecc-buffer-local-init))
    
    ;; Set default enabled state
    (setq-local ecc-buffer-auto-response-enabled
                ecc-auto-response-buffer-local-default-enabled)
    
    ;; Register for background detection
    (ecc-background-detection-add-buffer (current-buffer))
    
    (when (called-interactively-p 'any)
      (message "Buffer-local auto-response initialized for %s" (buffer-name)))))

;; Start/stop system

;;;###autoload
(defun ecc-auto-response-buffer-local-start ()
  "Start the buffer-local auto-response system.
Initializes background detection with appropriate callback."
  (interactive)
  
  ;; Initialize current buffer
  (ecc-auto-response-buffer-local-init)
  
  ;; Enable auto-response for current buffer
  (setq-local ecc-buffer-auto-response-enabled t)
  
  ;; Start background detection with our callback
  (ecc-background-detection-start #'ecc-auto-response-buffer-local-check)
  
  (message "Buffer-local auto-response started"))

;;;###autoload
(defun ecc-auto-response-buffer-local-stop ()
  "Stop the buffer-local auto-response system."
  (interactive)
  
  ;; Stop background detection
  (ecc-background-detection-stop)
  
  ;; Disable auto-response for current buffer
  (setq-local ecc-buffer-auto-response-enabled nil)
  
  (message "Buffer-local auto-response stopped"))

;;;###autoload
(defun ecc-auto-response-buffer-local-toggle ()
  "Toggle the buffer-local auto-response system."
  (interactive)
  (if (and (boundp 'ecc-background-detection-active)
           ecc-background-detection-active)
      (ecc-auto-response-buffer-local-stop)
    (ecc-auto-response-buffer-local-start)))

;; Per-buffer enable/disable

;;;###autoload
(defun ecc-auto-response-buffer-local-enable-buffer (&optional buffer)
  "Enable auto-response for BUFFER or current buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    ;; Initialize if not already done
    (unless (boundp 'ecc-buffer-auto-response-enabled)
      (ecc-auto-response-buffer-local-init))
    
    ;; Enable for this buffer
    (setq-local ecc-buffer-auto-response-enabled t)
    
    ;; Make sure buffer is registered for background detection
    (ecc-background-detection-add-buffer (current-buffer))
    
    (message "Auto-response enabled for buffer %s" (buffer-name))))

;;;###autoload
(defun ecc-auto-response-buffer-local-disable-buffer (&optional buffer)
  "Disable auto-response for BUFFER or current buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    ;; Disable for this buffer
    (setq-local ecc-buffer-auto-response-enabled nil)
    
    (message "Auto-response disabled for buffer %s" (buffer-name))))

;; Debugging and status

;;;###autoload
(defun ecc-auto-response-buffer-local-debug-toggle (&optional buffer)
  "Toggle debug output for BUFFER or current buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (setq-local ecc-buffer-debug-enabled (not ecc-buffer-debug-enabled))
    (message "Debug output %s for buffer %s"
             (if ecc-buffer-debug-enabled "enabled" "disabled")
             (buffer-name))))

;;;###autoload
(defun ecc-auto-response-buffer-local-status (&optional buffer)
  "Display status of buffer-local auto-response for BUFFER or current buffer."
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
                          (if ecc-buffer-auto-response-enabled "Enabled" "Disabled")
                          (ecc-buffer-state-get-prompt)
                          ecc-buffer-auto-response-y/n
                          ecc-buffer-auto-response-y/y/n
                          ecc-buffer-auto-response-waiting
                          ecc-buffer-auto-response-initial-waiting
                          (if ecc-buffer-auto-notify-completions "Enabled" "Disabled")
                          (if ecc-buffer-debug-enabled "Enabled" "Disabled"))))
      (if (called-interactively-p 'any)
          (message "%s" status)
        status))))

(provide 'ecc-auto-response-buffer-local)

;;; ecc-auto-response-buffer-local.el ends here