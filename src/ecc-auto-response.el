;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 23:40:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-auto-response.el

;;; Commentary:
;;; Main auto-response module for Claude in Emacs.
;;; This module provides the primary user interface for automatically
;;; responding to Claude prompts. It integrates the core infrastructure,
;;; detection, notification, and buffer-local functionality into a
;;; cohesive auto-response system.

(require 'ecc-variables)
(require 'ecc-auto-core)
(require 'ecc-auto-detect)
(require 'ecc-auto-notify)

;;; Code:

;; Attempt to load optional dependencies
(when (locate-library "ecc-auto-buffer")
  (require 'ecc-auto-buffer))

;; Customization options
(defgroup ecc-auto-response nil
  "Automatic response settings for Claude."
  :group 'ecc
  :prefix "ecc-auto-response-")

(defcustom ecc-auto-response-enabled nil
  "Whether auto-response functionality is enabled."
  :type 'boolean
  :group 'ecc-auto-response)

(defcustom ecc-auto-response-y/n "1"
  "Response to send for Y/N prompts (typically \"1\" for \"yes\")."
  :type 'string
  :group 'ecc-auto-response)

(defcustom ecc-auto-response-y/y/n "2"
  "Response to send for Y/Y/N prompts (typically \"2\" for second option)."
  :type 'string
  :group 'ecc-auto-response)

(defcustom ecc-auto-response-waiting "/auto"
  "Response to send for waiting state (typically \"/auto\" or \"/continue\")."
  :type 'string 
  :group 'ecc-auto-response)

(defcustom ecc-auto-response-initial-waiting "/user:understand-guidelines"
  "Response to send for initial waiting state."
  :type 'string
  :group 'ecc-auto-response)

(defcustom ecc-auto-response-notify t
  "Whether to show notifications when automatic responses are sent."
  :type 'boolean
  :group 'ecc-auto-response)

;; Internal variables
(defvar ecc-auto-response--callback nil
  "Callback function for processing detected states.")

;; Core functionality

;;;###autoload
(defun ecc-auto-response-check-and-respond (buffer)
  "Check BUFFER for Claude prompts and respond if appropriate.
Returns t if a response was sent, nil otherwise."
  (when (and ecc-auto-response-enabled
             (buffer-live-p buffer))
    (with-current-buffer buffer
      (let ((state (ecc-auto-detect-prompt)))
        (when (and state (not (ecc-auto-core-throttled-p)))
          (ecc-auto-response-send buffer state)
          (ecc-auto-core-update-time)
          t)))))

;;;###autoload
(defun ecc-auto-response-send (buffer state)
  "Send appropriate response to Claude prompt in BUFFER.
STATE indicates the type of prompt detected.
Returns t if a response was sent, nil otherwise."
  (interactive (list (current-buffer) (ecc-auto-detect-prompt)))
  
  ;; Skip if auto-response is disabled
  (unless ecc-auto-response-enabled
    (when ecc-auto-core-debug
      (message "Auto-response is disabled, not sending response"))
    (cl-return-from ecc-auto-response-send nil))
  
  ;; Skip if buffer is not live
  (unless (buffer-live-p buffer)
    (when ecc-auto-core-debug
      (message "Buffer is not live, not sending response"))
    (cl-return-from ecc-auto-response-send nil))
  
  ;; Process response based on state
  (with-current-buffer buffer
    (cond
     ((eq state :y/y/n)
      (ecc-auto-response--send-message buffer ecc-auto-response-y/y/n "Y/Y/N"))
     
     ((eq state :y/n)
      (ecc-auto-response--send-message buffer ecc-auto-response-y/n "Y/N"))
     
     ((eq state :initial-waiting)
      (ecc-auto-response--send-message buffer ecc-auto-response-initial-waiting "Initial-Waiting"))
     
     ((eq state :waiting)
      (ecc-auto-response--send-message buffer ecc-auto-response-waiting "Continue"))
     
     (t ;; No recognized state
      (when ecc-auto-core-debug
        (message "No recognized prompt state detected"))
      nil))))

;;;###autoload
(defun ecc-auto-response-process-timer ()
  "Callback function for the auto-response timer.
Checks all registered buffers for prompts and responds if needed."
  (ecc-auto-core-process-all-buffers #'ecc-auto-response-check-and-respond)
  (when ecc-auto-core-debug
    (message "Processed auto-response timer")))

;; Helper functions

(defun ecc-auto-response--send-message (buffer response type)
  "Send RESPONSE to Claude in BUFFER.
TYPE is used for notification messages."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (cond
       ;; vterm mode
       ((derived-mode-p 'vterm-mode)
        (ecc-auto-response--send-to-vterm buffer response))
       
       ;; comint mode (e.g., shell)
       ((derived-mode-p 'comint-mode)
        (comint-send-string
         (get-buffer-process buffer)
         (concat response "\n")))
       
       ;; Default fallback
       (t
        (insert response)
        (message "Inserted response in buffer %s" (buffer-name buffer))))))
  
  ;; Notify user if notifications are enabled
  (when ecc-auto-response-notify
    (ecc-auto-notify-response type response))
  
  ;; Return t to indicate success
  t)

(defun ecc-auto-response--send-to-vterm (buffer response)
  "Send RESPONSE to Claude in vterm BUFFER."
  (if (fboundp 'vterm-send-string)
      (progn
        (with-current-buffer buffer
          (vterm-send-string response)
          (vterm-send-return)))
    (message "vterm-send-string not available")))

;; System start/stop

;;;###autoload
(defun ecc-auto-response-start ()
  "Start the auto-response system.
Initializes and activates the auto-response system that automatically responds
to different types of Claude prompts."
  (interactive)
  
  ;; Enable auto-response
  (setq ecc-auto-response-enabled t)
  
  ;; Initialize core system
  (ecc-auto-core-initialize)
  
  ;; Start timer with our callback
  (ecc-auto-core-timer-start #'ecc-auto-response-process-timer)
  
  ;; Register current buffer if any
  (when-let ((buf (current-buffer)))
    (ecc-auto-core-register-buffer buf)
    
    ;; Initialize buffer-local state if available
    (when (fboundp 'ecc-auto-buffer-init)
      (ecc-auto-buffer-init buf)))
  
  ;; Start buffer-local system if available
  (when (fboundp 'ecc-auto-buffer-enable)
    (ecc-auto-buffer-enable))
  
  (message "Auto-response started: Y/N=%s, Y/Y/N=%s, Continue=%s" 
           ecc-auto-response-y/n
           ecc-auto-response-y/y/n
           ecc-auto-response-waiting))

;;;###autoload
(defun ecc-auto-response-stop ()
  "Stop the auto-response system.
Deactivates the auto-response system and cancels any pending timers."
  (interactive)
  
  ;; Disable auto-response
  (setq ecc-auto-response-enabled nil)
  
  ;; Shut down core system
  (ecc-auto-core-shutdown)
  
  ;; Stop buffer-local system if available
  (when (fboundp 'ecc-auto-buffer-disable)
    (ecc-auto-buffer-disable))
  
  (message "Auto-response stopped"))

;;;###autoload
(defun ecc-auto-response-toggle ()
  "Toggle auto-response on/off."
  (interactive)
  (if ecc-auto-response-enabled
      (ecc-auto-response-stop)
    (ecc-auto-response-start)))

;;;###autoload
(defun ecc-auto-response-register-buffer (&optional buffer)
  "Register BUFFER for auto-response.
If BUFFER is nil, use current buffer.
Returns the buffer if registered successfully."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    ;; Register with core system
    (ecc-auto-core-register-buffer buf)
    
    ;; Initialize buffer-local state if available
    (when (fboundp 'ecc-auto-buffer-init)
      (ecc-auto-buffer-init buf))
    
    ;; Notify if interactive
    (when (called-interactively-p 'any)
      (message "Buffer %s registered for auto-response" (buffer-name buf)))
    
    buf))

;; Convenience functions

;;;###autoload
(defun ecc-auto-response-yes (&optional buffer)
  "Automatically send Y response to Claude Y/N prompt.
Sends the yes response (defined by `ecc-auto-response-y/n`) to
Claude when it's in a Y/N prompt state.

If BUFFER is nil, use current buffer."
  (interactive)
  (ecc-auto-response--send-message 
   (or buffer (current-buffer))
   ecc-auto-response-y/n
   "Y/N"))

;;;###autoload
(defun ecc-auto-response-yes-plus (&optional buffer)
  "Automatically send Y response to Claude Y/Y/N prompt.
Sends the yes-plus response (defined by `ecc-auto-response-y/y/n`)
to Claude when it's in a Y/Y/N prompt state.

If BUFFER is nil, use current buffer."
  (interactive)
  (ecc-auto-response--send-message
   (or buffer (current-buffer))
   ecc-auto-response-y/y/n
   "Y/Y/N"))

;;;###autoload
(defun ecc-auto-response-continue (&optional buffer)
  "Automatically send continue to Claude waiting prompt.
Sends the continue response (defined by `ecc-auto-response-waiting`) to
Claude when it's in a waiting state, prompting for more output.

If BUFFER is nil, use current buffer."
  (interactive)
  (ecc-auto-response--send-message
   (or buffer (current-buffer))
   ecc-auto-response-waiting
   "Continue"))

;;;###autoload
(defun ecc-auto-response-custom (response-text)
  "Send custom RESPONSE-TEXT to Claude.
This allows sending natural language responses instead of just command options."
  (interactive "sEnter your response: ")
  (let ((buffer (current-buffer)))
    (when (buffer-live-p buffer)
      (ecc-auto-response--send-message
       buffer
       response-text
       (format "Custom: %s" response-text)))))

;; Debugging

;;;###autoload
(defun ecc-auto-response-debug-toggle ()
  "Toggle debugging for auto-response system."
  (interactive)
  (ecc-auto-core-toggle-debug))

;;;###autoload
(defun ecc-auto-response-status ()
  "Display status information for the auto-response system."
  (interactive)
  (message "Auto-response Status:
Enabled: %s
Timer Active: %s
Core Debug: %s
Y/N Response: %s
Y/Y/N Response: %s
Continue Response: %s
Initial Response: %s
Buffer-local Available: %s"
           (if ecc-auto-response-enabled "Yes" "No")
           (if (ecc-auto-core-timer-active-p) "Yes" "No")
           (if ecc-auto-core-debug "Enabled" "Disabled")
           ecc-auto-response-y/n
           ecc-auto-response-y/y/n
           ecc-auto-response-waiting
           ecc-auto-response-initial-waiting
           (if (fboundp 'ecc-auto-buffer-status) "Yes" "No")))

;; Backward compatibility aliases
(defalias 'ecc-start-auto-response 'ecc-auto-response-start)
(defalias 'ecc-stop-auto-response 'ecc-auto-response-stop)
(defalias 'ecc-toggle-auto-response 'ecc-auto-response-toggle)
(defalias 'ecc-auto-accept-send 'ecc-auto-response-send)
(defalias 'ecc-auto-response-template 'ecc-auto-response-custom)

(provide 'ecc-auto-response)

;;; ecc-auto-response.el ends here