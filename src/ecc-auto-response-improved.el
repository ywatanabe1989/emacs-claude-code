;;; -*- coding: utf-8; lexical-binding: t -*-
;;; ecc-auto-response-improved.el --- Improved auto-response for Claude prompts
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 16:15:00>

;;; Commentary:
;;; Improved auto-response functionality for Claude prompts.
;;; This module builds on the ecc-auto-core infrastructure and provides
;;; user-facing commands and functions for automatically responding to
;;; different types of Claude prompts.

;;; The module follows clean code principles:
;;; - Functions do one thing
;;; - Related code appears vertically dense
;;; - Clear organization by functionality
;;; - Descriptive naming
;;; - Minimal complexity

;;; Code:

(require 'ecc-variables)
(require 'ecc-auto-core)
(require 'ecc-state-detection)
(require 'ecc-vterm-utils)
(require 'ecc-debug-utils)

;;;; Customization

(defgroup ecc-auto-response nil
  "Automatic response settings for Claude."
  :group 'ecc
  :prefix "ecc-auto-response-")

(defcustom ecc-auto-response-enabled nil
  "Whether auto-response functionality is enabled."
  :type 'boolean
  :group 'ecc-auto-response)

(defcustom ecc-auto-response-yes "1"
  "Response to send for Y/N prompts (typically \"1\" for \"yes\")."
  :type 'string
  :group 'ecc-auto-response)

(defcustom ecc-auto-response-yes-plus "2"
  "Response to send for Y/Y/N prompts (typically \"2\" for second option)."
  :type 'string
  :group 'ecc-auto-response)

(defcustom ecc-auto-response-continue "/auto"
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

;;;; Internal variables

(defvar ecc-auto-response--registered-callback nil
  "Callback function to process detected states in registered buffers.")

;;;; Core functionality

(defun ecc-auto-response-start ()
  "Start the auto-response system.
Initializes and activates the auto-response system that automatically responds
to different types of Claude prompts.

Sets up a timer using the core infrastructure that periodically checks
buffers with Claude interactions for prompts and sends appropriate responses."
  (interactive)
  
  ;; Enable auto-response
  (setq ecc-auto-response-enabled t)
  
  ;; Set up callback for processing detected states
  (setq ecc-auto-response--registered-callback 
        (lambda (buffer state)
          (ecc-auto-response-send buffer state)))
  
  ;; Register currently active buffer
  (when-let ((buf (current-buffer)))
    (ecc-auto-core-register-buffer buf))
  
  ;; Initialize core timer for periodic checking
  (ecc-auto-core-timer-start 
   (lambda () 
     (ecc-auto-core-process-all-buffers ecc-auto-response--registered-callback)))
  
  ;; Also do an immediate initial check for waiting prompts
  (when-let ((buf (current-buffer)))
    (ecc-auto-core-initial-check buf ecc-auto-response--registered-callback))
  
  (ecc-auto-response--report-status "started"))

(defun ecc-auto-response-stop ()
  "Stop the auto-response system.
Deactivates the auto-response system and cancels any pending timers."
  (interactive)
  
  ;; Disable auto-response
  (setq ecc-auto-response-enabled nil)
  
  ;; Clear callback
  (setq ecc-auto-response--registered-callback nil)
  
  ;; Stop core timer
  (ecc-auto-core-timer-stop)
  
  ;; Reset core state
  (ecc-auto-core-reset-state)
  
  (message "Auto-response stopped"))

(defun ecc-auto-response-toggle ()
  "Toggle auto-response on/off."
  (interactive)
  (if ecc-auto-response-enabled
      (ecc-auto-response-stop)
    (ecc-auto-response-start)))

(defun ecc-auto-response-register-buffer (&optional buffer)
  "Register BUFFER for auto-response.
If BUFFER is nil, use current buffer.
Returns the buffer if registered successfully."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (prog1 (ecc-auto-core-register-buffer buf)
      (when (called-interactively-p 'any)
        (message "Buffer %s registered for auto-response" (buffer-name buf))))))

;;;; Response dispatch

(defun ecc-auto-response-send (buffer &optional state)
  "Send appropriate response to Claude prompts in BUFFER.
Examines the buffer content to detect Claude's current prompt state, then
sends an appropriate pre-configured response based on that state.

BUFFER is the buffer containing Claude's output to respond to.
Optional STATE can be provided to override automatic state detection.
Valid states are: `:y/n`, `:y/y/n`, `:initial-waiting`, `:waiting`

Responds with configured variables for different prompt types.
Returns t if a response was sent, nil otherwise."
  (interactive (list (current-buffer)))
  
  ;; Skip if auto-response is disabled
  (unless ecc-auto-response-enabled
    (ecc-auto-response--debug-message "Auto-response is disabled, not sending response")
    (cl-return-from ecc-auto-response-send nil))
  
  ;; Skip if buffer is not live
  (unless (buffer-live-p buffer)
    (ecc-auto-response--debug-message "Buffer is not live, not sending response")
    (cl-return-from ecc-auto-response-send nil))
  
  (with-current-buffer buffer
    ;; Get current state if not provided
    (let ((current-state (or state (ecc-detect-state))))
      (pcase current-state
       (:y/y/n (ecc-auto-response--dispatch-response 
                buffer ecc-auto-response-yes-plus "Y/Y/N"))
       
       (:y/n (ecc-auto-response--dispatch-response 
              buffer ecc-auto-response-yes "Y/N"))
       
       (:initial-waiting (ecc-auto-response--dispatch-response 
                         buffer ecc-auto-response-initial-waiting "Initial-Waiting"))
       
       (:waiting (ecc-auto-response--dispatch-response 
                  buffer ecc-auto-response-continue "Continue"))
       
       (_ ;; No recognized state
        (ecc-auto-response--debug-message "No recognized prompt state detected")
        nil)))))

;;;; Response handling

(defun ecc-auto-response--dispatch-response (buffer response type)
  "Send RESPONSE to Claude in BUFFER.
TYPE is used for notification messages.
Returns t if response was sent successfully."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      ;; Send the response using the appropriate method for the buffer mode
      (cond
       ;; vterm mode
       ((derived-mode-p 'vterm-mode)
        (ecc-auto-response--send-to-vterm buffer response))
       
       ;; comint mode (e.g., shell)
       ((derived-mode-p 'comint-mode)
        (comint-send-string
         (get-buffer-process buffer)
         (concat response "\n")))
       
       ;; Default fallback - just insert at point
       (t
        (insert response)
        (message "Inserted response in buffer %s" (buffer-name buffer))))))
  
  ;; Notify user about the response if notifications are enabled
  (when ecc-auto-response-notify
    (ecc-auto-response--notify type response))
  
  ;; Return t to indicate success
  t)

(defun ecc-auto-response--send-to-vterm (buffer response)
  "Send RESPONSE to Claude in vterm BUFFER."
  ;; Use the shared utility function with appropriate debug function
  (ecc-vterm-utils-send-string buffer response (ecc-debug-utils-make-debug-fn)))

(defun ecc-auto-response--notify (type response)
  "Display notification about auto-response of TYPE with RESPONSE string.
TYPE is a description of the response context (e.g., \"Y/N\").
RESPONSE is the actual string sent to Claude."
  (let ((msg (format "Auto-responded: %s (\"%s\")" type response)))
    (message msg)))

;;;; Convenience response functions

(defun ecc-auto-response-yes (&optional buffer)
  "Send Y response to Claude Y/N prompt in BUFFER (or current buffer)."
  (interactive)
  (ecc-auto-response--dispatch-response 
   (or buffer (current-buffer))
   ecc-auto-response-yes
   "Y/N"))

(defun ecc-auto-response-yes-plus (&optional buffer)
  "Send Y+ response to Claude Y/Y/N prompt in BUFFER (or current buffer)."
  (interactive)
  (ecc-auto-response--dispatch-response
   (or buffer (current-buffer))
   ecc-auto-response-yes-plus
   "Y/Y/N"))

(defun ecc-auto-response-continue (&optional buffer)
  "Send continue response to Claude waiting prompt in BUFFER (or current buffer)."
  (interactive)
  (ecc-auto-response--dispatch-response
   (or buffer (current-buffer))
   ecc-auto-response-continue
   "Continue"))

(defun ecc-auto-response-custom (response-text)
  "Send custom RESPONSE-TEXT to Claude."
  (interactive "sEnter your response: ")
  (let ((buffer (current-buffer)))
    (when (buffer-live-p buffer)
      (ecc-auto-response--dispatch-response
       buffer
       response-text
       (format "Custom: %s" response-text)))))

;;;; Utility functions

(defun ecc-auto-response--report-status (action)
  "Report status change with ACTION and current settings."
  (message "Auto-response %s: Y/N=%s, Y/Y/N=%s, Continue=%s" 
           action
           ecc-auto-response-yes
           ecc-auto-response-yes-plus
           ecc-auto-response-continue))

(defun ecc-auto-response--debug-message (message)
  "Output debug MESSAGE if debugging is enabled."
  (when (and (boundp 'ecc-debug-enabled)
             ecc-debug-enabled
             (fboundp 'ecc-debug-message))
    (ecc-debug-message message)))

;;;; Backward compatibility

;; Backward compatibility aliases for existing code
(defalias 'ecc-start-auto-response 'ecc-auto-response-start)
(defalias 'ecc-stop-auto-response 'ecc-auto-response-stop)
(defalias 'ecc-toggle-auto-response 'ecc-auto-response-toggle)
(defalias 'ecc-auto-accept-send 'ecc-auto-response-send)
(defalias 'ecc-auto-response-template 'ecc-auto-response-custom)

;; Backward compatibility function
(defun ecc-check-and-respond ()
  "Compatibility function for old auto-response checking."
  (when ecc-auto-response-enabled
    (ecc-auto-core-process-all-buffers ecc-auto-response--registered-callback)))

(provide 'ecc-auto-response-improved)
;;; ecc-auto-response-improved.el ends here