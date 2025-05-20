;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 23:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-auto-response-enhanced.el

;;; Commentary:
;;; Enhanced auto-response functionality for Claude prompts.
;;; This module builds on the ecc-auto-core infrastructure and ecc-buffer-state
;;; tracking to provide an improved auto-response system with:
;;;
;;; - Buffer-local state tracking for more reliable responses
;;; - State history awareness to prevent duplicate responses
;;; - More intelligent throttling of responses
;;; - Per-buffer auto-response settings

(require 'ecc-variables)
(require 'ecc-auto-core)
(require 'ecc-state-detection)
(require 'ecc-buffer-state)
(require 'ecc-vterm-utils)
(require 'ecc-debug-utils)

;;; Code:

;; Customization options
(defgroup ecc-auto-response-enhanced nil
  "Enhanced automatic response settings for Claude."
  :group 'ecc
  :prefix "ecc-auto-response-enhanced-")

(defcustom ecc-auto-response-enhanced-enabled nil
  "Whether enhanced auto-response functionality is enabled."
  :type 'boolean
  :group 'ecc-auto-response-enhanced)

(defcustom ecc-auto-response-enhanced-yes "1"
  "Response to send for Y/N prompts (typically \"1\" for \"yes\")."
  :type 'string
  :group 'ecc-auto-response-enhanced)

(defcustom ecc-auto-response-enhanced-yes-plus "2"
  "Response to send for Y/Y/N prompts (typically \"2\" for second option)."
  :type 'string
  :group 'ecc-auto-response-enhanced)

(defcustom ecc-auto-response-enhanced-continue "/auto"
  "Response to send for waiting state (typically \"/auto\" or \"/continue\")."
  :type 'string 
  :group 'ecc-auto-response-enhanced)

(defcustom ecc-auto-response-enhanced-initial-waiting "/user:understand-guidelines"
  "Response to send for initial waiting state."
  :type 'string
  :group 'ecc-auto-response-enhanced)

(defcustom ecc-auto-response-enhanced-notify t
  "Whether to show notifications when automatic responses are sent."
  :type 'boolean
  :group 'ecc-auto-response-enhanced)

(defcustom ecc-auto-response-enhanced-check-interval 1.0
  "Interval in seconds for checking buffers for auto-response."
  :type 'number
  :group 'ecc-auto-response-enhanced)

;; Internal variables
(defvar ecc-auto-response-enhanced--registered-callback nil
  "Callback function to process detected states in registered buffers.")

(defvar ecc-auto-response-enhanced--timer nil
  "Timer for enhanced auto-response system.")

;; Buffer-local response settings
(defvar-local ecc-auto-response-enhanced-buffer-enabled nil
  "Whether enhanced auto-response is enabled for this buffer.")

(defvar-local ecc-auto-response-enhanced-last-response-time 0
  "Timestamp of last auto-response in this buffer.")

(defvar-local ecc-auto-response-enhanced-last-response-state nil
  "Last state that received an auto-response in this buffer.")

;; Core functions

;;;###autoload
(defun ecc-auto-response-enhanced-start ()
  "Start the enhanced auto-response system.
Initializes buffer-local state tracking and activates the auto-response system
that automatically responds to different types of Claude prompts.

Sets up a timer that periodically checks buffers with Claude interactions
for prompts and sends appropriate responses based on buffer-local state.

Displays a message with the configured response values when started."
  (interactive)
  
  ;; Enable auto-response
  (setq ecc-auto-response-enhanced-enabled t)
  
  ;; Set up callback for processing detected states
  (setq ecc-auto-response-enhanced--registered-callback 
        (lambda (buffer state)
          (ecc-auto-response-enhanced-send buffer state)))
  
  ;; Register currently active buffer and enable state tracking
  (when-let ((buf (current-buffer)))
    (ecc-buffer-state-enable-auto-response buf)
    (with-current-buffer buf
      (setq ecc-auto-response-enhanced-buffer-enabled t)))
  
  ;; Initialize timer for periodic checking
  (ecc-auto-response-enhanced--start-timer)
  
  ;; Also do an immediate check for the current buffer
  (when-let ((buf (current-buffer)))
    (ecc-buffer-state-update buf t)
    (ecc-auto-response-enhanced-check-buffer buf))
  
  (message "Enhanced auto-response started: Y/N=%s, Y/Y/N=%s, Continue=%s" 
           ecc-auto-response-enhanced-yes
           ecc-auto-response-enhanced-yes-plus
           ecc-auto-response-enhanced-continue))

;;;###autoload
(defun ecc-auto-response-enhanced-stop ()
  "Stop the enhanced auto-response system.
Deactivates the auto-response system and cancels any pending timers."
  (interactive)
  
  ;; Disable auto-response
  (setq ecc-auto-response-enhanced-enabled nil)
  
  ;; Clear callback
  (setq ecc-auto-response-enhanced--registered-callback nil)
  
  ;; Stop timer
  (ecc-auto-response-enhanced--stop-timer)
  
  ;; Disable in current buffer if active
  (when-let ((buf (current-buffer)))
    (with-current-buffer buf
      (when ecc-auto-response-enhanced-buffer-enabled
        (setq ecc-auto-response-enhanced-buffer-enabled nil))))
  
  (message "Enhanced auto-response stopped"))

;;;###autoload
(defun ecc-auto-response-enhanced-toggle ()
  "Toggle enhanced auto-response on/off."
  (interactive)
  (if ecc-auto-response-enhanced-enabled
      (ecc-auto-response-enhanced-stop)
    (ecc-auto-response-enhanced-start)))

;;;###autoload
(defun ecc-auto-response-enhanced-register-buffer (&optional buffer)
  "Register BUFFER for enhanced auto-response.
If BUFFER is nil, use current buffer.
Returns the buffer if registered successfully."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (prog1 (ecc-buffer-state-enable-auto-response buf)
      (with-current-buffer buf
        (setq ecc-auto-response-enhanced-buffer-enabled t
              ecc-auto-response-enhanced-last-response-time 0
              ecc-auto-response-enhanced-last-response-state nil))
      (when (called-interactively-p 'any)
        (message "Buffer %s registered for enhanced auto-response" (buffer-name buf))))))

;;;###autoload
(defun ecc-auto-response-enhanced-unregister-buffer (&optional buffer)
  "Unregister BUFFER from enhanced auto-response.
If BUFFER is nil, use current buffer."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (ecc-buffer-state-disable-auto-response buf)
    (with-current-buffer buf
      (setq ecc-auto-response-enhanced-buffer-enabled nil))
    (when (called-interactively-p 'any)
      (message "Buffer %s unregistered from enhanced auto-response" (buffer-name buf)))))

;; Timer management

(defun ecc-auto-response-enhanced--start-timer ()
  "Start the timer for enhanced auto-response checking."
  (ecc-auto-response-enhanced--stop-timer)
  (setq ecc-auto-response-enhanced--timer
        (run-with-timer 1.0 ecc-auto-response-enhanced-check-interval
                        #'ecc-auto-response-enhanced-check-all-buffers)))

(defun ecc-auto-response-enhanced--stop-timer ()
  "Stop the timer for enhanced auto-response if it exists."
  (when (timerp ecc-auto-response-enhanced--timer)
    (cancel-timer ecc-auto-response-enhanced--timer)
    (setq ecc-auto-response-enhanced--timer nil)))

;; Buffer checking

(defun ecc-auto-response-enhanced-check-all-buffers ()
  "Check all registered buffers for prompt states and respond if needed."
  (when ecc-auto-response-enhanced-enabled
    (dolist (buffer (ecc-buffer-state-cleanup-buffers))
      (when (buffer-live-p buffer)
        (ecc-auto-response-enhanced-check-buffer buffer)))))

(defun ecc-auto-response-enhanced-check-buffer (buffer)
  "Check BUFFER for prompt states and respond if needed."
  (when (and (buffer-live-p buffer)
             ecc-auto-response-enhanced-enabled)
    (with-current-buffer buffer
      (when ecc-auto-response-enhanced-buffer-enabled
        ;; Update buffer state
        (let ((state (ecc-buffer-state-update)))
          ;; If we have a state and should respond, do so
          (when (and state (ecc-auto-response-enhanced-should-respond-p state))
            (ecc-auto-response-enhanced-send buffer state)))))))

(defun ecc-auto-response-enhanced-should-respond-p (state)
  "Return t if we should respond to STATE in the current buffer.
Checks throttling and prevents duplicate responses."
  (and state
       ;; Don't respond to the same state twice in a row unless it's been a while
       (or (not (eq state ecc-auto-response-enhanced-last-response-state))
           (> (- (float-time) ecc-auto-response-enhanced-last-response-time) 30.0))
       ;; Make sure we're not responding too frequently
       (> (- (float-time) ecc-auto-response-enhanced-last-response-time)
          (or (and (boundp 'ecc-auto-core-throttle-time)
                   ecc-auto-core-throttle-time)
              3.0))))

;; Response functions

;;;###autoload
(defun ecc-auto-response-enhanced-send (buffer &optional state)
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
  (unless ecc-auto-response-enhanced-enabled
    (ecc-debug-message "Enhanced auto-response is disabled, not sending response")
    (cl-return-from ecc-auto-response-enhanced-send nil))
  
  ;; Skip if buffer is not live
  (unless (buffer-live-p buffer)
    (ecc-debug-message "Buffer is not live, not sending response")
    (cl-return-from ecc-auto-response-enhanced-send nil))
  
  (with-current-buffer buffer
    ;; Skip if buffer-specific auto-response is disabled
    (unless ecc-auto-response-enhanced-buffer-enabled
      (ecc-debug-message "Buffer-specific auto-response is disabled, not sending response")
      (cl-return-from ecc-auto-response-enhanced-send nil))
    
    ;; Get current state if not provided, prefer buffer-local state
    (let ((current-state (or state 
                             (and (boundp 'ecc-buffer-state-enabled)
                                  ecc-buffer-state-enabled
                                  ecc-buffer-state-current)
                             (ecc-detect-state))))
      (cond
       ((eq current-state :y/y/n)
        (ecc-auto-response-enhanced--send-message buffer ecc-auto-response-enhanced-yes-plus "Y/Y/N"))
       
       ((eq current-state :y/n)
        (ecc-auto-response-enhanced--send-message buffer ecc-auto-response-enhanced-yes "Y/N"))
       
       ((eq current-state :initial-waiting)
        (ecc-auto-response-enhanced--send-message buffer ecc-auto-response-enhanced-initial-waiting "Initial-Waiting"))
       
       ((eq current-state :waiting)
        (ecc-auto-response-enhanced--send-message buffer ecc-auto-response-enhanced-continue "Continue"))
       
       (t ;; No recognized state
        (ecc-debug-message "No recognized prompt state detected")
        nil)))))

(defun ecc-auto-response-enhanced--send-message (buffer response type)
  "Send RESPONSE to Claude in BUFFER.
TYPE is used for notification messages.
Updates buffer-local tracking of response time and state."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      ;; Update response tracking
      (setq ecc-auto-response-enhanced-last-response-time (float-time)
            ecc-auto-response-enhanced-last-response-state
            (ecc-buffer-state-get))
      
      ;; Check if we're in a terminal mode
      (cond
       ;; vterm mode
       ((derived-mode-p 'vterm-mode)
        (ecc-auto-response-enhanced--send-to-vterm buffer response))
       
       ;; comint mode (e.g., shell)
       ((derived-mode-p 'comint-mode)
        (comint-send-string
         (get-buffer-process buffer)
         (concat response "\n")))
       
       ;; Default fallback - just insert at point
       (t
        (insert response)
        (ecc-debug-message "Inserted response in buffer %s" (buffer-name buffer))))))
  
  ;; Notify user about the response if notifications are enabled
  (when ecc-auto-response-enhanced-notify
    (ecc-auto-response-enhanced--notify type response))
  
  ;; Return t to indicate success
  t)

(defun ecc-auto-response-enhanced--send-to-vterm (buffer response)
  "Send RESPONSE to Claude in vterm BUFFER."
  ;; Use the shared utility function with appropriate debug function
  (ecc-vterm-utils-send-string buffer response))

(defun ecc-auto-response-enhanced--notify (type response)
  "Display notification about auto-response of TYPE with actual RESPONSE string.
TYPE is a description of the response context (e.g., \"Y/N\").
RESPONSE is the actual string sent to Claude."
  (let ((msg (format "Auto-responded: %s (\"%s\")" type response)))
    (message msg)))

;; Convenience functions

;;;###autoload
(defun ecc-auto-response-enhanced-yes (&optional buffer)
  "Automatically send Y response to Claude Y/N prompt.
Sends the yes response (defined by `ecc-auto-response-enhanced-yes`) to
Claude when it's in a Y/N prompt state.

If BUFFER is nil, use current buffer."
  (interactive)
  (ecc-auto-response-enhanced--send-message 
   (or buffer (current-buffer))
   ecc-auto-response-enhanced-yes
   "Y/N"))

;;;###autoload
(defun ecc-auto-response-enhanced-yes-plus (&optional buffer)
  "Automatically send Y response to Claude Y/Y/N prompt.
Sends the yes-plus response (defined by `ecc-auto-response-enhanced-yes-plus`)
to Claude when it's in a Y/Y/N prompt state.

If BUFFER is nil, use current buffer."
  (interactive)
  (ecc-auto-response-enhanced--send-message
   (or buffer (current-buffer))
   ecc-auto-response-enhanced-yes-plus
   "Y/Y/N"))

;;;###autoload
(defun ecc-auto-response-enhanced-continue (&optional buffer)
  "Automatically send continue to Claude waiting prompt.
Sends the continue response (defined by `ecc-auto-response-enhanced-continue`) to
Claude when it's in a waiting state, prompting for more output.

If BUFFER is nil, use current buffer."
  (interactive)
  (ecc-auto-response-enhanced--send-message
   (or buffer (current-buffer))
   ecc-auto-response-enhanced-continue
   "Continue"))

;;;###autoload
(defun ecc-auto-response-enhanced-custom (response-text)
  "Send custom RESPONSE-TEXT to Claude.
This allows sending natural language responses instead of just command options."
  (interactive "sEnter your response: ")
  (let ((buffer (current-buffer)))
    (when (buffer-live-p buffer)
      (ecc-auto-response-enhanced--send-message
       buffer
       response-text
       (format "Custom: %s" response-text)))))

;; Status reporting

;;;###autoload
(defun ecc-auto-response-enhanced-status (&optional buffer)
  "Return status string for enhanced auto-response in BUFFER or current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (format "Enhanced Auto-Response Status for %s:
  Global Status: %s
  Buffer Status: %s
  Last Response: %s (%.2f seconds ago)
  State Tracking: %s
  Current State: %s"
            (buffer-name)
            (if ecc-auto-response-enhanced-enabled "Enabled" "Disabled")
            (if ecc-auto-response-enhanced-buffer-enabled "Enabled" "Disabled")
            (if ecc-auto-response-enhanced-last-response-state
                (ecc-state-get-name ecc-auto-response-enhanced-last-response-state)
              "None")
            (- (float-time) ecc-auto-response-enhanced-last-response-time)
            (if (and (boundp 'ecc-buffer-state-enabled) ecc-buffer-state-enabled)
                "Enabled" "Disabled")
            (if (and (boundp 'ecc-buffer-state-current) ecc-buffer-state-current)
                (ecc-state-get-name ecc-buffer-state-current)
              "None"))))

;;;###autoload
(defun ecc-auto-response-enhanced-print-status (&optional buffer)
  "Print status information about enhanced auto-response to messages."
  (interactive)
  (message "%s" (ecc-auto-response-enhanced-status buffer)))

;; Backward compatibility aliases

;; Compatibility with old code
(defalias 'ecc-start-enhanced-auto-response 'ecc-auto-response-enhanced-start)
(defalias 'ecc-stop-enhanced-auto-response 'ecc-auto-response-enhanced-stop)
(defalias 'ecc-toggle-enhanced-auto-response 'ecc-auto-response-enhanced-toggle)

(provide 'ecc-auto-response-enhanced)

;;; ecc-auto-response-enhanced.el ends here