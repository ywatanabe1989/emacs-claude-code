;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 15:45:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-auto-response.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:
;;; Automatic response functionality for Claude prompts.
;;;
;;; This module provides functionality for automatically responding
;;; to different types of Claude prompts, such as Y/N questions,
;;; waiting prompts, and initial waiting states.
;;;
;;; Features:
;;; - Automatic response to different types of Claude prompts
;;; - Support for both global and buffer-local configurations
;;; - Integration with state detection system
;;; - Support for different terminal modes (vterm, comint)
;;; - Throttling to prevent excessive responses
;;; - Customizable response values
;;; - Notification system
;;;
;;; Example usage:
;;; ```elisp
;;; ;; Start auto-response in global mode
;;; (setq ecc-auto-response-buffer-local-default nil)
;;; (ecc-auto-response-start)
;;;
;;; ;; Start auto-response in buffer-local mode
;;; (setq ecc-auto-response-buffer-local-default t)
;;; (ecc-auto-response-start)
;;;
;;; ;; Manually send responses to prompts
;;; (ecc-auto-response-yes)          ; Send yes to Y/N prompt
;;; (ecc-auto-response-yes-plus)     ; Send yes to Y/Y/N prompt
;;; (ecc-auto-response-continue)     ; Send continue to waiting prompt
;;; (ecc-auto-response-custom "Hello Claude") ; Send custom text
;;; ```

(require 'cl-lib)
(require 'ecc-variables)
(require 'ecc-auto-core)
(require 'ecc-state-detection)
(require 'ecc-vterm-utils)
(require 'ecc-debug-utils)
(when (featurep 'ecc-buffer-local)
  (require 'ecc-buffer-local))
(when (featurep 'ecc-buffer-state)
  (require 'ecc-buffer-state))

;;; Code:

;;;; Customization Options

(defgroup ecc-auto-response nil
  "Automatic response settings for Claude."
  :group 'ecc
  :prefix "ecc-auto-response-")

(defcustom ecc-auto-response-enabled nil
  "Whether auto-response functionality is enabled globally.
When enabled, the system will automatically respond to Claude prompts
based on the configured response values.

This setting applies to global mode only. For buffer-local mode,
each buffer has its own enabled state."
  :type 'boolean
  :group 'ecc-auto-response)

(defcustom ecc-auto-response-yes "1"
  "Response to send for Y/N prompts (typically \"1\" for \"yes\").
This string will be sent automatically when Claude displays a Y/N prompt
and auto-response is enabled."
  :type 'string
  :group 'ecc-auto-response)

(defcustom ecc-auto-response-yes-plus "2"
  "Response to send for Y/Y/N prompts (typically \"2\" for second option).
This string will be sent automatically when Claude displays a Y/Y/N prompt
and auto-response is enabled."
  :type 'string
  :group 'ecc-auto-response)

(defcustom ecc-auto-response-continue "/auto"
  "Response to send for waiting state (typically \"/auto\" or \"/continue\").
This string will be sent automatically when Claude displays a waiting prompt
and auto-response is enabled."
  :type 'string 
  :group 'ecc-auto-response)

(defcustom ecc-auto-response-initial-waiting "/user:understand-guidelines"
  "Response to send for initial waiting state.
This string will be sent automatically when Claude displays its initial
waiting prompt and auto-response is enabled. This is typically a command
that starts the interaction."
  :type 'string
  :group 'ecc-auto-response)

(defcustom ecc-auto-response-notify t
  "Whether to show notifications when automatic responses are sent.
When enabled, a message will be displayed in the echo area each time
an automatic response is sent."
  :type 'boolean
  :group 'ecc-auto-response)

(defcustom ecc-auto-response-buffer-local-default t
  "Whether to use buffer-local mode by default instead of global mode.
When non-nil, auto-response will operate on a per-buffer basis with
independent settings. When nil, global configuration will be used."
  :type 'boolean
  :group 'ecc-auto-response)

(defcustom ecc-auto-response-default-enabled nil
  "Whether buffer-local auto-response is enabled by default for new buffers.
When starting buffer-local auto-response mode, this value is used as the
initial enabled state for buffers that don't already have a setting."
  :type 'boolean
  :group 'ecc-auto-response)

;;;; Internal Variables

(defvar ecc-auto-response--registered-callback nil
  "Callback function to process detected states in registered buffers.")

;;;; Core Functionality

;;;###autoload
(defun ecc-auto-response-start ()
  "Start the auto-response system.
Initializes and activates the auto-response system that automatically responds
to different types of Claude prompts.

Sets up a timer using the core infrastructure that periodically checks
buffers with Claude interactions for prompts and sends appropriate responses."
  (interactive)
  
  (if ecc-auto-response-buffer-local-default
      ;; Start buffer-local mode
      (ecc-auto-response-buffer-local-start)
    
    ;; Start global mode
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
    
    (ecc-auto-response--report-status "started")))

;;;###autoload
(defun ecc-auto-response-stop ()
  "Stop the auto-response system.
Deactivates the auto-response system and cancels any pending timers."
  (interactive)
  
  (if ecc-auto-response-buffer-local-default
      ;; Stop buffer-local mode
      (ecc-auto-response-buffer-local-stop)
    
    ;; Stop global mode
    ;; Disable auto-response
    (setq ecc-auto-response-enabled nil)
    
    ;; Clear callback
    (setq ecc-auto-response--registered-callback nil)
    
    ;; Stop core timer
    (ecc-auto-core-timer-stop)
    
    ;; Reset core state
    (ecc-auto-core-reset-state)
    
    (message "Auto-response stopped")))

;;;###autoload
(defun ecc-auto-response-toggle ()
  "Toggle auto-response on/off."
  (interactive)
  (if ecc-auto-response-buffer-local-default
      ;; Toggle buffer-local mode
      (if (and (boundp 'ecc-buffer-auto-response-enabled)
               ecc-buffer-auto-response-enabled)
          (ecc-auto-response-buffer-local-stop)
        (ecc-auto-response-buffer-local-start))
    
    ;; Toggle global mode
    (if ecc-auto-response-enabled
        (ecc-auto-response-stop)
      (ecc-auto-response-start))))

;;;###autoload
(defun ecc-auto-response-register-buffer (&optional buffer)
  "Register BUFFER for auto-response.
If BUFFER is nil, use current buffer.
Returns the buffer if registered successfully."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (if ecc-auto-response-buffer-local-default
        ;; Register with buffer-local system
        (ecc-auto-response-buffer-local-init buf)
      
      ;; Register with global system
      (prog1 (ecc-auto-core-register-buffer buf)
        (when (called-interactively-p 'any)
          (message "Buffer %s registered for auto-response" (buffer-name buf)))))))

;;;; Response Handling (Global Mode)

;;;###autoload
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
  (ecc-vterm-utils-send-string buffer response (when (fboundp 'ecc-debug-make-debug-fn)
                                                  (ecc-debug-make-debug-fn))))

(defun ecc-auto-response--notify (type response)
  "Display notification about auto-response of TYPE with RESPONSE string.
TYPE is a description of the response context (e.g., \"Y/N\").
RESPONSE is the actual string sent to Claude."
  (let ((msg (format "Auto-responded: %s (\"%s\")" type response)))
    (message msg)))

;;;; Buffer-Local Auto-Response System

;;;###autoload
(defun ecc-auto-response-buffer-local-init (&optional buffer)
  "Initialize buffer-local auto-response for BUFFER or current buffer.
Sets up the necessary buffer-local state tracking and configuration."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    ;; Initialize buffer state container if available
    (when (fboundp 'ecc-buffer-state-init)
      (ecc-buffer-state-init))
    
    ;; Ensure buffer-local variables are set up
    (unless (boundp 'ecc-buffer-auto-response-enabled)
      (when (fboundp 'ecc-buffer-local-init)
        (ecc-buffer-local-init)))
    
    ;; Set default enabled state
    (when (boundp 'ecc-buffer-auto-response-enabled)
      (setq-local ecc-buffer-auto-response-enabled
                  ecc-auto-response-default-enabled))
    
    ;; Set default response values from global settings
    (when (boundp 'ecc-buffer-auto-response-y/n)
      (setq-local ecc-buffer-auto-response-y/n ecc-auto-response-yes))
    (when (boundp 'ecc-buffer-auto-response-y/y/n)
      (setq-local ecc-buffer-auto-response-y/y/n ecc-auto-response-yes-plus))
    (when (boundp 'ecc-buffer-auto-response-waiting)
      (setq-local ecc-buffer-auto-response-waiting ecc-auto-response-continue))
    (when (boundp 'ecc-buffer-auto-response-initial-waiting)
      (setq-local ecc-buffer-auto-response-initial-waiting ecc-auto-response-initial-waiting))
    (when (boundp 'ecc-buffer-auto-notify-completions)
      (setq-local ecc-buffer-auto-notify-completions ecc-auto-response-notify))
    
    ;; Register for background detection if available
    (when (fboundp 'ecc-auto-core-register-buffer-local)
      (ecc-auto-core-register-buffer-local (current-buffer)))
    
    (when (called-interactively-p 'any)
      (message "Buffer-local auto-response initialized for %s" (buffer-name)))))

;;;###autoload
(defun ecc-auto-response-buffer-local-start ()
  "Start the buffer-local auto-response system.
Initializes current buffer with buffer-local auto-response."
  (interactive)
  
  ;; Initialize current buffer
  (ecc-auto-response-buffer-local-init)
  
  ;; Enable auto-response for current buffer if variable exists
  (when (boundp 'ecc-buffer-auto-response-enabled)
    (setq-local ecc-buffer-auto-response-enabled t))
  
  ;; Start background detection with our callback if available
  (when (fboundp 'ecc-auto-core-timer-start-buffer-local)
    (ecc-auto-core-timer-start-buffer-local 
     (lambda (buffer) 
       (ecc-auto-response-buffer-local-check buffer))))
  
  (message "Buffer-local auto-response started in %s" (buffer-name)))

;;;###autoload
(defun ecc-auto-response-buffer-local-stop ()
  "Stop the buffer-local auto-response system."
  (interactive)
  
  ;; Stop background detection if available
  (when (fboundp 'ecc-auto-core-timer-stop-buffer-local)
    (ecc-auto-core-timer-stop-buffer-local))
  
  ;; Disable auto-response for current buffer if variable exists
  (when (boundp 'ecc-buffer-auto-response-enabled)
    (setq-local ecc-buffer-auto-response-enabled nil))
  
  (message "Buffer-local auto-response stopped in %s" (buffer-name)))

;;;###autoload
(defun ecc-auto-response-buffer-local-check (buffer)
  "Check BUFFER for Claude prompt and respond if appropriate.
Uses buffer-local configuration and state tracking.
Returns t if a response was sent, nil otherwise."
  (interactive (list (current-buffer)))
  
  ;; Skip if buffer is not live
  (unless (buffer-live-p buffer)
    (ecc-auto-response--debug-message "Buffer is not live, not sending response")
    (cl-return-from ecc-auto-response-buffer-local-check nil))
  
  ;; Check if auto-response is enabled for this buffer
  (with-current-buffer buffer
    ;; Skip if auto-response is disabled for this buffer
    (unless (and (boundp 'ecc-buffer-auto-response-enabled)
                 ecc-buffer-auto-response-enabled)
      (ecc-auto-response--debug-buffer-message "Auto-response disabled for buffer %s" (buffer-name))
      (cl-return-from ecc-auto-response-buffer-local-check nil))
    
    ;; Get current prompt state 
    (let ((current-state (ecc-detect-state)))
      
      ;; Skip if no state detected
      (unless current-state
        (ecc-auto-response--debug-buffer-message "No state detected in buffer %s" (buffer-name))
        (cl-return-from ecc-auto-response-buffer-local-check nil))
      
      ;; Check if we should throttle responses if buffer-state is available
      (when (and (fboundp 'ecc-buffer-state-throttled-p)
                 (ecc-buffer-state-throttled-p current-state))
        (ecc-auto-response--debug-buffer-message "Throttled response to %s in buffer %s"
                         current-state (buffer-name))
        (cl-return-from ecc-auto-response-buffer-local-check nil))
      
      ;; Update buffer state with current state if available
      (when (fboundp 'ecc-buffer-state-set)
        (ecc-buffer-state-set 'prompt-state current-state))
      
      ;; Send response based on state type
      (pcase current-state
       (:y/y/n (ecc-auto-response-buffer-local-send-message 
                buffer 
                (if (boundp 'ecc-buffer-auto-response-y/y/n)
                    ecc-buffer-auto-response-y/y/n
                  ecc-auto-response-yes-plus)
                "Y/Y/N"))
       
       (:y/n (ecc-auto-response-buffer-local-send-message 
              buffer 
              (if (boundp 'ecc-buffer-auto-response-y/n)
                  ecc-buffer-auto-response-y/n
                ecc-auto-response-yes)
              "Y/N"))
       
       (:initial-waiting (ecc-auto-response-buffer-local-send-message 
                         buffer 
                         (if (boundp 'ecc-buffer-auto-response-initial-waiting)
                             ecc-buffer-auto-response-initial-waiting
                           ecc-auto-response-initial-waiting)
                         "Initial-Waiting"))
       
       (:waiting (ecc-auto-response-buffer-local-send-message 
                  buffer 
                  (if (boundp 'ecc-buffer-auto-response-waiting)
                      ecc-buffer-auto-response-waiting
                    ecc-auto-response-continue)
                  "Continue"))
       
       (_ ;; Unrecognized state
        (ecc-auto-response--debug-buffer-message "Unrecognized state %s in buffer %s"
                       current-state (buffer-name))
        nil)))))

(defun ecc-auto-response-buffer-local-send-message (buffer response type)
  "Send RESPONSE to Claude in BUFFER.
TYPE is a descriptive string for notification messages.
Uses buffer-local configuration."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      ;; Set as active state to prevent duplicate responses if buffer-state is available
      (when (and (fboundp 'ecc-buffer-state-set)
                 (fboundp 'ecc-buffer-state-get))
        (ecc-buffer-state-set 'active-state (ecc-buffer-state-get 'prompt-state)))
      
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
        (ecc-auto-response--debug-buffer-message "Cannot send response to buffer %s in %s mode"
                     (buffer-name) major-mode)
        (cl-return-from ecc-auto-response-buffer-local-send-message nil)))
      
      ;; Update time tracking if buffer-state is available
      (when (and (fboundp 'ecc-buffer-state-set)
                 (fboundp 'ecc-buffer-state-get))
        (let ((state (ecc-buffer-state-get 'prompt-state)))
          (ecc-buffer-state-set 
           (intern (format "last-%s-time" (symbol-name state)))
           (float-time))))
      
      ;; Notify user if enabled
      (when (boundp 'ecc-buffer-auto-notify-completions)
        (ecc-auto-response-buffer-local-notify buffer type response))))
  
  ;; Return t to indicate success
  t)

(defun ecc-auto-response-buffer-local-send-to-vterm (buffer response)
  "Send RESPONSE to Claude in vterm BUFFER.
Uses buffer-local state tracking."
  ;; Use the shared utility function with debug support
  (ecc-vterm-utils-send-string 
   buffer response (when (fboundp 'ecc-debug-make-debug-fn)
                      (ecc-debug-make-debug-fn buffer))))

(defun ecc-auto-response-buffer-local-notify (buffer type response)
  "Display notification about auto-response in BUFFER of TYPE with RESPONSE.
Uses buffer-local notification settings."
  (with-current-buffer buffer
    (when (and (boundp 'ecc-buffer-auto-notify-completions)
               ecc-buffer-auto-notify-completions)
      (let ((msg (format "Auto-responded in %s: %s (\"%s\")"
                         (buffer-name) type response)))
        (message msg)))))

;;;; Convenience Functions

;;;###autoload
(defun ecc-auto-response-yes (&optional buffer)
  "Automatically send Y response to Claude Y/N prompt.
Sends the yes response to Claude when it's in a Y/N prompt state.
If BUFFER is nil, use current buffer."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (if (and (buffer-local-value 'ecc-buffer-auto-response-enabled buf)
             ecc-auto-response-buffer-local-default)
        ;; Use buffer-local mode
        (ecc-auto-response-buffer-local-send-message 
         buf 
         (buffer-local-value 'ecc-buffer-auto-response-y/n buf)
         "Y/N")
      ;; Use global mode
      (ecc-auto-response--dispatch-response 
       buf
       ecc-auto-response-yes
       "Y/N"))))

;;;###autoload
(defun ecc-auto-response-yes-plus (&optional buffer)
  "Automatically send Y response to Claude Y/Y/N prompt.
Sends the yes-plus response to Claude when it's in a Y/Y/N prompt state.
If BUFFER is nil, use current buffer."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (if (and (buffer-local-value 'ecc-buffer-auto-response-enabled buf)
             ecc-auto-response-buffer-local-default)
        ;; Use buffer-local mode
        (ecc-auto-response-buffer-local-send-message 
         buf 
         (buffer-local-value 'ecc-buffer-auto-response-y/y/n buf)
         "Y/Y/N")
      ;; Use global mode
      (ecc-auto-response--dispatch-response
       buf
       ecc-auto-response-yes-plus
       "Y/Y/N"))))

;;;###autoload
(defun ecc-auto-response-continue (&optional buffer)
  "Automatically send continue to Claude waiting prompt.
Sends the continue response to Claude when it's in a waiting state.
If BUFFER is nil, use current buffer."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (if (and (buffer-local-value 'ecc-buffer-auto-response-enabled buf)
             ecc-auto-response-buffer-local-default)
        ;; Use buffer-local mode
        (ecc-auto-response-buffer-local-send-message 
         buf 
         (buffer-local-value 'ecc-buffer-auto-response-waiting buf)
         "Continue")
      ;; Use global mode
      (ecc-auto-response--dispatch-response
       buf
       ecc-auto-response-continue
       "Continue"))))

;;;###autoload
(defun ecc-auto-response-custom (response-text)
  "Send custom RESPONSE-TEXT to Claude.
This allows sending natural language responses instead of just command options."
  (interactive "sEnter your response: ")
  (let ((buffer (current-buffer)))
    (when (buffer-live-p buffer)
      (if (and (buffer-local-value 'ecc-buffer-auto-response-enabled buffer)
               ecc-auto-response-buffer-local-default)
          ;; Use buffer-local mode
          (ecc-auto-response-buffer-local-send-message
           buffer
           response-text
           (format "Custom: %s" response-text))
        ;; Use global mode
        (ecc-auto-response--dispatch-response
         buffer
         response-text
         (format "Custom: %s" response-text))))))

;;;; Utility Functions

(defun ecc-auto-response--report-status (action)
  "Report status change with ACTION and current settings."
  (message "Auto-response %s: Y/N=%s, Y/Y/N=%s, Continue=%s" 
           action
           ecc-auto-response-yes
           ecc-auto-response-yes-plus
           ecc-auto-response-continue))

(defun ecc-auto-response--debug-message (message)
  "Output debug MESSAGE if debugging is enabled."
  (when (fboundp 'ecc-debug-message)
    (ecc-debug-message message)))

(defun ecc-auto-response--debug-buffer-message (format-string &rest args)
  "Output formatted debug message with FORMAT-STRING and ARGS if buffer debugging is enabled."
  (when (and (boundp 'ecc-buffer-debug-enabled)
             ecc-buffer-debug-enabled
             (fboundp 'ecc-debug-message))
    (apply #'ecc-debug-message format-string args)))

;;;; Backward Compatibility

;; Aliases for compatibility with older code
(defalias 'ecc-start-auto-response 'ecc-auto-response-start)
(defalias 'ecc-stop-auto-response 'ecc-auto-response-stop)
(defalias 'ecc-toggle-auto-response 'ecc-auto-response-toggle)
(defalias 'ecc-auto-accept-send 'ecc-auto-response-send)
(defalias 'ecc-auto-response-template 'ecc-auto-response-custom)
(defalias 'ecc-auto-response-buffer-local-start 'ecc-auto-response-start)
(defalias 'ecc-auto-response-buffer-local-stop 'ecc-auto-response-stop)
(defalias 'ecc-auto-response-buffer-local-toggle 'ecc-auto-response-toggle)

(defun ecc-check-and-respond ()
  "Compatibility function for old auto-response checking."
  (when ecc-auto-response-enabled
    (if ecc-auto-response-buffer-local-default
        ;; Process all buffers with buffer-local mode
        (mapc #'ecc-auto-response-buffer-local-check
              (ecc-auto-core-get-registered-buffers))
      ;; Process all buffers with global mode
      (ecc-auto-core-process-all-buffers ecc-auto-response--registered-callback))))

(provide 'ecc-auto-response)
(provide 'ecc-auto-response-improved)
(provide 'ecc-auto-response-buffer-local)
(provide 'ecc-auto-response-consolidated)
(provide 'ecc-auto-response-enhanced)

;;; ecc-auto-response.el ends here