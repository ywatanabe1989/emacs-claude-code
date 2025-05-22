;;; -*- coding: utf-8; lexical-binding: t -*-
;;; ecc-auto-response-consolidated.el --- Consolidated auto-response system
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 17:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-auto-response-consolidated.el

;;; Commentary:
;;; Consolidated auto-response system for Claude prompts.
;;;
;;; This module provides a unified auto-response system for Claude prompts,
;;; combining functionality from multiple previous implementations:
;;; - ecc-auto-response.el
;;; - ecc-auto-response-improved.el
;;; - ecc-auto-response-buffer-local.el
;;; - ecc-auto-response-enhanced.el
;;;
;;; Features:
;;; - Both global and buffer-local auto-response modes
;;; - Reliable state detection integration
;;; - Intelligent throttling to prevent response spam
;;; - Comprehensive notification system
;;; - State history awareness to prevent duplicate responses
;;; - Full backward compatibility with all previous versions
;;; - Extensive debugging capabilities
;;;
;;; Basic usage:
;;;
;;; Global mode:
;;; ```elisp
;;; ;; Start global auto-response
;;; (ecc-auto-response-start)
;;;
;;; ;; Configure response values (optional)
;;; (setq ecc-auto-response-yes "1")
;;; (setq ecc-auto-response-continue "/auto")
;;;
;;; ;; Toggle auto-response on/off
;;; (ecc-auto-response-toggle)
;;; ```
;;;
;;; Buffer-local mode:
;;; ```elisp
;;; ;; Enable buffer-local auto-response for current buffer
;;; (ecc-auto-response-buffer-start)
;;;
;;; ;; Configure buffer-local responses (optional)
;;; (setq-local ecc-auto-response-buffer-yes "y")
;;; (setq-local ecc-auto-response-buffer-continue "/continue")
;;;
;;; ;; Toggle buffer-local auto-response on/off
;;; (ecc-auto-response-buffer-toggle)
;;; ```
;;;
;;; Manual response:
;;; ```elisp
;;; ;; Send responses manually
;;; (ecc-auto-response-yes)          ;; Send yes to Y/N prompt
;;; (ecc-auto-response-continue)     ;; Send continue response
;;; (ecc-auto-response-send "text")  ;; Send custom text
;;; ```

;;; Code:

;; Dependencies - prefer consolidated modules when available
(require 'cl-lib)

(if (featurep 'ecc-variables-consolidated)
    (require 'ecc-variables-consolidated)
  (require 'ecc-variables))

(if (featurep 'ecc-state-detection-consolidated)
    (require 'ecc-state-detection-consolidated)
  (require 'ecc-state-detection))

(if (featurep 'ecc-debug-utils-consolidated)
    (require 'ecc-debug-utils-consolidated)
  (require 'ecc-debug-utils))

(require 'ecc-auto-core)
(require 'ecc-vterm-utils)

;; Buffer-local module dependencies (optional)
(when (featurep 'ecc-buffer-local)
  (require 'ecc-buffer-local))

(when (featurep 'ecc-buffer-state)
  (require 'ecc-buffer-state))

(when (featurep 'ecc-background-detection)
  (require 'ecc-background-detection))

;;;; Customization Options

(defgroup ecc-auto-response nil
  "Automatic response settings for Claude."
  :group 'ecc
  :prefix "ecc-auto-response-")

;; Global mode settings
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
an automatic response is sent to Claude."
  :type 'boolean
  :group 'ecc-auto-response)

(defcustom ecc-auto-response-check-interval 1.0
  "Interval in seconds for checking buffers for auto-response.
Lower values make the auto-response system more responsive but use more CPU."
  :type 'number
  :group 'ecc-auto-response)

(defcustom ecc-auto-response-throttle-duration 5.0
  "Minimum time in seconds between auto-responses for the same state.
This prevents sending multiple responses to the same prompt in rapid succession."
  :type 'number
  :group 'ecc-auto-response)

(defcustom ecc-auto-response-debug nil
  "Whether to enable debug messages for auto-response system.
When enabled, detailed debug messages are displayed for auto-response operations."
  :type 'boolean
  :group 'ecc-auto-response)

;; Buffer-local mode settings
(defcustom ecc-auto-response-buffer-local-default nil
  "Whether to use buffer-local mode by default.
When enabled, auto-response operates independently in each buffer with its
own configuration and state tracking. When disabled, global mode is used
where a single set of configurations applies to all buffers."
  :type 'boolean
  :group 'ecc-auto-response)

;;;; Internal Variables

;; Global state tracking
(defvar ecc-auto-response--timer nil
  "Timer for auto-response system.")

(defvar ecc-auto-response--registered-buffers (make-hash-table :test 'eq)
  "Hash table of registered buffers for auto-response.")

(defvar ecc-auto-response--last-state nil
  "Last Claude state that received an auto-response.")

(defvar ecc-auto-response--last-response-time 0
  "Timestamp of last auto-response.")

(defvar ecc-auto-response--registered-callback nil
  "Callback function registered with auto-core system.")

;; Buffer-local variables
(defvar-local ecc-auto-response-buffer-enabled nil
  "Whether auto-response is enabled for this buffer.")

(defvar-local ecc-auto-response-buffer-yes "1"
  "Response to send for Y/N prompts in this buffer.")

(defvar-local ecc-auto-response-buffer-yes-plus "2"
  "Response to send for Y/Y/N prompts in this buffer.")

(defvar-local ecc-auto-response-buffer-continue "/auto"
  "Response to send for waiting state in this buffer.")

(defvar-local ecc-auto-response-buffer-initial-waiting "/user:understand-guidelines"
  "Response to send for initial waiting state in this buffer.")

(defvar-local ecc-auto-response-buffer-last-state nil
  "Last Claude state that received an auto-response in this buffer.")

(defvar-local ecc-auto-response-buffer-last-response-time 0
  "Timestamp of last auto-response in this buffer.")

;;;; Debugging Utilities

(defun ecc-auto-response--debug (format-string &rest args)
  "Log a debug message for auto-response with FORMAT-STRING and ARGS.
Only displays the message if `ecc-auto-response-debug' is non-nil.
Uses the consolidated debug utils if available, otherwise falls back."
  (when ecc-auto-response-debug
    (if (fboundp 'ecc-debug-message)
        (apply #'ecc-debug-message (concat "[Auto-Response] " format-string) args)
      (message "[Auto-Response Debug] %s" (apply #'format format-string args)))))

;;;; Core Functions - Global Mode

;;;###autoload
(defun ecc-auto-response-start (&optional yes yes-plus continue)
  "Start the auto-response system with optional response values.
When started, automatically responds to Claude prompts based on the detected state.

Optional arguments:
  YES: Response for Y/N prompts (default: `ecc-auto-response-yes')
  YES-PLUS: Response for Y/Y/N prompts (default: `ecc-auto-response-yes-plus')
  CONTINUE: Response for waiting prompts (default: `ecc-auto-response-continue')

Example:
  (ecc-auto-response-start)  ;; Start with default responses
  (ecc-auto-response-start \"y\" \"y\" \"/continue\")  ;; Start with custom responses"
  (interactive)
  (ecc-auto-response--debug "Starting auto-response system")
  
  ;; Update response values if provided
  (when yes (setq ecc-auto-response-yes yes))
  (when yes-plus (setq ecc-auto-response-yes-plus yes-plus))
  (when continue (setq ecc-auto-response-continue continue))
  
  ;; Enable auto-response
  (setq ecc-auto-response-enabled t)
  
  ;; Reset last response tracking
  (setq ecc-auto-response--last-state nil)
  (setq ecc-auto-response--last-response-time 0)
  
  ;; Start timer if not already running
  (unless ecc-auto-response--timer
    (setq ecc-auto-response--timer
          (run-with-timer 0 ecc-auto-response-check-interval
                         'ecc-auto-response--process-all-buffers))
    (ecc-auto-response--debug "Started auto-response timer with interval %s"
                             ecc-auto-response-check-interval))
  
  ;; Show startup message
  (message "Auto-response enabled: Y/N=%s, Y/Y/N=%s, Continue=%s"
           ecc-auto-response-yes
           ecc-auto-response-yes-plus
           ecc-auto-response-continue))

;;;###autoload
(defun ecc-auto-response-stop ()
  "Stop the auto-response system.
Cancels the timer and disables automatic responses to Claude prompts."
  (interactive)
  (ecc-auto-response--debug "Stopping auto-response system")
  
  ;; Disable auto-response
  (setq ecc-auto-response-enabled nil)
  
  ;; Cancel timer if running
  (when ecc-auto-response--timer
    (cancel-timer ecc-auto-response--timer)
    (setq ecc-auto-response--timer nil)
    (ecc-auto-response--debug "Cancelled auto-response timer"))
  
  ;; Show stop message
  (message "Auto-response disabled"))

;;;###autoload
(defun ecc-auto-response-toggle ()
  "Toggle the auto-response system on or off.
Enables or disables automatic responses to Claude prompts."
  (interactive)
  (if ecc-auto-response-enabled
      (ecc-auto-response-stop)
    (ecc-auto-response-start)))

;;;###autoload
(defun ecc-auto-response-reset ()
  "Reset and restart the auto-response system.
Useful to apply new settings or clear state."
  (interactive)
  (ecc-auto-response--debug "Resetting auto-response system")
  (ecc-auto-response-stop)
  (ecc-auto-response-start))

;;;; Buffer Management

;;;###autoload
(defun ecc-auto-response-register-buffer (buffer)
  "Register BUFFER for auto-response.
Registered buffers will be checked for Claude prompts when auto-response is enabled."
  (interactive (list (current-buffer)))
  (unless (buffer-live-p buffer)
    (ecc-auto-response--debug "Cannot register non-live buffer")
    (user-error "Buffer is not alive"))
  
  (puthash buffer t ecc-auto-response--registered-buffers)
  (ecc-auto-response--debug "Registered buffer: %s" (buffer-name buffer))
  buffer)

;;;###autoload
(defun ecc-auto-response-unregister-buffer (buffer)
  "Unregister BUFFER from auto-response.
The buffer will no longer be checked for Claude prompts."
  (interactive (list (current-buffer)))
  (remhash buffer ecc-auto-response--registered-buffers)
  (ecc-auto-response--debug "Unregistered buffer: %s" 
                           (if (buffer-live-p buffer)
                               (buffer-name buffer)
                             "non-live buffer"))
  buffer)

(defun ecc-auto-response--process-all-buffers ()
  "Process all registered buffers for auto-response.
Checks each buffer for Claude prompts and sends responses if appropriate."
  (when ecc-auto-response-enabled
    (ecc-auto-response--debug "Processing all buffers for auto-response")
    
    ;; Loop through registered buffers
    (maphash
     (lambda (buffer _)
       ;; Skip if buffer is not live
       (unless (buffer-live-p buffer)
         (ecc-auto-response--debug "Removing non-live buffer from registered list")
         (ecc-auto-response-unregister-buffer buffer)
         (cl-return))
       
       ;; Process buffer based on mode
       (if ecc-auto-response-buffer-local-default
           ;; Buffer-local mode
           (with-current-buffer buffer
             (when (or ecc-auto-response-buffer-enabled 
                      ;; For backward compatibility
                      (and (boundp 'ecc-buffer-auto-response-enabled)
                           ecc-buffer-auto-response-enabled))
               (ecc-auto-response--process-buffer-local buffer)))
         
         ;; Global mode
         (ecc-auto-response--process-buffer-global buffer)))
     ecc-auto-response--registered-buffers)))

(defun ecc-auto-response--process-buffer-global (buffer)
  "Process BUFFER for auto-response using global settings.
Checks buffer for Claude prompts and sends responses if appropriate."
  (with-current-buffer buffer
    (let ((state (ecc-detect-state)))
      (when state
        (ecc-auto-response--debug "Detected state %s in buffer %s" 
                                 state (buffer-name))
        
        ;; Check if throttling needed
        (unless (ecc-auto-response--throttled-p state)
          ;; Send response based on state
          (ecc-auto-response--send-for-state state buffer))))))

(defun ecc-auto-response--throttled-p (state)
  "Check if auto-response for STATE should be throttled.
Prevents sending multiple responses to the same prompt in rapid succession.
Returns t if throttled, nil otherwise."
  (let ((current-time (float-time))
        (same-state (eq state ecc-auto-response--last-state)))
    
    ;; Check if we need to throttle
    (when (and same-state
               (< (- current-time ecc-auto-response--last-response-time)
                  ecc-auto-response-throttle-duration))
      (ecc-auto-response--debug "Throttled response to %s (time since last: %s)"
                               state
                               (- current-time ecc-auto-response--last-response-time))
      t)))

(defun ecc-auto-response--send-for-state (state buffer)
  "Send appropriate response for STATE in BUFFER.
Handles different types of Claude prompts with the configured responses."
  (ecc-auto-response--debug "Sending response for state %s in buffer %s"
                           state (buffer-name buffer))

  ;; Record state and time for throttling
  (setq ecc-auto-response--last-state state)
  (setq ecc-auto-response--last-response-time (float-time))
  
  ;; Send response based on state
  (with-current-buffer buffer
    (ecc-auto-notify-prompt (format "%s" state))
    (cond
     ((eq state :y/y/n)
      (ecc-auto-response--send-to-buffer 
       buffer ecc-auto-response-yes-plus "Y/Y/N"))
     
     ((eq state :y/n)
      (ecc-auto-response--send-to-buffer 
       buffer ecc-auto-response-yes "Y/N"))
     
     ((eq state :initial-waiting)
      (ecc-auto-response--send-to-buffer 
       buffer ecc-auto-response-initial-waiting "Initial-Waiting"))
     
     ((eq state :waiting)
      (ecc-auto-response--send-to-buffer 
       buffer ecc-auto-response-continue "Continue"))
     
     (t ;; Unrecognized state
      (ecc-auto-response--debug "Unknown state %s, not sending response" state)
      nil))))

(defun ecc-auto-response--send-to-buffer (buffer text state-name)
  "Send TEXT to BUFFER and notify with STATE-NAME.
Handles sending the text through the appropriate buffer mode (vterm, comint, etc.)."
  (ecc-auto-response--debug "Sending '%s' to buffer %s for %s state"
                           text (buffer-name buffer) state-name)
  
  (with-current-buffer buffer
    ;; Send text based on buffer mode
    (cond
     ;; VTerm mode
     ((derived-mode-p 'vterm-mode)
      (vterm-send-string text)
      (vterm-send-return))
     
     ;; Comint mode
     ((derived-mode-p 'comint-mode)
      (goto-char (point-max))
      (insert text)
      (comint-send-input))
     
     ;; Default - insert and newline
     (t
      (goto-char (point-max))
      (insert text "\n"))))
  
  ;; Show notification if enabled
  (when ecc-auto-response-notify
    (message "Auto-response to %s: %s" state-name text)))

;;;; Core Functions - Buffer-Local Mode

;;;###autoload
(defun ecc-auto-response-buffer-start (&optional buffer yes yes-plus continue)
  "Start buffer-local auto-response for BUFFER with optional response values.
Enables automatic responses to Claude prompts in the specific buffer.

Arguments:
  BUFFER: Buffer to enable auto-response for (default: current buffer)
  YES: Response for Y/N prompts (default: `ecc-auto-response-buffer-yes')
  YES-PLUS: Response for Y/Y/N prompts (default: `ecc-auto-response-buffer-yes-plus')
  CONTINUE: Response for waiting prompts (default: `ecc-auto-response-buffer-continue')"
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (ecc-auto-response--debug "Starting buffer-local auto-response for %s"
                             (buffer-name buf))
    
    ;; Register buffer if not already registered
    (ecc-auto-response-register-buffer buf)
    
    ;; Update buffer-local settings
    (with-current-buffer buf
      ;; Update response values if provided
      (when yes (setq-local ecc-auto-response-buffer-yes yes))
      (when yes-plus (setq-local ecc-auto-response-buffer-yes-plus yes-plus))
      (when continue (setq-local ecc-auto-response-buffer-continue continue))
      
      ;; Enable buffer-local auto-response
      (setq-local ecc-auto-response-buffer-enabled t)
      
      ;; Set compatibility variables for backward compatibility
      (setq-local ecc-buffer-auto-response-enabled t)
      (setq-local ecc-buffer-auto-response-y/n ecc-auto-response-buffer-yes)
      (setq-local ecc-buffer-auto-response-y/y/n ecc-auto-response-buffer-yes-plus)
      (setq-local ecc-buffer-auto-response-waiting ecc-auto-response-buffer-continue)
      (setq-local ecc-buffer-auto-response-initial-waiting ecc-auto-response-buffer-initial-waiting)
      
      ;; Reset buffer-local response tracking
      (setq-local ecc-auto-response-buffer-last-state nil)
      (setq-local ecc-auto-response-buffer-last-response-time 0))
    
    ;; Start global timer if not already running
    (unless ecc-auto-response--timer
      (setq ecc-auto-response--timer
            (run-with-timer 0 ecc-auto-response-check-interval
                          'ecc-auto-response--process-all-buffers))
      (ecc-auto-response--debug "Started auto-response timer with interval %s"
                               ecc-auto-response-check-interval))
    
    ;; Show startup message
    (message "Buffer-local auto-response enabled for %s" (buffer-name buf))))

;;;###autoload
(defun ecc-auto-response-buffer-stop (&optional buffer)
  "Stop buffer-local auto-response for BUFFER.
Disables automatic responses to Claude prompts in the specific buffer.
The buffer remains registered but won't receive auto-responses."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (ecc-auto-response--debug "Stopping buffer-local auto-response for %s"
                             (buffer-name buf))
    
    ;; Disable buffer-local auto-response
    (with-current-buffer buf
      (setq-local ecc-auto-response-buffer-enabled nil)
      
      ;; Update compatibility variables
      (setq-local ecc-buffer-auto-response-enabled nil))
    
    ;; Show stop message
    (message "Buffer-local auto-response disabled for %s" (buffer-name buf))))

;;;###autoload
(defun ecc-auto-response-buffer-toggle (&optional buffer)
  "Toggle buffer-local auto-response for BUFFER on or off."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      (if ecc-auto-response-buffer-enabled
          (ecc-auto-response-buffer-stop buf)
        (ecc-auto-response-buffer-start buf)))))

(defun ecc-auto-response--process-buffer-local (buffer)
  "Process BUFFER for auto-response using buffer-local settings."
  (with-current-buffer buffer
    (let ((state (ecc-detect-state)))
      (when state
        (ecc-auto-response--debug "Detected state %s in buffer %s (buffer-local mode)" 
                                 state (buffer-name))
        
        ;; Check if throttling needed
        (let ((current-time (float-time))
              (same-state (eq state ecc-auto-response-buffer-last-state))
              (time-since-last (- (float-time) 
                                 ecc-auto-response-buffer-last-response-time)))
          
          ;; Skip if throttled
          (when (and same-state
                     (< time-since-last ecc-auto-response-throttle-duration))
            (ecc-auto-response--debug "Throttled buffer-local response to %s (time since last: %s)"
                                     state time-since-last)
            (cl-return-from ecc-auto-response--process-buffer-local nil))
          
          ;; Record state and time for throttling
          (setq-local ecc-auto-response-buffer-last-state state)
          (setq-local ecc-auto-response-buffer-last-response-time (float-time))
          
          ;; Send response based on state
          (cond
           ((eq state :y/y/n)
            (ecc-auto-response--send-to-buffer 
             buffer ecc-auto-response-buffer-yes-plus "Y/Y/N"))
           
           ((eq state :y/n)
            (ecc-auto-response--send-to-buffer 
             buffer ecc-auto-response-buffer-yes "Y/N"))
           
           ((eq state :initial-waiting)
            (ecc-auto-response--send-to-buffer 
             buffer ecc-auto-response-buffer-initial-waiting "Initial-Waiting"))
           
           ((eq state :waiting)
            (ecc-auto-response--send-to-buffer 
             buffer ecc-auto-response-buffer-continue "Continue"))
           
           (t ;; Unrecognized state
            (ecc-auto-response--debug "Unknown state %s, not sending response" state)
            nil)))))))

;;;; Manual Response Commands

;;;###autoload
(defun ecc-auto-response-yes (&optional buffer)
  "Manually send 'yes' response to Y/N prompt in BUFFER."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (ecc-auto-response--debug "Manual yes response in %s" (buffer-name buf))
    (with-current-buffer buf
      (if ecc-auto-response-buffer-local-default
          (ecc-auto-response--send-to-buffer buf ecc-auto-response-buffer-yes "Y/N (manual)")
        (ecc-auto-response--send-to-buffer buf ecc-auto-response-yes "Y/N (manual)")))))

;;;###autoload
(defun ecc-auto-response-yes-plus (&optional buffer)
  "Manually send 'yes-plus' response to Y/Y/N prompt in BUFFER."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (ecc-auto-response--debug "Manual yes-plus response in %s" (buffer-name buf))
    (with-current-buffer buf
      (if ecc-auto-response-buffer-local-default
          (ecc-auto-response--send-to-buffer buf ecc-auto-response-buffer-yes-plus "Y/Y/N (manual)")
        (ecc-auto-response--send-to-buffer buf ecc-auto-response-yes-plus "Y/Y/N (manual)")))))

;;;###autoload
(defun ecc-auto-response-continue (&optional buffer)
  "Manually send continue response to waiting prompt in BUFFER."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (ecc-auto-response--debug "Manual continue response in %s" (buffer-name buf))
    (with-current-buffer buf
      (if ecc-auto-response-buffer-local-default
          (ecc-auto-response--send-to-buffer buf ecc-auto-response-buffer-continue "Continue (manual)")
        (ecc-auto-response--send-to-buffer buf ecc-auto-response-continue "Continue (manual)")))))

;;;###autoload
(defun ecc-auto-response-initial-waiting (&optional buffer)
  "Manually send initial-waiting response in BUFFER."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (ecc-auto-response--debug "Manual initial-waiting response in %s" (buffer-name buf))
    (with-current-buffer buf
      (if ecc-auto-response-buffer-local-default
          (ecc-auto-response--send-to-buffer buf ecc-auto-response-buffer-initial-waiting "Initial-Waiting (manual)")
        (ecc-auto-response--send-to-buffer buf ecc-auto-response-initial-waiting "Initial-Waiting (manual)")))))

;;;###autoload
(defun ecc-auto-response-send (text &optional buffer)
  "Manually send TEXT as response in BUFFER.
Useful for sending custom responses not covered by the standard commands."
  (interactive "sResponse text: ")
  (let ((buf (or buffer (current-buffer))))
    (ecc-auto-response--debug "Manual custom response '%s' in %s" 
                             text (buffer-name buf))
    (ecc-auto-response--send-to-buffer buf text "Custom (manual)")))

;;;; Utility Functions

(defun ecc-auto-response-get-registered-buffers ()
  "Get a list of all registered buffers for auto-response.
Returns a list of live buffers that are currently registered."
  (let ((result nil))
    (maphash
     (lambda (buffer _)
       (when (buffer-live-p buffer)
         (push buffer result)))
     ecc-auto-response--registered-buffers)
    result))

(defun ecc-auto-response-status (&optional buffer)
  "Show status of auto-response system for BUFFER.
Displays current settings, state, and statistics."
  (interactive)
  (let ((buf (or buffer (current-buffer)))
        (global-status (if ecc-auto-response-enabled "enabled" "disabled"))
        (global-mode (if ecc-auto-response-buffer-local-default "buffer-local" "global"))
        (registered-count (hash-table-count ecc-auto-response--registered-buffers)))
    
    (with-current-buffer buf
      (let ((buffer-status (if ecc-auto-response-buffer-enabled "enabled" "disabled")))
        (message "Auto-response status: Global: %s (%s mode), Buffer: %s, Registered buffers: %d"
                 global-status global-mode buffer-status registered-count)))))

;;;; Backward Compatibility - Enhanced Module

;; Variables
(defvaralias 'ecc-auto-response-enhanced-enabled 'ecc-auto-response-enabled)
(defvaralias 'ecc-auto-response-enhanced-yes 'ecc-auto-response-yes)
(defvaralias 'ecc-auto-response-enhanced-yes-plus 'ecc-auto-response-yes-plus)
(defvaralias 'ecc-auto-response-enhanced-continue 'ecc-auto-response-continue)
(defvaralias 'ecc-auto-response-enhanced-initial-waiting 'ecc-auto-response-initial-waiting)
(defvaralias 'ecc-auto-response-enhanced-notify 'ecc-auto-response-notify)
(defvaralias 'ecc-auto-response-enhanced-check-interval 'ecc-auto-response-check-interval)

;; Functions
(defalias 'ecc-auto-response-enhanced-start 'ecc-auto-response-start)
(defalias 'ecc-auto-response-enhanced-stop 'ecc-auto-response-stop)
(defalias 'ecc-auto-response-enhanced-toggle 'ecc-auto-response-toggle)
(defalias 'ecc-auto-response-enhanced-reset 'ecc-auto-response-reset)
(defalias 'ecc-auto-response-enhanced-yes 'ecc-auto-response-yes)
(defalias 'ecc-auto-response-enhanced-yes-plus 'ecc-auto-response-yes-plus)
(defalias 'ecc-auto-response-enhanced-continue 'ecc-auto-response-continue)
(defalias 'ecc-auto-response-enhanced-send 'ecc-auto-response-send)

;;;; Backward Compatibility - Buffer-Local Module

;; Missing Functions for Test Compatibility

(defun ecc-auto-response-buffer-local-init (buffer &optional yes yes-plus continue initial-waiting)
  "Initialize buffer-local auto-response for BUFFER.
This function sets up buffer-local variables and prepares the buffer
for auto-response functionality."
  (with-current-buffer buffer
    (setq-local ecc-buffer-auto-response-enabled t)
    (setq-local ecc-buffer-auto-response-y/n (or yes ecc-auto-response-yes))
    (setq-local ecc-buffer-auto-response-y/y/n (or yes-plus ecc-auto-response-yes-plus))
    (setq-local ecc-buffer-auto-response-waiting (or continue ecc-auto-response-continue))
    (setq-local ecc-buffer-auto-response-initial-waiting (or initial-waiting ecc-auto-response-initial-waiting))
    (setq-local ecc-buffer-auto-notify-completions ecc-auto-response-notify)
    ;; Initialize buffer state if needed
    (when (fboundp 'ecc-buffer-state-init)
      (ecc-buffer-state-init))
    (when (fboundp 'ecc-buffer-local-init)
      (ecc-buffer-local-init))
    (when (fboundp 'ecc-auto-core-register-buffer-local)
      (ecc-auto-core-register-buffer-local buffer))))

(defun ecc-auto-response-custom (text &optional buffer)
  "Send custom TEXT as auto-response to BUFFER."
  (ecc-auto-response-send text buffer))

(defun ecc-auto-response--dispatch-response (buffer response type)
  "Dispatch RESPONSE of TYPE to BUFFER.
This is the core dispatch function that sends responses to buffers."
  (ecc-auto-response--send-to-buffer buffer response type))

;; Variables
(defvaralias 'ecc-auto-response-buffer-local-default-enabled 'ecc-auto-response-buffer-local-default)
(defvaralias 'ecc-auto-response-buffer-local-notify 'ecc-auto-response-notify)

;; Define compatibility buffer-local variables 
;; Note: Cannot use defvaralias for buffer-local variables in Emacs
(defvar-local ecc-buffer-auto-response-enabled nil
  "Buffer-local auto-response enabled status (compatibility variable).")

(defvar-local ecc-buffer-auto-response-y/n "1"
  "Buffer-local Y/N response (compatibility variable).")

(defvar-local ecc-buffer-auto-response-y/y/n "2"
  "Buffer-local Y/Y/N response (compatibility variable).")

(defvar-local ecc-buffer-auto-response-waiting "/auto"
  "Buffer-local waiting response (compatibility variable).")

(defvar-local ecc-buffer-auto-response-initial-waiting "/user:understand-guidelines"
  "Buffer-local initial waiting response (compatibility variable).")

;; Functions
(defalias 'ecc-auto-response-buffer-local-check 'ecc-auto-response--process-buffer-local)
(defalias 'ecc-auto-response-buffer-local-send-message 'ecc-auto-response--send-to-buffer)
(defalias 'ecc-auto-response-buffer-local-register 'ecc-auto-response-register-buffer)
(defalias 'ecc-auto-response-buffer-local-unregister 'ecc-auto-response-unregister-buffer)
(defalias 'ecc-auto-response-buffer-local-start 'ecc-auto-response-buffer-start)
(defalias 'ecc-auto-response-buffer-local-stop 'ecc-auto-response-buffer-stop)
(defalias 'ecc-auto-response-buffer-local-toggle 'ecc-auto-response-buffer-toggle)

;;;; Backward Compatibility - Original Module

;; Functions
(defalias 'ecc-start-auto-response 'ecc-auto-response-start)
(defalias 'ecc-stop-auto-response 'ecc-auto-response-stop)
(defalias 'ecc-toggle-auto-response 'ecc-auto-response-toggle)
(defalias 'ecc-reset-auto-response 'ecc-auto-response-reset)
(defalias 'ecc-send-auto-response-for-state 'ecc-auto-response--send-for-state)
(defalias 'ecc-auto-response-for-state 'ecc-auto-response--send-for-state)
(defalias 'ecc-send-auto-response 'ecc-auto-response-send)

;; Provide both the consolidated name and the original names for backward compatibility
(provide 'ecc-auto-response-consolidated)
(provide 'ecc-auto-response)
(provide 'ecc-auto-response-enhanced)
(provide 'ecc-auto-response-buffer-local)

;;; ecc-auto-response-consolidated.el ends here
