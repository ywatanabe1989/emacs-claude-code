;;; -*- coding: utf-8; lexical-binding: t -*-
;;; ecc-auto-response-consolidated.el --- Consolidated auto-response system
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 17:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-auto-response-consolidated.el

;;; Commentary:
;;; Consolidated auto-response system for Claude prompts.
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
;;; (setq ecc-auto-response-continue "/user:auto")
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
    (require 'ecc-variables)
  (require 'ecc-variables))

(if (featurep 'ecc-state-detection)
    (require 'ecc-state-detection)
  (require 'ecc-state-detection))

(if (featurep 'ecc-debug-utils-consolidated)
    (require 'ecc-debug-utils)
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

(defcustom ecc-auto-response-continue "/user:auto"
  "Response to send for waiting state (typically \"/user:auto\" or \"/continue\").
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

(defcustom ecc-auto-response-accumulation-threshold 5
  "Maximum number of responses allowed within accumulation window.
If more than this many responses are sent within the accumulation window,
the system will detect accumulation and automatically stop auto-response."
  :type 'number
  :group 'ecc-auto-response)

(defcustom ecc-auto-response-accumulation-window 3.0
  "Time window in seconds for accumulation detection.
If more than `ecc-auto-response-accumulation-threshold' responses are sent
within this time window, accumulation will be detected."
  :type 'number
  :group 'ecc-auto-response)

;; Buffer-local mode settings
(defcustom ecc-auto-response-default nil
  "Whether to use buffer-local mode by default.
When enabled, auto-response operates independently in each buffer with its
own configuration and state tracking. When disabled, global mode is used
where a single set of configurations applies to all buffers."
  :type 'boolean
  :group 'ecc-auto-response)

;;;; Internal Variables

;; Global state tracking (system-wide)
(defvar ecc-auto-response--timer nil
  "Timer for auto-response system.")

(defvar ecc-auto-response--registered-buffers (make-hash-table :test 'eq)
  "Hash table of registered buffers for auto-response.")

(defvar ecc-auto-response--registered-callback nil
  "Callback function registered with auto-core system.")

;; Buffer-local state tracking
(defvar-local ecc-auto-response--last-state nil
  "Last Claude state that received an auto-response in this buffer.")

(defvar-local ecc-auto-response--last-response-time 0
  "Timestamp of last auto-response in this buffer.")

;; Buffer-local accumulation tracking
(defvar-local ecc-auto-response--accumulation-count 0
  "Counter for responses sent within current accumulation window in this buffer.")

(defvar-local ecc-auto-response--accumulation-start-time 0
  "Start time of current accumulation tracking window in this buffer.")

;; Buffer-local variables
(defvar-local ecc-auto-response-buffer-enabled nil
  "Whether auto-response is enabled for this buffer.")

(defvar-local ecc-auto-response-buffer-yes "1"
  "Response to send for Y/N prompts in this buffer.")

(defvar-local ecc-auto-response-buffer-yes-plus "2"
  "Response to send for Y/Y/N prompts in this buffer.")

(defvar-local ecc-auto-response-buffer-continue "/user:auto"
  "Response to send for waiting state in this buffer.")

(defvar-local ecc-auto-response-buffer-initial-waiting "/user:understand-guidelines"
  "Response to send for initial waiting state in this buffer.")

(defvar-local ecc-auto-response-buffer-last-state nil
  "Last Claude state that received an auto-response in this buffer.")

(defvar-local ecc-auto-response-buffer-last-response-time 0
  "Timestamp of last auto-response in this buffer.")

;; Buffer-local accumulation tracking
(defvar-local ecc-auto-response-buffer-accumulation-count 0
  "Buffer-local counter for responses sent within current accumulation window.")

(defvar-local ecc-auto-response-buffer-accumulation-start-time 0
  "Buffer-local start time of current accumulation tracking window.")

(defvar-local ecc-auto-response-buffer-interrupt-detected nil
  "Buffer-local flag indicating if ESC interrupt was detected.")

;;;; Debugging Utilities

(defvar-local ecc-auto-response-buffer-debug-enabled nil
  "Whether debug messages are enabled for auto-response in this buffer.")

(defun ecc-auto-response--debug (format-string &rest args)
  "Log a debug message for auto-response with FORMAT-STRING and ARGS.
Only displays the message if `ecc-auto-response-debug' is non-nil globally
or `ecc-auto-response-buffer-debug-enabled' is non-nil locally.
Messages go to *Messages* buffer without minibuffer echo."
  (when (or ecc-auto-response-debug ecc-auto-response-buffer-debug-enabled)
    (let ((inhibit-message t))  ; Prevent minibuffer echo
      (if (fboundp 'ecc-debug-message)
          (apply #'ecc-debug-message (concat "[Auto-Response] " format-string) args)
        (ecc-debug-message "[ECC-AUTO-RESPONSE DEBUG %s] %s" 
                 (buffer-name) 
                 (apply #'format format-string args))))))

;;;; Accumulation Detection

(defun ecc-auto-response--accumulation-detected-p ()
  "Check if auto-response accumulation has been detected.
Returns t if too many responses have been sent within the accumulation window."
  (let ((current-time (float-time)))
    ;; Reset counter if outside window
    (when (> (- current-time ecc-auto-response--accumulation-start-time)
             ecc-auto-response-accumulation-window)
      (ecc-auto-response--reset-accumulation-counter))
    
    ;; Check if threshold exceeded
    (when (>= ecc-auto-response--accumulation-count
              ecc-auto-response-accumulation-threshold)
      (ecc-auto-response--debug "Accumulation detected: %d responses in %s seconds"
                               ecc-auto-response--accumulation-count
                               (- current-time ecc-auto-response--accumulation-start-time))
      t)))

(defun ecc-auto-response--reset-accumulation-counter ()
  "Reset the accumulation counter and start time."
  (setq ecc-auto-response--accumulation-count 0
        ecc-auto-response--accumulation-start-time 0)
  (ecc-auto-response--debug "Reset accumulation counter"))

(defun ecc-auto-response--increment-accumulation-counter ()
  "Increment the accumulation counter and set start time if needed."
  (let ((current-time (float-time)))
    ;; Set start time if this is the first response in window
    (when (= ecc-auto-response--accumulation-count 0)
      (setq ecc-auto-response--accumulation-start-time current-time))
    
    ;; Increment counter
    (setq ecc-auto-response--accumulation-count 
          (1+ ecc-auto-response--accumulation-count))
    
    (ecc-auto-response--debug "Accumulation count: %d (started at %s)"
                             ecc-auto-response--accumulation-count
                             ecc-auto-response--accumulation-start-time)))

;;;; ESC Interrupt Detection

(defun ecc-auto-response--esc-interrupt-detected-p (buffer)
  "Check if ESC interrupt sequence is detected in BUFFER.
Returns t if buffer content contains ESC interrupt patterns."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      ;; Look for ESC patterns in recent output (last 1000 chars)
      (let ((start-pos (max (point-min) (- (point-max) 1000))))
        (goto-char start-pos)
        (or
         ;; Look for literal ESC character (ASCII 27)
         (search-forward "\e" nil t)
         ;; Look for caret notation ^[
         (search-forward "^[" nil t)
         ;; Look for text patterns
         (re-search-forward "\\(ESC\\|esc\\)\\s-+to\\s-+interrupt" nil t)
         ;; Look for bracketed ESC
         (search-forward "[ESC]" nil t))))))

(defun ecc-auto-response--running-state-with-esc-p (buffer)
  "Check if buffer shows running state with ESC interrupt available.
Returns t if ESC interrupt is present but command is still running.
This means auto-response should be disabled during execution but 
can resume if prompt appears after the ESC line."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      ;; Look for ESC interrupt pattern in recent output
      (let ((start-pos (max (point-min) (- (point-max) 1000)))
            (esc-pos nil)
            (prompt-pos nil))
        (goto-char start-pos)
        
        ;; Find ESC interrupt position
        (when (re-search-forward "\\(ESC\\|esc\\)\\s-+to\\s-+interrupt" nil t)
          (setq esc-pos (match-beginning 0)))
        
        ;; If ESC found, check if there's a prompt after it
        (when esc-pos
          (goto-char esc-pos)
          ;; Look for prompt patterns after ESC line
          (when (re-search-forward "\\[y/n\\]\\|\\[y/y/n\\]\\|>\\s-*$" nil t)
            (setq prompt-pos (match-beginning 0)))
          
          ;; Return t if ESC exists but no prompt after it (still running)
          ;; Return nil if prompt exists after ESC (can respond to prompt)
          (not prompt-pos))))))

;;;; Buffer-Local Accumulation Detection

(defun ecc-auto-response--buffer-accumulation-detected-p ()
  "Check if buffer-local auto-response accumulation has been detected.
Returns t if too many responses have been sent within the accumulation window."
  (let ((current-time (float-time)))
    ;; Reset counter if outside window
    (when (> (- current-time ecc-auto-response-buffer-accumulation-start-time)
             ecc-auto-response-accumulation-window)
      (ecc-auto-response--reset-buffer-accumulation-counter))
    
    ;; Check if threshold exceeded
    (when (>= ecc-auto-response-buffer-accumulation-count
              ecc-auto-response-accumulation-threshold)
      (ecc-auto-response--debug "Buffer-local accumulation detected: %d responses in %s seconds"
                               ecc-auto-response-buffer-accumulation-count
                               (- current-time ecc-auto-response-buffer-accumulation-start-time))
      t)))

(defun ecc-auto-response--reset-buffer-accumulation-counter ()
  "Reset the buffer-local accumulation counter and start time."
  (setq-local ecc-auto-response-buffer-accumulation-count 0)
  (setq-local ecc-auto-response-buffer-accumulation-start-time 0)
  (ecc-auto-response--debug "Reset buffer-local accumulation counter"))

(defun ecc-auto-response--increment-buffer-accumulation-counter ()
  "Increment the buffer-local accumulation counter and set start time if needed."
  (let ((current-time (float-time)))
    ;; Set start time if this is the first response in window
    (when (= ecc-auto-response-buffer-accumulation-count 0)
      (setq-local ecc-auto-response-buffer-accumulation-start-time current-time))
    
    ;; Increment counter
    (setq-local ecc-auto-response-buffer-accumulation-count 
                (1+ ecc-auto-response-buffer-accumulation-count))
    
    (ecc-auto-response--debug "Buffer-local accumulation count: %d (started at %s)"
                             ecc-auto-response-buffer-accumulation-count
                             ecc-auto-response-buffer-accumulation-start-time)))

;;;; Core Functions - Global Mode

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
  (ecc-auto-response--debug "ecc-auto-response-start called")
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
    (ecc-auto-response--debug "Starting timer with interval: %s" ecc-auto-response-check-interval)
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
  (ecc-debug-message "Auto-response disabled"))


;;;###autoload
(defun ecc-auto-response-toggle ()
  "Toggle the auto-response system on or off.
Enables or disables automatic responses to Claude prompts.
If not already started, initializes with default settings."
  (interactive)
  (ecc-auto-response--debug "ecc-auto-response-toggle called. Current state: %s" ecc-auto-response-enabled)
  (if ecc-auto-response-enabled
      (ecc-auto-response-stop)
    ;; When starting, ensure we have a registered buffer
    (progn
      (ecc-auto-response--debug "Registering current buffer before starting")
      (ecc-auto-response-register-buffer (current-buffer))
      (ecc-auto-response-start))))


(defun ecc-auto-response-reset ()
  "Reset and restart the auto-response system.
Useful to apply new settings or clear state."
  (interactive)
  (ecc-auto-response--debug "Resetting auto-response system")
  (ecc-auto-response-stop)
  (ecc-auto-response-start))

;;;; Buffer Management


(defun ecc-auto-response-register-buffer (buffer)
  "Register BUFFER for auto-response.
Registered buffers will be checked for Claude prompts when auto-response is enabled."
  (interactive (list (current-buffer)))
  (ecc-auto-response--debug "ecc-auto-response-register-buffer called for: %s" 
           (if (buffer-live-p buffer) (buffer-name buffer) "non-live buffer"))
  (unless (buffer-live-p buffer)
    (ecc-auto-response--debug "Cannot register non-live buffer")
    (user-error "Buffer is not alive"))
  
  (puthash buffer t ecc-auto-response--registered-buffers)
  (ecc-auto-response--debug "Buffer registered. Total registered buffers: %s" 
           (hash-table-count ecc-auto-response--registered-buffers))
  (ecc-auto-response--debug "Registered buffer: %s" (buffer-name buffer))
  buffer)


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
  "Process buffers for auto-response, prioritizing current buffer.
Checks current buffer first, then other registered buffers for Claude prompts."
  (ecc-auto-response--debug "ecc-auto-response--process-all-buffers called")
  (ecc-auto-response--debug "Auto-response enabled: %s" ecc-auto-response-enabled)
  (ecc-auto-response--debug "Registered buffers count: %s" (hash-table-count ecc-auto-response--registered-buffers))
  
  (when ecc-auto-response-enabled
    (ecc-auto-response--debug "Processing buffers for auto-response")
    
    ;; First, prioritize current buffer if it's a vterm buffer
    (when (and (derived-mode-p 'vterm-mode)
               (buffer-live-p (current-buffer)))
      (ecc-auto-response--debug "Prioritizing current buffer: %s" (buffer-name))
      
      ;; Auto-register current buffer if not already registered
      (unless (gethash (current-buffer) ecc-auto-response--registered-buffers)
        (ecc-auto-response--debug "Auto-registering current buffer")
        (ecc-auto-response-register-buffer (current-buffer)))
      
      ;; Process current buffer first
      (ecc-auto-response--process-single-buffer (current-buffer)))
    
    ;; Then process other registered buffers (excluding current to avoid duplication)
    (maphash
     (lambda (buffer _)
       ;; Skip current buffer (already processed above)
       (unless (eq buffer (current-buffer))
         (ecc-auto-response--debug "Processing registered buffer: %s" 
                                  (if (buffer-live-p buffer) (buffer-name buffer) "non-live"))
         
         ;; Skip if buffer is not live
         (unless (buffer-live-p buffer)
           (ecc-auto-response--debug "Removing non-live buffer from registered list")
           (ecc-auto-response-unregister-buffer buffer)
           (cl-return))
         
         ;; Process the buffer
         (ecc-auto-response--process-single-buffer buffer)))
     ecc-auto-response--registered-buffers)))

(defun ecc-auto-response--process-single-buffer (buffer)
  "Process a single BUFFER for auto-response based on its configuration."
  (ecc-auto-response--debug "Processing single buffer: %s" (buffer-name buffer))
  
  ;; Process buffer based on mode
  (if ecc-auto-response-default
      ;; Buffer-local mode
      (with-current-buffer buffer
        (ecc-auto-response--debug "Buffer-local mode check - enabled: %s" 
                 (or ecc-auto-response-buffer-enabled
                     (and (boundp 'ecc-buffer-auto-response-enabled)
                          ecc-buffer-auto-response-enabled)))
        (when (or ecc-auto-response-buffer-enabled 
                 ;; For backward compatibility
                 (and (boundp 'ecc-buffer-auto-response-enabled)
                      ecc-buffer-auto-response-enabled))
          (ecc-auto-response--process-buffer-local buffer)))
    
    ;; Global mode
    (ecc-auto-response--debug "Processing buffer in global mode")
    (ecc-auto-response--process-buffer-global buffer)))

(defun ecc-auto-response--process-buffer-global (buffer)
  "Process BUFFER for auto-response using global settings.
Checks buffer for Claude prompts and sends responses if appropriate."
  (ecc-auto-response--debug "ecc-auto-response--process-buffer-global called for buffer: %s" (buffer-name buffer))
  
  (cl-block ecc-auto-response--process-buffer-global
    (with-current-buffer buffer
    ;; Check if command is running with ESC interrupt available
    (when (ecc-auto-response--running-state-with-esc-p buffer)
      (ecc-auto-response--debug "Command running with ESC interrupt, skipping")
      (ecc-auto-response--debug "Command running with ESC interrupt in %s, skipping auto-response" 
                               (buffer-name buffer))
      (cl-return-from ecc-auto-response--process-buffer-global))
    
    ;; Check for ESC interrupt (command finished with interrupt)
    (when (ecc-auto-response--esc-interrupt-detected-p buffer)
      (ecc-auto-response--debug "ESC interrupt detected, stopping")
      (ecc-auto-response--debug "ESC interrupt detected in %s, stopping auto-response" 
                               (buffer-name buffer))
      (setq ecc-auto-response-enabled nil)
      (ecc-debug-message "Auto-response stopped due to ESC interrupt")
      (cl-return-from ecc-auto-response--process-buffer-global))
    
    ;; Check for accumulation
    (when (ecc-auto-response--accumulation-detected-p)
      (ecc-auto-response--debug "Accumulation detected, stopping")
      (ecc-auto-response--debug "Accumulation detected, stopping auto-response")
      (setq ecc-auto-response-enabled nil)
      (ecc-debug-message "Auto-response stopped due to accumulation detection")
      (cl-return-from ecc-auto-response--process-buffer-global))
    
    (ecc-auto-response--debug "Detecting state...")
    (let ((state (ecc-detect-state)))
      (ecc-auto-response--debug "Detected state: %s" state)
      (when state
        (ecc-auto-response--debug "Detected state %s in buffer %s" 
                                 state (buffer-name))
        
        ;; Check if throttling needed
        (if (ecc-auto-response--throttled-p state)
            (ecc-auto-response--debug "Response throttled for state: %s" state)
          ;; Increment accumulation counter
          (ecc-auto-response--debug "Sending response for state: %s" state)
          (ecc-auto-response--increment-accumulation-counter)
          
          ;; Send response based on state
          (ecc-auto-response--send-for-state state buffer)))))))

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
  (ecc-auto-response--debug "ecc-auto-response--send-for-state called with state: %s" state)
  (ecc-auto-response--debug "Sending response for state %s in buffer %s"
                           state (buffer-name buffer))

  ;; Record state and time for throttling
  (setq ecc-auto-response--last-state state)
  (setq ecc-auto-response--last-response-time (float-time))
  
  ;; Send response based on state
  (with-current-buffer buffer
    (when (fboundp 'ecc-notification-dispatch)
      (ecc-notification-dispatch state buffer))
    (cond
     ((eq state :y/y/n)
      (ecc-auto-response--debug "Sending Y/Y/N response: %s" ecc-auto-response-yes-plus)
      (ecc-auto-response--send-to-buffer 
       buffer ecc-auto-response-yes-plus "Y/Y/N"))
     
     ((eq state :y/n)
      (ecc-auto-response--debug "Sending Y/N response: %s" ecc-auto-response-yes)
      (ecc-auto-response--send-to-buffer 
       buffer ecc-auto-response-yes "Y/N"))
     
     ((eq state :initial-waiting)
      (ecc-auto-response--debug "Sending Initial-Waiting response: %s" ecc-auto-response-initial-waiting)
      (ecc-auto-response--send-to-buffer 
       buffer ecc-auto-response-initial-waiting "Initial-Waiting"))
     
     ((eq state :waiting)
      (ecc-auto-response--debug "Sending Continue response: %s" ecc-auto-response-continue)
      (ecc-auto-response--send-to-buffer 
       buffer ecc-auto-response-continue "Continue"))
     
     (t ;; Unrecognized state
      (ecc-auto-response--debug "Unknown state: %s" state)
      (ecc-auto-response--debug "Unknown state %s, not sending response" state)
      nil))))

(defun ecc-auto-response--send-to-buffer (buffer text state-name)
  "Send TEXT to BUFFER and notify with STATE-NAME.
Handles sending the text through the appropriate buffer mode (vterm, comint, etc.)."
  (ecc-auto-response--debug "ecc-auto-response--send-to-buffer called")
  (ecc-auto-response--debug "  Buffer: %s" (buffer-name buffer))
  (ecc-auto-response--debug "  Text: %s" text)
  (ecc-auto-response--debug "  State: %s" state-name)
  
  (ecc-auto-response--debug "Sending '%s' to buffer %s for %s state"
                           text (buffer-name buffer) state-name)
  
  (with-current-buffer buffer
    (ecc-auto-response--debug "Buffer mode: %s" major-mode)
    ;; Send text based on buffer mode
    (cond
     ;; VTerm mode
     ((derived-mode-p 'vterm-mode)
      (ecc-auto-response--debug "Using vterm-send-string")
      (if (fboundp 'vterm-send-string)
          (progn
            (vterm-send-string text)
            (vterm-send-return))
        (ecc-auto-response--debug "vterm-send-string not available")))
     
     ;; Comint mode
     ((derived-mode-p 'comint-mode)
      (ecc-auto-response--debug "Using comint-send-input")
      (goto-char (point-max))
      (insert text)
      (comint-send-input))
     
     ;; Default - insert and newline
     (t
      (ecc-auto-response--debug "Using default insert method")
      (goto-char (point-max))
      (insert text "\n"))))
  
  ;; Show notification if enabled
  (when ecc-auto-response-notify
    (ecc-debug-message "[%s] Auto-response to %s: %s" (buffer-name buffer) state-name text)))

;;;; Core Functions - Buffer-Local Mode


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
    (message "Starting buffer-local auto-response for %s"
                             (buffer-name buf))

    ;; Optimize VTerm for Claude Code
    (ecc-optimize-vterm)

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
      (setq-local ecc-buffer-auto-notify-completions ecc-auto-response-notify)
      
      ;; Reset buffer-local response tracking
      (setq-local ecc-auto-response-buffer-last-state nil)
      (setq-local ecc-auto-response-buffer-last-response-time 0))
    
    ;; Enable global auto-response system if not already enabled
    (unless ecc-auto-response-enabled
      (setq ecc-auto-response-enabled t)
      (ecc-auto-response--debug "Enabled global auto-response system"))
    
    ;; Start global timer if not already running
    (unless ecc-auto-response--timer
      (setq ecc-auto-response--timer
            (run-with-timer 0 ecc-auto-response-check-interval
                          'ecc-auto-response--process-all-buffers))
      (ecc-auto-response--debug "Started auto-response timer with interval %s"
                               ecc-auto-response-check-interval))
    
    ;; Show startup message
    (ecc-debug-message "Buffer-local auto-response enabled for %s" (buffer-name buf))))


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
    (ecc-debug-message "Buffer-local auto-response disabled for %s" (buffer-name buf))))


;;;###autoload
(defun ecc-auto-response-buffer-toggle (&optional buffer)
  "Toggle buffer-local auto-response for BUFFER on or off.
Always operates on buffer-local state regardless of global settings."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      ;; Always use buffer-local mode
      (if ecc-auto-response-buffer-enabled
          (ecc-auto-response-buffer-stop buf)
        ;; When starting, ensure buffer is registered
        (progn
          (ecc-auto-response-register-buffer buf)
          (ecc-auto-response-buffer-start buf))))))

(defun ecc-auto-response--process-buffer-local (buffer)
  "Process BUFFER for auto-response using buffer-local settings."
  (cl-block ecc-auto-response--process-buffer-local
    (with-current-buffer buffer
      ;; Check if command is running with ESC interrupt available (buffer-local)
      (when (ecc-auto-response--running-state-with-esc-p buffer)
        (ecc-auto-response--debug "Command running with ESC interrupt in %s, skipping buffer-local auto-response" 
                                 (buffer-name buffer))
        (cl-return-from ecc-auto-response--process-buffer-local nil))
      
      ;; Check for ESC interrupt first (buffer-local)
      (when (and (not ecc-auto-response-buffer-interrupt-detected)
                 (ecc-auto-response--esc-interrupt-detected-p buffer))
        (ecc-auto-response--debug "ESC interrupt detected in %s, disabling buffer-local auto-response" 
                                 (buffer-name buffer))
        (setq-local ecc-auto-response-buffer-enabled nil)
        (setq-local ecc-auto-response-buffer-interrupt-detected t)
        (ecc-debug-message "Buffer-local auto-response stopped due to ESC interrupt in %s" (buffer-name buffer))
        (cl-return-from ecc-auto-response--process-buffer-local nil))
      
      ;; Check for buffer-local accumulation
      (when (ecc-auto-response--buffer-accumulation-detected-p)
        (ecc-auto-response--debug "Buffer-local accumulation detected, disabling auto-response")
        (setq-local ecc-auto-response-buffer-enabled nil)
        (ecc-debug-message "Buffer-local auto-response stopped due to accumulation detection in %s" (buffer-name buffer))
        (cl-return-from ecc-auto-response--process-buffer-local nil))
      
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
          
          ;; Increment buffer-local accumulation counter
          (ecc-auto-response--increment-buffer-accumulation-counter)
          
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
            nil))))))))

;;;; Manual Response Commands


;;;###autoload
(defun ecc-auto-response-yes (&optional buffer)
  "Manually send 'yes' response to Y/N prompt in BUFFER."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (ecc-auto-response--debug "Manual yes response in %s" (buffer-name buf))
    (with-current-buffer buf
      (if ecc-auto-response-default
          (ecc-auto-response--send-to-buffer buf ecc-auto-response-buffer-yes "Y/N (manual)")
        (ecc-auto-response--send-to-buffer buf ecc-auto-response-yes "Y/N (manual)")))))


(defun ecc-auto-response-yes-plus (&optional buffer)
  "Manually send 'yes-plus' response to Y/Y/N prompt in BUFFER."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (ecc-auto-response--debug "Manual yes-plus response in %s" (buffer-name buf))
    (with-current-buffer buf
      (if ecc-auto-response-default
          (ecc-auto-response--send-to-buffer buf ecc-auto-response-buffer-yes-plus "Y/Y/N (manual)")
        (ecc-auto-response--send-to-buffer buf ecc-auto-response-yes-plus "Y/Y/N (manual)")))))


;;;###autoload
(defun ecc-auto-response-continue (&optional buffer)
  "Manually send continue response to waiting prompt in BUFFER."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (ecc-auto-response--debug "Manual continue response in %s" (buffer-name buf))
    (with-current-buffer buf
      (if ecc-auto-response-default
          (ecc-auto-response--send-to-buffer buf ecc-auto-response-buffer-continue "Continue (manual)")
        (ecc-auto-response--send-to-buffer buf ecc-auto-response-continue "Continue (manual)")))))


(defun ecc-auto-response-initial-waiting (&optional buffer)
  "Manually send initial-waiting response in BUFFER."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (ecc-auto-response--debug "Manual initial-waiting response in %s" (buffer-name buf))
    (with-current-buffer buf
      (if ecc-auto-response-default
          (ecc-auto-response--send-to-buffer buf ecc-auto-response-buffer-initial-waiting "Initial-Waiting (manual)")
        (ecc-auto-response--send-to-buffer buf ecc-auto-response-initial-waiting "Initial-Waiting (manual)")))))


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
        (global-mode (if ecc-auto-response-default "buffer-local" "global"))
        (registered-count (hash-table-count ecc-auto-response--registered-buffers)))
    
    (with-current-buffer buf
      (let ((buffer-status (if ecc-auto-response-buffer-enabled "enabled" "disabled")))
        (ecc-debug-message "Auto-response status: Global: %s (%s mode), Buffer: %s, Registered buffers: %d"
                 global-status global-mode buffer-status registered-count)))))

;;;; Backward Compatibility - Buffer-Local Module

;; Missing Functions for Test Compatibility

(defun ecc-auto-response-init (buffer &optional yes yes-plus continue initial-waiting)
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

;; Variables (removing circular references)
(defvaralias 'ecc-auto-response-default-enabled 'ecc-auto-response-default)

;; Define compatibility buffer-local variables 
;; Note: Cannot use defvaralias for buffer-local variables in Emacs
(defvar-local ecc-buffer-auto-response-enabled nil
  "Buffer-local auto-response enabled status (compatibility variable).")

(defvar-local ecc-buffer-auto-response-y/n "1"
  "Buffer-local Y/N response (compatibility variable).")

(defvar-local ecc-buffer-auto-response-y/y/n "2"
  "Buffer-local Y/Y/N response (compatibility variable).")

(defvar-local ecc-buffer-auto-response-waiting "/user:auto"
  "Buffer-local waiting response (compatibility variable).")

(defvar-local ecc-buffer-auto-response-initial-waiting "/user:understand-guidelines"
  "Buffer-local initial waiting response (compatibility variable).")

;; ;; Functions
;; (defalias 'ecc-auto-response-check 'ecc-auto-response--process-buffer-local)
;; (defalias 'ecc-auto-response-send-message 'ecc-auto-response--send-to-buffer)
;; (defalias 'ecc-auto-response-register 'ecc-auto-response-register-buffer)
;; (defalias 'ecc-auto-response-unregister 'ecc-auto-response-unregister-buffer)
;; (defalias 'ecc-auto-response-start 'ecc-auto-response-buffer-start)
;; (defalias 'ecc-auto-response-stop 'ecc-auto-response-buffer-stop)
;; (defalias 'ecc-auto-response-toggle 'ecc-auto-response-buffer-toggle)

;; Provide both the consolidated name and the original names for backward compatibility
(provide 'ecc-auto-response)

;;; ecc-auto-response-consolidated.el ends here
