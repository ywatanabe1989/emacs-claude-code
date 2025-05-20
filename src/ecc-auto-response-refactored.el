;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 17:45:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-auto-response-refactored.el

;;; Commentary:
;;; Unified auto-response functionality for Claude prompts.
;;; This module unifies and standardizes the auto-response functionality
;;; that was previously spread across multiple files, eliminating duplication
;;; and ensuring consistent behavior.

(require 'ecc-variables)
(require 'ecc-state-detection)

;;; Code:

;; Customization group for auto-response
(defgroup ecc-auto-response nil
  "Settings for Claude auto-response system."
  :group 'ecc
  :prefix "ecc-auto-response-")

(defcustom ecc-auto-response-throttle-time 5.0
  "Minimum seconds between auto-responses to the same state.
Prevents rapid consecutive auto-responses to waiting prompts."
  :type 'float
  :group 'ecc-auto-response)

(defcustom ecc-auto-response-timer-interval 0.5
  "Interval in seconds for auto-response timer checks."
  :type 'float
  :group 'ecc-auto-response)

(defcustom ecc-auto-response-check-on-output t
  "Whether to check for prompts whenever new output appears."
  :type 'boolean
  :group 'ecc-auto-response)

;; Internal variables
(defvar ecc-auto-response-timer nil
  "Timer for checking and responding to Claude prompts.")

(defvar ecc-auto-response-last-time-alist
  '((:y/n . 0.0)
    (:y/y/n . 0.0)
    (:waiting . 0.0)
    (:initial-waiting . 0.0))
  "Alist tracking last time each type of response was sent.")

(defvar ecc-auto-response-active-state nil
  "The currently active Claude prompt state being processed.
Used to prevent duplicate responses to the same prompt.")

(defvar ecc-auto-response-hooks nil
  "Hooks to run after an auto-response is sent.")

;; Core auto-response functions

;;;###autoload
(defun ecc-auto-response-start ()
  "Start the auto-response system.
This function initializes and activates the auto-response system.
Sets up a timer that periodically checks the current buffer for
prompts and sends appropriate responses."
  (interactive)
  ;; Enable auto-response mode globally
  (setq ecc-buffer-auto-response-enabled t)
  
  ;; Initialize timer for periodic checking
  (when (bound-and-true-p ecc-auto-response-timer)
    (cancel-timer ecc-auto-response-timer))
  
  (setq ecc-auto-response-timer
        (run-with-timer 1 ecc-auto-response-timer-interval #'ecc-auto-response-check))
  
  ;; Run initial checks with staggered timing to ensure we catch the prompt
  (ecc-auto-response-initial-check)
  (run-at-time 0.5 nil #'ecc-auto-response-initial-check)
  (run-at-time 1.0 nil #'ecc-auto-response-initial-check)
  (run-at-time 2.0 nil #'ecc-auto-response-initial-check)
  
  (message "Auto-response started: Y/N=%s, Y/Y/N=%s, Continue=%s, Initial=%s" 
           ecc-auto-response-y/n 
           ecc-auto-response-y/y/n
           ecc-auto-response-waiting
           ecc-auto-response-initial-waiting))

;;;###autoload
(defun ecc-auto-response-stop ()
  "Stop the auto-response system."
  (interactive)
  (when (bound-and-true-p ecc-auto-response-timer)
    (cancel-timer ecc-auto-response-timer)
    (setq ecc-auto-response-timer nil))
  (setq ecc-buffer-auto-response-enabled nil)
  
  ;; Reset all timestamps and active state
  (setq ecc-auto-response-last-time-alist
        '((:y/n . 0.0)
          (:y/y/n . 0.0)
          (:waiting . 0.0)
          (:initial-waiting . 0.0)))
  (setq ecc-auto-response-active-state nil)
  
  (message "Auto-response stopped"))

;;;###autoload
(defun ecc-auto-response-toggle ()
  "Toggle auto-response on/off."
  (interactive)
  (if ecc-buffer-auto-response-enabled
      (ecc-auto-response-stop)
    (ecc-auto-response-start)))

;; Enhanced check and respond function

;;;###autoload
(defun ecc-auto-response-check ()
  "Check current buffer state and respond automatically if appropriate."
  (when (and (boundp 'ecc-buffer-auto-response-enabled)
             ecc-buffer-auto-response-enabled
             (boundp 'ecc-buffer-current-buffer)
             (buffer-live-p ecc-buffer-current-buffer))
    (with-current-buffer ecc-buffer-current-buffer
      ;; First check if we should notify about the prompt itself
      (ecc-state-notify-if-prompt-detected (current-buffer))
      
      ;; Detect the current state
      (let ((state (ecc-detect-state)))
        ;; Add debug logging
        (ecc-debug-message "Auto-response check - State detected: %s" state)
        
        ;; Special case for initial-waiting - always respond regardless of throttling
        (when (eq state :initial-waiting)
          (ecc-debug-message "Detected initial-waiting state - forcing response")
          (setq ecc-auto-response-active-state state)
          (ecc-auto-response-update-time state)
          (ecc-auto-response-send-message 
           ecc-buffer-current-buffer
           ecc-auto-response-initial-waiting 
           "Initial-Waiting"))
        
        ;; Handle other states with throttling
        (when (and state 
                  (not (eq state :initial-waiting))
                  (not (ecc-auto-response-throttled-p state)))
          ;; Set active state to prevent duplicates during processing
          (let ((ecc-auto-response-active-state state))
            ;; Update the timestamp for this state
            (ecc-auto-response-update-time state)
            ;; Process the response based on state
            (ecc-auto-response-process-state state)))))))

(defun ecc-auto-response-process-state (state)
  "Process auto-response for detected STATE."
  (cond
   ((eq state :y/y/n)
    (ecc-auto-response-send-message 
     ecc-buffer-current-buffer
     ecc-auto-response-y/y/n 
     "Y/Y/N"))
   ((eq state :y/n)
    (ecc-auto-response-send-message 
     ecc-buffer-current-buffer
     ecc-auto-response-y/n 
     "Y/N"))
   ((eq state :waiting)
    (ecc-auto-response-send-message 
     ecc-buffer-current-buffer
     ecc-auto-response-waiting 
     "Continue"))))

;;;###autoload
(defun ecc-auto-response-send (buffer &optional state)
  "Automatically respond to Claude prompts in BUFFER.
Examines the buffer content to detect Claude's current prompt state, then
sends an appropriate pre-configured response based on that state.

BUFFER is the buffer containing Claude's output to respond to.
Optional STATE can be provided to override automatic state detection.
Valid states are: `:y/n`, `:y/y/n`, `:initial-waiting`, `:waiting`

Responds with:
- `ecc-auto-response-y/n` for Y/N prompts (default: \"1\")
- `ecc-auto-response-y/y/n` for Y/Y/N prompts (default: \"2\")
- `ecc-auto-response-initial-waiting` for initial waiting state (default: \"/user:understand-guidelines\")
- `ecc-auto-response-waiting` for waiting prompts (default: \"/auto\")

Returns nil. Displays notification message when response is sent if
`ecc-auto-notify-completions` is enabled."
  (interactive (list (current-buffer)))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((detected-state (or state (ecc-detect-state))))
        (cond
         ((eq detected-state :y/y/n)
          (ecc-auto-response-send-message buffer 
                                   ecc-auto-response-y/y/n 
                                   "Y/Y/N"))
         ((eq detected-state :y/n)
          (ecc-auto-response-send-message buffer 
                                   ecc-auto-response-y/n 
                                   "Y/N"))
         ((eq detected-state :initial-waiting)
          (ecc-auto-response-send-message buffer 
                                   ecc-auto-response-initial-waiting 
                                   "Initial-Waiting"))
         ((eq detected-state :waiting)
          (ecc-auto-response-send-message buffer 
                                   ecc-auto-response-waiting 
                                   "Continue")))))))

(defun ecc-auto-response-send-message (buffer response type)
  "Send RESPONSE to Claude in BUFFER.
TYPE is used for notification messages."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      ;; Ensure we have the right mode to send commands
      (cond
       ;; vterm mode
       ((derived-mode-p 'vterm-mode)
        (ecc-auto-response-send-to-vterm response))
       
       ;; Default fallback
       (t
        (message "Cannot send auto-response to buffer in %s mode" major-mode)))))
  
  ;; Notify user about the response
  (when (and (boundp 'ecc-auto-notify-completions) ecc-auto-notify-completions)
    (ecc-auto-response-display-notification type response))
  
  ;; Run hooks
  (run-hooks 'ecc-auto-response-hooks))

(defun ecc-auto-response-send-to-vterm (response)
  "Send RESPONSE to Claude in a vterm buffer.
Special handling is applied for the first interaction to ensure reliability."
  (when (fboundp 'vterm-send-string)
    ;; Ensure interaction counter is properly bound
    (unless (boundp 'ecc-interaction-counter)
      (setq ecc-interaction-counter 0))
    
    ;; Save current point position
    (let ((old-point (point))
          (first-interaction-p (= ecc-interaction-counter 0))
          (delay-base (if (= ecc-interaction-counter 0) 1.5 1.0))
          (distance-from-end (- (point-max) (point))))
      
      ;; Output debug information
      (ecc-debug-message "DEBUG: Interaction Counter: %d" ecc-interaction-counter)
      (ecc-debug-message "DEBUG: Point: %d, Max: %d, Distance from end: %d" 
                      (point) (point-max) distance-from-end)
      (ecc-debug-message "DEBUG: First interaction? %s" 
                      (if first-interaction-p "Yes" "No"))
      
      ;; For the first interaction, we always send at current point
      ;; For later interactions, check if user is reading earlier content
      (if (or first-interaction-p
              (< distance-from-end 40)) ; Point is near the end
          ;; User is at end of buffer or it's first interaction - send directly
          (progn
            (ecc-debug-message "DEBUG: Sending directly at current point")
            (sit-for delay-base)
            (vterm-send-string response)
            (sit-for delay-base)                    
            (vterm-send-return)
            (sit-for delay-base))
        ;; User might be reading earlier content, so don't move point
        (save-excursion
          (ecc-debug-message "DEBUG: Using save-excursion to send at end")
          (goto-char (point-max))
          (sit-for delay-base)
          (vterm-send-string response)
          (sit-for delay-base)            
          (vterm-send-return)
          (sit-for delay-base)))
      
      ;; Increment interaction counter after the first successful response
      (when first-interaction-p
        (ecc-debug-message "DEBUG: Incrementing interaction counter")
        (setq ecc-interaction-counter (1+ ecc-interaction-counter))
        ;; Add timestamp for this interaction
        (when (boundp 'ecc-interaction-timestamps)
          (push (current-time) ecc-interaction-timestamps)))
      
      (ecc-debug-message "DEBUG: Send complete. Final point: %d" (point)))))

(defun ecc-auto-response-display-notification (type response)
  "Display notification about auto-response of TYPE with actual RESPONSE string."
  (let ((msg (format "Auto-responded: %s (\"%s\")" type response)))
    (message msg)))

;; Throttling functions

(defun ecc-auto-response-throttled-p (state)
  "Check if auto-response for STATE should be throttled.
Returns t if we responded to this state recently and should wait."
  (let* ((now (float-time))
         (last-time
          (alist-get state ecc-auto-response-last-time-alist 0.0))
         (elapsed (- now last-time)))
    (or
     ;; Check if this is a duplicate of the currently active state
     (eq state ecc-auto-response-active-state)
     ;; Check if we need to throttle based on time
     (< elapsed ecc-auto-response-throttle-time))))

(defun ecc-auto-response-update-time (state)
  "Update the last response time for STATE."
  (setf (alist-get state ecc-auto-response-last-time-alist)
        (float-time)))

;; Initial checking

(defun ecc-auto-response-initial-check ()
  "Perform an enhanced initial check for prompts when auto-response is started.
This ensures we respond to prompts immediately without waiting for the timer."
  (when (and ecc-buffer-auto-response-enabled
            (buffer-live-p ecc-buffer-current-buffer))
    (with-current-buffer ecc-buffer-current-buffer
      ;; Force a state check
      (let ((state (ecc-detect-state)))
        ;; When state is detected, reset throttling to ensure response happens
        (when state
          (ecc-debug-message "Initial check detected state: %s" state)
          ;; Reset throttling for initial check to ensure we respond
          (setq ecc-auto-response-active-state nil)
          (setf (alist-get state ecc-auto-response-last-time-alist) 0.0)
          ;; Run the check and respond function
          (ecc-auto-response-check))))))

;; Convenience functions for direct usage

;;;###autoload
(defun ecc-auto-response-send-current-buffer ()
  "Automatically check and respond to Claude prompts in the current buffer.
Detects the current prompt state in the active buffer and sends an appropriate
response based on the detected state. This is a convenience function that calls
`ecc-auto-response-send` with the current buffer.

This function can be bound to a key for quick access when you want to manually
trigger an automatic response to the current prompt."
  (interactive)
  (ecc-auto-response-send (current-buffer)))

;;;###autoload
(defun ecc-auto-response-send-yes (&optional buffer)
  "Automatically send Y response to Claude Y/N prompt in BUFFER."
  (interactive)
  (ecc-auto-response-send-message 
   (or buffer (current-buffer))
   ecc-auto-response-y/n 
   "Y/N"))

;;;###autoload
(defun ecc-auto-response-send-yes-plus (&optional buffer)
  "Automatically send Y response to Claude Y/Y/N prompt in BUFFER."
  (interactive)
  (ecc-auto-response-send-message
   (or buffer (current-buffer))
   ecc-auto-response-y/y/n 
   "Y/Y/N"))

;;;###autoload
(defun ecc-auto-response-send-continue (&optional buffer)
  "Automatically send continue to Claude waiting prompt in BUFFER."
  (interactive)
  (ecc-auto-response-send-message
   (or buffer (current-buffer))
   ecc-auto-response-waiting 
   "Continue"))

;;;###autoload
(defun ecc-auto-response-send-template (template-text)
  "Send custom TEMPLATE-TEXT to Claude."
  (interactive "sEnter your response: ")
  (ecc-auto-response-send-message 
   (current-buffer)
   template-text 
   (format "Template: %s" template-text)))

;; Function to connect auto-response to vterm output hook
;;;###autoload
(defun ecc-auto-response-connect-to-vterm-hook ()
  "Connect auto-response to vterm output hooks."
  (interactive)
  (when (derived-mode-p 'vterm-mode)
    (add-hook 'vterm-update-functions
              (lambda (&rest _)
                (when ecc-auto-response-check-on-output
                  (ecc-auto-response-check)))
              nil t)
    (message "Connected auto-response to vterm output hook")))

;; Register buffer functions

;;;###autoload
(defun ecc-register-buffer ()
  "Register the current buffer for auto-response.
This adds the current buffer to the list of buffers that will
be monitored for Claude prompts by the auto-response system."
  (interactive)
  (setq ecc-buffer-current-buffer (current-buffer))
  (message "Registered buffer %s for auto-response" (buffer-name)))

(provide 'ecc-auto-response-refactored)

;;; ecc-auto-response-refactored.el ends here