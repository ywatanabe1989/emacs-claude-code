;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 07:45:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-auto/ecc-auto-response.el

;;; Commentary:
;;; Auto-response functionality for Claude prompts.

(require 'ecc-variables)

;;;###autoload
(defun ecc-auto-accept-send ()
  "Automatically check and respond to Claude prompts in the current buffer.
Detects the current prompt state in the active buffer and sends an appropriate
response based on the detected state. This is a convenience function that calls
`ecc-auto-response-send` with the current buffer.

This function can be bound to a key for quick access when you want to manually
trigger an automatic response to the current prompt.

See also `ecc-auto-response-send` for the underlying implementation."
  (interactive)
  (ecc-auto-response-send (current-buffer)))

;;;###autoload
(defun ecc-auto-response-send (buffer &optional state)
  "Automatically respond to Claude prompts in BUFFER.
Examines the buffer content to detect Claude's current prompt state, then
sends an appropriate pre-configured response based on that state.

BUFFER is the buffer containing Claude's output to respond to.
Optional STATE can be provided to override automatic state detection.
Valid states are: `:y/n`, `:y/y/n`, `:initial-waiting`, `:waiting`

Responds with:
- `ecc-auto-response-y/n` for Y/N prompts (default: "1")
- `ecc-auto-response-y/y/n` for Y/Y/N prompts (default: "2")
- `ecc-auto-response-initial-waiting` for initial waiting state (default: "/user:understand-guidelines")
- `ecc-auto-response-waiting` for waiting prompts (default: "/auto")

Returns nil. Displays notification message when response is sent if
`ecc-auto-notify-completions` is enabled."
  (interactive (list (current-buffer)))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((state (or state (ecc-detect-simple-state))))
        (cond
         ((eq state :y/y/n)
          (ecc-auto--send-response buffer 
                                ecc-auto-response-y/y/n 
                                "Y/Y/N"))
         ((eq state :y/n)
          (ecc-auto--send-response buffer 
                                ecc-auto-response-y/n 
                                "Y/N"))
         ((eq state :initial-waiting)
          (ecc-auto--send-response buffer 
                                ecc-auto-response-initial-waiting 
                                "Initial-Waiting"))
         ((eq state :waiting)
          (ecc-auto--send-response buffer 
                                ecc-auto-response-waiting 
                                "Continue")))))))

(defun ecc-auto--send-response (buffer response type)
  "Send RESPONSE to Claude in BUFFER.
TYPE is used for notification messages."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      ;; Ensure we have the right mode to send commands
      (cond
       ;; vterm mode
       ((derived-mode-p 'vterm-mode)
        (ecc-auto--send-vterm-response response))
       
       ;; Default fallback
       (t
        (message "Cannot send auto-response to buffer in %s mode" major-mode)))))
  
  ;; Notify user about the response
  (when (and (boundp 'ecc-auto-notify-completions) ecc-auto-notify-completions)
    (ecc-auto--notify type response)))


;; Make sure we have interaction counter defined
(defvar ecc-interaction-counter 0
  "Counter for tracking the number of interactions with Claude.")

(defun ecc-auto--send-vterm-response (response)
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

      ;; Output debug information using our macro
      (ecc-debug-message "DEBUG: Interaction Counter: %d" ecc-interaction-counter)
      (ecc-debug-message "DEBUG: Point: %d, Max: %d, Distance from end: %d" 
                       (point) (point-max) distance-from-end)
      (ecc-debug-message "DEBUG: First interaction? %s" 
                       (if first-interaction-p "Yes" "No"))
      (ecc-debug-message "DEBUG: Using condition: %s" 
                       (if (or first-interaction-p (< distance-from-end 40))
                           "Direct send (at point)" 
                         "Save-excursion (at end)"))
      
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

;; (defun ecc-auto--send-vterm-response (response)
;;   "Send RESPONSE to Claude in a vterm buffer.
;; Special handling is applied for the first interaction to ensure reliability."
;;   (when (fboundp 'vterm-send-string)
;;     ;; Save current point position
;;     (let ((old-point (point))
;;           (is-first-interaction (= ecc-interaction-counter 0))
;;           (delay-base (if (= ecc-interaction-counter 0) 1.5 1.0)))
      
;;       ;; Check if point is near the end (user not reading earlier content)
;;       ;; while accepting initial interaction
;;       (if (or (> (point-max) (+ (point) 40))
;;                   is-first-interaction)
;;           ;; User might be reading earlier content, so don't move point
;;           (save-excursion
;;             (goto-char (point-max))
;;             (sit-for delay-base)
;;             (vterm-send-string response)
;;             (sit-for delay-base)            
;;             (vterm-send-return)
;;             (sit-for delay-base))
;;         ;; User is at end of buffer, proceed normally
;;         (sit-for delay-base)
;;         (vterm-send-string response)
;;         (sit-for delay-base)                    
;;         (vterm-send-return)
;;         (sit-for delay-base))
      
;;       ;; Increment interaction counter after the first successful response
;;       (when is-first-interaction
;;         (setq ecc-interaction-counter (1+ ecc-interaction-counter))
;;         ;; Add timestamp for this interaction
;;         (when (boundp 'ecc-interaction-timestamps)
;;           (push (current-time) ecc-interaction-timestamps))))))

(defun ecc-auto--notify (type response)
  "Display notification about auto-response of TYPE with actual RESPONSE string.
TYPE is a description of the response context (e.g., \"Y/N\").
RESPONSE is the actual string sent to Claude."
  (let ((msg (format "Auto-responded: %s (\"%s\")" type response)))
    (message msg)))

;; Specific response functions with defaults (useful for interactive use)

;;;###autoload
(defun ecc-auto-response-continue (&optional buffer)
  "Automatically send continue to Claude waiting prompt.
Sends the continue response (defined by `ecc-auto-response-waiting`) to
Claude when it's in a waiting state, prompting for more output.

If BUFFER is nil, use current buffer.

This is an interactive convenience function that can be bound to a key
for quickly continuing Claude's output without needing to type the
continue command manually.

See also `ecc-auto--send-response` for the underlying implementation."
  (interactive)
  (ecc-auto--send-response (or buffer (current-buffer))
                       ecc-auto-response-waiting
                       "Continue"))

;;;###autoload
(defun ecc-auto-response-yes (&optional buffer)
  "Automatically send Y response to Claude Y/N prompt.
Sends the yes response (defined by `ecc-auto-response-y/n`) to
Claude when it's in a Y/N prompt state.

If BUFFER is nil, use current buffer.

This is an interactive convenience function that can be bound to a key
for quickly responding affirmatively to Claude's yes/no questions without
needing to type the response manually.

See also `ecc-auto--send-response` for the underlying implementation."
  (interactive)
  (ecc-auto--send-response (or buffer (current-buffer))
                       ecc-auto-response-y/n
                       "Y/N"))

;;;###autoload
(defun ecc-auto-response-yes-plus (&optional buffer)
  "Automatically send Y response to Claude Y/Y/N prompt.
If BUFFER is nil, use current buffer."
  (interactive)
  (ecc-auto--send-response (or buffer (current-buffer))
                       ecc-auto-response-y/y/n
                       "Y/Y/N"))

;;;###autoload
(defun ecc-auto-response-template (template-text)
  "Send custom TEMPLATE-TEXT to Claude.
This allows sending natural language responses instead of just number options."
  (interactive "sEnter your response: ")
  (let ((buffer (current-buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (ecc-auto--send-response buffer
                             template-text
                             (format "Template: %s" template-text))))))

;;;###autoload
(defun ecc-start-auto-response (&optional response-y/n response-y/y/n response-waiting)
  "Start automatic response system for Claude prompts.
Initializes and activates the auto-response system that automatically responds
to different types of Claude prompts. Sets up a timer that periodically checks
the current buffer for prompts and sends appropriate responses.

RESPONSE-Y/N: Response for Y/N prompts (default: "1")
RESPONSE-Y/Y/N: Response for Y/Y/N prompts (default: "2")
RESPONSE-WAITING: Response for waiting prompts (default: "/auto")

The timer runs every 0.5 seconds and calls `ecc-check-and-respond` to
detect and respond to prompts. The timer can be cancelled with
`ecc-stop-auto-response`.

Displays a message with the configured response values when started.
See also `ecc-buffer-auto-response-enabled` for the global toggle."
  (interactive)
  
  ;; Set response values
  (when response-y/n
    (setq ecc-auto-response-y/n response-y/n))
  (when response-y/y/n
    (setq ecc-auto-response-y/y/n response-y/y/n))
  (when response-waiting
    (setq ecc-auto-response-waiting response-waiting))
  
  ;; Enable auto-response mode globally  
  (setq ecc-buffer-auto-response-enabled t)
  
  ;; Initialize timer for periodic checking
  (when (bound-and-true-p ecc-auto-response-timer)
    (cancel-timer ecc-auto-response-timer))
  
  (setq ecc-auto-response-timer
        (run-with-timer 1 0.5 #'ecc-check-and-respond))
  
  (message "Auto-response started: Y/N=%s, Y/Y/N=%s, Continue=%s" 
           ecc-auto-response-y/n 
           ecc-auto-response-y/y/n
           ecc-auto-response-waiting))

;;;###autoload
(defun ecc-check-and-respond ()
  "Check current buffer state and respond automatically."
  (when (and ecc-buffer-auto-response-enabled
             (buffer-live-p ecc-buffer-current-buffer))
    (with-current-buffer ecc-buffer-current-buffer
      (let ((state (ecc-detect-simple-state)))
        (cond
         ((eq state :y/y/n)
          (ecc-auto--send-response ecc-buffer-current-buffer
                                 ecc-auto-response-y/y/n
                                 "Y/Y/N"))
         ((eq state :y/n)
          (ecc-auto--send-response ecc-buffer-current-buffer
                                 ecc-auto-response-y/n
                                 "Y/N"))
         ((eq state :initial-waiting)
          (ecc-auto--send-response ecc-buffer-current-buffer
                                 ecc-auto-response-initial-waiting
                                 "Initial-Waiting"))
         ((eq state :waiting)
          (ecc-auto--send-response ecc-buffer-current-buffer
                                 ecc-auto-response-waiting
                                 "Continue")))))))

;;;###autoload
(defun ecc-stop-auto-response ()
  "Stop the auto-response system."
  (interactive)
  (when (bound-and-true-p ecc-auto-response-timer)
    (cancel-timer ecc-auto-response-timer)
    (setq ecc-auto-response-timer nil))
  (setq ecc-buffer-auto-response-enabled nil)
  (message "Auto-response stopped"))

;;;###autoload
(defun ecc-toggle-auto-response ()
  "Toggle auto-response on/off."
  (interactive)
  (if ecc-buffer-auto-response-enabled
      (progn
        (ecc-stop-auto-response)
        (message "Auto-response disabled"))
    (progn
      (ecc-register-buffer)
      (ecc-start-auto-response)
      (message "Auto-response enabled"))))


(provide 'ecc-auto-response)

;;; ecc-auto-response.el ends here
