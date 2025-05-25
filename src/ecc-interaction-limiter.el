;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 09:35:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-interaction-limiter.el

;;; Commentary:
;;; Limits the number of automatic Claude responses in a session or time period.
;;; This module focuses on limiting Claude's auto-responses while preserving
;;; the ability to interact manually when limits are reached.

(require 'ecc-variables)
(require 'ecc-interaction-tracker)

;; Customization group
(defgroup ecc-interaction-limits nil
  "Settings for limiting Claude interactions."
  :group 'ecc-interaction
  :prefix "ecc-interaction-limit-")

(defcustom ecc-interaction-limit-enabled nil
  "Whether to enable auto-response limits for Claude."
  :type 'boolean
  :group 'ecc-interaction-limits)

(defcustom ecc-interaction-limit-count 50
  "Maximum number of automatic responses allowed in a session."
  :type 'integer
  :group 'ecc-interaction-limits)

(defcustom ecc-interaction-limit-warn-threshold 0.8
  "Threshold (percentage of max) at which to warn about approaching limits."
  :type 'float
  :group 'ecc-interaction-limits)

(defcustom ecc-interaction-limit-action 'warn
  "Action to take when auto-response limit is reached.
'warn: Display a warning message but allow auto-response to continue
'block: Block further auto-responses (requires manual responses)
'prompt: Ask user for confirmation before allowing auto-response"
  :type '(choice (const :tag "Warn only" warn)
                (const :tag "Block auto-response" block)
                (const :tag "Prompt for confirmation" prompt))
  :group 'ecc-interaction-limits)

(defcustom ecc-interaction-limit-per-time-enabled nil
  "Whether to enable time-based auto-response limits."
  :type 'boolean
  :group 'ecc-interaction-limits)

(defcustom ecc-interaction-limit-per-hour 20
  "Maximum number of automatic responses allowed per hour."
  :type 'integer
  :group 'ecc-interaction-limits)

(defvar ecc-interaction-limit-handler-function nil
  "Function to call when limit is reached. 
Function receives two args: limit-type and current-count.")

;; Main functions
(defun ecc-interaction-check-limits ()
  "Check auto-response limits before allowing a new auto-response.
Returns t if auto-response is allowed, nil otherwise."
  (if (not ecc-interaction-limit-enabled)
      t ; If limits are disabled, always allow
    (let ((allowed t)
          (count-limit-reached (>= ecc-interaction-counter ecc-interaction-limit-count))
          (time-limit-reached (and ecc-interaction-limit-per-time-enabled
                                  (ecc-interaction-time-limit-reached-p))))
      
      ;; Check for warning threshold
      (when (and (not count-limit-reached)
                 (>= ecc-interaction-counter 
                     (* ecc-interaction-limit-count ecc-interaction-limit-warn-threshold)))
        (ecc-debug-message "Warning: Approaching interaction limit (%d/%d)" 
                 ecc-interaction-counter ecc-interaction-limit-count))
      
      ;; Handle count limit
      (when count-limit-reached
        (setq allowed (ecc-interaction-handle-limit 'count ecc-interaction-counter)))
      
      ;; Handle time limit
      (when (and allowed time-limit-reached)
        (setq allowed (ecc-interaction-handle-limit 'time 
                                                   (ecc-interaction-count-in-last-hour))))
      
      allowed)))

(defun ecc-interaction-handle-limit (limit-type current-count)
  "Handle reaching a limit of type LIMIT-TYPE with CURRENT-COUNT.
Returns whether to allow the auto-response to proceed."
  ;; Call custom handler if defined
  (when (functionp ecc-interaction-limit-handler-function)
    (funcall ecc-interaction-limit-handler-function limit-type current-count))
  
  ;; Handle based on configured action
  (pcase ecc-interaction-limit-action
    ('warn
     (ecc-interaction-display-limit-warning limit-type current-count)
     t) ; Allow interaction
    
    ('block
     (ecc-interaction-display-limit-warning limit-type current-count)
     nil) ; Block interaction
    
    ('prompt
     (ecc-interaction-prompt-for-override limit-type current-count))))

(defun ecc-interaction-display-limit-warning (limit-type current-count)
  "Display a warning about reaching LIMIT-TYPE with CURRENT-COUNT."
  (let* ((limit (pcase limit-type
                  ('count ecc-interaction-limit-count)
                  ('time ecc-interaction-limit-per-hour)
                  (_ 0)))
         (type-str (pcase limit-type
                     ('count "session")
                     ('time "hourly")
                     (_ "unknown"))))
    (ecc-debug-message "Claude auto-response %s limit reached: %d/%d" 
             type-str current-count limit)))

(defun ecc-interaction-prompt-for-override (limit-type current-count)
  "Prompt user to override LIMIT-TYPE with CURRENT-COUNT.
Returns t if user chooses to override, nil otherwise."
  (let* ((limit (pcase limit-type
                  ('count ecc-interaction-limit-count)
                  ('time ecc-interaction-limit-per-hour)
                  (_ 0)))
         (type-str (pcase limit-type
                     ('count "session")
                     ('time "hourly")
                     (_ "unknown"))))
    (yes-or-no-p 
     (format "Claude %s auto-response limit reached (%d/%d). Continue with auto-response? " 
             type-str current-count limit))))

(defun ecc-interaction-time-limit-reached-p ()
  "Check if the hourly interaction limit has been reached.
Returns t if limit reached, nil otherwise."
  (when ecc-interaction-limit-per-time-enabled
    (>= (ecc-interaction-count-in-last-hour) 
        ecc-interaction-limit-per-hour)))

(defun ecc-interaction-count-in-last-hour ()
  "Count interactions in the last hour."
  (let* ((now (float-time))
         (hour-ago (- now 3600))
         (recent-timestamps (seq-filter 
                            (lambda (time) (>= time hour-ago))
                            ecc-interaction-timestamps)))
    (length recent-timestamps)))

;;;###autoload
(defun ecc-toggle-interaction-limits ()
  "Toggle auto-response limits on/off."
  (interactive)
  (setq ecc-interaction-limit-enabled (not ecc-interaction-limit-enabled))
  (ecc-debug-message "Claude auto-response limits %s (max: %d per session, %s)"
           (if ecc-interaction-limit-enabled "enabled" "disabled")
           ecc-interaction-limit-count
           (if ecc-interaction-limit-per-time-enabled
               (format "max: %d per hour" ecc-interaction-limit-per-hour)
             "no hourly limit")))

;;;###autoload
(defun ecc-set-interaction-limit (count)
  "Set the maximum number of allowed auto-responses to COUNT."
  (interactive "nMax auto-responses: ")
  (setq ecc-interaction-limit-count count)
  (ecc-debug-message "Claude auto-response limit set to %d %s"
           count
           (if ecc-interaction-limit-enabled "auto-responses (enabled)" "auto-responses (currently disabled)")))

;;;###autoload
(defun ecc-toggle-time-based-limits ()
  "Toggle time-based auto-response limits on/off."
  (interactive)
  (setq ecc-interaction-limit-per-time-enabled 
        (not ecc-interaction-limit-per-time-enabled))
  (ecc-debug-message "Time-based auto-response limits %s (max: %d per hour)"
           (if ecc-interaction-limit-per-time-enabled "enabled" "disabled")
           ecc-interaction-limit-per-hour))

;;;###autoload
(defun ecc-set-hourly-interaction-limit (count)
  "Set the maximum number of allowed auto-responses per hour to COUNT."
  (interactive "nMax auto-responses per hour: ")
  (setq ecc-interaction-limit-per-hour count)
  (ecc-debug-message "Hourly auto-response limit set to %d %s"
           count
           (if ecc-interaction-limit-per-time-enabled 
               "auto-responses (enabled)" 
             "auto-responses (currently disabled)")))

;; Setup integration with auto-response system
(defun ecc-auto-response-check-limits ()
  "Check if auto-response should be blocked due to limits."
  (when ecc-interaction-limit-enabled
    (not (ecc-interaction-check-limits))))

;; Integrate with the auto-response system
(with-eval-after-load 'ecc-auto-response
  (when (fboundp 'ecc-auto-add-block-check)
    (ecc-auto-add-block-check #'ecc-auto-response-check-limits)))

;; Reset counter when auto-response is stopped
(with-eval-after-load 'ecc-auto-response
  (advice-add 'ecc-stop-auto-response :after
              (lambda ()
                "Reset interaction counter when auto-response is stopped."
                (ecc-reset-interaction-counter))))

;; Integration with keybindings
(defvar ecc-interaction-limits-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") 'ecc-toggle-interaction-limits)
    (define-key map (kbd "s") 'ecc-set-interaction-limit)
    (define-key map (kbd "h") 'ecc-toggle-time-based-limits)
    (define-key map (kbd "H") 'ecc-set-hourly-interaction-limit)
    map)
  "Keymap for interaction limits commands.")

(provide 'ecc-interaction-limiter)

;;; ecc-interaction-limiter.el ends here