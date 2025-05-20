;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 23:20:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-auto-notify.el

;;; Commentary:
;;; Notification system for Claude auto-response.
;;; This module provides a clean notification API for alerting users
;;; about Claude prompts and auto-responses. It consolidates the notification
;;; functionality previously spread across multiple modules.

(require 'ecc-variables)
(require 'ecc-auto-detect)

;;; Code:

;; Customization options
(defgroup ecc-auto-notify nil
  "Notification settings for Claude auto-response."
  :group 'ecc
  :prefix "ecc-auto-notify-")

(defcustom ecc-auto-notify-enabled t
  "Whether notifications are enabled for Claude prompts and responses."
  :type 'boolean
  :group 'ecc-auto-notify)

(defcustom ecc-auto-notify-prompt-types
  '(:y/n :y/y/n :waiting :initial-waiting)
  "Types of Claude prompts to notify about."
  :type '(repeat symbol)
  :group 'ecc-auto-notify)

(defcustom ecc-auto-notify-method 'both
  "Method for notifying about Claude prompts.
Possible values:
- 'bell: Use auditory bell notification
- 'visual: Use visual notification (mode line flashing)
- 'both: Use both auditory and visual notifications
- 'message: Use message area only
- 'none: No notification"
  :type '(choice (const :tag "Auditory bell" bell)
                 (const :tag "Visual indicator" visual)
                 (const :tag "Both auditory and visual" both)
                 (const :tag "Message only" message)
                 (const :tag "No notification" none))
  :group 'ecc-auto-notify)

(defcustom ecc-auto-notify-bell-method 'visible-bell
  "Method for bell notification.
Possible values:
- 'visible-bell: Use Emacs' visible bell
- 'beep: Use terminal beep
- 'flash: Flash the mode line"
  :type '(choice (const :tag "Visible bell" visible-bell)
                 (const :tag "Terminal beep" beep)
                 (const :tag "Flash mode line" flash))
  :group 'ecc-auto-notify)

(defcustom ecc-auto-notify-flash-time 0.3
  "Duration in seconds for mode line flash."
  :type 'number
  :group 'ecc-auto-notify)

(defcustom ecc-auto-notify-interval 5.0
  "Minimum interval in seconds between consecutive notifications."
  :type 'number
  :group 'ecc-auto-notify)

;; Internal variables
(defvar ecc-auto-notify-last-time 0.0
  "Time of last notification.")

(defvar ecc-auto-notify-last-type nil
  "Last prompt type that triggered a notification.")

(defvar ecc-auto-notify-mode-line-cookie nil
  "Mode line cookie for restoring original mode line face.")

;; Core notification functions

;;;###autoload
(defun ecc-auto-notify-prompt (prompt-type)
  "Notify the user about a Claude prompt of PROMPT-TYPE.
PROMPT-TYPE should be one of: :y/n, :y/y/n, :waiting, :initial-waiting."
  (interactive (list (ecc-auto-detect-prompt)))
  
  (when (and ecc-auto-notify-enabled
             (memq prompt-type ecc-auto-notify-prompt-types))
    
    ;; Check for notification throttling
    (let ((now (float-time))
          (elapsed (- (float-time) ecc-auto-notify-last-time)))
      
      ;; Only notify if different type or enough time has elapsed
      (when (or (not (eq prompt-type ecc-auto-notify-last-type))
                (> elapsed ecc-auto-notify-interval))
        
        ;; Update tracking variables
        (setq ecc-auto-notify-last-type prompt-type)
        (setq ecc-auto-notify-last-time now)
        
        ;; Show appropriate notifications
        (let ((type-name (ecc-auto-detect-name prompt-type)))
          ;; Always show message
          (message "Claude prompt detected: %s" type-name)
          
          ;; Auditory notification
          (when (memq ecc-auto-notify-method '(bell both))
            (ecc-auto-notify-ring-bell))
          
          ;; Visual notification
          (when (memq ecc-auto-notify-method '(visual both))
            (ecc-auto-notify-flash-mode-line)))))))

;;;###autoload
(defun ecc-auto-notify-response (prompt-type response)
  "Notify the user about an auto-response to PROMPT-TYPE.
PROMPT-TYPE should be one of: :y/n, :y/y/n, :waiting, :initial-waiting.
RESPONSE is the text that was sent."
  (when ecc-auto-notify-enabled
    (let ((type-name (ecc-auto-detect-name prompt-type)))
      (message "Auto-responded to %s: \"%s\"" type-name response))))

;; Notification methods

(defun ecc-auto-notify-ring-bell ()
  "Ring the terminal bell using configured method."
  (cond
   ;; Use Emacs visible bell
   ((eq ecc-auto-notify-bell-method 'visible-bell)
    (let ((ring-bell-function nil)
          (visible-bell t))
      (ding)))
   
   ;; Use terminal beep
   ((eq ecc-auto-notify-bell-method 'beep)
    (let ((ring-bell-function nil)
          (visible-bell nil))
      (ding)))
   
   ;; Use mode line flash (additional visual indicator)
   ((eq ecc-auto-notify-bell-method 'flash)
    (ecc-auto-notify-flash-mode-line))))

(defun ecc-auto-notify-flash-mode-line ()
  "Flash the mode line to get attention."
  (when ecc-auto-notify-mode-line-cookie
    (face-remap-remove-relative ecc-auto-notify-mode-line-cookie))
  
  ;; Set mode-line to inverse video
  (setq ecc-auto-notify-mode-line-cookie
        (face-remap-add-relative 'mode-line '(:inverse-video t)))
  
  ;; Set timer to restore mode-line
  (run-with-timer
   ecc-auto-notify-flash-time nil
   (lambda ()
     (when ecc-auto-notify-mode-line-cookie
       (face-remap-remove-relative ecc-auto-notify-mode-line-cookie)
       (setq ecc-auto-notify-mode-line-cookie nil)))))

;; User commands

;;;###autoload
(defun ecc-auto-notify-toggle ()
  "Toggle notification for Claude prompts."
  (interactive)
  (setq ecc-auto-notify-enabled (not ecc-auto-notify-enabled))
  (message "Claude notifications %s"
           (if ecc-auto-notify-enabled "enabled" "disabled")))

;;;###autoload
(defun ecc-auto-notify-bell-toggle ()
  "Toggle bell notification for Claude prompts."
  (interactive)
  (setq ecc-auto-notify-method
        (if (memq ecc-auto-notify-method '(bell both))
            (if (eq ecc-auto-notify-method 'both) 'visual 'none)
          (if (eq ecc-auto-notify-method 'visual) 'both 'bell)))
  (message "Claude bell notifications %s"
           (if (memq ecc-auto-notify-method '(bell both))
               "enabled" "disabled")))

;; Backward compatibility
;;;###autoload
(defalias 'ecc-auto-notify-check-state 'ecc-auto-notify-prompt
  "Compatibility alias for `ecc-auto-notify-prompt'.")

;;;###autoload
(define-obsolete-function-alias 'ecc-notify-toggle
  'ecc-auto-notify-toggle "May 2025")

;;;###autoload
(define-obsolete-function-alias 'ecc-bell-toggle
  'ecc-auto-notify-bell-toggle "May 2025")

(provide 'ecc-auto-notify)

;;; ecc-auto-notify.el ends here