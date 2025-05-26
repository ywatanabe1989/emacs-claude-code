;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 22:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-notification.el

;;; Commentary:
;;; Consolidated notification system for Claude interactions.
;;; 
;;; This module provides a comprehensive notification framework for Claude states,
;;; integrating audible and visual alerts with configurable behavior.
;;;
;;; Key features:
;;; - Multiple notification methods (bell, visual flash, messages)
;;; - Configurable throttling to prevent excessive notifications
;;; - Integration with state detection system
;;; - Buffer-specific notification settings
;;; - Extensible notification action system
;;;
;;; Example usage:
;;;
;;;   ;; Toggle notifications on/off
;;;   (ecc-notification-toggle)
;;;
;;;   ;; Toggle bell notifications
;;;   (ecc-notification-toggle-bell)
;;;
;;;   ;; Notify about a specific state
;;;   (ecc-notification-dispatch :y/n)
;;;
;;;   ;; Setup notifications for current buffer
;;;   (ecc-notification-setup-for-buffer)

(require 'ecc-variables)

;; Try to load state detection module if available
(when (locate-library "ecc-state-detection")
  (require 'ecc-state-detection))

;;; Code:

;; Constants for notification

(defconst ecc-notification-state-initial :initial-waiting
  "State constant for initial waiting prompt.")

(defconst ecc-notification-state-waiting :waiting
  "State constant for waiting prompt.")

(defconst ecc-notification-state-yn :y/n
  "State constant for yes/no prompt.")

(defconst ecc-notification-state-yyn :y/y/n
  "State constant for yes/yes-and/no prompt.")

(defconst ecc-notification-method-bell 'bell
  "Notification method for audible bell.")

(defconst ecc-notification-method-flash 'flash
  "Notification method for visual flash.")

(defconst ecc-notification-method-message 'message
  "Notification method for message display.")

;; Customization group
(defgroup ecc-notification nil
  "Notification settings for Claude interactions."
  :group 'ecc
  :prefix "ecc-notification-")

(defcustom ecc-notification-enabled t
  "Whether notifications are enabled globally."
  :type 'boolean
  :group 'ecc-notification)

(defcustom ecc-notification-methods '(bell flash message)
  "List of notification methods to use.
Possible values in the list:
- 'bell: Audible alert
- 'flash: Visual mode line flash
- 'message: Echo area message"
  :type '(set (const :tag "Audible bell" bell)
             (const :tag "Mode line flash" flash)
             (const :tag "Echo area message" message))
  :group 'ecc-notification)

(defcustom ecc-notification-states 
  '(:initial-waiting :waiting :y/n :y/y/n)
  "List of Claude states that should trigger notification."
  :type '(repeat symbol)
  :group 'ecc-notification)

(defcustom ecc-notification-throttle-interval 2.0
  "Minimum interval between notifications in seconds."
  :type 'number
  :group 'ecc-notification)

(defcustom ecc-notification-bell-method 'audible
  "Method to use for bell notifications.
Possible values:
- 'audible: Standard audible bell (ding)
- 'visible: Flash the screen instead of sound
- 'both: Both audible and visible
- 'external: Use external command for bell"
  :type '(choice (const :tag "Audible" audible)
                (const :tag "Visible" visible)
                (const :tag "Both" both)
                (const :tag "External command" external))
  :group 'ecc-notification)

(defcustom ecc-notification-bell-external-command nil
  "External command to run for bell notifications.
This is used when `ecc-notification-bell-method' is set to 'external.
Example: \"paplay /usr/share/sounds/freedesktop/stereo/bell.oga\""
  :type '(choice (const :tag "None" nil)
                (string :tag "Command"))
  :group 'ecc-notification)

(defcustom ecc-notification-flash-duration 0.5
  "Duration in seconds for visible flashes."
  :type 'number
  :group 'ecc-notification)

(defcustom ecc-notification-state-descriptions
  '((:initial-waiting . "initial waiting for input")
    (:waiting . "waiting for input")
    (:y/n . "yes/no prompt")
    (:y/y/n . "multi-choice prompt"))
  "Alist mapping state symbols to human-readable descriptions."
  :type '(alist :key-type symbol :value-type string)
  :group 'ecc-notification)

;; Internal variables

(defvar ecc-notification--last-time 0
  "Time of the last notification in float seconds.")

(defvar ecc-notification--last-state nil
  "Last Claude state that triggered a notification.")

(defvar ecc-notification--flash-timer nil
  "Timer for mode line flashing.")

;; Helper macros and functions

(defmacro ecc-notification-with-buffer (buffer &rest body)
  "Execute BODY in BUFFER or current buffer."
  (declare (indent 1) (debug t))
  `(with-current-buffer (or ,buffer (current-buffer))
     ,@body))

(defun ecc-notification--state-description (state)
  "Get human-readable description for STATE."
  (or (cdr (assq state ecc-notification-state-descriptions))
      (format "%s" (if (keywordp state)
                       (substring (symbol-name state) 1)
                     state))))

(defun ecc-notification--should-notify-p (state)
  "Check if we should notify about STATE now.
Takes into account state type, throttling, and previous state."
  (and ecc-notification-enabled
       state
       (memq state ecc-notification-states)
       (or 
        ;; Always notify for new state
        (not (eq state ecc-notification--last-state))
        ;; For same state, only notify after interval
        (> (- (float-time) ecc-notification--last-time) 
           ecc-notification-throttle-interval))))

(defun ecc-notification--update-state (state)
  "Update tracking variables for STATE.
This should be called after notification to track throttling."
  (setq ecc-notification--last-state state)
  (setq ecc-notification--last-time (float-time)))

;; Bell notification methods

(defun ecc-notification-ring-bell-audible ()
  "Ring an audible bell."
  ;; Try to play sound file if available
  (when (fboundp 'play-sound-file)
    (ignore-errors
      (let ((bell-sound (expand-file-name "~/.emacs.d/sounds/bell.wav")))
        (when (file-exists-p bell-sound)
          (play-sound-file bell-sound)))))
  
  ;; Fall back to standard bell
  (let ((ring-bell-function nil)) ; Temporarily disable any custom bell function
    (ding t)))

(defun ecc-notification-ring-bell-visible ()
  "Ring a visible bell."
  (let ((visible-bell t))
    (ding)))

(defun ecc-notification-ring-bell-both ()
  "Ring both an audible and visible bell."
  (let ((visible-bell nil))
    (ding t))
  (invert-face 'mode-line)
  (run-with-timer ecc-notification-flash-duration nil
                 (lambda () (invert-face 'mode-line))))

(defun ecc-notification-ring-bell-external ()
  "Ring an external bell using a configured command."
  (when (and ecc-notification-bell-external-command
             (not (string-empty-p ecc-notification-bell-external-command)))
    (start-process "ecc-bell" nil shell-file-name shell-command-switch
                   ecc-notification-bell-external-command)))

(defun ecc-notification-ring-bell ()
  "Ring the terminal bell using configured method.
Handles different system configurations to ensure bell is audible."
  (pcase ecc-notification-bell-method
    ('audible (ecc-notification-ring-bell-audible))
    ('visible (ecc-notification-ring-bell-visible))
    ('both (ecc-notification-ring-bell-both))
    ('external (ecc-notification-ring-bell-external))
    (_ (ecc-notification-ring-bell-audible)))) ; Default fallback

(defun ecc-auto-notify-flash-mode-line (&optional buffer)
  "Flash the mode line to get attention in BUFFER.
If BUFFER is not provided, uses the current buffer."
  (let ((target-buffer (or buffer (current-buffer))))
    (when ecc-notification--flash-timer
      (cancel-timer ecc-notification--flash-timer))
    
    ;; First flash - invert immediately
    (with-current-buffer target-buffer
      (invert-face 'mode-line)
      (force-mode-line-update))
    
    ;; Set timer to restore mode line
    (setq ecc-notification--flash-timer
          (run-with-timer ecc-notification-flash-duration nil
                        (lambda ()
                          (when (buffer-live-p target-buffer)
                            (with-current-buffer target-buffer
                              (invert-face 'mode-line)
                              (force-mode-line-update))))))))

(defun ecc-notification-display-message (state &optional buffer)
  "Display a message about STATE in the echo area.
If BUFFER is provided, include the buffer name in the message."
  (if buffer
      (message "[%s] Claude prompt detected: %s" 
               (buffer-name buffer)
               (ecc-notification--state-description state))
    (message "Claude prompt detected: %s" 
             (ecc-notification--state-description state))))

;; Core notification functions


(defun ecc-notification-dispatch (state &optional buffer)
  "Notify the user about a Claude STATE using configured methods.
If BUFFER is provided, include the buffer name in message notifications.
Returns non-nil if notification was performed."
  (when (ecc-notification--should-notify-p state)
    ;; Apply each enabled notification method
    (when (memq 'bell ecc-notification-methods)
      (ecc-notification-ring-bell))
    
    (when (memq 'flash ecc-notification-methods)
      (ecc-auto-notify-flash-mode-line buffer))
    
    (when (memq 'message ecc-notification-methods)
      (ecc-notification-display-message state buffer))
    
    ;; Update tracking state
    (ecc-notification--update-state state)
    
    ;; Return non-nil to indicate notification was performed
    t))


(defun ecc-notification-check-state (state)
  "Check if STATE requires notification and dispatch if needed.
Returns non-nil if notification was performed."
  (ecc-notification-dispatch state))

;; Toggle functions


(defun ecc-notification-toggle ()
  "Toggle notifications for Claude prompts.
Enables or disables notifications globally."
  (interactive)
  (setq ecc-notification-enabled (not ecc-notification-enabled))
  (ecc-debug-message "Claude notifications %s"
           (if ecc-notification-enabled "enabled" "disabled")))


(defun ecc-notification-toggle-bell ()
  "Toggle bell notification for Claude prompts."
  (interactive)
  (if (memq 'bell ecc-notification-methods)
      (setq ecc-notification-methods 
            (delq 'bell ecc-notification-methods))
    (push 'bell ecc-notification-methods))
  (ecc-debug-message "Bell notifications %s"
           (if (memq 'bell ecc-notification-methods)
               "enabled" "disabled")))


(defun ecc-notification-toggle-flash ()
  "Toggle visual flash notification for Claude prompts."
  (interactive)
  (if (memq 'flash ecc-notification-methods)
      (setq ecc-notification-methods 
            (delq 'flash ecc-notification-methods))
    (push 'flash ecc-notification-methods))
  (ecc-debug-message "Flash notifications %s"
           (if (memq 'flash ecc-notification-methods)
               "enabled" "disabled")))

;; Integration with buffer and state detection


(defun ecc-notification-setup-for-buffer ()
  "Set up notifications for the current buffer.
This function is designed to be added to mode hooks for Claude-related
buffer modes to automatically set up prompt notifications."
  (interactive)
  (when (derived-mode-p 'ecc-term-claude-mode 'vterm-mode)
    ;; Add detection hook for this buffer
    (add-hook 'ecc-term-claude-update-functions
              (lambda ()
                (let ((state (if (fboundp 'ecc-detect-state)
                               ;; Use new detection system if available
                               (ecc-detect-state)
                             ;; Fall back to simple state detection 
                             (when (fboundp 'ecc-detect-state)
                               (ecc-detect-state)))))
                  (when state
                    (ecc-notification-check-state state))))
              nil t)))

;; Autoload and initialization


(defun ecc-notification-setup ()
  "Set up the notification system."
  (interactive)
  ;; Add hooks for buffer modes
  (add-hook 'ecc-term-claude-mode-hook #'ecc-notification-setup-for-buffer)
  (add-hook 'vterm-mode-hook
            (lambda ()
              (when (string-match-p "\\*CLAUDE.*\\*" (buffer-name))
                (ecc-notification-setup-for-buffer)))))

;; Install the setup hooks when this module is loaded
(ecc-notification-setup)

(provide 'ecc-notification)

;;; ecc-notification.el ends here