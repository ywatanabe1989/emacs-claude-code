;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 20:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-auto-notify-improved.el

;;; Commentary:
;;; Enhanced notification functionality for Claude prompts, including bells,
;;; mode line flashing, and customizable notification behavior.
;;;
;;; This module provides a comprehensive notification system for different
;;; Claude states, with support for various notification methods and
;;; throttling to prevent excessive notifications.

(require 'ecc-variables)

;; Customization group
(defgroup ecc-auto-notify nil
  "Notification settings for Claude Auto mode."
  :group 'ecc-auto
  :prefix "ecc-auto-notify-")

(defcustom ecc-auto-notify-bell t
  "Whether to ring the bell when a prompt is detected."
  :type 'boolean
  :group 'ecc-auto-notify)

(defcustom ecc-auto-notify-flash t
  "Whether to flash the mode line when a prompt is detected."
  :type 'boolean
  :group 'ecc-auto-notify)

(defcustom ecc-auto-notify-prompt-types '(:initial-waiting :waiting :y/n :y/y/n)
  "List of prompt types to notify about."
  :type '(repeat symbol)
  :group 'ecc-auto-notify)

(defcustom ecc-auto-notify-interval 2.0
  "Minimum interval between notifications in seconds."
  :type 'number
  :group 'ecc-auto-notify)

(defcustom ecc-auto-notify-bell-method 'audible
  "Method to use for bell notifications.
Possible values:
- 'audible: Standard audible bell (ding)
- 'visible: Flash the screen instead of sound
- 'both: Both audible and visible
- 'external: Use external command for bell"
  :type '(choice (const :tag "Audible" audible)
                (const :tag "Visible" visible)
                (const :tag "Both" both)
                (const :tag "External Command" external))
  :group 'ecc-auto-notify)

(defcustom ecc-auto-notify-bell-external-command nil
  "External command to run for bell notifications.
This is used when `ecc-auto-notify-bell-method' is set to 'external.
Example: \"paplay /usr/share/sounds/freedesktop/stereo/bell.oga\""
  :type '(choice (const :tag "None" nil)
                (string :tag "Command"))
  :group 'ecc-auto-notify)

(defcustom ecc-auto-notify-bell-duration 0.5
  "Duration in seconds for visible bell flash."
  :type 'number
  :group 'ecc-auto-notify)

(defcustom ecc-auto-notify-bell-volume 100
  "Volume for the bell (1-100). May not work on all systems."
  :type 'integer
  :group 'ecc-auto-notify)

;; Internal variables
(defvar ecc-auto-notify--last-time 0
  "Time of the last notification.")

(defvar ecc-auto-notify--last-state nil
  "Last Claude state that triggered a notification.")

(defvar ecc-auto-notify--flash-timer nil
  "Timer for mode line flashing.")

;;;###autoload
(defun ecc-auto-notify-check-state (state)
  "Check if STATE requires notification and notify if needed.
This function checks the current state against configured notification
settings and triggers notifications when appropriate. It includes
throttling to prevent excessive notifications.

STATE is a symbol representing the detected Claude state, such as
:waiting, :y/n, :y/y/n, or :initial-waiting."
  (when (and (boundp 'ecc-auto-notify-on-claude-prompt)
             ecc-auto-notify-on-claude-prompt
             state
             (memq state ecc-auto-notify-prompt-types)
             (or (not (eq state ecc-auto-notify--last-state))
                 (> (- (float-time) ecc-auto-notify--last-time) 
                    ecc-auto-notify-interval)))
    (ecc-auto-notify-prompt state)
    (setq ecc-auto-notify--last-state state)
    (setq ecc-auto-notify--last-time (float-time))))

;;;###autoload
(defun ecc-auto-notify-prompt (type)
  "Notify the user about a Claude prompt of TYPE.
This function handles the actual notification using the configured
methods (bell, mode line flash, etc.).

TYPE is a symbol representing the prompt type, such as :waiting, :y/n, etc."
  (let ((type-name (pcase type
                     (:initial-waiting "initial waiting for input")
                     (:waiting "waiting for input")
                     (:y/n "yes/no prompt")
                     (:y/y/n "multi-choice prompt")
                     (_ (format "%s" type)))))
    ;; Ring bell if enabled
    (when ecc-auto-notify-bell
      (ecc-auto-notify-ring-bell))
    
    ;; Flash mode line if enabled
    (when ecc-auto-notify-flash
      (ecc-auto-notify-flash-mode-line))
    
    ;; Display message
    (message "Claude prompt detected: %s" type-name)))

;;;###autoload
(defun ecc-auto-notify-ring-bell ()
  "Ring the terminal bell using configured method.
Handles different system configurations to ensure bell is audible.
Uses the method specified by `ecc-auto-notify-bell-method'."
  (pcase ecc-auto-notify-bell-method
    ('audible
     ;; Try multiple bell methods to ensure at least one works
     (when (fboundp 'play-sound-file)
       (ignore-errors
         (let ((bell-sound (expand-file-name "~/.emacs.d/sounds/bell.wav")))
           (when (file-exists-p bell-sound)
             (play-sound-file bell-sound)))))
     
     ;; Fall back to standard bell
     (let ((ring-bell-function nil)) ; Temporarily disable any custom bell function
       (ding t)))
    
    ('visible
     ;; Use visible bell
     (let ((visible-bell t))
       (ding)))
    
    ('both
     ;; Both audible and visible
     (let ((visible-bell nil))
       (ding t))
     (invert-face 'mode-line)
     (run-with-timer ecc-auto-notify-bell-duration nil
                     (lambda () (invert-face 'mode-line))))
    
    ('external
     ;; Use external command
     (when (and ecc-auto-notify-bell-external-command
                (not (string-empty-p ecc-auto-notify-bell-external-command)))
       (start-process "ecc-bell" nil shell-file-name shell-command-switch
                      ecc-auto-notify-bell-external-command)))
    
    (_ 
     ;; Default fallback
     (let ((ring-bell-function nil)
           (visible-bell nil))
       (ding t)))))

;;;###autoload
(defun ecc-auto-notify-flash-mode-line ()
  "Flash the mode line to get attention.
Temporarily inverts the mode line colors and then restores them
after a short delay to create a visual notification effect."
  (when ecc-auto-notify--flash-timer
    (cancel-timer ecc-auto-notify--flash-timer))
  
  (invert-face 'mode-line)
  (setq ecc-auto-notify--flash-timer
        (run-with-timer 0.5 nil
                      (lambda ()
                        (invert-face 'mode-line)))))

;;;###autoload
(defun ecc-auto-notify-toggle ()
  "Toggle notification for Claude prompts.
Enables or disables notifications globally."
  (interactive)
  (setq ecc-auto-notify-on-claude-prompt 
        (not ecc-auto-notify-on-claude-prompt))
  (message "Claude prompt notifications %s"
           (if ecc-auto-notify-on-claude-prompt "enabled" "disabled")))

;;;###autoload
(defun ecc-auto-notify-toggle-bell ()
  "Toggle bell notification for Claude prompts.
Enables or disables the audible bell component of notifications."
  (interactive)
  (setq ecc-auto-notify-bell 
        (not ecc-auto-notify-bell))
  (message "Bell notifications %s"
           (if ecc-auto-notify-bell "enabled" "disabled")))

;;;###autoload
(defun ecc-auto-notify-setup-for-buffer ()
  "Set up notifications for the current buffer.
This function is designed to be added to mode hooks for Claude-related
buffer modes to automatically set up prompt notifications."
  (when (derived-mode-p 'ecc-term-claude-mode 'vterm-mode)
    ;; Add state check hook for this buffer
    (add-hook 'ecc-term-claude-update-functions
              (lambda ()
                (ecc-auto-notify-check-state (ecc-detect-state)))
              nil t)))

;; Add hook to set up notifications
(add-hook 'ecc-term-claude-mode-hook #'ecc-auto-notify-setup-for-buffer)
(add-hook 'vterm-mode-hook
          (lambda ()
            (when (string-match-p "\\*CLAUDE.*\\*" (buffer-name))
              (ecc-auto-notify-setup-for-buffer))))

(provide 'ecc-auto-notify-improved)

;;; ecc-auto-notify-improved.el ends here