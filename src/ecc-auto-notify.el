;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 15:55:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-auto-notify.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:
;;; Notification functionality for Claude prompts.
;;;
;;; This module provides a comprehensive notification system for different
;;; Claude states, with support for various notification methods and
;;; throttling to prevent excessive notifications.
;;;
;;; Features:
;;; - Bell notifications (audible, visible, both, or external command)
;;; - Mode line flashing for visual notifications
;;; - Message display for different prompt types
;;; - Throttling to prevent excessive notifications
;;; - Compatibility with different state detection approaches
;;; - Buffer-specific notification support
;;; - Hooks into terminal modes
;;;
;;; Example usage:
;;; ```elisp
;;; ;; Enable notifications globally
;;; (setq ecc-auto-notify-on-claude-prompt t)
;;;
;;; ;; Toggle notifications
;;; (ecc-auto-notify-toggle)          ; Toggle all notifications
;;; (ecc-auto-notify-toggle-bell)     ; Toggle bell notifications
;;; (ecc-auto-notify-toggle-flash)    ; Toggle flash notifications
;;;
;;; ;; Initialize buffer-local notifications
;;; (ecc-auto-notify-buffer-local-init)
;;; (ecc-auto-notify-buffer-local-toggle)
;;; ```

;; Required dependencies
(require 'cl-lib)
(require 'ecc-variables)
(when (locate-library "ecc-state-detection")
  (require 'ecc-state-detection))
(when (locate-library "ecc-debug-utils")
  (require 'ecc-debug-utils))

;;; Code:

;;;; Customization Options

(defgroup ecc-auto-notify nil
  "Notification settings for Claude Auto mode."
  :group 'ecc
  :prefix "ecc-auto-notify-")

(defcustom ecc-auto-notify-on-claude-prompt t
  "Whether to show notifications for Claude prompts.
When enabled, the system will display notifications for Claude prompts
based on the configured notification methods."
  :type 'boolean
  :group 'ecc-auto-notify)

(defcustom ecc-auto-notify-bell t
  "Whether to ring the bell when a prompt is detected.
When enabled, an audible or visible bell (based on `ecc-auto-notify-bell-method`)
will be triggered when Claude prompts are detected."
  :type 'boolean
  :group 'ecc-auto-notify)

(defcustom ecc-auto-notify-flash t
  "Whether to flash the mode line when a prompt is detected.
When enabled, the mode line will briefly invert colors to provide
a visual notification when Claude prompts are detected."
  :type 'boolean
  :group 'ecc-auto-notify)

(defcustom ecc-auto-notify-prompt-types '(:initial-waiting :waiting :y/n :y/y/n)
  "List of prompt types to notify about.
Only the prompt types in this list will trigger notifications.
Valid prompt types are:
- :initial-waiting - Initial Claude prompt waiting for input
- :waiting - Claude waiting for more input during generation
- :y/n - Yes/No prompts from Claude
- :y/y/n - Multi-choice Yes/Yes+/No prompts from Claude"
  :type '(repeat symbol)
  :group 'ecc-auto-notify)

(defcustom ecc-auto-notify-interval 2.0
  "Minimum interval between notifications in seconds.
This prevents excessive notifications when the same prompt state
persists for a long time."
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
  "Duration in seconds for visible bell flash.
This controls how long the screen or mode line is flashed
when using a visible bell notification."
  :type 'number
  :group 'ecc-auto-notify)

(defcustom ecc-auto-notify-bell-volume 100
  "Volume for the bell (1-100). May not work on all systems.
Some systems may ignore this setting depending on their audio configuration."
  :type 'integer
  :group 'ecc-auto-notify)

(defcustom ecc-auto-notify-buffer-local-default nil
  "Whether to use buffer-local notification by default.
When non-nil, notifications will be managed on a per-buffer basis,
with each buffer having its own notification settings."
  :type 'boolean
  :group 'ecc-auto-notify)

(defcustom ecc-auto-notify-default-enabled t
  "Whether buffer-local notifications are enabled by default for new buffers.
This setting only applies when using buffer-local mode. When true,
new buffers will have notifications enabled by default."
  :type 'boolean
  :group 'ecc-auto-notify)

;;;; Internal Variables

(defvar ecc-auto-notify--last-time 0
  "Time of the last notification.")

(defvar ecc-auto-notify--last-state nil
  "Last Claude state that triggered a notification.")

(defvar ecc-auto-notify--flash-timer nil
  "Timer for mode line flashing.")

(defvar ecc-auto-notify-hooks nil
  "Hooks run after notification is displayed.
These hooks are called with a single argument, the state symbol
that triggered the notification.")

;;;; Core Notification Functions

;;;###autoload
(defun ecc-auto-notify-check-state (state)
  "Check if STATE requires notification and notify if needed.
This function checks the current state against configured notification
settings and triggers notifications when appropriate. It includes
throttling to prevent excessive notifications.

Arguments:
  STATE: A symbol representing the detected Claude state, such as
         :waiting, :y/n, :y/y/n, or :initial-waiting.

Returns:
  t if a notification was sent, nil otherwise."
  ;; Skip if notifications are disabled
  (unless (and (boundp 'ecc-auto-notify-on-claude-prompt)
               ecc-auto-notify-on-claude-prompt)
    (when (fboundp 'ecc-debug-message)
      (ecc-debug-message "Notifications disabled, not checking state"))
    (cl-return-from ecc-auto-notify-check-state nil))

  ;; Skip if no state is detected
  (unless state
    (when (fboundp 'ecc-debug-message)
      (ecc-debug-message "No state detected, not notifying"))
    (cl-return-from ecc-auto-notify-check-state nil))
  
  ;; Skip if state type is not in our notification list
  (unless (memq state ecc-auto-notify-prompt-types)
    (when (fboundp 'ecc-debug-message)
      (ecc-debug-message "State %s not in notification list, not notifying" state))
    (cl-return-from ecc-auto-notify-check-state nil))
  
  ;; Check if we should throttle notifications
  (unless (or
           ;; Always notify for new state
           (not (eq state ecc-auto-notify--last-state))
           ;; For same state, only notify after interval
           (> (- (float-time) ecc-auto-notify--last-time) 
              ecc-auto-notify-interval))
    (when (fboundp 'ecc-debug-message)
      (ecc-debug-message "Throttling notification for state %s" state))
    (cl-return-from ecc-auto-notify-check-state nil))
  
  ;; Proceed with notification
  (ecc-auto-notify-prompt state)
  
  ;; Update tracking variables
  (setq ecc-auto-notify--last-state state)
  (setq ecc-auto-notify--last-time (float-time))
  
  ;; Run hooks
  (run-hook-with-args 'ecc-auto-notify-hooks state)
  
  ;; Return t to indicate notification was sent
  t)

;;;###autoload
(defun ecc-auto-notify-prompt (type)
  "Notify the user about a Claude prompt of TYPE.
This function handles the actual notification using the configured
methods (bell, mode line flash, etc.).

Arguments:
  TYPE: A symbol representing the prompt type, such as
        :waiting, :y/n, etc."
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
        (run-with-timer ecc-auto-notify-bell-duration nil
                      (lambda ()
                        (invert-face 'mode-line)))))

;;;; User Commands

;;;###autoload
(defun ecc-auto-notify-toggle ()
  "Toggle notification for Claude prompts.
Enables or disables notifications globally."
  (interactive)
  (if ecc-auto-notify-buffer-local-default
      ;; Toggle buffer-local setting
      (ecc-auto-notify-buffer-local-toggle)
    
    ;; Toggle global setting
    (setq ecc-auto-notify-on-claude-prompt 
          (not ecc-auto-notify-on-claude-prompt))
    (message "Claude prompt notifications %s"
             (if ecc-auto-notify-on-claude-prompt "enabled" "disabled"))))

;;;###autoload
(defun ecc-auto-notify-toggle-bell ()
  "Toggle bell notification for Claude prompts.
Enables or disables the audible bell component of notifications."
  (interactive)
  (if ecc-auto-notify-buffer-local-default
      ;; Toggle buffer-local setting
      (ecc-auto-notify-buffer-local-toggle-bell)
    
    ;; Toggle global setting
    (setq ecc-auto-notify-bell 
          (not ecc-auto-notify-bell))
    (message "Bell notifications %s"
             (if ecc-auto-notify-bell "enabled" "disabled"))))

;;;###autoload
(defun ecc-auto-notify-toggle-flash ()
  "Toggle mode line flash notification for Claude prompts.
Enables or disables the visual mode line flash component of notifications."
  (interactive)
  (if ecc-auto-notify-buffer-local-default
      ;; Toggle buffer-local setting
      (ecc-auto-notify-buffer-local-toggle-flash)
    
    ;; Toggle global setting
    (setq ecc-auto-notify-flash 
          (not ecc-auto-notify-flash))
    (message "Mode line flash notifications %s"
             (if ecc-auto-notify-flash "enabled" "disabled"))))

;;;; Buffer Setup & Integration

;;;###autoload
(defun ecc-auto-notify-setup-for-buffer ()
  "Set up notifications for the current buffer.
This function is designed to be added to mode hooks for Claude-related
buffer modes to automatically set up prompt notifications.

If buffer-local mode is enabled, this initializes buffer-local settings."
  (when (derived-mode-p 'ecc-term-claude-mode 'vterm-mode)
    (if ecc-auto-notify-buffer-local-default
        ;; Set up buffer-local notifications
        (ecc-auto-notify-buffer-local-init)
      
      ;; Set up global notification for this buffer
      (add-hook 'ecc-term-claude-update-functions
                (lambda ()
                  (let ((state (if (fboundp 'ecc-detect-state)
                                   (ecc-detect-state)
                                 ;; Fallback to older detection if needed
                                 (ecc-detect-simple-state))))
                    (ecc-auto-notify-check-state state)))
                nil t))))

;;;###autoload
(defun ecc-auto-notify-setup-hooks ()
  "Set up hooks for buffer-based notification.
This installs hooks to automatically set up notification in relevant buffers."
  (add-hook 'ecc-term-claude-mode-hook #'ecc-auto-notify-setup-for-buffer)
  
  ;; For vterm buffers, check if they're Claude buffers first
  (add-hook 'vterm-mode-hook
            (lambda ()
              (when (string-match-p "\\*CLAUDE.*\\*" (buffer-name))
                (ecc-auto-notify-setup-for-buffer)))))

;;;; Buffer Content Change Handler

;;;###autoload
(defun ecc-auto-notify-buffer-change-handler ()
  "Handler for buffer content changes that checks for Claude prompts.
This function works with the unified state detection system and can handle
both global and buffer-local configurations."
  (if ecc-auto-notify-buffer-local-default
      ;; Handle buffer-local notifications
      (when (boundp 'ecc-buffer-auto-notify-enabled)
        (when ecc-buffer-auto-notify-enabled
          (let ((buffer (current-buffer)))
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                ;; Use the unified detection function
                (when-let ((state (if (fboundp 'ecc-detect-state)
                                     (ecc-detect-state)
                                   (ecc-detect-simple-state))))
                  (ecc-auto-notify-buffer-local-check-state state)))))))
    
    ;; Handle global notifications
    (when ecc-auto-notify-on-claude-prompt
      (let ((buffer (current-buffer)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            ;; Use the unified detection function
            (when-let ((state (if (fboundp 'ecc-detect-state)
                                 (ecc-detect-state)
                               (ecc-detect-simple-state))))
              (ecc-auto-notify-check-state state))))))))

;;;; Buffer-Local Notification Support

(defvar-local ecc-buffer-auto-notify-enabled nil
  "Whether notifications are enabled for this buffer.")

(defvar-local ecc-buffer-auto-notify-bell t
  "Whether to ring the bell for notifications in this buffer.")

(defvar-local ecc-buffer-auto-notify-flash t
  "Whether to flash mode line for notifications in this buffer.")

;;;###autoload
(defun ecc-auto-notify-buffer-local-init (&optional buffer)
  "Initialize buffer-local notification settings for BUFFER.
If BUFFER is nil, use the current buffer.

This sets up the buffer with default notification settings that can
then be customized independently from global settings.

Arguments:
  BUFFER: The buffer to initialize. Defaults to the current buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (setq-local ecc-buffer-auto-notify-enabled ecc-auto-notify-default-enabled)
    (setq-local ecc-buffer-auto-notify-bell ecc-auto-notify-bell)
    (setq-local ecc-buffer-auto-notify-flash ecc-auto-notify-flash)
    
    (when (called-interactively-p 'any)
      (message "Buffer-local notifications initialized for %s" (buffer-name)))))

;;;###autoload
(defun ecc-auto-notify-buffer-local-toggle (&optional buffer)
  "Toggle buffer-local notifications for BUFFER.
If BUFFER is nil, use the current buffer.

Arguments:
  BUFFER: The buffer to toggle notifications for. Defaults to the current buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (unless (boundp 'ecc-buffer-auto-notify-enabled)
      (ecc-auto-notify-buffer-local-init))
    
    (setq-local ecc-buffer-auto-notify-enabled 
                (not ecc-buffer-auto-notify-enabled))
    (message "Buffer-local notifications %s for %s"
             (if ecc-buffer-auto-notify-enabled "enabled" "disabled")
             (buffer-name))))

;;;###autoload
(defun ecc-auto-notify-buffer-local-toggle-bell (&optional buffer)
  "Toggle buffer-local bell notifications for BUFFER.
If BUFFER is nil, use the current buffer.

Arguments:
  BUFFER: The buffer to toggle bell for. Defaults to the current buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (unless (boundp 'ecc-buffer-auto-notify-enabled)
      (ecc-auto-notify-buffer-local-init))
    
    (setq-local ecc-buffer-auto-notify-bell 
                (not ecc-buffer-auto-notify-bell))
    (message "Buffer-local bell notifications %s for %s"
             (if ecc-buffer-auto-notify-bell "enabled" "disabled")
             (buffer-name))))

;;;###autoload
(defun ecc-auto-notify-buffer-local-toggle-flash (&optional buffer)
  "Toggle buffer-local mode line flash for BUFFER.
If BUFFER is nil, use the current buffer.

Arguments:
  BUFFER: The buffer to toggle flash for. Defaults to the current buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (unless (boundp 'ecc-buffer-auto-notify-enabled)
      (ecc-auto-notify-buffer-local-init))
    
    (setq-local ecc-buffer-auto-notify-flash 
                (not ecc-buffer-auto-notify-flash))
    (message "Buffer-local flash notifications %s for %s"
             (if ecc-buffer-auto-notify-flash "enabled" "disabled")
             (buffer-name))))

;;;###autoload
(defun ecc-auto-notify-buffer-local-check-state (state)
  "Check if STATE requires notification using buffer-local settings.
This is the buffer-local equivalent of `ecc-auto-notify-check-state`.

Arguments:
  STATE: The state to check for notification.

Returns:
  t if a notification was sent, nil otherwise."
  ;; Skip if notifications are disabled for this buffer
  (unless ecc-buffer-auto-notify-enabled
    (when (fboundp 'ecc-debug-message)
      (ecc-debug-message "Buffer-local notifications disabled for %s" (buffer-name)))
    (cl-return-from ecc-auto-notify-buffer-local-check-state nil))
  
  ;; Skip if no state is detected
  (unless state
    (when (fboundp 'ecc-debug-message)
      (ecc-debug-message "No state detected for %s" (buffer-name)))
    (cl-return-from ecc-auto-notify-buffer-local-check-state nil))
  
  ;; Skip if state type is not in our notification list
  (unless (memq state ecc-auto-notify-prompt-types)
    (when (fboundp 'ecc-debug-message)
      (ecc-debug-message "State %s not in notification list for %s" 
                          state (buffer-name)))
    (cl-return-from ecc-auto-notify-buffer-local-check-state nil))
  
  ;; Check for throttling (using buffer-local variables)
  (defvar-local ecc-buffer-auto-notify--last-time 0)
  (defvar-local ecc-buffer-auto-notify--last-state nil)
  
  (unless (or 
           ;; Always notify for new state
           (not (eq state ecc-buffer-auto-notify--last-state))
           ;; For same state, only notify after interval
           (> (- (float-time) ecc-buffer-auto-notify--last-time) 
              ecc-auto-notify-interval))
    (when (fboundp 'ecc-debug-message)
      (ecc-debug-message "Throttling notification for state %s in %s" 
                          state (buffer-name)))
    (cl-return-from ecc-auto-notify-buffer-local-check-state nil))
  
  ;; Proceed with notification
  (ecc-auto-notify-buffer-local-prompt state)
  
  ;; Update buffer-local tracking variables
  (setq-local ecc-buffer-auto-notify--last-state state)
  (setq-local ecc-buffer-auto-notify--last-time (float-time))
  
  ;; Return t to indicate notification was sent
  t)

;;;###autoload
(defun ecc-auto-notify-buffer-local-prompt (type)
  "Notify about Claude prompt of TYPE using buffer-local settings.

Arguments:
  TYPE: A symbol representing the prompt type."
  (let ((type-name (pcase type
                     (:initial-waiting "initial waiting for input")
                     (:waiting "waiting for input")
                     (:y/n "yes/no prompt")
                     (:y/y/n "multi-choice prompt")
                     (_ (format "%s" type)))))
    ;; Ring bell if enabled for this buffer
    (when ecc-buffer-auto-notify-bell
      (ecc-auto-notify-ring-bell))
    
    ;; Flash mode line if enabled for this buffer
    (when ecc-buffer-auto-notify-flash
      (ecc-auto-notify-flash-mode-line))
    
    ;; Display buffer-specific message
    (message "Claude prompt in %s: %s" (buffer-name) type-name)))

;;;; Unified Checking Functions

;;;###autoload
(defun ecc-auto-notify-check-unified (buffer)
  "Check BUFFER for Claude prompts and notify if appropriate.
Uses either global or buffer-local configuration based on settings.

Arguments:
  BUFFER: The buffer to check for Claude prompts."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((state (if (fboundp 'ecc-detect-state)
                        (ecc-detect-state)
                      (ecc-detect-simple-state))))
        (if (and (boundp 'ecc-buffer-auto-notify-enabled)
                 ecc-auto-notify-buffer-local-default)
            ;; Use buffer-local settings
            (when ecc-buffer-auto-notify-enabled
              (ecc-auto-notify-buffer-local-check-state state))
          ;; Use global settings
          (ecc-auto-notify-check-state state))))))

;;;; Debug Utilities

(defun ecc-auto-notify--debug-message (format-string &rest args)
  "Output debug message with FORMAT-STRING and ARGS if debugging is enabled."
  (when (and (featurep 'ecc-debug-utils)
             (boundp 'ecc-debug-enabled)
             ecc-debug-enabled)
    (apply #'ecc-debug-message format-string args)))

;;;; Initialize Hook Setup

;; Set up hooks automatically when this module is loaded
(ecc-auto-notify-setup-hooks)

;;;; Backward Compatibility

;; Aliases for backward compatibility
(defalias 'ecc-notify-toggle 'ecc-auto-notify-toggle
  "Backward compatibility alias for `ecc-auto-notify-toggle'.")
(defalias 'ecc-notify-bell 'ecc-auto-notify-ring-bell
  "Backward compatibility alias for `ecc-auto-notify-ring-bell'.")
(defalias 'ecc-notify-flash 'ecc-auto-notify-flash-mode-line
  "Backward compatibility alias for `ecc-auto-notify-flash-mode-line'.")

;; Multiple provides for backward compatibility
(provide 'ecc-auto-notify)
(provide 'ecc-auto-notify-improved)
(provide 'ecc-auto-notify-fix)
(provide 'ecc-auto-notify-consolidated)

;;; ecc-auto-notify.el ends here