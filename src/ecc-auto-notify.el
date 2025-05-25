;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 20:15:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-auto-notify-consolidated.el

;;; Commentary:
;;; Consolidated notification functionality for Claude prompts.
;;;
;;; This module combines functionality from:
;;; - ecc-auto-notify.el (original implementation)
;;; - ecc-auto-notify-improved.el (enhanced documentation)
;;; - ecc-auto-notify-fix.el (compatibility with new state detection)
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
;;; ;; Enable notifications with default settings
;;; (setq ecc-auto-notify-on-claude-prompt t)
;;;
;;; ;; Configure notification methods
;;; (setq ecc-auto-notify-bell t
;;;       ecc-auto-notify-flash t
;;;       ecc-auto-notify-bell-method 'audible)
;;;
;;; ;; For buffer-local notifications
;;; (ecc-auto-notify-buffer-local-init)
;;; (ecc-auto-notify-buffer-local-toggle)
;;;
;;; ;; Manually check state and notify
;;; (ecc-auto-notify-check-unified (current-buffer))
;;; ```

(require 'cl-lib)
(require 'ecc-variables)

;; Try to load optimized modules if available
(when (locate-library "ecc-state-detection")
  (require 'ecc-state-detection))

(when (locate-library "ecc-debug-utils-consolidated")
  (require 'ecc-debug-utils)
  (defalias 'ecc-auto-notify-debug 'ecc-debug-message))

;;;; Customization Options

(defgroup ecc-auto-notify nil
  "Notification settings for Claude Auto mode."
  :group 'emacs-claude-code
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
will be triggered whenever a Claude prompt is detected."
  :type 'boolean
  :group 'ecc-auto-notify)

(defcustom ecc-auto-notify-flash t
  "Whether to flash the mode line when a prompt is detected.
When enabled, the mode line will briefly invert its colors
whenever a Claude prompt is detected, providing a visual cue."
  :type 'boolean
  :group 'ecc-auto-notify)

(defcustom ecc-notification-dispatch-types '(:initial-waiting :waiting :y/n :y/y/n)
  "List of prompt types to notify about.
Notification will only be triggered for prompt types in this list.
Possible values are:
- `:initial-waiting`: Claude's initial prompt when first started
- `:waiting`: Claude's 'continue' prompt
- `:y/n`: Claude's yes/no question prompt
- `:y/y/n`: Claude's multi-choice question prompt"
  :type '(repeat symbol)
  :group 'ecc-auto-notify)

(defcustom ecc-auto-notify-interval 2.0
  "Minimum interval between notifications in seconds.
This prevents excessive notifications when the same prompt
is detected multiple times in rapid succession."
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
Controls how long the screen flashes when using visible bell."
  :type 'number
  :group 'ecc-auto-notify)

(defcustom ecc-auto-notify-bell-volume 100
  "Volume for the bell (1-100). May not work on all systems.
Some systems allow controlling the volume of the bell sound."
  :type 'integer
  :group 'ecc-auto-notify)

(defcustom ecc-auto-notify-buffer-local-default nil
  "Whether to use buffer-local notification by default.
When non-nil, notifications will be managed on a per-buffer basis."
  :type 'boolean
  :group 'ecc-auto-notify)

(defcustom ecc-auto-notify-debug nil
  "Whether to display debug messages for notification actions.
When enabled, information about state detection, throttling decisions,
and notification events will be shown."
  :type 'boolean
  :group 'ecc-auto-notify)

;;;; Internal Variables

(defvar ecc-auto-notify--last-time 0
  "Time of the last notification.")

(defvar ecc-auto-notify--last-state nil
  "Last Claude state that triggered a notification.")

(defvar ecc-auto-notify--flash-timer nil
  "Timer for mode line flashing.")

(defvar-local ecc-buffer-auto-notify-enabled nil
  "Whether notifications are enabled for this buffer.")

(defvar-local ecc-buffer-auto-notify-bell t
  "Whether to ring the bell for notifications in this buffer.")

(defvar-local ecc-buffer-auto-notify-flash t
  "Whether to flash mode line for notifications in this buffer.")

(defvar-local ecc-buffer-auto-notify-last-time 0
  "Time of the last notification for this buffer.")

(defvar-local ecc-buffer-auto-notify-last-state nil
  "Last Claude state that triggered a notification for this buffer.")

;;;; Debug Utilities

(defun ecc-auto-notify--log (format-string &rest args)
  "Log a debug message if debugging is enabled.
FORMAT-STRING and ARGS are passed to `format`."
  (when ecc-auto-notify-debug
    (if (fboundp 'ecc-auto-notify-debug)
        (apply #'ecc-auto-notify-debug format-string args)
      (apply #'message (concat "[Auto Notify] " format-string) args))))

;;;; Core Notification Functions

;;;###autoload
(defun ecc-notification-check-state (state)
  "Check if STATE requires notification and notify if needed.
This function checks the current state against configured notification
settings and triggers notifications when appropriate. It includes
throttling to prevent excessive notifications.

Arguments:
  STATE: A symbol representing the detected Claude state, such as
         :waiting, :y/n, :y/y/n, or :initial-waiting.

Returns:
  t if a notification was sent, nil otherwise."
  (cl-block ecc-notification-check-state
    ;; Skip if notifications are disabled
    (unless (and (boundp 'ecc-auto-notify-on-claude-prompt)
                 ecc-auto-notify-on-claude-prompt)
      (ecc-auto-notify--log "Notifications disabled, not checking state")
      (cl-return-from ecc-notification-check-state nil))

    ;; Skip if no state is detected
    (unless state
      (ecc-auto-notify--log "No state detected, not notifying")
      (cl-return-from ecc-notification-check-state nil))
    
    ;; Skip if state type is not in our notification list
    (unless (memq state ecc-notification-dispatch-types)
      (ecc-auto-notify--log "State %s not in notification list, not notifying" state)
      (cl-return-from ecc-notification-check-state nil))
    
    ;; Check if we should throttle notifications
    (unless (or
             ;; Always notify for new state
             (not (eq state ecc-auto-notify--last-state))
             ;; For same state, only notify after interval
             (> (- (float-time) ecc-auto-notify--last-time) 
                ecc-auto-notify-interval))
      (ecc-auto-notify--log "Throttling notification for state %s" state)
      (cl-return-from ecc-notification-check-state nil))
    
    ;; Proceed with notification
    (ecc-notification-dispatch state)
    
    ;; Update tracking variables
    (setq ecc-auto-notify--last-state state)
    (setq ecc-auto-notify--last-time (float-time))
    
    ;; Return t to indicate notification was sent
    t))

;;;###autoload
(defun ecc-notification-dispatch (type &optional buffer)
  "Notify the user about a Claude prompt of TYPE.
This function handles the actual notification using the configured
methods (bell, mode line flash, etc.).

Arguments:
  TYPE: A symbol representing the prompt type, such as :waiting, :y/n, etc.
  BUFFER: Optional buffer where the prompt was detected. If provided,
          the buffer name will be included in the notification message."
  (let ((type-name (pcase type
                     (:initial-waiting "initial waiting for input")
                     (:waiting "waiting for input")
                     (:y/n "yes/no prompt")
                     (:y/y/n "multi-choice prompt")
                     (_ (format "%s" type)))))
    ;; Ring bell if enabled
    (when ecc-auto-notify-bell
      (ecc-notification-ring-bell))
    
    ;; Flash mode line if enabled
    (when ecc-auto-notify-flash
      (ecc-notification-flash-mode-line))
    
    ;; Display message with optional buffer name
    (if buffer
        (ecc-debug-message "[%s] Claude prompt detected: %s" 
                 (buffer-name buffer) type-name)
      (ecc-debug-message "Claude prompt detected: %s" type-name))
    
    ;; Log notification
    (ecc-auto-notify--log "Notification sent for state %s" type)))

;;;###autoload
(defun ecc-notification-ring-bell ()
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
(defun ecc-notification-flash-mode-line ()
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
(defun ecc-notification-toggle ()
  "Toggle notification for Claude prompts.
Enables or disables notifications globally."
  (interactive)
  (setq ecc-auto-notify-on-claude-prompt 
        (not ecc-auto-notify-on-claude-prompt))
  (ecc-debug-message "Claude prompt notifications %s"
           (if ecc-auto-notify-on-claude-prompt "enabled" "disabled")))

;;;###autoload
(defun ecc-notification-toggle-bell ()
  "Toggle bell notification for Claude prompts.
Enables or disables the audible bell component of notifications."
  (interactive)
  (setq ecc-auto-notify-bell 
        (not ecc-auto-notify-bell))
  (ecc-debug-message "Bell notifications %s"
           (if ecc-auto-notify-bell "enabled" "disabled")))

;;;###autoload
(defun ecc-notification-toggle-flash ()
  "Toggle mode line flash notification for Claude prompts.
Enables or disables the visual mode line flash component of notifications."
  (interactive)
  (setq ecc-auto-notify-flash 
        (not ecc-auto-notify-flash))
  (ecc-debug-message "Mode line flash notifications %s"
           (if ecc-auto-notify-flash "enabled" "disabled")))

;;;###autoload
(defun ecc-notification-toggle-debug ()
  "Toggle debug output for notification events.
Enables or disables detailed logging of notification activities."
  (interactive)
  (setq ecc-auto-notify-debug
        (not ecc-auto-notify-debug))
  (ecc-debug-message "Notification debug messages %s"
           (if ecc-auto-notify-debug "enabled" "disabled")))

;;;; Buffer Setup & Integration

;;;###autoload
(defun ecc-notification-setup-for-buffer ()
  "Set up notifications for the current buffer.
This function is designed to be added to mode hooks for Claude-related
buffer modes to automatically set up prompt notifications."
  (when (derived-mode-p 'ecc-term-claude-mode 'vterm-mode)
    ;; Add state check hook for this buffer using the unified detection function
    (add-hook 'ecc-term-claude-update-functions
              (lambda ()
                (let ((state (if (fboundp 'ecc-detect-state)
                               ;; Use new detection system if available
                               (ecc-detect-state)
                              ;; Fall back to simple state detection
                              (when (boundp 'ecc-detect-state)
                                (ecc-detect-state)))))
                  (when state
                    (ecc-notification-check-state state))))
              nil t)))

;;;###autoload
(defun ecc-auto-notify-setup-hooks ()
  "Set up hooks for buffer-based notification.
This installs hooks to automatically set up notification in relevant buffers."
  (add-hook 'ecc-term-claude-mode-hook #'ecc-notification-setup-for-buffer)
  
  ;; For vterm buffers, check if they're Claude buffers first
  (add-hook 'vterm-mode-hook
            (lambda ()
              (when (string-match-p "\\*CLAUDE.*\\*" (buffer-name))
                (ecc-notification-setup-for-buffer)))))

;;;; Buffer Content Change Handler

;;;###autoload
(defun ecc-auto-notify-buffer-change-handler ()
  "Handler for buffer content changes that checks for Claude prompts.
This function works with the unified state detection system and can handle
both global and buffer-local configurations."
  (when ecc-auto-notify-on-claude-prompt
    (let ((buffer (current-buffer)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          ;; Use the unified detection function if available
          (let ((state (cond
                       ((fboundp 'ecc-detect-state)
                        (ecc-detect-state))
                       ((fboundp 'ecc-detect-prompt-in-last-lines)
                        (ecc-detect-prompt-in-last-lines))
                       ((fboundp 'ecc-detect-state)
                        (ecc-detect-state))
                       (t nil))))
            (when state
              (ecc-auto-notify-check-unified buffer))))))))

;;;; Buffer-Local Notification Support

;;;###autoload
(defun ecc-auto-notify-buffer-local-init (&optional buffer)
  "Initialize buffer-local notification settings for BUFFER.
If BUFFER is nil, use the current buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (setq-local ecc-buffer-auto-notify-enabled ecc-auto-notify-on-claude-prompt)
    (setq-local ecc-buffer-auto-notify-bell ecc-auto-notify-bell)
    (setq-local ecc-buffer-auto-notify-flash ecc-auto-notify-flash)
    (setq-local ecc-buffer-auto-notify-last-time 0)
    (setq-local ecc-buffer-auto-notify-last-state nil)
    
    (when (called-interactively-p 'any)
      (ecc-debug-message "Buffer-local notifications initialized for %s" (buffer-name)))))

;;;###autoload
(defun ecc-auto-notify-buffer-local-toggle (&optional buffer)
  "Toggle buffer-local notifications for BUFFER.
If BUFFER is nil, use the current buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (unless (local-variable-p 'ecc-buffer-auto-notify-enabled)
      (ecc-auto-notify-buffer-local-init))
    
    (setq-local ecc-buffer-auto-notify-enabled 
                (not ecc-buffer-auto-notify-enabled))
    (ecc-debug-message "Buffer-local notifications %s for %s"
             (if ecc-buffer-auto-notify-enabled "enabled" "disabled")
             (buffer-name))))

;;;###autoload
(defun ecc-auto-notify-buffer-local-check-state (state)
  "Check if STATE requires notification using buffer-local settings.
This is the buffer-local equivalent of `ecc-notification-check-state`.

Arguments:
  STATE: The state to check for notification.

Returns:
  t if a notification was sent, nil otherwise."
  (cl-block ecc-auto-notify-buffer-local-check-state
    ;; Skip if notifications are disabled for this buffer
    (unless ecc-buffer-auto-notify-enabled
      (ecc-auto-notify--log "Buffer-local notifications disabled for %s" (buffer-name))
      (cl-return-from ecc-auto-notify-buffer-local-check-state nil))
    
    ;; Skip if no state is detected
    (unless state
      (ecc-auto-notify--log "No state detected for %s" (buffer-name))
      (cl-return-from ecc-auto-notify-buffer-local-check-state nil))
    
    ;; Skip if state type is not in our notification list
    (unless (memq state ecc-notification-dispatch-types)
      (ecc-auto-notify--log "State %s not in notification list for %s"
                          state (buffer-name))
      (cl-return-from ecc-auto-notify-buffer-local-check-state nil))
    
    ;; Check if we should throttle notifications for this buffer
    (unless (or
             ;; Always notify for new state
             (not (eq state ecc-buffer-auto-notify-last-state))
             ;; For same state, only notify after interval
             (> (- (float-time) ecc-buffer-auto-notify-last-time) 
                ecc-auto-notify-interval))
      (ecc-auto-notify--log "Throttling buffer-local notification for state %s in %s" 
                          state (buffer-name))
      (cl-return-from ecc-auto-notify-buffer-local-check-state nil))
    
    ;; Proceed with notification
    (ecc-notification-dispatch-buffer-local state)
    
    ;; Update tracking variables
    (setq-local ecc-buffer-auto-notify-last-state state)
    (setq-local ecc-buffer-auto-notify-last-time (float-time))
    
    ;; Return t to indicate notification was sent
    t))

;;;###autoload
(defun ecc-notification-dispatch-buffer-local (type)
  "Notify about Claude prompt of TYPE using buffer-local settings.
Handles notification using the configured methods specific to this buffer.

Arguments:
  TYPE: A symbol representing the prompt type, such as :waiting, :y/n, etc."
  (let ((type-name (pcase type
                     (:initial-waiting "initial waiting for input")
                     (:waiting "waiting for input")
                     (:y/n "yes/no prompt")
                     (:y/y/n "multi-choice prompt")
                     (_ (format "%s" type)))))
    ;; Ring bell if enabled for this buffer
    (when ecc-buffer-auto-notify-bell
      (ecc-notification-ring-bell))
    
    ;; Flash mode line if enabled for this buffer
    (when ecc-buffer-auto-notify-flash
      (ecc-notification-flash-mode-line))
    
    ;; Display buffer-specific message
    (ecc-debug-message "Claude prompt in %s: %s" (buffer-name) type-name)
    
    ;; Log notification
    (ecc-auto-notify--log "Buffer-local notification sent for state %s in %s" 
                        type (buffer-name))))

;;;; Unified Checking Functions

;;;###autoload
(defun ecc-auto-notify-check-unified (buffer)
  "Check BUFFER for Claude prompts and notify if appropriate.
Uses either global or buffer-local configuration based on settings.

Arguments:
  BUFFER: The buffer to check for Claude prompts."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when-let ((state (if (fboundp 'ecc-detect-state)
                          (ecc-detect-state)
                         (when (fboundp 'ecc-detect-state)
                           (ecc-detect-state)))))
        (if (and (local-variable-p 'ecc-buffer-auto-notify-enabled)
                 ecc-auto-notify-buffer-local-default)
            ;; Use buffer-local settings
            (ecc-auto-notify-buffer-local-check-state state)
          ;; Use global settings
          (ecc-notification-check-state state))))))

;;;; Initialize Hook Setup

;; Set up hooks automatically when this module is loaded
(ecc-auto-notify-setup-hooks)

;;;; Backward Compatibility

(defalias 'ecc-notification-check-state-improved 'ecc-notification-check-state
  "Compatibility function for older code.")

(defalias 'ecc-notification-setup-for-buffer-improved 'ecc-notification-setup-for-buffer
  "Compatibility function for older code.")

;; Provide all names for backward compatibility
(provide 'ecc-auto-notify)
(provide 'ecc-auto-notify-improved)
(provide 'ecc-auto-notify-fix)
(provide 'ecc-auto-notify-consolidated)

;;; ecc-auto-notify-consolidated.el ends here