;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-28 06:36:19>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-notification.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;; 1. Dependencies
;; ----------------------------------------
(require 'ecc-debug)
(require 'ecc-state-detection)

;; 2. Configuration
;; ----------------------------------------

(defcustom --ecc-notification-enabled t
  "Whether notifications are enabled globally."
  :type 'boolean
  :group 'ecc)

(defcustom --ecc-notification-methods '(bell flash message)
  "List of notification methods to use."
  :type '(set (const :tag "Audible bell" bell)
              (const :tag "Mode line flash" flash)
              (const :tag "Echo area message" message))
  :group 'ecc)

(defcustom --ecc-notification-throttle-duration 2.0
  "Minimum interval between notifications in seconds."
  :type 'number
  :group 'ecc)

;; 3. Variables
;; ----------------------------------------

(defvar --ecc-notification--last-time 0
  "Time of the last notification in float seconds.")

(defvar --ecc-notification--last-state nil
  "Last Claude state that triggered a notification.")

(defvar --ecc-notification--flash-timer nil
  "Timer for mode line flashing.")

;; 4. Main Entry Points
;; ----------------------------------------

(defun --ecc-notification-notify (state &optional buffer)
  "Notify about STATE using configured methods."
  (--ecc-debug-message
   "Notification requested for state: %s in buffer: %s"
   state (if buffer (buffer-name buffer) "current"))
  (when (--ecc-notification--should-notify-p state)
    (--ecc-debug-message "Notification will be sent (not throttled)")
    (when (memq 'bell --ecc-notification-methods)
      (--ecc-debug-message "Ringing bell")
      (--ecc-notification--ring-bell))
    (when (memq 'flash --ecc-notification-methods)
      (--ecc-debug-message "Flashing mode line")
      (--ecc-notification--flash-mode-line buffer))
    (when (memq 'message --ecc-notification-methods)
      (--ecc-debug-message "Displaying message")
      (--ecc-notification--display-message state buffer))
    (--ecc-notification--update-state state))
  (unless (--ecc-notification--should-notify-p state)
    (--ecc-debug-message "Notification throttled for state: %s" state)))


;;;###autoload
(defun ecc-notification-toggle ()
  "Toggle notifications for Claude prompts."
  (interactive)
  (setq --ecc-notification-enabled (not --ecc-notification-enabled))
  (--ecc-debug-message "Notifications toggled: %s"
                       (if --ecc-notification-enabled "enabled"
                         "disabled"))
  (message "Claude notifications %s"
           (if --ecc-notification-enabled "enabled" "disabled")))

;; 5. Core Functions
;; ----------------------------------------

(defun --ecc-notification--should-notify-p (state)
  "Check if we should notify about STATE now."
  (let
      ((time-since-last (- (float-time) --ecc-notification--last-time))
       (state-changed (not (eq state --ecc-notification--last-state))))
    (--ecc-debug-message
     "Notification check: enabled=%s, state=%s, changed=%s, time-since-last=%.2f"
     --ecc-notification-enabled state state-changed
     time-since-last)
    (and --ecc-notification-enabled
         state
         (or state-changed
             (> time-since-last --ecc-notification-throttle-duration)))))

(defun --ecc-notification--update-state (state)
  "Update tracking variables for STATE."
  (setq --ecc-notification--last-state state)
  (setq --ecc-notification--last-time (float-time)))

(defun --ecc-notification--flash-mode-line (&optional buffer)
  "Flash the mode line in BUFFER."
  (let ((target-buffer (or buffer (current-buffer))))
    (--ecc-debug-message "Flashing mode line for buffer: %s"
                         (buffer-name target-buffer))
    (when --ecc-notification--flash-timer
      (cancel-timer --ecc-notification--flash-timer))
    (with-current-buffer target-buffer
      (invert-face 'mode-line)
      (force-mode-line-update))
    (setq --ecc-notification--flash-timer
          (run-with-timer 0.5 nil
                          (lambda ()
                            (when (buffer-live-p target-buffer)
                              (with-current-buffer target-buffer
                                (invert-face 'mode-line)
                                (force-mode-line-update))
                              (--ecc-debug-message
                               "Mode line flash completed")))))))

;; 6. Helper/Utility Functions
;; ----------------------------------------

(defun --ecc-notification--ring-bell ()
  "Ring an audible bell."
  (let ((ring-bell-function nil))
    (ding t)))

(defun --ecc-notification--display-message (state &optional buffer)
  "Display a message about STATE."
  (let ((state-name (--ecc-state-detection-get-name state)))
    (if buffer
        (message "[%s] Claude prompt detected: %s"
                 (buffer-name buffer) state-name)
      (message "Claude prompt detected: %s" state-name))))


(provide 'ecc-notification)

(when
    (not load-file-name)
  (message "ecc-notification.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))