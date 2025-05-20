;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 23:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-auto-core.el

;;; Commentary:
;;; Core infrastructure for Claude auto-response system.
;;; This module provides the fundamental infrastructure for timer management,
;;; buffer registration, and periodic checking without including any
;;; specific detection or response logic.

(require 'cl-lib)
(require 'ecc-variables)

;;; Code:

;; Customization options
(defgroup ecc-auto-core nil
  "Core settings for Claude auto-response functionality."
  :group 'ecc
  :prefix "ecc-auto-core-")

(defcustom ecc-auto-core-interval 1.0
  "Interval in seconds for checking Claude state and auto-responding."
  :type 'number
  :group 'ecc-auto-core)

(defcustom ecc-auto-core-throttle-time 2.0
  "Minimum time in seconds between auto-responses to prevent rapid firing."
  :type 'number
  :group 'ecc-auto-core)

(defcustom ecc-auto-core-initial-wait-time 1.0
  "Time to wait for the initial check after starting auto-response."
  :type 'number
  :group 'ecc-auto-core)

(defcustom ecc-auto-core-debug nil
  "Whether to show debug messages for auto-response activity."
  :type 'boolean
  :group 'ecc-auto-core)

;; Internal variables
(defvar ecc-auto-core--timer nil
  "Timer object for auto-response checks.")

(defvar ecc-auto-core--last-response-time 0
  "Timestamp of the last auto-response, used for throttling.")

(defvar ecc-auto-core--registered-buffers nil
  "List of buffers registered for auto-response.")

;; Debug messaging
(defun ecc-auto-core-debug (format-string &rest args)
  "Output a debug message if auto-core debugging is enabled.
Uses FORMAT-STRING and ARGS like `message'."
  (when ecc-auto-core-debug
    (apply #'message (concat "[Auto Core] " format-string) args)))

;; Timer management functions

;;;###autoload
(defun ecc-auto-core-timer-active-p ()
  "Return non-nil if the auto-response timer is active."
  (and ecc-auto-core--timer
       (timerp ecc-auto-core--timer)))

;;;###autoload
(defun ecc-auto-core-timer-start (callback)
  "Start the auto-response timer with CALLBACK function.
Cancels any existing timer first."
  (ecc-auto-core-timer-stop)
  (setq ecc-auto-core--timer
        (run-with-timer ecc-auto-core-initial-wait-time
                        ecc-auto-core-interval
                        callback))
  (ecc-auto-core-debug "Timer started with %s second interval" 
                       ecc-auto-core-interval))

;;;###autoload
(defun ecc-auto-core-timer-stop ()
  "Stop the auto-response timer if it exists."
  (when (timerp ecc-auto-core--timer)
    (cancel-timer ecc-auto-core--timer)
    (setq ecc-auto-core--timer nil)
    (ecc-auto-core-debug "Timer stopped")))

;; Throttling management

;;;###autoload
(defun ecc-auto-core-throttled-p ()
  "Return non-nil if auto-response should be throttled.
Prevents responses that are too frequent."
  (< (- (float-time) ecc-auto-core--last-response-time)
     ecc-auto-core-throttle-time))

;;;###autoload
(defun ecc-auto-core-update-time ()
  "Update the last response time to now."
  (setq ecc-auto-core--last-response-time (float-time)))

;;;###autoload
(defun ecc-auto-core-reset-state ()
  "Reset the auto-response state tracking."
  (setq ecc-auto-core--last-response-time 0))

;; Buffer management

;;;###autoload
(defun ecc-auto-core-register-buffer (buffer)
  "Register BUFFER for auto-response.
Returns the buffer if registered, nil otherwise."
  (when (buffer-live-p buffer)
    (ecc-auto-core-debug "Registered buffer %s" (buffer-name buffer))
    (add-to-list 'ecc-auto-core--registered-buffers buffer)
    buffer))

;;;###autoload
(defun ecc-auto-core-unregister-buffer (buffer)
  "Remove BUFFER from auto-response registry."
  (when (memq buffer ecc-auto-core--registered-buffers)
    (ecc-auto-core-debug "Unregistered buffer %s" (buffer-name buffer))
    (setq ecc-auto-core--registered-buffers
          (delq buffer ecc-auto-core--registered-buffers))))

;;;###autoload
(defun ecc-auto-core-registered-buffers ()
  "Return list of currently registered buffers that are still live."
  (setq ecc-auto-core--registered-buffers
        (seq-filter #'buffer-live-p ecc-auto-core--registered-buffers)))

;;;###autoload
(defun ecc-auto-core-cleanup-buffers ()
  "Clean up the buffer registry by removing dead buffers."
  (ecc-auto-core-registered-buffers))

;; Core processing

;;;###autoload
(defun ecc-auto-core-process-buffer (buffer callback)
  "Process BUFFER for auto-response, calling CALLBACK with buffer.
The callback should take the buffer as the only argument."
  (when (buffer-live-p buffer)
    (ecc-auto-core-debug "Processing buffer %s" (buffer-name buffer))
    (funcall callback buffer)))

;;;###autoload
(defun ecc-auto-core-process-all-buffers (callback)
  "Process all registered buffers using CALLBACK."
  (dolist (buffer (ecc-auto-core-registered-buffers))
    (ecc-auto-core-process-buffer buffer callback)))

;;;###autoload
(defun ecc-auto-core-initialize ()
  "Initialize the auto-core system.
Resets state tracking and cleans up any existing resources."
  (ecc-auto-core-timer-stop)
  (ecc-auto-core-reset-state)
  (ecc-auto-core-cleanup-buffers)
  (ecc-auto-core-debug "Core system initialized"))

;;;###autoload
(defun ecc-auto-core-shutdown ()
  "Shut down the auto-core system, cleaning up all resources."
  (ecc-auto-core-timer-stop)
  (ecc-auto-core-reset-state)
  (setq ecc-auto-core--registered-buffers nil)
  (ecc-auto-core-debug "Core system shut down"))

;; Debugging

;;;###autoload
(defun ecc-auto-core-debug-status ()
  "Return a string with debug status information for auto-core."
  (format "Auto-Core Status:
  Timer Active: %s
  Last Response: %.2f seconds ago
  Registered Buffers: %d"
          (if (ecc-auto-core-timer-active-p) "Yes" "No")
          (- (float-time) ecc-auto-core--last-response-time)
          (length (ecc-auto-core-registered-buffers))))

;;;###autoload
(defun ecc-auto-core-toggle-debug ()
  "Toggle debug output for auto-core."
  (interactive)
  (setq ecc-auto-core-debug (not ecc-auto-core-debug))
  (message "Auto-core debug %s" (if ecc-auto-core-debug "enabled" "disabled"))
  (when ecc-auto-core-debug
    (message "%s" (ecc-auto-core-debug-status))))

(provide 'ecc-auto-core)

;;; ecc-auto-core.el ends here