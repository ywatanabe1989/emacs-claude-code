;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 17:50:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-auto-core.el

;;; Commentary:
;;; Core functionality for Claude auto-response system.
;;; This module provides the infrastructure for timer management,
;;; state tracking, throttling, and buffer management used by
;;; the auto-response and notification systems.

(require 'cl-lib)
(require 'ecc-variables)
(require 'ecc-state-detection)

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

(defvar ecc-auto-core--last-state nil
  "The last detected Claude prompt state.")

(defvar ecc-auto-core--last-response-time 0
  "Timestamp of the last auto-response, used for throttling.")

(defvar ecc-auto-core--registered-buffers nil
  "List of buffers registered for auto-response.")

(defvar ecc-auto-core--initial-check-count 0
  "Counter for initial checking attempts.")

;; Timer management functions

(defun ecc-auto-core-timer-active-p ()
  "Return non-nil if the auto-response timer is active."
  (and ecc-auto-core--timer
       (timerp ecc-auto-core--timer)))

(defun ecc-auto-core-timer-start (callback)
  "Start the auto-response timer with CALLBACK function.
Cancels any existing timer first."
  (ecc-auto-core-timer-stop)
  (setq ecc-auto-core--timer
        (run-with-timer ecc-auto-core-initial-wait-time
                        ecc-auto-core-interval
                        callback)))

(defun ecc-auto-core-timer-stop ()
  "Stop the auto-response timer if it exists."
  (when (timerp ecc-auto-core--timer)
    (cancel-timer ecc-auto-core--timer)
    (setq ecc-auto-core--timer nil)))

;; State and throttling management

(defun ecc-auto-core-update-state (state)
  "Update the tracked state to STATE and record timestamp."
  (setq ecc-auto-core--last-state state
        ecc-auto-core--last-response-time (float-time)))

(defun ecc-auto-core-throttled-p (state)
  "Return non-nil if auto-response for STATE should be throttled.
Prevents responses that are too frequent for the same state."
  (and ecc-auto-core--last-state
       (eq ecc-auto-core--last-state state)
       (< (- (float-time) ecc-auto-core--last-response-time)
          ecc-auto-core-throttle-time)))

(defun ecc-auto-core-reset-state ()
  "Reset the auto-response state tracking."
  (setq ecc-auto-core--last-state nil
        ecc-auto-core--last-response-time 0
        ecc-auto-core--initial-check-count 0))

;; Buffer management

(defun ecc-auto-core-register-buffer (buffer)
  "Register BUFFER for auto-response.
Returns the buffer if registered, nil otherwise."
  (when (buffer-live-p buffer)
    (add-to-list 'ecc-auto-core--registered-buffers buffer)
    buffer))

(defun ecc-auto-core-unregister-buffer (buffer)
  "Remove BUFFER from auto-response registry."
  (setq ecc-auto-core--registered-buffers
        (delq buffer ecc-auto-core--registered-buffers)))

(defun ecc-auto-core-registered-buffers ()
  "Return list of currently registered buffers that are still live."
  (setq ecc-auto-core--registered-buffers
        (seq-filter #'buffer-live-p ecc-auto-core--registered-buffers)))

(defun ecc-auto-core-cleanup-buffers ()
  "Clean up the buffer registry by removing dead buffers."
  (ecc-auto-core-registered-buffers))

;; Core processing

(defun ecc-auto-core-process-buffer (buffer callback)
  "Process BUFFER for auto-response, calling CALLBACK with state if needed.
The callback should take two arguments: the buffer and the detected state."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((state (ecc-detect-state)))
        (when (and state (not (ecc-auto-core-throttled-p state)))
          (when ecc-auto-core-debug
            (message "[Auto Core] Detected state %s in %s"
                     (ecc-state-get-name state)
                     (buffer-name buffer)))
          (funcall callback buffer state)
          (ecc-auto-core-update-state state))))))

(defun ecc-auto-core-initial-check (buffer callback)
  "Perform initial check for BUFFER, calling CALLBACK if state detected.
This function specifically targets the initial-waiting state with more
aggressive checking to catch Claude's initial prompt."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((state (ecc-detect-state)))
        (when (and state 
                   (eq state :initial-waiting)
                   (not (ecc-auto-core-throttled-p state)))
          (when ecc-auto-core-debug
            (message "[Auto Core] Detected initial state in %s"
                     (buffer-name buffer)))
          (funcall callback buffer state)
          (ecc-auto-core-update-state state)))))
  
  ;; Increment counter and schedule next check if needed
  (setq ecc-auto-core--initial-check-count (1+ ecc-auto-core--initial-check-count))
  (when (< ecc-auto-core--initial-check-count 5)
    (run-with-timer 0.5 nil #'ecc-auto-core-initial-check buffer callback)))

(defun ecc-auto-core-process-all-buffers (callback)
  "Process all registered buffers for auto-response using CALLBACK."
  (dolist (buffer (ecc-auto-core-registered-buffers))
    (ecc-auto-core-process-buffer buffer callback)))

;; Initialization and shutdown

(defun ecc-auto-core-initialize ()
  "Initialize the auto-core system.
This resets state tracking and cleans up any existing resources."
  (ecc-auto-core-timer-stop)
  (ecc-auto-core-reset-state)
  (ecc-auto-core-cleanup-buffers))

(defun ecc-auto-core-shutdown ()
  "Shut down the auto-core system, cleaning up all resources."
  (ecc-auto-core-timer-stop)
  (ecc-auto-core-reset-state)
  (setq ecc-auto-core--registered-buffers nil))

;; Debugging

(defun ecc-auto-core-debug-status ()
  "Return a string with debug status information for auto-core."
  (format "Auto-Core Status:
  Timer Active: %s
  Last State: %s
  Last Response: %.2f seconds ago
  Registered Buffers: %d
  Initial Check Count: %d"
          (if (ecc-auto-core-timer-active-p) "Yes" "No")
          (if ecc-auto-core--last-state 
              (ecc-state-get-name ecc-auto-core--last-state)
            "None")
          (- (float-time) ecc-auto-core--last-response-time)
          (length (ecc-auto-core-registered-buffers))
          ecc-auto-core--initial-check-count))

(defun ecc-auto-core-toggle-debug ()
  "Toggle debug output for auto-core."
  (interactive)
  (setq ecc-auto-core-debug (not ecc-auto-core-debug))
  (message "Auto-core debug %s" (if ecc-auto-core-debug "enabled" "disabled"))
  (when ecc-auto-core-debug
    (message "%s" (ecc-auto-core-debug-status))))

;; Backward compatibility

;; Add compatibility functions for existing code
(defun ecc-auto--get-timer ()
  "Compatibility function for old code. Returns non-nil if timer is active."
  (ecc-auto-core-timer-active-p))

(defun ecc-auto--start-timer (callback)
  "Compatibility function for old code. Starts timer with CALLBACK."
  (ecc-auto-core-timer-start callback))

(defun ecc-auto--stop-timer ()
  "Compatibility function for old code. Stops the timer."
  (ecc-auto-core-timer-stop))

(provide 'ecc-auto-core)

;;; ecc-auto-core.el ends here