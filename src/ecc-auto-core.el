;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 16:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-auto-core.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:
;;; Core infrastructure for Claude auto-response system.
;;; This module provides the foundation for timer management,
;;; state tracking, throttling, and buffer management used by
;;; the auto-response and notification systems.
;;;
;;; The module follows clean code principles:
;;; - Functions do one thing
;;; - Related code appears vertically dense
;;; - Clear organization by functionality
;;; - Descriptive naming
;;; - Minimal complexity
;;;
;;; Key functionality:
;;; - Timer management: Starting, stopping, and checking timers
;;; - State tracking: Recording and throttling based on Claude states
;;; - Buffer management: Registration and cleanup of buffers
;;; - Core processing: Detecting states and calling responses
;;; - Lifecycle management: Initialization and shutdown
;;;
;;; Example usage:
;;; ```elisp
;;; ;; Register a buffer
;;; (ecc-auto-core-register-buffer (current-buffer))
;;;
;;; ;; Start timer with callback
;;; (ecc-auto-core-timer-start
;;;  (lambda ()
;;;    (ecc-auto-core-process-all-buffers
;;;     (lambda (buffer state)
;;;       (ecc-debug-message "Detected state %s in %s"
;;;                state (buffer-name buffer))))))
;;; ```

(require 'cl-lib)
(require 'cl-lib)
(require 'ecc-variables)
(require 'ecc-state-detection)
(when (locate-library "ecc-debug-utils")
  (require 'ecc-debug-utils))

;;; Code:

;;;; Customization

(defgroup ecc-auto-core nil
  "Core settings for Claude auto-response functionality."
  :group 'emacs-claude-code
  :prefix "ecc-auto-core-")

(defcustom ecc-auto-core-interval 1.0
  "Interval in seconds for checking Claude state and auto-responding.
This controls how frequently the system checks for Claude prompts
that might require a response. Lower values provide faster response
but may use more CPU."
  :type 'number
  :group 'ecc-auto-core)

(defcustom ecc-auto-core-throttle-time 3.0
  "Minimum time in seconds between auto-responses to prevent rapid firing.
This prevents the system from responding too frequently to the same
prompt type, which could flood Claude with responses."
  :type 'number
  :group 'ecc-auto-core)

(defcustom ecc-auto-core-initial-wait-time 1.0
  "Time to wait for the initial check after starting auto-response.
This delay allows Claude to stabilize before the first check."
  :type 'number
  :group 'ecc-auto-core)

(defcustom ecc-auto-core-max-initial-checks 5
  "Maximum number of initial checks to perform after starting auto-response.
This is used to aggressively check for the initial prompt when the
auto-response system is first started. Higher values increase the
chance of catching an initial prompt but may use more resources."
  :type 'integer
  :group 'ecc-auto-core)

(defcustom ecc-auto-core-initial-check-interval 0.5
  "Interval in seconds between initial checks.
Initial checks are more frequent than regular checks to catch
the initial prompt more quickly."
  :type 'number
  :group 'ecc-auto-core)

(defcustom ecc-auto-core-debug nil
  "Whether to show debug messages for auto-response activity.
When enabled, information about state detection, buffer registration,
and other auto-response activities will be displayed in the echo area."
  :type 'boolean
  :group 'ecc-auto-core)

;;;; Internal variables

;; Global system state
(defvar ecc-auto-core--timer nil
  "Timer object for auto-response checks.")

(defvar ecc-auto-core--registered-buffers nil
  "List of buffers registered for auto-response.")

;; Buffer-local state tracking
(defvar-local ecc-auto-core--last-state nil
  "The last detected Claude prompt state in this buffer.")

(defvar-local ecc-auto-core--last-response-time 0
  "Timestamp of the last auto-response in this buffer, used for throttling.")

(defvar-local ecc-auto-core--initial-check-count 0
  "Counter for initial checking attempts in this buffer.")

;;;; Timer management

;;;###autoload
(defun ecc-auto-core-timer-active-p ()
  "Return non-nil if the auto-response timer is active.
This can be used to check if the auto-response system is currently running."
  (and ecc-auto-core--timer
       (timerp ecc-auto-core--timer)))

;;;###autoload
(defun ecc-auto-core-timer-start (callback)
  "Start the auto-response timer with CALLBACK function.
Cancels any existing timer first.

Arguments:
  CALLBACK: Function to call on each timer tick. Should take no arguments.
            Typically this will call `ecc-auto-core-process-all-buffers'.

Example:
  (ecc-auto-core-timer-start
   (lambda ()
     (ecc-auto-core-process-all-buffers
      (lambda (buffer state) (ecc-debug-message \"State: %s\" state)))))"
  (ecc-auto-core-timer-stop)
  (setq ecc-auto-core--timer
        (run-with-timer ecc-auto-core-initial-wait-time
                        ecc-auto-core-interval
                        callback)))

;;;###autoload
(defun ecc-auto-core-timer-stop ()
  "Stop the auto-response timer if it exists.
This cancels the timer and clears the timer variable,
ensuring no further callbacks are executed."
  (when (timerp ecc-auto-core--timer)
    (cancel-timer ecc-auto-core--timer)
    (setq ecc-auto-core--timer nil)))

;;;; State and throttling management

;;;###autoload
(defun ecc-auto-core-update-state (state)
  "Update the tracked state to STATE and record timestamp.
This function is called after a response is sent to update
the tracking state and prevent rapid repeat responses.

Arguments:
  STATE: The Claude prompt state symbol (e.g., `:y/n`, `:waiting`)."
  (setq ecc-auto-core--last-state state
        ecc-auto-core--last-response-time (float-time)))

;;;###autoload
(defun ecc-auto-core-throttled-p (state)
  "Return non-nil if auto-response for STATE should be throttled.
Prevents responses that are too frequent for the same state.

This function compares the current time with the last response time
for the same state, and returns t if the time difference is less than
`ecc-auto-core-throttle-time`.

Arguments:
  STATE: The Claude prompt state symbol to check for throttling.

Returns:
  Non-nil if responses to STATE should be throttled,
  nil otherwise."
  (and ecc-auto-core--last-state
       (eq ecc-auto-core--last-state state)
       (< (- (float-time) ecc-auto-core--last-response-time)
          ecc-auto-core-throttle-time)))

;;;###autoload
(defun ecc-auto-core-reset-state ()
  "Reset the auto-response state tracking.
Clears the last detected state, response timestamp,
and initial check counter."
  (setq ecc-auto-core--last-state nil
        ecc-auto-core--last-response-time 0
        ecc-auto-core--initial-check-count 0))

;;;; Buffer management

;;;###autoload
(defun ecc-auto-core-register-buffer (buffer)
  "Register BUFFER for auto-response.
Adds the buffer to the list of buffers that will be checked
for Claude prompts during auto-response processing.

Arguments:
  BUFFER: The buffer to register.

Returns:
  The buffer if registered successfully, nil otherwise."
  (when (buffer-live-p buffer)
    (add-to-list 'ecc-auto-core--registered-buffers buffer)
    (ecc-auto-core--log-buffer-registration buffer)
    buffer))

;;;###autoload
(defun ecc-auto-core-unregister-buffer (buffer)
  "Remove BUFFER from auto-response registry.
Removes the buffer from the list of buffers that are checked
for Claude prompts during auto-response processing.

Arguments:
  BUFFER: The buffer to unregister."
  (setq ecc-auto-core--registered-buffers
        (delq buffer ecc-auto-core--registered-buffers))
  (ecc-auto-core--log-buffer-unregistration buffer))

;;;###autoload
(defun ecc-auto-core-registered-buffers ()
  "Return list of currently registered buffers that are still live.
Filters out any dead buffers from the registry and returns
the updated list of live buffers.

Returns:
  List of live buffers registered for auto-response."
  (setq ecc-auto-core--registered-buffers
        (seq-filter #'buffer-live-p ecc-auto-core--registered-buffers)))

;;;###autoload
(defun ecc-auto-core-cleanup-buffers ()
  "Clean up the buffer registry by removing dead buffers.
Returns the updated list of registered buffers.

Returns:
  List of live buffers registered for auto-response."
  (let ((old-count (length ecc-auto-core--registered-buffers))
        (new-buffers (ecc-auto-core-registered-buffers))
        (new-count 0))
    (setq new-count (length new-buffers))
    (when (and ecc-auto-core-debug (< new-count old-count))
      (ecc-debug-message "[Auto Core] Cleaned up %d dead buffers from registry" 
               (- old-count new-count)))
    new-buffers))

;;;; Core processing

;;;###autoload
(defun ecc-auto-core-process-buffer (buffer callback)
  "Process BUFFER for auto-response with CALLBACK.
The callback should take two arguments: the buffer and the detected state.

This function checks if the buffer contains a Claude prompt state
that requires a response. If a state is detected and not throttled,
it calls the callback with the buffer and state, then updates the
state tracking.

Arguments:
  BUFFER: The buffer to check for Claude prompt state.
  CALLBACK: Function to call if a state is detected. Should take
            two arguments: the buffer and the detected state."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((state (ecc-detect-state)))
        (when (and state (not (ecc-auto-core-throttled-p state)))
          (ecc-auto-core--log-detection buffer state)
          (funcall callback buffer state)
          (ecc-auto-core-update-state state))))))

(defun ecc-auto-core--log-detection (buffer state)
  "Log detection of STATE in BUFFER if debug is enabled.
Arguments:
  BUFFER: The buffer where the state was detected.
  STATE: The detected Claude prompt state."
  (when ecc-auto-core-debug
    (if (and (featurep 'ecc-debug-utils) (fboundp 'ecc-debug-message))
        (ecc-debug-message "Detected state %s in %s"
                         (ecc-state-get-name state)
                         (buffer-name buffer))
      (ecc-debug-message "[Auto Core] Detected state %s in %s"
               (ecc-state-get-name state)
               (buffer-name buffer)))))

(defun ecc-auto-core--log-buffer-registration (buffer)
  "Log registration of BUFFER if debug is enabled.
Arguments:
  BUFFER: The buffer being registered."
  (when (and ecc-auto-core-debug (buffer-live-p buffer))
    (if (and (featurep 'ecc-debug-utils) (fboundp 'ecc-debug-message))
        (ecc-debug-message "Registered buffer %s for auto-response"
                         (buffer-name buffer))
      (ecc-debug-message "[Auto Core] Registered buffer %s for auto-response"
               (buffer-name buffer)))))

(defun ecc-auto-core--log-buffer-unregistration (buffer)
  "Log unregistration of BUFFER if debug is enabled.
Arguments:
  BUFFER: The buffer being unregistered."
  (when (and ecc-auto-core-debug (buffer-live-p buffer))
    (if (and (featurep 'ecc-debug-utils) (fboundp 'ecc-debug-message))
        (ecc-debug-message "Unregistered buffer %s from auto-response"
                         (buffer-name buffer))
      (ecc-debug-message "[Auto Core] Unregistered buffer %s from auto-response"
               (buffer-name buffer)))))

;;;###autoload
(defun ecc-auto-core-initial-check (buffer callback)
  "Perform initial check for BUFFER, calling CALLBACK if state detected.
This function specifically targets the initial-waiting state with more
aggressive checking to catch Claude's initial prompt.

This is useful for catching the initial prompt that appears when
Claude first starts, which might be missed by the regular timer.

Arguments:
  BUFFER: The buffer to check for Claude prompt state.
  CALLBACK: Function to call if a state is detected. Should take
            two arguments: the buffer and the detected state."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((state (ecc-detect-state)))
        (when (and state
                   (eq state :initial-waiting)
                   (not (ecc-auto-core-throttled-p state)))
          (ecc-auto-core--log-initial-detection buffer)
          (funcall callback buffer state)
          (ecc-auto-core-update-state state)))))
  
  ;; Increment counter and schedule next check if needed
  (setq ecc-auto-core--initial-check-count
        (1+ ecc-auto-core--initial-check-count))
  
  (ecc-auto-core--schedule-next-initial-check buffer callback))

(defun ecc-auto-core--log-initial-detection (buffer)
  "Log detection of initial state in BUFFER if debug is enabled.
Arguments:
  BUFFER: The buffer where the initial state was detected."
  (when ecc-auto-core-debug
    (if (and (featurep 'ecc-debug-utils) (fboundp 'ecc-debug-message))
        (ecc-debug-message "Detected initial state in %s"
                         (buffer-name buffer))
      (ecc-debug-message "[Auto Core] Detected initial state in %s"
               (buffer-name buffer)))))

(defun ecc-auto-core--schedule-next-initial-check (buffer callback)
  "Schedule next initial check for BUFFER with CALLBACK if needed.
Arguments:
  BUFFER: The buffer to check for Claude prompt state.
  CALLBACK: Function to call if a state is detected."
  (when (< ecc-auto-core--initial-check-count ecc-auto-core-max-initial-checks)
    (run-with-timer ecc-auto-core-initial-check-interval nil 
                    #'ecc-auto-core-initial-check buffer callback)))

;;;###autoload
(defun ecc-auto-core-process-all-buffers (callback)
  "Process all registered buffers for auto-response using CALLBACK.
Calls `ecc-auto-core-process-buffer` for each registered buffer.

Arguments:
  CALLBACK: Function to call for each buffer with a detected state.
            Should take two arguments: the buffer and the detected state."
  (dolist (buffer (ecc-auto-core-registered-buffers))
    (ecc-auto-core-process-buffer buffer callback)))

;;;; Lifecycle management

;;;###autoload
(defun ecc-auto-core-initialize ()
  "Initialize the auto-core system.
This resets state tracking and cleans up any existing resources.
Call this function before starting the auto-response system."
  (ecc-auto-core-timer-stop)
  (ecc-auto-core-reset-state)
  (ecc-auto-core-cleanup-buffers)
  (when ecc-auto-core-debug
    (ecc-debug-message "[Auto Core] Initialized")))

;;;###autoload
(defun ecc-auto-core-shutdown ()
  "Shut down the auto-core system, cleaning up all resources.
Stops timers, resets state tracking, and clears buffer registry.
Call this function when you're done with the auto-response system."
  (ecc-auto-core-timer-stop)
  (ecc-auto-core-reset-state)
  (setq ecc-auto-core--registered-buffers nil)
  (when ecc-auto-core-debug
    (ecc-debug-message "[Auto Core] Shut down")))

;;;; Debugging utilities

;;;###autoload
(defun ecc-auto-core-debug-status ()
  "Return a string with debug status information for auto-core.
This is useful for debugging and understanding the current state
of the auto-response system.

Returns:
  String with formatted status information."
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

;;;###autoload
(defun ecc-auto-core-toggle-debug ()
  "Toggle debug output for auto-core.
When debug is enabled, messages about auto-response operations
will be displayed in the echo area. This is useful for troubleshooting."
  (interactive)
  (setq ecc-auto-core-debug (not ecc-auto-core-debug))
  (ecc-debug-message "Auto-core debug %s"
           (if ecc-auto-core-debug "enabled" "disabled"))
  (when ecc-auto-core-debug
    (ecc-debug-message "%s" (ecc-auto-core-debug-status))))

;;;###autoload
(defun ecc-auto-core-print-status ()
  "Print status information about auto-core to messages.
Displays current timer state, last detected prompt, and buffer information."
  (interactive)
  (ecc-debug-message "%s" (ecc-auto-core-debug-status)))

;;;; Integration with other modules

;;;###autoload
(defun ecc-auto-core-get-registered-buffers ()
  "Return list of currently registered live buffers.
This is an alias for `ecc-auto-core-registered-buffers` for better naming consistency.
Returns:
  List of live buffers registered for auto-response."
  (ecc-auto-core-registered-buffers))

;;;###autoload
(defun ecc-auto-core-debug-toggle ()
  "Toggle debug output for auto-core.
This is an alias for `ecc-auto-core-toggle-debug` for better naming consistency."
  (interactive)
  (ecc-auto-core-toggle-debug))

(provide 'ecc-auto-core)

;;; ecc-auto-core.el ends here
