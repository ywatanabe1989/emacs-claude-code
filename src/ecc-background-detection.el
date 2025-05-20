;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 21:15:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-background-detection.el

;;; Commentary:
;;; Background detection system for Claude state.
;;; This module provides a cursor-independent detection system that works
;;; in the background to detect Claude prompt states without relying on
;;; cursor position or interfering with user interactions.

(require 'ecc-variables)
(require 'ecc-state-detection)
(require 'ecc-buffer-state)

;;; Code:

;; Customization options
(defgroup ecc-background-detection nil
  "Background detection settings for Claude."
  :group 'ecc
  :prefix "ecc-background-detection-")

(defcustom ecc-background-detection-interval 1.0
  "Interval in seconds for background state detection checks."
  :type 'number
  :group 'ecc-background-detection)

(defcustom ecc-background-detection-idle-delay 0.5
  "Idle time in seconds before running background detection.
Using idle timers helps ensure detection doesn't interfere with
user typing or interactions."
  :type 'number
  :group 'ecc-background-detection)

(defcustom ecc-background-detection-chunk-size 5
  "Maximum number of buffers to check in one background cycle.
Breaking detection into chunks prevents long processing delays.
A value of 0 means no limit."
  :type 'integer
  :group 'ecc-background-detection)

(defcustom ecc-background-detection-debug nil
  "Whether to show debug messages for background detection."
  :type 'boolean
  :group 'ecc-background-detection)

;; Internal variables
(defvar ecc-background-detection-timer nil
  "Timer for background state detection.")

(defvar ecc-background-detection-idle-timer nil
  "Idle timer for background state detection.")

(defvar ecc-background-detection-active nil
  "Whether background detection is currently active.")

(defvar ecc-background-detection-registered-buffers nil
  "List of buffers registered for background detection.")

(defvar ecc-background-detection-processing nil
  "Whether a background detection cycle is in progress.")

(defvar ecc-background-detection-callback nil
  "Callback function to call when state is detected.")

;; Core detection functions

(defun ecc-background-detect-state-in-buffer (buffer)
  "Detect Claude prompt state in BUFFER without affecting cursor.
Returns the detected state or nil."
  (when (buffer-live-p buffer)
    (condition-case err
        (with-current-buffer buffer
          (save-excursion
            ;; Detect using the entire buffer
            (let ((state (ecc-detect-state)))
              (when state
                ;; Update buffer-local state tracking
                (ecc-buffer-state-update-prompt state)
                ;; Also update traditional variables for compatibility
                (ecc-buffer-state-export-standard)
                ;; Return detected state
                state))))
      (error
       (when ecc-background-detection-debug
         (message "Error in background detection for %s: %s"
                  (buffer-name buffer) (error-message-string err)))
       nil))))

(defun ecc-background-detection-check-buffer (buffer)
  "Check BUFFER for prompt state and call callback if state detected.
Returns the detected state or nil."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((state (ecc-background-detect-state-in-buffer buffer)))
        (when (and state ecc-background-detection-callback)
          (condition-case err
              (funcall ecc-background-detection-callback buffer state)
            (error
             (when ecc-background-detection-debug
               (message "Error in background detection callback: %s"
                        (error-message-string err))))))
        state))))

(defun ecc-background-detection-process-buffers ()
  "Process registered buffers for background detection.
Checks each buffer for Claude prompt states and updates state tracking."
  (when (and ecc-background-detection-active
             (not ecc-background-detection-processing))
    ;; Set processing flag to avoid recursion
    (setq ecc-background-detection-processing t)
    
    ;; Clean up list first
    (setq ecc-background-detection-registered-buffers
          (seq-filter #'buffer-live-p
                     ecc-background-detection-registered-buffers))
    
    (let ((buffers-checked 0)
          (buffers-with-states 0)
          (max-buffers-to-check (if (> ecc-background-detection-chunk-size 0)
                                    ecc-background-detection-chunk-size
                                  (length ecc-background-detection-registered-buffers))))
      
      ;; Process buffers up to chunk size
      (dolist (buffer (seq-take ecc-background-detection-registered-buffers
                               max-buffers-to-check))
        (when (buffer-live-p buffer)
          (setq buffers-checked (1+ buffers-checked))
          (when (ecc-background-detection-check-buffer buffer)
            (setq buffers-with-states (1+ buffers-with-states)))))
      
      ;; Optional debug output
      (when (and ecc-background-detection-debug (> buffers-checked 0))
        (message "Background detection processed %d buffers, found states in %d"
                 buffers-checked buffers-with-states)))
    
    ;; Clear processing flag
    (setq ecc-background-detection-processing nil)))

;; Timer management

(defun ecc-background-detection-timer-start ()
  "Start the background detection timer.
Cancels any existing timer first."
  (ecc-background-detection-timer-stop)
  (setq ecc-background-detection-timer
        (run-with-timer 
         ecc-background-detection-interval
         ecc-background-detection-interval
         #'ecc-background-detection-process-buffers))
  
  ;; Also set up idle timer for more responsive detection
  (setq ecc-background-detection-idle-timer
        (run-with-idle-timer
         ecc-background-detection-idle-delay
         t  ; Repeat
         #'ecc-background-detection-process-buffers)))

(defun ecc-background-detection-timer-stop ()
  "Stop the background detection timer if it exists."
  (when (timerp ecc-background-detection-timer)
    (cancel-timer ecc-background-detection-timer)
    (setq ecc-background-detection-timer nil))
  
  (when (timerp ecc-background-detection-idle-timer)
    (cancel-timer ecc-background-detection-idle-timer)
    (setq ecc-background-detection-idle-timer nil)))

;; Buffer registration management

(defun ecc-background-detection-register-buffer (buffer)
  "Register BUFFER for background detection.
Returns the buffer if registered, nil otherwise."
  (when (buffer-live-p buffer)
    (add-to-list 'ecc-background-detection-registered-buffers buffer)
    ;; Initialize buffer state if this is a new registration
    (with-current-buffer buffer
      (unless (ecc-buffer-state-has-key-p 'prompt-state)
        (ecc-buffer-state-init)))
    buffer))

(defun ecc-background-detection-unregister-buffer (buffer)
  "Remove BUFFER from background detection registry."
  (setq ecc-background-detection-registered-buffers
        (delq buffer ecc-background-detection-registered-buffers)))

(defun ecc-background-detection-registered-buffers ()
  "Return list of currently registered buffers that are still live."
  (setq ecc-background-detection-registered-buffers
        (seq-filter #'buffer-live-p ecc-background-detection-registered-buffers)))

;; Public API

;;;###autoload
(defun ecc-background-detection-start (&optional callback)
  "Start background detection with optional CALLBACK function.
The callback should take two arguments: buffer and detected state.
If CALLBACK is nil, detection runs but no actions are taken."
  (interactive)
  (unless ecc-background-detection-active
    (setq ecc-background-detection-active t)
    (setq ecc-background-detection-callback callback)
    (ecc-background-detection-timer-start)
    (message "Background detection started"))
  
  ;; Try to register current buffer if interactive
  (when (called-interactively-p 'any)
    (ecc-background-detection-register-buffer (current-buffer))))

;;;###autoload
(defun ecc-background-detection-stop ()
  "Stop background detection system."
  (interactive)
  (when ecc-background-detection-active
    (setq ecc-background-detection-active nil)
    (setq ecc-background-detection-callback nil)
    (ecc-background-detection-timer-stop)
    (message "Background detection stopped")))

;;;###autoload
(defun ecc-background-detection-toggle (&optional callback)
  "Toggle background detection with optional CALLBACK function."
  (interactive)
  (if ecc-background-detection-active
      (ecc-background-detection-stop)
    (ecc-background-detection-start callback)))

;;;###autoload
(defun ecc-background-detection-add-buffer (&optional buffer)
  "Add BUFFER to background detection.
If BUFFER is nil, use current buffer."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (when (ecc-background-detection-register-buffer buf)
      (when (called-interactively-p 'any)
        (message "Added buffer %s to background detection" (buffer-name buf)))
      buf)))

;;;###autoload
(defun ecc-background-detection-remove-buffer (&optional buffer)
  "Remove BUFFER from background detection.
If BUFFER is nil, use current buffer."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (ecc-background-detection-unregister-buffer buf)
    (when (called-interactively-p 'any)
      (message "Removed buffer %s from background detection" (buffer-name buf)))))

;; Debugging helpers

;;;###autoload
(defun ecc-background-detection-debug-toggle ()
  "Toggle debug output for background detection."
  (interactive)
  (setq ecc-background-detection-debug (not ecc-background-detection-debug))
  (message "Background detection debug %s"
           (if ecc-background-detection-debug "enabled" "disabled")))

;;;###autoload
(defun ecc-background-detection-status ()
  "Display status information about background detection."
  (interactive)
  (let ((status-msg
         (format "Background Detection Status:
  Active: %s
  Timer Running: %s
  Idle Timer Running: %s
  Registered Buffers: %d
  Processing Cycle: %s
  Debug Output: %s
  Interval: %.1f seconds
  Idle Delay: %.1f seconds
  Chunk Size: %d"
                 (if ecc-background-detection-active "Yes" "No")
                 (if (timerp ecc-background-detection-timer) "Yes" "No")
                 (if (timerp ecc-background-detection-idle-timer) "Yes" "No")
                 (length (ecc-background-detection-registered-buffers))
                 (if ecc-background-detection-processing "Yes" "No")
                 (if ecc-background-detection-debug "Enabled" "Disabled")
                 ecc-background-detection-interval
                 ecc-background-detection-idle-delay
                 ecc-background-detection-chunk-size)))
    (if (called-interactively-p 'any)
        (message "%s" status-msg)
      status-msg)))

(provide 'ecc-background-detection)

;;; ecc-background-detection.el ends here