;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-02-28 02:45:36>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-auto-response.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)

;; 1. Dependencies
;; ----------------------------------------

(require 'cl-lib)
(require 'ecc-debug)
(require 'ecc-state-detection)
(require 'ecc-notification)
(require 'ecc-auto-response-logging)
(require 'vterm nil t)  ; Optional dependency

;; Declare functions from sub-modules (loaded at end of this file)
(declare-function --ecc-auto-response--update-mode-line
		  "ecc-auto-response-ui" ())
(declare-function --ecc-auto-response--start-pulse-timer
		  "ecc-auto-response-ui" ())
(declare-function --ecc-auto-response--stop-pulse-timer
		  "ecc-auto-response-ui" ())
(declare-function --ecc-auto-response--disable-visual-modes
		  "ecc-auto-response-ui" ())
(declare-function --ecc-auto-response--restore-visual-modes
		  "ecc-auto-response-ui" ())
(declare-function --ecc-auto-response-refresh-all-mode-lines
		  "ecc-auto-response-ui" ())
(declare-function --ecc-auto-response--start-timer
		  "ecc-auto-response-core" ())
(declare-function --ecc-auto-response--stop-timer
		  "ecc-auto-response-core" ())
(declare-function --ecc-auto-response--start-periodic-timer
		  "ecc-auto-response-core" ())
(declare-function --ecc-auto-response--stop-periodic-timer
		  "ecc-auto-response-core" ())
(declare-function --ecc-auto-response--process-all-buffers
		  "ecc-auto-response-core" ())
(declare-function --ecc-auto-response--process-buffer
		  "ecc-auto-response-core" (buffer))
(declare-function --ecc-auto-response--start-running-beep-timer
		  "ecc-auto-response-beep" ())
(declare-function --ecc-auto-response--stop-running-beep-timer
		  "ecc-auto-response-beep" ())
(declare-function --ecc-auto-response--force-beep
		  "ecc-auto-response-beep" ())
(declare-function --ecc-auto-response--do-notify
		  "ecc-auto-response-beep" (event))
(declare-function ecc-auto-response-cleanup-timers
		  "ecc-auto-response-beep" ())
(declare-function --ecc-notification--remove-thunder-icon
		  "ecc-notification" ())
(declare-function ecc-auto-periodical-setup-hook "ecc-auto-periodical"
		  ())
(declare-function ecc-tab-highlight--restore "ecc-tab-highlight" ())

;; 2. Configuration
;; ----------------------------------------

(defface ecc-auto-indicator-face
  '((t :background "#700000" :foreground "#ffffff" :weight bold))
  "Face for AUTO indicator in mode-line."
  :group 'ecc)

(defcustom --ecc-auto-response-interval 1.5
  "Interval in seconds for auto-response timer checks."
  :type 'float
  :group 'ecc)

(defcustom --ecc-auto-response-skip-unchanged-buffers t
  "Whether to skip processing buffers that haven't changed since last check."
  :type 'boolean
  :group 'ecc)

(defcustom --ecc-auto-response-max-buffers-per-cycle 3
  "Maximum number of buffers to process per timer cycle."
  :type '(choice (const :tag "Process all buffers" nil)
                 (integer :tag "Max buffers per cycle"))
  :group 'ecc)

(defcustom --ecc-auto-response-verbose-logging nil
  "Whether to enable verbose debug logging for auto-response."
  :type 'boolean
  :group 'ecc)

(defcustom --ecc-auto-response-use-idle-timer nil
  "Whether to use idle timer instead of regular timer."
  :type 'boolean
  :group 'ecc)

(defcustom --ecc-auto-response-safe-interval 1.0
  "Safety delay in seconds before and after sending responses."
  :type 'float
  :group 'ecc)

(defcustom --ecc-auto-response-vterm-return-delay 1.0
  "Additional delay in seconds between text and return in vterm mode."
  :type 'float
  :group 'ecc)

(defcustom --ecc-auto-response-mode-line-color "#700000"
  "Background color for mode-line when auto-response is enabled."
  :type 'color
  :group 'ecc)

(defcustom --ecc-auto-response-same-state-delay 1.5
  "Minimum seconds between auto-responses to the same state."
  :type 'float
  :group 'ecc)

(defcustom --ecc-auto-response-burst-limit 10
  "Maximum number of responses allowed within the burst window."
  :type 'number
  :group 'ecc)

(defcustom --ecc-auto-response-burst-window 3
  "Time window in seconds for burst rate limiting."
  :type 'number
  :group 'ecc)

;; Backward compatibility aliases

(defvaralias '--ecc-auto-response-throttle-duration
  '--ecc-auto-response-same-state-delay
  "Deprecated: Use `--ecc-auto-response-same-state-delay' instead.")

(defvaralias '--ecc-auto-response-accumulation-threshold
  '--ecc-auto-response-burst-limit
  "Deprecated: Use `--ecc-auto-response-burst-limit' instead.")

(defvaralias '--ecc-auto-response-accumulation-window
  '--ecc-auto-response-burst-window
  "Deprecated: Use `--ecc-auto-response-burst-window' instead.")

(defcustom --ecc-auto-response-send-escape-first t
  "Whether to send ESC before free text auto-responses."
  :type 'boolean
  :group 'ecc)

(defcustom --ecc-auto-response-responses
  '((:y/n . "1")
    (:y/y/n . "2")
    (:waiting . "/speak-signature"))
  "Alist of auto-responses for different Claude states."
  :type '(alist :key-type symbol :value-type string)
  :group 'ecc)

(defcustom --ecc-auto-response-periodic-interval 300.0
  "Interval in seconds for periodic return sending (default: 5 minutes)."
  :type 'float
  :group 'ecc)

(defcustom --ecc-auto-response-periodic-enabled t
  "Whether to enable periodic return sending as a fallback."
  :type 'boolean
  :group 'ecc)

;; 3. Variables
;; ----------------------------------------

(defvar --ecc-auto-response--registered-buffers
  (make-hash-table :test 'eq)
  "Hash table of registered buffers for auto-response.")

(defvar --ecc-auto-response--timer nil
  "Timer for checking and responding to Claude prompts.")

(defvar-local --ecc-auto-response--enabled nil
  "Whether auto-response is enabled for this buffer.")

(defvar-local --ecc-auto-response--last-state nil
  "Last Claude state that received an auto-response in this buffer.")

(defvar-local --ecc-auto-response--last-time 0
  "Timestamp of last auto-response in this buffer.")

(defvar-local --ecc-auto-response--response-timestamps nil
  "List of timestamps when responses were sent.")

(defvar-local --ecc-auto-response--sent-positions nil
  "List of buffer positions where responses have been sent.")

(defvar-local --ecc-auto-response--last-content-hash nil
  "Hash of last processed buffer content.")

(defvar-local --ecc-auto-response--original-mode-line nil
  "Original mode-line-format before AUTO indicator was added.")

(defvar-local --ecc-auto-response--last-periodic-time 0
  "Timestamp of last periodic return sent.")

(defvar --ecc-auto-response--periodic-timer nil
  "Global timer for periodic return sending across all buffers.")

(defvar --ecc-auto-response--pulse-timer nil
  "Global timer for pulsing the mode-line indicator across all buffers.")

(defvar-local --ecc-auto-response--pulse-state nil
  "Current pulse state for mode-line indicator.")

(defvar-local --ecc-auto-response--yellow-flash-state nil
  "Current yellow flash state for mode-line indicator when sending text.")

(defvar-local --ecc-auto-response--disabled-modes nil
  "List of modes that were disabled for performance.")

(defvar --ecc-auto-response--buffer-rotation-index 0
  "Current index for round-robin buffer processing.")

;; 4. Main Entry Point
;; ----------------------------------------

;;;###autoload

(defun --ecc-auto-response-toggle-buffer (&optional buffer)
  "Toggle auto-response for BUFFER."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      (if --ecc-auto-response--enabled
          (--ecc-auto-response-disable-buffer buf)
        (--ecc-auto-response-enable-buffer buf)))))

(defalias 'ecc-auto-toggle '--ecc-auto-response-toggle-buffer)

;; 5. Core Functions
;; ----------------------------------------

(defun --ecc-auto-response-enable-buffer (&optional buffer)
  "Enable auto-response for BUFFER."
  (let ((buf (or buffer (current-buffer))))
    (--ecc-auto-response-register-buffer buf)
    (with-current-buffer buf
      (setq-local --ecc-auto-response--enabled t)
      (when
          (local-variable-p '--ecc-auto-response--original-mode-line)
        (kill-local-variable '--ecc-auto-response--original-mode-line))
      (--ecc-auto-response--update-mode-line)
      (when (and --ecc-auto-response-periodic-enabled
                 (not --ecc-auto-response--periodic-timer))
        (--ecc-auto-response--start-periodic-timer))
      (unless --ecc-auto-response--pulse-timer
        (--ecc-auto-response--start-pulse-timer))
      (--ecc-auto-response--disable-visual-modes))
    (unless --ecc-auto-response--timer
      (--ecc-auto-response--start-timer))
    ;; Start running-beep timer
    (--ecc-auto-response--start-running-beep-timer)
    (--ecc-auto-response--do-notify "sent")
    (force-mode-line-update)
    (run-with-timer 0.1 nil
                    (lambda (buffer)
                      (when (buffer-live-p buffer)
                        (with-current-buffer buffer
                          (when --ecc-auto-response--enabled
                            (--ecc-auto-response--update-mode-line)
                            (force-mode-line-update)))))
                    buf)
    (run-with-timer 0.2 nil
                    (lambda (buffer)
                      (when (buffer-live-p buffer)
                        (--ecc-auto-response--process-buffer buffer)))
                    buf)
    (--ecc-debug-message "Auto-response enabled for buffer: %s"
                         (buffer-name buf))
    (--ecc-debug-message
     "Auto-response enabled - look for pulsing red ⚡ AUTO CLAUDE in mode-line")))

(defun --ecc-auto-response-disable-buffer (&optional buffer)
  "Disable auto-response for BUFFER.
When no auto-enabled buffers remain, cleans up ALL timers."
  (let ((buf (or buffer (current-buffer))))
    (--ecc-auto-response-unregister-buffer buf)
    (with-current-buffer buf
      (setq-local --ecc-auto-response--enabled nil)
      (--ecc-auto-response--restore-visual-modes)
      (when (fboundp '--ecc-notification--remove-thunder-icon)
        (--ecc-notification--remove-thunder-icon))
      (--ecc-auto-response--update-mode-line))
    ;; Check if ANY registered buffer still has auto mode enabled
    (let ((any-enabled nil))
      (dolist (b (--ecc-auto-response-get-registered-buffers))
        (when (and (buffer-live-p b)
                   (buffer-local-value '--ecc-auto-response--enabled b))
          (setq any-enabled t)))
      (if any-enabled
          ;; Some buffers still active -- only stop beep timer conditionally
          (--ecc-auto-response--stop-running-beep-timer)
        ;; No buffers remain -- full cleanup of ALL timers
        (ecc-auto-response-cleanup-timers)
        ;; Restore tab-bar face
        (when (fboundp 'ecc-tab-highlight--restore)
          (ecc-tab-highlight--restore))
        (--ecc-debug-message
         "All auto-response buffers disabled, all timers cleaned up")))
    (--ecc-debug-message "Auto-response disabled for buffer: %s"
                         (buffer-name buf))))

;; 6. Buffer Registry
;; ----------------------------------------

(defun --ecc-auto-response-register-buffer (buffer)
  "Register BUFFER for auto-response."
  (unless (buffer-live-p buffer)
    (user-error "Buffer is not alive"))
  (puthash buffer t --ecc-auto-response--registered-buffers)
  (--ecc-debug-message "Registered buffer: %s" (buffer-name buffer))
  buffer)

(defun --ecc-auto-response-unregister-buffer (buffer)
  "Unregister BUFFER from auto-response."
  (remhash buffer --ecc-auto-response--registered-buffers)
  (--ecc-debug-message "Unregistered buffer: %s" (buffer-name buffer))
  buffer)

(defun --ecc-auto-response-get-registered-buffers ()
  "Get list of all registered buffers."
  (let ((buffers nil))
    (maphash (lambda (buffer _)
               (when (buffer-live-p buffer)
                 (push buffer buffers)))
             --ecc-auto-response--registered-buffers)
    buffers))

(defun --ecc-auto-response-cleanup-registry ()
  "Remove dead buffers from registry."
  (let ((dead-buffers nil))
    (maphash (lambda (buffer _)
               (unless (buffer-live-p buffer)
                 (push buffer dead-buffers)))
             --ecc-auto-response--registered-buffers)
    (dolist (buffer dead-buffers)
      (remhash buffer --ecc-auto-response--registered-buffers))
    (when dead-buffers
      (--ecc-debug-message "Cleaned up %d dead buffers"
                           (length dead-buffers)))))

;; 7. Interactive Commands
;; ----------------------------------------

(defun --ecc-auto-response-restart ()
  "Restart the auto-response system."
  (interactive)
  (--ecc-auto-response--stop-timer)
  (--ecc-auto-response--start-timer)
  (message "Auto-response system restarted"))

(defun --ecc-auto-response-test-mode-line ()
  "Test the mode-line indicator."
  (interactive)
  (with-current-buffer (current-buffer)
    (message "Testing mode-line in buffer: %s" (buffer-name))
    (message "Auto-response enabled: %s" --ecc-auto-response--enabled)
    (message "Mode-line format: %s" mode-line-format)
    (when --ecc-auto-response--enabled
      (message "Face background should be: %s"
               --ecc-auto-response-mode-line-color))))

(defun --ecc-auto-response-disable-all ()
  "Disable auto-response in all buffers."
  (interactive)
  (--ecc-auto-response--stop-timer)
  (dolist (buffer (--ecc-auto-response-get-registered-buffers))
    (when (buffer-live-p buffer)
      (--ecc-auto-response-disable-buffer buffer)))
  (message "Auto-response disabled in all buffers"))

(defun --ecc-auto-response-toggle-periodic ()
  "Toggle periodic return sending globally."
  (interactive)
  (setq --ecc-auto-response-periodic-enabled
        (not --ecc-auto-response-periodic-enabled))
  (if --ecc-auto-response-periodic-enabled
      (unless --ecc-auto-response--periodic-timer
        (--ecc-auto-response--start-periodic-timer))
    (--ecc-auto-response--stop-periodic-timer))
  (message "Periodic return sending %s"
           (if --ecc-auto-response-periodic-enabled "enabled"
             "disabled")))

(defun ecc-auto-response-diagnose ()
  "Diagnose auto-response issues in current buffer."
  (interactive)
  (if (not --ecc-auto-response--enabled)
      (message
       "Auto-response is not enabled in this buffer. Use C-c C-a to enable.")
    (let ((state (--ecc-state-detection-detect)))
      (message "=== Auto-Response Diagnosis ===")
      (message "Buffer: %s" (buffer-name))
      (message "Auto-response enabled: %s"
               --ecc-auto-response--enabled)
      (message "Current state: %s" (or state "none"))
      (message "Last state: %s"
               (or --ecc-auto-response--last-state "none"))
      (message "Time since last response: %s seconds"
               (round (- (float-time) --ecc-auto-response--last-time)))
      (message "Expected response: %s"
               (or (cdr (assq state --ecc-auto-response-responses))
                   "none"))
      (message "Timer running: %s"
               (if --ecc-auto-response--timer "yes" "no"))
      (message "Registered buffers: %d"
               (hash-table-count
                --ecc-auto-response--registered-buffers))
      (message "===============================")
      (when (and state --ecc-auto-response--timer)
        (message "Triggering manual timer process...")
        (--ecc-auto-response--process-all-buffers))
      state)))

;; 8. Sub-module Requires
;; ----------------------------------------
;; Loaded after variable definitions so sub-modules can use them.

(require 'ecc-auto-response-ui)
(require 'ecc-auto-response-core)
(require 'ecc-auto-response-beep)

(when
    (not load-file-name)
  (--ecc-debug-message "ecc-auto-response.el loaded."
                       (file-name-nondirectory
                        (or load-file-name buffer-file-name))))

(provide 'ecc-auto-response)

(when
    (not load-file-name)
  (message "ecc-auto-response.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
