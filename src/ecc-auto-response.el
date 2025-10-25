;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-10-24 18:36:41>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-auto-response.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;; 1. Dependencies
;; ----------------------------------------

(require 'cl-lib)
(require 'ecc-debug)
(require 'ecc-state-detection)
(require 'ecc-notification)
(require 'ecc-auto-response-logging)
(require 'vterm nil t)  ; Optional dependency

;; Declare function to avoid compiler warnings
(declare-function ecc-auto-periodical-setup-hook "ecc-auto-periodical"
                  ())

;; 2. Configuration
;; ----------------------------------------

;; Define the face globally

(defface ecc-auto-indicator-face
  '((t :background "#700000" :foreground "#ffffff" :weight bold))
  "Face for AUTO indicator in mode-line."
  :group 'ecc)

(defcustom --ecc-auto-response-interval 1.5
  "Interval in seconds for auto-response timer checks.
Increased from 1.0 to 1.5 to reduce CPU usage with multiple vterm buffers."
  :type 'float
  :group 'ecc)

(defcustom --ecc-auto-response-skip-unchanged-buffers t
  "Whether to skip processing buffers that haven't changed since last check.
This significantly improves performance when multiple vterm buffers are active."
  :type 'boolean
  :group 'ecc)

(defcustom --ecc-auto-response-max-buffers-per-cycle 3
  "Maximum number of buffers to process per timer cycle.
When you have many Claude buffers, processing all of them every 1.5 seconds
can make Emacs heavy. This limits how many buffers are checked each cycle,
rotating through them in round-robin fashion. Set to nil to process all buffers."
  :type '(choice (const :tag "Process all buffers" nil)
                 (integer :tag "Max buffers per cycle"))
  :group 'ecc)

(defcustom --ecc-auto-response-verbose-logging nil
  "Whether to enable verbose debug logging for auto-response.
When nil, reduces logging overhead for better performance with many buffers.
Set to t only when debugging auto-response issues."
  :type 'boolean
  :group 'ecc)

(defcustom --ecc-auto-response-use-idle-timer nil
  "Whether to use idle timer instead of regular timer.
When t, buffer processing only happens when Emacs is idle, reducing
interference with typing and other activities. This can improve
perceived performance but may delay auto-responses slightly."
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

(defcustom --ecc-auto-response-same-state-delay 0.3
  "Minimum seconds between auto-responses to the same state.
Reduced from 1.0 to 0.3 for more responsive auto mode."
  :type 'float
  :group 'ecc)

(defcustom --ecc-auto-response-burst-limit 10
  "Maximum number of responses allowed within the burst window.
Increased from 3 to 10 for more generous rate limiting."
  :type 'number
  :group 'ecc)

(defcustom --ecc-auto-response-burst-window 3
  "Time window in seconds for burst rate limiting.
Increased from 1 to 3 seconds for wider window."
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
  "Whether to send ESC before free text auto-responses.
Only applies to :waiting and :initial-waiting states (like /auto, encouraging words).
Does NOT apply to simple selections like '1' or '2' for :y/n or :y/y/n states.
This helps clear any partial input before sending command-like responses."
  :type 'boolean
  :group 'ecc)

(defcustom --ecc-auto-response-responses
  '((:y/n . "1")
    (:y/y/n . "2")
    (:waiting . "/auto")
    (:initial-waiting . "/understand-guidelines"))
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
  "List of timestamps when responses were sent.
Used for sliding window accumulation detection.")

(defvar-local --ecc-auto-response--sent-positions nil
  "List of buffer positions where responses have been sent.
Each element is (POSITION . TIMESTAMP).")

(defvar-local --ecc-auto-response--last-content-hash nil
  "Hash of last processed buffer content.
Used to skip processing unchanged buffers for performance.")

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
  "Current index for round-robin buffer processing.
Used when `--ecc-auto-response-max-buffers-per-cycle' is set.")

;; 4. Main entry point
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

;; 5. Core functions
;; ----------------------------------------

(defun --ecc-auto-response-enable-buffer (&optional buffer)
  "Enable auto-response for BUFFER."
  (let ((buf (or buffer (current-buffer))))
    (--ecc-auto-response-register-buffer buf)
    (with-current-buffer buf
      (setq-local --ecc-auto-response--enabled t)
      ;; Clear any existing mode-line modifications first
      (when
          (local-variable-p '--ecc-auto-response--original-mode-line)
        (kill-local-variable '--ecc-auto-response--original-mode-line))
      ;; Update mode-line
      (--ecc-auto-response--update-mode-line)
      ;; Start global periodic timer if enabled and not already running
      (when (and --ecc-auto-response-periodic-enabled
                 (not --ecc-auto-response--periodic-timer))
        (--ecc-auto-response--start-periodic-timer))
      ;; Start global pulse timer if not already running
      (unless --ecc-auto-response--pulse-timer
        (--ecc-auto-response--start-pulse-timer))
      ;; Disable performance-heavy modes
      (--ecc-auto-response--disable-visual-modes))
    (unless --ecc-auto-response--timer
      (--ecc-auto-response--start-timer))
    ;; Play buzzer sound
    (beep)
    ;; Don't show thunder icon - we have permanent ⚡ AUTO CLAUDE indicator
    ;; (when (fboundp '--ecc-notification--flash-mode-line)
    ;;   (--ecc-notification--flash-mode-line buf))
    ;; Force immediate update
    (force-mode-line-update)
    ;; Schedule a mode-line refresh to ensure persistence
    (run-with-timer 0.1 nil
                    (lambda (buffer)
                      (when (buffer-live-p buffer)
                        (with-current-buffer buffer
                          (when --ecc-auto-response--enabled
                            (--ecc-auto-response--update-mode-line)
                            (force-mode-line-update)))))
                    buf)
    ;; Immediately process the buffer for auto-response
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
  "Disable auto-response for BUFFER."
  (let ((buf (or buffer (current-buffer))))
    (--ecc-auto-response-unregister-buffer buf)
    (with-current-buffer buf
      (setq-local --ecc-auto-response--enabled nil)
      ;; Stop periodic timer
      (--ecc-auto-response--stop-periodic-timer)
      ;; Stop pulse timer
      (--ecc-auto-response--stop-pulse-timer)
      ;; Re-enable visual modes
      (--ecc-auto-response--restore-visual-modes)
      ;; Remove thunder icon when disabling auto-response
      (when (fboundp '--ecc-notification--remove-thunder-icon)
        (--ecc-notification--remove-thunder-icon))
      ;; Update mode-line
      (--ecc-auto-response--update-mode-line))
    (--ecc-debug-message "Auto-response disabled for buffer: %s"
                         (buffer-name buf))))

;; 6. Buffer registry functions
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

;; 7. Timer management functions
;; ----------------------------------------

(defun --ecc-auto-response--start-timer ()
  "Start the auto-response timer (regular or idle based on config)."
  (when --ecc-auto-response--timer
    (cancel-timer --ecc-auto-response--timer))
  (setq --ecc-auto-response--timer
        (if --ecc-auto-response-use-idle-timer
            ;; Use idle timer - only runs when Emacs is idle
            (run-with-idle-timer --ecc-auto-response-interval t
                                 '--ecc-auto-response--process-all-buffers)
          ;; Use regular timer - runs at fixed intervals
          (run-with-timer 0 --ecc-auto-response-interval
                          '--ecc-auto-response--process-all-buffers)))
  (--ecc-debug-message "Auto-response %s timer started"
                       (if --ecc-auto-response-use-idle-timer "idle"
                         "regular"))
  ;; Force an immediate check (only for regular timer)
  (unless --ecc-auto-response-use-idle-timer
    (run-with-timer 0.1 nil '--ecc-auto-response--process-all-buffers)))

(defun --ecc-auto-response--stop-timer ()
  "Stop the auto-response timer."
  (when --ecc-auto-response--timer
    (cancel-timer --ecc-auto-response--timer)
    (setq --ecc-auto-response--timer nil))
  (--ecc-debug-message "Auto-response timer stopped"))

(defun --ecc-auto-response--start-periodic-timer ()
  "Start global periodic timer for all registered buffers."
  (when --ecc-auto-response--periodic-timer
    (cancel-timer --ecc-auto-response--periodic-timer))
  (setq --ecc-auto-response--periodic-timer
        (run-with-timer --ecc-auto-response-periodic-interval
                        --ecc-auto-response-periodic-interval
                        (lambda ()
                          ;; Send periodic return to all registered buffers
                          (dolist
                              (buffer
                               (--ecc-auto-response-get-registered-buffers))
                            (when (buffer-live-p buffer)
                              (--ecc-auto-response--send-periodic-return
                               buffer))))))
  (--ecc-debug-message
   "Started global periodic timer (interval: %s seconds)"
   --ecc-auto-response-periodic-interval))

(defun --ecc-auto-response--stop-periodic-timer ()
  "Stop the global periodic timer."
  (when --ecc-auto-response--periodic-timer
    (cancel-timer --ecc-auto-response--periodic-timer)
    (setq --ecc-auto-response--periodic-timer nil)
    (--ecc-debug-message "Stopped global periodic timer")))

(defun --ecc-auto-response--send-periodic-return (buffer)
  "Send periodic return to BUFFER if appropriate."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and --ecc-auto-response--enabled
                 --ecc-auto-response-periodic-enabled)
        (let ((current-time (float-time))
              (current-state (--ecc-state-detection-detect)))
          ;; Skip if Claude is running
          (unless (eq current-state :running)
            ;; Only send if enough time has passed since last periodic send
            (when (>=
                   (- current-time
                      --ecc-auto-response--last-periodic-time)
                   --ecc-auto-response-periodic-interval)
              (--ecc-debug-message
               "Sending periodic return to buffer %s"
               (buffer-name buffer))
              (setq-local --ecc-auto-response--last-periodic-time
                          current-time)
              ;; Send return key
              (cond
               ((derived-mode-p 'vterm-mode)
                (when (fboundp 'vterm-send-return)
                  (vterm-send-return)))
               ((derived-mode-p 'comint-mode)
                (goto-char (point-max))
                (comint-send-input))
               (t
                (goto-char (point-max))
                (insert "\n")))
              (--ecc-debug-message "Periodic return sent to %s"
                                   (buffer-name buffer))))
          (when (eq current-state :running)
            (--ecc-debug-message
             "Claude is running, skipping periodic return")))))))

;; 8. Processing functions
;; ----------------------------------------

(defun --ecc-auto-response--process-all-buffers ()
  "Process registered buffers for auto-response.
If `--ecc-auto-response-max-buffers-per-cycle' is set, processes only
that many buffers per cycle in round-robin fashion for better performance."
  (--ecc-auto-response-cleanup-registry)
  (let* ((all-buffers (--ecc-auto-response-get-registered-buffers))
         (total-count (length all-buffers))
         (buffers-to-process
          (if (and --ecc-auto-response-max-buffers-per-cycle
                   (> total-count
                      --ecc-auto-response-max-buffers-per-cycle))
              ;; Rotate through buffers in round-robin fashion
              (let* ((start-idx (mod
                                 --ecc-auto-response--buffer-rotation-index
                                 total-count))
                     (end-idx (min (+ start-idx
                                      --ecc-auto-response-max-buffers-per-cycle)
                                   total-count))
                     (selected
                      (cl-subseq all-buffers start-idx end-idx)))
                ;; Update rotation index for next cycle
                (setq --ecc-auto-response--buffer-rotation-index
                      (mod (+ start-idx
                              --ecc-auto-response-max-buffers-per-cycle)
                           total-count))
                (when --ecc-auto-response-verbose-logging
                  (--ecc-debug-message
                   "Timer tick: processing %d/%d buffers (rotation idx: %d->%d)"
                   (length selected) total-count start-idx
                   --ecc-auto-response--buffer-rotation-index))
                selected)
            ;; Process all buffers
            (when --ecc-auto-response-verbose-logging
              (--ecc-debug-message
               "Timer tick: processing all %d buffers"
               total-count))
            all-buffers)))
    (dolist (buffer buffers-to-process)
      (when (buffer-live-p buffer)
        (--ecc-auto-response--process-buffer buffer)))))

(defun --ecc-auto-response--process-buffer (buffer)
  "Process BUFFER for auto-response."
  (with-current-buffer buffer
    (when --ecc-auto-response--enabled
      (let* ((buffer-content (buffer-substring-no-properties
                              (max (point-min) (- (point-max) 200))
                              (point-max)))
             (content-hash (sxhash buffer-content))
             (content-unchanged (and
                                 --ecc-auto-response-skip-unchanged-buffers
                                 --ecc-auto-response--last-content-hash
                                 (= content-hash
                                    --ecc-auto-response--last-content-hash))))
        ;; Skip if buffer content hasn't changed (performance optimization)
        (unless content-unchanged
          ;; Buffer changed - process it
          (setq-local --ecc-auto-response--last-content-hash
                      content-hash)
          (let ((state (--ecc-state-detection-detect)))
            (when state
              ;; Flash/highlight the detected pattern
              (--ecc-state-detection-flash-pattern state buffer)
              (when --ecc-auto-response-verbose-logging
                (--ecc-debug-message "Processing buffer %s: state=%s"
                                     (buffer-name buffer) state))
              (when --ecc-auto-response-verbose-logging
                (ecc-auto-response-log-state-detection state
                                                       buffer-content))
              (cond
               ;; Skip auto-response when Claude is running
               ((eq state :running)
                (when --ecc-auto-response-verbose-logging
                  (--ecc-debug-message
                   "Claude is running, skipping auto-response")
                  (ecc-auto-response-log 'info
                                         "Claude is running, skipping auto-response")))
               ;; Normal processing for other states
               ((not (--ecc-auto-response--already-sent-p))
                (when --ecc-auto-response-verbose-logging
                  (--ecc-debug-message
                   "State detected, checking throttle for %s" state))
                (unless (--ecc-auto-response--should-throttle-p state)
                  (when --ecc-auto-response-verbose-logging
                    (--ecc-debug-message
                     "Not throttled, sending response for %s" state))
                  (--ecc-auto-response--send-response state buffer)))))))))))

;; 9. Throttle detection functions
;; ----------------------------------------

(defun --ecc-auto-response--should-throttle-p (state)
  "Check if auto-response for STATE should be throttled."
  (let ((current-time (float-time))
        (throttle-reason nil))
    (--ecc-debug-message
     "Throttle check: state=%s, last-state=%s, time-diff=%s, same-state-delay=%s"
     state --ecc-auto-response--last-state
     (- current-time
        --ecc-auto-response--last-time)
     --ecc-auto-response-same-state-delay)
    (cond
     ;; Throttle if same state within delay duration
     ((and (eq state --ecc-auto-response--last-state)
           (< (- current-time --ecc-auto-response--last-time)
              --ecc-auto-response-same-state-delay))
      (setq throttle-reason (format "Same state within %s seconds"
                                    --ecc-auto-response-same-state-delay))
      (ecc-auto-response-log-throttle state throttle-reason)
      t)
     ;; Check if we're at or would exceed burst limit
     ((let
          ((window-start
            (- current-time --ecc-auto-response-burst-window)))
        ;; Count recent responses within window
        (let ((recent-count (cl-count-if (lambda (timestamp)
                                           (>= timestamp
                                               window-start))
                                         --ecc-auto-response--response-timestamps)))
          (--ecc-debug-message
           "Recent responses: %d (burst-limit: %d)"
           recent-count
           --ecc-auto-response-burst-limit)
          ;; Block if we've already exceeded the burst limit
          (when
              (>= recent-count
                  --ecc-auto-response-burst-limit)
            (setq throttle-reason (format
                                   "Burst limit reached: %d responses in %s seconds"
                                   recent-count
                                   --ecc-auto-response-burst-window))
            (ecc-auto-response-log-throttle state throttle-reason)
            t))))
     (t nil))))

(defun --ecc-auto-response--accumulation-detected-p ()
  "Check if auto-response burst limit has been detected.
Uses a sliding window approach to count responses within the burst window."
  (let ((current-time (float-time))
        (window-start
         (- (float-time) --ecc-auto-response-burst-window)))
    ;; Remove timestamps outside the sliding window
    (setq-local --ecc-auto-response--response-timestamps
                (cl-remove-if (lambda (timestamp)
                                (< timestamp window-start))
                              --ecc-auto-response--response-timestamps))
    ;; Check if we've exceeded the burst limit
    (let ((count (length --ecc-auto-response--response-timestamps)))
      (--ecc-debug-message
       "Burst check: %d responses in last %s seconds (burst-limit: %d)"
       count
       --ecc-auto-response-burst-window
       --ecc-auto-response-burst-limit)
      (>= count --ecc-auto-response-burst-limit))))

;; 10. Response detection functions
;; ----------------------------------------

(defun --ecc-auto-response--already-sent-p ()
  "Check if we've already sent a response near current position.
Only blocks if position is similar AND state hasn't changed."
  (let ((current-pos (point-max))
        (current-state (--ecc-state-detection-detect))
        (threshold 100))
    ;; Allow sending if state has changed, even if position is similar
    (if (not (eq current-state --ecc-auto-response--last-state))
        (progn
          (--ecc-debug-message
           "State changed from %s to %s, allowing response"
           --ecc-auto-response--last-state current-state)
          nil)
      ;; Same state: check position threshold
      (let ((result (cl-some (lambda (pos-time)
                               (<
                                (abs (- current-pos (car pos-time)))
                                threshold))
                             --ecc-auto-response--sent-positions)))
        (when result
          (--ecc-debug-message
           "Already sent check: pos=%d, sent-positions=%s, result=%s"
           current-pos
           --ecc-auto-response--sent-positions
           result))
        result))))

;; 11. Response sending functions
;; ----------------------------------------

(defun --ecc-auto-response--send-response (state buffer)
  "Send appropriate response for STATE in BUFFER."
  (let ((response (if (and
                       (fboundp
                        'ecc-encouragement-get-phrase-for-state)
                       (memq state '(:waiting :initial-waiting)))
                      (ecc-encouragement-get-phrase-for-state state)
                    (cdr (assq state --ecc-auto-response-responses)))))
    (when response
      ;; Update tracking BEFORE sending to prevent duplicate sends
      (with-current-buffer buffer
        (--ecc-auto-response--update-tracking state))
      ;; Now send the response (this has delays, so tracking must be updated first)
      (--ecc-auto-response--send-to-buffer buffer response state)
      ;; Play buzzer sound when sending auto-response
      (beep)
      ;; Don't show thunder CLAUDE icon - we have permanent ⚡ AUTO CLAUDE indicator
      ;; (when (fboundp '--ecc-notification-notify)
      ;;   (--ecc-notification-notify state buffer))
      ;; Trigger auto-periodical check if available
      (when (fboundp 'ecc-auto-periodical-setup-hook)
        (with-current-buffer buffer
          (ecc-auto-periodical-setup-hook))))))

(defun --ecc-auto-response--update-tracking (state)
  "Update tracking variables for STATE."
  (let ((current-time (float-time)))
    (setq-local --ecc-auto-response--last-state state)
    (setq-local --ecc-auto-response--last-time current-time)
    ;; Add timestamp to sliding window
    (push current-time --ecc-auto-response--response-timestamps)
    ;; Record the position where we sent the response
    (push (cons (point-max) current-time)
          --ecc-auto-response--sent-positions)
    ;; Clean up old sent positions (older than 60 seconds)
    (setq --ecc-auto-response--sent-positions
          (cl-remove-if (lambda (pos-time)
                          (> (- current-time (cdr pos-time)) 60))
                        --ecc-auto-response--sent-positions))))

(defun --ecc-auto-response--send-to-buffer (buffer text state)
  "Send TEXT to BUFFER for STATE.
If STATE is :waiting or :initial-waiting (free text responses),
send ESC first to clear partial input."
  (with-current-buffer buffer
    (let ((text-sender (cond
                        ((derived-mode-p 'vterm-mode)
                         (lambda () (vterm-send-string text)))
                        ((derived-mode-p 'comint-mode)
                         (lambda () (insert text)))
                        (t
                         (lambda () (insert text)))))
          (return-sender (lambda ()
                           (cond
                            ((derived-mode-p 'vterm-mode)
                             (vterm-send-return))
                            ((derived-mode-p 'comint-mode)
                             (comint-send-input))
                            (t
                             (insert "\n")))))
          (auto-response-text-initial-waiting
           (cdr
            (assoc :initial-waiting --ecc-auto-response-responses)))
          (auto-response-text-waiting
           (cdr (assoc :waiting --ecc-auto-response-responses)))
          (is-free-response
           (memq state '(:waiting :initial-waiting))))
      ;; Main sending sequence
      (sit-for --ecc-auto-response-safe-interval)

      ;; Don't send ESC before encouragement words - it interferes with the prompt
      ;; (when (and --ecc-auto-response-send-escape-first
      ;;            is-free-response)
      ;;   (when (derived-mode-p 'vterm-mode)
      ;;     (vterm-send-escape))
      ;;   (sit-for --ecc-auto-response-safe-interval))

      ;; Auto Message
      (funcall text-sender)
      (sit-for --ecc-auto-response-safe-interval)

      ;; Return
      (funcall return-sender)
      (sit-for --ecc-auto-response-safe-interval)

      ;; Show encouragement
      (--ecc-auto-response--show-encouragement buffer text)))
  (--ecc-debug-message "Sent response to %s: %s"
                       (buffer-name buffer)
                       text))

;; 12. Pulse timer functions
;; ----------------------------------------

(defun --ecc-auto-response--start-pulse-timer ()
  "Start the global pulse timer for mode-line indicator across all buffers."
  (when --ecc-auto-response--pulse-timer
    (cancel-timer --ecc-auto-response--pulse-timer))
  (setq --ecc-auto-response--pulse-timer
        (run-with-timer 0 1.0
                        (lambda ()
                          ;; Update pulse state for all registered buffers
                          (dolist
                              (buffer
                               (--ecc-auto-response-get-registered-buffers))
                            (when (buffer-live-p buffer)
                              (with-current-buffer buffer
                                (when --ecc-auto-response--enabled
                                  (setq-local
                                   --ecc-auto-response--pulse-state
                                   (not
                                    --ecc-auto-response--pulse-state))
                                  (force-mode-line-update)))))))))

(defun --ecc-auto-response--stop-pulse-timer ()
  "Stop the global pulse timer."
  (when --ecc-auto-response--pulse-timer
    (cancel-timer --ecc-auto-response--pulse-timer)
    (setq --ecc-auto-response--pulse-timer nil)))

(defun --ecc-auto-response--flash-yellow (buffer)
  "Flash the mode-line indicator dark for 5.0 seconds in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local --ecc-auto-response--yellow-flash-state t)
      (force-mode-line-update)
      (run-with-timer 5.0 nil
                      (lambda (buf)
                        (when (buffer-live-p buf)
                          (with-current-buffer buf
                            (setq-local
                             --ecc-auto-response--yellow-flash-state
                             nil)
                            (force-mode-line-update))))
                      buffer))))

(defun --ecc-auto-response--show-encouragement (buffer text)
  "Highlight the sent TEXT in BUFFER with yellow background."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        ;; Search backward for the sent text in the buffer
        (goto-char (point-max))
        (when (search-backward text nil t)
          (let
              ((overlay
                (make-overlay (match-beginning 0) (match-end 0))))
            ;; Highlight the actual sent text in yellow
            (overlay-put overlay 'face
                         '(:background "#8B7500" :foreground "#000000"
                                       :weight bold))
            (overlay-put overlay 'priority 1000)
            ;; ;; Show message in echo area
            ;; (message (propertize (format "✓ Sent: %s" text)
            ;;                     'face '(:foreground "#90EE90")))
            ;; Remove highlight after 2 seconds
            (run-with-timer 2.0 nil
                            (lambda (ov)
                              (when (overlayp ov)
                                (delete-overlay ov)))
                            overlay)))))))

;; 13. Visual mode management
;; ----------------------------------------

(defun --ecc-auto-response--disable-visual-modes ()
  "Disable performance-heavy visual modes during auto-response."
  (setq-local --ecc-auto-response--disabled-modes nil)
  ;; Disable highlight-parentheses-mode if active
  (when
      (and (boundp 'highlight-parentheses-mode)
           highlight-parentheses-mode)
    (push 'highlight-parentheses-mode
          --ecc-auto-response--disabled-modes)
    (highlight-parentheses-mode -1))
  ;; Disable show-paren-mode if active
  (when (and (boundp 'show-paren-mode) show-paren-mode)
    (push 'show-paren-mode --ecc-auto-response--disabled-modes)
    (show-paren-mode -1))
  ;; Disable rainbow-delimiters-mode if active
  (when
      (and (boundp 'rainbow-delimiters-mode) rainbow-delimiters-mode)
    (push 'rainbow-delimiters-mode
          --ecc-auto-response--disabled-modes)
    (rainbow-delimiters-mode -1))
  (--ecc-debug-message "Disabled visual modes: %s"
                       --ecc-auto-response--disabled-modes))

(defun --ecc-auto-response--restore-visual-modes ()
  "Restore visual modes that were disabled."
  (dolist (mode --ecc-auto-response--disabled-modes)
    (when (fboundp mode)
      (funcall mode 1)))
  (setq-local --ecc-auto-response--disabled-modes nil)
  (--ecc-debug-message "Restored visual modes"))

;; 14. Mode-line functions
;; ----------------------------------------

(defun --ecc-auto-response--update-mode-line ()
  "Update mode-line to show auto-response status."
  (if --ecc-auto-response--enabled
      ;; Add AUTO indicator to mode-line
      (progn
        ;; Store the current mode-line-format before modifying
        (unless
            (local-variable-p
             '--ecc-auto-response--original-mode-line)
          (setq-local --ecc-auto-response--original-mode-line
                      (if (local-variable-p 'mode-line-format)
                          mode-line-format
                        (default-value 'mode-line-format))))

        ;; Always recreate to ensure it's properly added
        (let ((has-indicator nil))
          ;; Create the indicator with pulse effect
          (let ((indicator '(:eval (when
                                       --ecc-auto-response--enabled
                                     (propertize " ⚡ AUTO CLAUDE "
                                                 'face (cond
                                                        ;; Dark flash when sending
                                                        (--ecc-auto-response--yellow-flash-state
                                                         '(:background
                                                           "#1a0f00"
                                                           :foreground
                                                           "#888888"
                                                           :weight
                                                           bold))
                                                        ;; Normal pulse
                                                        (--ecc-auto-response--pulse-state
                                                         'ecc-auto-indicator-face)
                                                        ;; Darker pulse
                                                        (t
                                                         '(:background
                                                           "#5c0000"
                                                           :foreground
                                                           "#ffffff"
                                                           :weight
                                                           bold)))
                                                 'help-echo
                                                 "Auto-response is active")))))
            ;; Use the stored original format
            (let
                ((original --ecc-auto-response--original-mode-line))
              (if (listp original)
                  (let ((new-format (copy-sequence original))
                        (buffer-id-pos
                         (cl-position
                          'mode-line-buffer-identification original)))
                    (if buffer-id-pos
                        ;; Insert after buffer identification
                        (setq mode-line-format
                              (append
                               (cl-subseq new-format 0
                                          (1+ buffer-id-pos))
                               (list indicator)
                               (cl-subseq new-format
                                          (1+ buffer-id-pos))))
                      ;; If no buffer-id found, prepend
                      (setq mode-line-format
                            (cons indicator new-format))))
                ;; If original is not a list, make it one
                (setq mode-line-format (list indicator original)))))))
    ;; Remove AUTO indicator and restore original
    (when
        (local-variable-p '--ecc-auto-response--original-mode-line)
      (setq mode-line-format
            --ecc-auto-response--original-mode-line)
      (kill-local-variable
       '--ecc-auto-response--original-mode-line)))
  (force-mode-line-update))

(defun --ecc-auto-response-refresh-all-mode-lines ()
  "Refresh mode-lines for all buffers with auto-response enabled."
  (interactive)
  ;; Refresh all registered buffers
  (dolist (buffer (--ecc-auto-response-get-registered-buffers))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when --ecc-auto-response--enabled
          ;; Reset mode-line to original first
          (when
              (local-variable-p
               '--ecc-auto-response--original-mode-line)
            (setq mode-line-format
                  --ecc-auto-response--original-mode-line)
            (kill-local-variable
             '--ecc-auto-response--original-mode-line))
          ;; Force recreation
          (--ecc-auto-response--update-mode-line)))))
  (message "Refreshed mode-lines for all auto-response buffers"))

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
  ;; Update all active buffers
  ;; Start or stop global periodic timer based on setting
  (if --ecc-auto-response-periodic-enabled
      (unless --ecc-auto-response--periodic-timer
        (--ecc-auto-response--start-periodic-timer))
    (--ecc-auto-response--stop-periodic-timer))
  (message "Periodic return sending %s"
           (if --ecc-auto-response-periodic-enabled "enabled"
             "disabled")))

;; Removed circular alias - ecc-refresh-timers is defined in ecc.el

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
      ;; Try to process manually
      (when (and state --ecc-auto-response--timer)
        (message "Triggering manual timer process...")
        (--ecc-auto-response--process-all-buffers))
      state)))

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