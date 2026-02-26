;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-02-20 15:10:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-auto-response-core.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;;; Core timer, processing, throttle, and response-sending for ecc-auto-response.

;; 1. Dependencies
;; ----------------------------------------

(require 'cl-lib)
(require 'ecc-debug)
(require 'ecc-state-detection)
(require 'ecc-notification)
(require 'ecc-auto-response-logging)
(require 'ecc-auto-response-ui)

;; Function stubs (defined in ecc-auto-response.el)
(declare-function --ecc-auto-response-get-registered-buffers
		  "ecc-auto-response" ())
(declare-function ecc-encouragement-get-phrase-for-state
		  "ecc-encouragement" (state))
(declare-function ecc-auto-periodical-setup-hook "ecc-auto-periodical"
		  ())
(declare-function --ecc-auto-response-cleanup-registry
		  "ecc-auto-response" ())
(declare-function --ecc-auto-response--force-beep
		  "ecc-auto-response-beep" ())
(declare-function --ecc-auto-response--notify-sent
		  "ecc-auto-response-beep" ())

;; Variable stubs (defined in ecc-auto-response.el)

(defvar --ecc-auto-response--timer nil)

(defvar --ecc-auto-response-interval 1.5)

(defvar --ecc-auto-response-use-idle-timer nil)

(defvar --ecc-auto-response--periodic-timer nil)

(defvar --ecc-auto-response-periodic-interval 300.0)

(defvar --ecc-auto-response-periodic-enabled t)

(defvar --ecc-auto-response-max-buffers-per-cycle 3)

(defvar --ecc-auto-response--buffer-rotation-index 0)

(defvar --ecc-auto-response-verbose-logging nil)

(defvar --ecc-auto-response-skip-unchanged-buffers t)

(defvar --ecc-auto-response-same-state-delay 1.5)

(defvar --ecc-auto-response-burst-window 3)

(defvar --ecc-auto-response-burst-limit 10)

(defvar --ecc-auto-response-safe-interval 1.0)

(defvar --ecc-auto-response-responses nil)

(defvar-local --ecc-auto-response--enabled nil)

(defvar-local --ecc-auto-response--last-state nil)

(defvar-local --ecc-auto-response--last-time 0)

(defvar-local --ecc-auto-response--response-timestamps nil)

(defvar-local --ecc-auto-response--sent-positions nil)

(defvar-local --ecc-auto-response--last-content-hash nil)

(defvar-local --ecc-auto-response--last-periodic-time 0)

(defvar --ecc-auto-response--sending-p nil
  "Non-nil while actively sending a response.
Prevents re-entrant processing during `sit-for' delays.")

(defvar --ecc-auto-response--sending-p-timestamp 0
  "Timestamp when `--ecc-auto-response--sending-p' was set to t.
Used by watchdog to detect stuck sending state.")

(defvar --ecc-auto-response-send-retry-max 8
  "Maximum retries if sent command was not accepted (state unchanged).")

(defvar --ecc-auto-response-send-verify-delay 2.0
  "Seconds to wait before checking if sent command was accepted.")

(defvar --ecc-auto-response-sending-timeout 30.0
  "Maximum seconds the sending-p guard can remain active.
After this, watchdog forcibly clears it to prevent permanent lockout.")

(defvar-local --ecc-auto-response--state-first-seen-time nil
  "Timestamp when the current actionable state was first detected.
Reset when state changes.  Used to detect stuck states.")

(defvar-local --ecc-auto-response--state-first-seen-state nil
  "The state that was first seen at `--ecc-auto-response--state-first-seen-time'.")

(defvar --ecc-auto-response-stuck-state-threshold 15.0
  "Seconds an actionable state can persist before watchdog forces a re-send.
If a :y/n or :y/y/n state persists this long, the send clearly failed.")

;; 2. Main Timer Management
;; ----------------------------------------

(defun --ecc-auto-response--start-timer ()
  "Start the auto-response timer (regular or idle based on config)."
  (when --ecc-auto-response--timer
    (cancel-timer --ecc-auto-response--timer))
  (setq --ecc-auto-response--timer
        (if --ecc-auto-response-use-idle-timer
            (run-with-idle-timer --ecc-auto-response-interval t
                                 '--ecc-auto-response--process-all-buffers)
          (run-with-timer 0 --ecc-auto-response-interval
                          '--ecc-auto-response--process-all-buffers)))
  (--ecc-debug-message "Auto-response %s timer started"
                       (if --ecc-auto-response-use-idle-timer "idle"
                         "regular"))
  (unless --ecc-auto-response-use-idle-timer
    (run-with-timer 0.1 nil '--ecc-auto-response--process-all-buffers)))

(defun --ecc-auto-response--stop-timer ()
  "Stop the auto-response timer."
  (when --ecc-auto-response--timer
    (cancel-timer --ecc-auto-response--timer)
    (setq --ecc-auto-response--timer nil))
  (--ecc-debug-message "Auto-response timer stopped"))

;; 3. Periodic Timer Management
;; ----------------------------------------

(defun --ecc-auto-response--start-periodic-timer ()
  "Start global periodic timer for all registered buffers."
  (when --ecc-auto-response--periodic-timer
    (cancel-timer --ecc-auto-response--periodic-timer))
  (setq --ecc-auto-response--periodic-timer
        (run-with-timer --ecc-auto-response-periodic-interval
                        --ecc-auto-response-periodic-interval
                        (lambda ()
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
          (unless (eq current-state :running)
            (when (>=
                   (- current-time
                      --ecc-auto-response--last-periodic-time)
                   --ecc-auto-response-periodic-interval)
              (--ecc-debug-message
               "Sending periodic return to buffer %s"
               (buffer-name buffer))
              (setq-local --ecc-auto-response--last-periodic-time
                          current-time)
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

;; 4. Processing Functions
;; ----------------------------------------

(defun --ecc-auto-response--process-all-buffers ()
  "Process registered buffers for auto-response.
Skips processing when a send is in progress to prevent re-entrant chattering.
Watchdog: forcibly clears stuck sending-p after timeout."
  ;; Watchdog: clear stuck sending-p guard
  (when (and --ecc-auto-response--sending-p
             (>
	      (- (float-time) --ecc-auto-response--sending-p-timestamp)
              --ecc-auto-response-sending-timeout))
    (--ecc-debug-message
     "WATCHDOG: sending-p stuck for %.0fs, forcibly clearing"
     (- (float-time) --ecc-auto-response--sending-p-timestamp))
    (setq --ecc-auto-response--sending-p nil))
  (unless --ecc-auto-response--sending-p
    (--ecc-auto-response-cleanup-registry)
    (let* ((all-buffers (--ecc-auto-response-get-registered-buffers))
           (total-count (length all-buffers))
           (buffers-to-process
            (if (and --ecc-auto-response-max-buffers-per-cycle
                     (> total-count
			--ecc-auto-response-max-buffers-per-cycle))
		(let* ((start-idx (mod
                                   --ecc-auto-response--buffer-rotation-index
                                   total-count))
                       (end-idx (min (+ start-idx
					--ecc-auto-response-max-buffers-per-cycle)
                                     total-count))
                       (selected
			(cl-subseq all-buffers start-idx end-idx)))
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
              (when --ecc-auto-response-verbose-logging
		(--ecc-debug-message
		 "Timer tick: processing all %d buffers"
		 total-count))
              all-buffers)))
      (dolist (buffer buffers-to-process)
	(when (buffer-live-p buffer)
          (--ecc-auto-response--process-buffer buffer))))))

(defun --ecc-auto-response--process-buffer (buffer)
  "Process BUFFER for auto-response.
Tracks state duration; forces re-send if an actionable state persists too long."
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
        (setq-local --ecc-auto-response--last-content-hash
                    content-hash)
        (let ((state (--ecc-state-detection-detect)))
          ;; Track state duration for watchdog
          (if
	      (and state
		   (eq state
		       --ecc-auto-response--state-first-seen-state))
              ;; Same state persists -- check if stuck
              (when (and
		     (memq state
			   '(:y/n :y/y/n :waiting :initial-waiting))
                     --ecc-auto-response--state-first-seen-time
                     (> (- (float-time)
                           --ecc-auto-response--state-first-seen-time)
                        --ecc-auto-response-stuck-state-threshold))
                ;; State has persisted too long -- force re-send
                (--ecc-debug-message
                 "WATCHDOG: state %s stuck for %.0fs, forcing re-send"
                 state (- (float-time)
                          --ecc-auto-response--state-first-seen-time))
                ;; Reset tracking so it tries again
                (setq-local --ecc-auto-response--state-first-seen-time
                            (float-time))
                (setq-local --ecc-auto-response--sent-positions nil)
                (--ecc-auto-response--send-response state buffer))
            ;; State changed -- reset tracking
            (setq-local --ecc-auto-response--state-first-seen-state
			state)
            (setq-local --ecc-auto-response--state-first-seen-time
                        (when state (float-time))))
          ;; Normal processing
          (when (or (not content-unchanged)
                    (memq state '(:waiting :initial-waiting)))
            (when state
              (--ecc-state-detection-flash-all-patterns buffer)
              (when --ecc-auto-response-verbose-logging
                (--ecc-debug-message "Processing buffer %s: state=%s"
                                     (buffer-name buffer) state))
              (when --ecc-auto-response-verbose-logging
                (ecc-auto-response-log-state-detection state
                                                       buffer-content))
              (cond
               ((eq state :running)
                (when --ecc-auto-response-verbose-logging
                  (--ecc-debug-message
                   "Claude is running, skipping auto-response")
                  (ecc-auto-response-log 'info
                                         "Claude is running, skipping auto-response")))
               ((not (--ecc-auto-response--already-sent-p))
                (when --ecc-auto-response-verbose-logging
                  (--ecc-debug-message
                   "State detected, checking throttle for %s" state))
                (unless (--ecc-auto-response--should-throttle-p state)
                  (when --ecc-auto-response-verbose-logging
                    (--ecc-debug-message
                     "Not throttled, sending response for %s" state))
                  (--ecc-auto-response--send-response state buffer)))))))))))

;; 5. Throttle Detection
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
     ((and (eq state --ecc-auto-response--last-state)
           (< (- current-time --ecc-auto-response--last-time)
              --ecc-auto-response-same-state-delay))
      (setq throttle-reason (format "Same state within %s seconds"
                                    --ecc-auto-response-same-state-delay))
      (ecc-auto-response-log-throttle state throttle-reason)
      t)
     ((let
          ((window-start
            (- current-time --ecc-auto-response-burst-window)))
        (let ((recent-count (cl-count-if (lambda (timestamp)
                                           (>= timestamp
                                               window-start))
                                         --ecc-auto-response--response-timestamps)))
          (--ecc-debug-message
           "Recent responses: %d (burst-limit: %d)"
           recent-count
           --ecc-auto-response-burst-limit)
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

;; 6. Response Detection
;; ----------------------------------------

(defun --ecc-auto-response--already-sent-p ()
  "Check if we've already sent a response near current position."
  (let ((current-pos (point-max))
        (current-state (--ecc-state-detection-detect))
        (threshold 100))
    (if (not (eq current-state --ecc-auto-response--last-state))
        (progn
          (--ecc-debug-message
           "State changed from %s to %s, allowing response"
           --ecc-auto-response--last-state current-state)
          nil)
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

;; 7. Response Sending
;; ----------------------------------------

(defun --ecc-auto-response--send-response (state buffer)
  "Send appropriate response for STATE in BUFFER.
Sets `--ecc-auto-response--sending-p' to prevent re-entrant processing.
Retries up to `--ecc-auto-response-send-retry-max' times if state unchanged."
  (let ((response (if (and
                       (fboundp
                        'ecc-encouragement-get-phrase-for-state)
                       (memq state '(:waiting :initial-waiting)))
                      (ecc-encouragement-get-phrase-for-state state)
                    (cdr (assq state --ecc-auto-response-responses)))))
    (when response
      (with-current-buffer buffer
        (--ecc-auto-response--update-tracking state))
      (setq --ecc-auto-response--sending-p t
            --ecc-auto-response--sending-p-timestamp (float-time))
      (unwind-protect
          (progn
            (--ecc-auto-response--send-to-buffer buffer response state)
            ;; Verify state changed; retry return if stuck
            (--ecc-auto-response--verify-send buffer state))
        ;; ALWAYS clear sending-p, even on error
        (setq --ecc-auto-response--sending-p nil))
      (when ecc-auto-response-running-beep-enabled
        (--ecc-auto-response--notify-sent))
      (when (fboundp 'ecc-auto-periodical-setup-hook)
        (with-current-buffer buffer
          (ecc-auto-periodical-setup-hook))))))

(defun --ecc-auto-response--verify-send (buffer original-state)
  "Verify that BUFFER accepted the sent command by checking state change.
If state is still ORIGINAL-STATE after delay, retry sending return."
  (when (and (buffer-live-p buffer)
             (memq original-state '(:waiting :initial-waiting)))
    (let ((retries 0))
      (while (< retries --ecc-auto-response-send-retry-max)
        (sit-for --ecc-auto-response-send-verify-delay)
        (let ((new-state (with-current-buffer buffer
                           (--ecc-state-detection-detect))))
          (if (not (eq new-state original-state))
              (progn
                (--ecc-debug-message
                 "Send verified: state changed %s -> %s"
                 original-state new-state)
                (setq retries --ecc-auto-response-send-retry-max))
            (setq retries (1+ retries))
            (--ecc-debug-message
             "Send retry %d/%d: state still %s, resending return"
             retries --ecc-auto-response-send-retry-max
             original-state)
            (with-current-buffer buffer
              (cond
               ((derived-mode-p 'vterm-mode)
                (when (fboundp 'vterm-send-return)
                  (vterm-send-return)))
               ((derived-mode-p 'comint-mode)
                (comint-send-input))
               (t
                (insert "\n"))))))))))

(defun --ecc-auto-response--update-tracking (state)
  "Update tracking variables for STATE."
  (let ((current-time (float-time)))
    (setq-local --ecc-auto-response--last-state state)
    (setq-local --ecc-auto-response--last-time current-time)
    (push current-time --ecc-auto-response--response-timestamps)
    (push (cons (point-max) current-time)
          --ecc-auto-response--sent-positions)
    (setq --ecc-auto-response--sent-positions
          (cl-remove-if (lambda (pos-time)
                          (> (- current-time (cdr pos-time)) 60))
                        --ecc-auto-response--sent-positions))))

(defun --ecc-auto-response--send-to-buffer (buffer text state)
  "Send TEXT to BUFFER for STATE."
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
                             (insert "\n"))))))
      (sit-for --ecc-auto-response-safe-interval)
      (funcall text-sender)
      (sit-for --ecc-auto-response-safe-interval)
      (funcall return-sender)
      (sit-for --ecc-auto-response-safe-interval)
      (--ecc-auto-response--show-encouragement buffer text)))
  (--ecc-debug-message "Sent response to %s: %s"
                       (buffer-name buffer)
                       text))

(provide 'ecc-auto-response-core)

(when
    (not load-file-name)
  (message "ecc-auto-response-core.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
