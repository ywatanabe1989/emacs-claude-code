;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-06-04 08:25:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-auto-response-diagnostic.el

(defun ecc-auto-response-deep-diagnose ()
  "Deep diagnostic for auto-response issues."
  (interactive)
  (if (not --ecc-auto-response--enabled)
      (message "Auto-response is not enabled in this buffer. Use C-c C-a to enable.")
    (let* ((state (--ecc-state-detection-detect))
           (current-time (float-time))
           (already-sent (--ecc-auto-response--already-sent-p))
           (throttle-check (when state (--ecc-auto-response--should-throttle-p state)))
           (response (when state (cdr (assq state --ecc-auto-response-responses)))))
      (with-output-to-temp-buffer "*ECC Auto-Response Diagnostic*"
        (princ "=== ECC Auto-Response Deep Diagnostic ===\n\n")
        (princ (format "Buffer: %s\n" (buffer-name)))
        (princ (format "Auto-response enabled: %s\n" --ecc-auto-response--enabled))
        (princ (format "Current detected state: %s\n" (or state "none")))
        (princ (format "Expected response for state: %s\n" (or response "none")))
        (princ "\n--- Response History ---\n")
        (princ (format "Last state responded to: %s\n" (or --ecc-auto-response--last-state "none")))
        (princ (format "Time since last response: %.1f seconds\n" 
                      (- current-time --ecc-auto-response--last-time)))
        (princ (format "Already sent at current position: %s\n" already-sent))
        (princ (format "Sent positions: %s\n" --ecc-auto-response--sent-positions))
        (princ "\n--- Throttle Check ---\n")
        (princ (format "Should throttle: %s\n" throttle-check))
        (princ (format "Throttle duration: %.1f seconds\n" --ecc-auto-response-throttle-duration))
        (princ (format "Response timestamps in window: %s\n" 
                      (let ((window-start (- current-time --ecc-auto-response-accumulation-window)))
                        (cl-count-if (lambda (ts) (>= ts window-start))
                                     --ecc-auto-response--response-timestamps))))
        (princ (format "Accumulation threshold: %d responses in %.1f seconds\n" 
                      --ecc-auto-response-accumulation-threshold
                      --ecc-auto-response-accumulation-window))
        (princ "\n--- Timer Status ---\n")
        (princ (format "Global timer active: %s\n" 
                      (if --ecc-auto-response--timer "yes" "no")))
        (princ (format "Timer interval: %.1f seconds\n" --ecc-auto-response-interval))
        (princ (format "Periodic timer active: %s\n" 
                      (if --ecc-auto-response--periodic-timer "yes" "no")))
        (princ "\n--- Buffer Content ---\n")
        (princ "Last 200 characters:\n")
        (princ (format "%S\n" (buffer-substring-no-properties 
                               (max (point-min) (- (point-max) 200))
                               (point-max))))
        (princ "\n--- Decision Tree ---\n")
        (cond
         ((not state)
          (princ "❌ No state detected - no response will be sent\n"))
         ((eq state :running)
          (princ "❌ Claude is running - no response will be sent\n"))
         (already-sent
          (princ "❌ Already sent response at this position - no response will be sent\n"))
         (throttle-check
          (princ "❌ Response is throttled - no response will be sent\n"))
         (t
          (princ "✓ All checks passed - response SHOULD be sent\n")
          (princ (format "Response to send: %s\n" response))))))))

(defun ecc-auto-response-force-send ()
  "Force send auto-response ignoring throttle."
  (interactive)
  (let ((state (--ecc-state-detection-detect)))
    (if state
        (progn
          (--ecc-auto-response--send-response state (current-buffer))
          (message "Forced response sent for state: %s" state))
      (message "No state detected to respond to"))))

(defun ecc-auto-response-reset-tracking ()
  "Reset all tracking variables for current buffer."
  (interactive)
  (setq-local --ecc-auto-response--last-state nil)
  (setq-local --ecc-auto-response--last-time 0)
  (setq-local --ecc-auto-response--response-timestamps nil)
  (setq-local --ecc-auto-response--sent-positions nil)
  (setq-local --ecc-auto-response--last-periodic-time 0)
  (message "Reset auto-response tracking for buffer %s" (buffer-name)))

(defun ecc-auto-response-test-timer ()
  "Manually trigger the auto-response timer processing."
  (interactive)
  (message "Testing auto-response timer...")
  (--ecc-auto-response--process-all-buffers)
  (message "Timer processing complete"))

(defun ecc-auto-response-check-timer-status ()
  "Check if the auto-response timer is running."
  (interactive)
  (message "Timer status: %s (interval: %.1f seconds)"
           (if --ecc-auto-response--timer "RUNNING" "STOPPED")
           --ecc-auto-response-interval)
  (when --ecc-auto-response--timer
    (message "Timer object: %s" --ecc-auto-response--timer)))

(provide 'ecc-auto-response-diagnostic)