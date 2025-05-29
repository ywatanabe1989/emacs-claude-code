;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-29 05:21:06>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-auto-response.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;; 1. Dependencies
;; ----------------------------------------

(require 'cl-lib)
(require 'ecc-debug)
(require 'ecc-state-detection)
(require 'vterm nil t)  ; Optional dependency

;; Declare function to avoid compiler warnings
(declare-function ecc-auto-periodical-setup-hook "ecc-auto-periodical" ())


;; 2. Configuration
;; ----------------------------------------

;; Define the face globally
(defface ecc-auto-indicator-face
  '((t :background "#ff8c00" :foreground "#ffffff" :weight bold))
  "Face for AUTO indicator in mode-line."
  :group 'ecc)

(defcustom --ecc-auto-response-interval 3.0
  "Interval in seconds for auto-response timer checks."
  :type 'float
  :group 'ecc)

(defcustom --ecc-auto-response-safe-interval 1.0
  "Safety delay in seconds before and after sending responses."
  :type 'float
  :group 'ecc)

(defcustom --ecc-auto-response-mode-line-color "#ff8c00"
  "Background color for mode-line when auto-response is enabled."
  :type 'color
  :group 'ecc)

(defcustom --ecc-auto-response-throttle-duration 5.0
  "Minimum seconds between auto-responses to the same state."
  :type 'float
  :group 'ecc)

(defcustom --ecc-auto-response-accumulation-threshold 5
  "Maximum number of responses allowed within accumulation window."
  :type 'number
  :group 'ecc)

(defcustom --ecc-auto-response-accumulation-window 3.0
  "Time window in seconds for accumulation detection."
  :type 'number
  :group 'ecc)

(defcustom --ecc-auto-response-responses
  '((:y/n . "1")
    (:y/y/n . "2")
    (:waiting . "/user:auto")
    (:initial-waiting . "/user:understand-guidelines"))
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

(defvar-local --ecc-auto-response--accumulation-count 0
  "Counter for responses sent within current accumulation window.")

(defvar-local --ecc-auto-response--accumulation-start-time 0
  "Start time of current accumulation tracking window.")

(defvar-local --ecc-auto-response--sent-positions nil
  "List of buffer positions where responses have been sent.
Each element is (POSITION . TIMESTAMP).")

(defvar-local --ecc-auto-response--original-mode-line nil
  "Original mode-line-format before AUTO indicator was added.")

(defvar-local --ecc-auto-response--last-periodic-time 0
  "Timestamp of last periodic return sent.")

(defvar-local --ecc-auto-response--periodic-timer nil
  "Buffer-local timer for periodic return sending.")


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
      ;; Update mode-line
      (--ecc-auto-response--update-mode-line)
      ;; Start periodic timer if enabled
      (when --ecc-auto-response-periodic-enabled
        (--ecc-auto-response--start-periodic-timer buf)))
    (unless --ecc-auto-response--timer
      (--ecc-auto-response--start-timer))
    ;; Play buzzer sound
    (beep)
    (--ecc-debug-message "Auto-response enabled for buffer: %s"
                         (buffer-name buf))))

(defun --ecc-auto-response-disable-buffer (&optional buffer)
  "Disable auto-response for BUFFER."
  (let ((buf (or buffer (current-buffer))))
    (--ecc-auto-response-unregister-buffer buf)
    (with-current-buffer buf
      (setq-local --ecc-auto-response--enabled nil)
      ;; Stop periodic timer
      (--ecc-auto-response--stop-periodic-timer)
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
  "Start the auto-response timer."
  (when --ecc-auto-response--timer
    (cancel-timer --ecc-auto-response--timer))
  (setq --ecc-auto-response--timer
        (run-with-timer 0 --ecc-auto-response-interval
                        '--ecc-auto-response--process-all-buffers))
  (--ecc-debug-message "Auto-response timer started"))

(defun --ecc-auto-response--stop-timer ()
  "Stop the auto-response timer."
  (when --ecc-auto-response--timer
    (cancel-timer --ecc-auto-response--timer)
    (setq --ecc-auto-response--timer nil))
  (--ecc-debug-message "Auto-response timer stopped"))

(defun --ecc-auto-response--start-periodic-timer (buffer)
  "Start periodic timer for BUFFER."
  (with-current-buffer buffer
    (when --ecc-auto-response--periodic-timer
      (cancel-timer --ecc-auto-response--periodic-timer))
    (setq-local --ecc-auto-response--periodic-timer
                (run-with-timer --ecc-auto-response-periodic-interval
                                --ecc-auto-response-periodic-interval
                                '--ecc-auto-response--send-periodic-return
                                buffer))
    (--ecc-debug-message "Started periodic timer for buffer %s (interval: %s seconds)"
                         (buffer-name buffer)
                         --ecc-auto-response-periodic-interval)))

(defun --ecc-auto-response--stop-periodic-timer ()
  "Stop the buffer-local periodic timer."
  (when --ecc-auto-response--periodic-timer
    (cancel-timer --ecc-auto-response--periodic-timer)
    (setq-local --ecc-auto-response--periodic-timer nil)
    (--ecc-debug-message "Stopped periodic timer for buffer %s" (buffer-name))))

(defun --ecc-auto-response--send-periodic-return (buffer)
  "Send periodic return to BUFFER if appropriate."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and --ecc-auto-response--enabled
                 --ecc-auto-response-periodic-enabled)
        (let ((current-time (float-time)))
          ;; Only send if enough time has passed since last periodic send
          (when (>= (- current-time --ecc-auto-response--last-periodic-time)
                    --ecc-auto-response-periodic-interval)
            (--ecc-debug-message "Sending periodic return to buffer %s" (buffer-name buffer))
            (setq-local --ecc-auto-response--last-periodic-time current-time)
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
            (--ecc-debug-message "Periodic return sent to %s" (buffer-name buffer))))))))


;; 8. Processing functions
;; ----------------------------------------

(defun --ecc-auto-response--process-all-buffers ()
  "Process all registered buffers for auto-response."
  (--ecc-auto-response-cleanup-registry)
  (dolist (buffer (--ecc-auto-response-get-registered-buffers))
    (when (buffer-live-p buffer)
      (--ecc-auto-response--process-buffer buffer))))

(defun --ecc-auto-response--process-buffer (buffer)
  "Process BUFFER for auto-response."
  (with-current-buffer buffer
    (when --ecc-auto-response--enabled
      (let ((state (--ecc-state-detection-detect)))
        (--ecc-debug-message "Processing buffer %s: state=%s" (buffer-name buffer) state)
        (when (and state
                   (not (--ecc-auto-response--already-sent-p)))
          (--ecc-debug-message "State detected, checking throttle for %s" state)
          (unless (--ecc-auto-response--should-throttle-p state)
            (--ecc-debug-message "Not throttled, sending response for %s" state)
            (--ecc-auto-response--send-response state buffer)))))))


;; 9. Throttle detection functions
;; ----------------------------------------

(defun --ecc-auto-response--should-throttle-p (state)
  "Check if auto-response for STATE should be throttled."
  (let ((current-time (float-time)))
    (--ecc-debug-message "Throttle check: state=%s, last-state=%s, time-diff=%s, throttle-duration=%s"
                         state --ecc-auto-response--last-state
                         (- current-time --ecc-auto-response--last-time)
                         --ecc-auto-response-throttle-duration)
    (or
     (and (eq state --ecc-auto-response--last-state)
          (< (- current-time --ecc-auto-response--last-time)
             --ecc-auto-response-throttle-duration))
     (--ecc-auto-response--accumulation-detected-p))))

(defun --ecc-auto-response--accumulation-detected-p ()
  "Check if auto-response accumulation has been detected."
  (let ((current-time (float-time)))
    (when (>
           (- current-time
              --ecc-auto-response--accumulation-start-time)
           --ecc-auto-response-accumulation-window)
      (setq-local --ecc-auto-response--accumulation-count 0)
      (setq-local --ecc-auto-response--accumulation-start-time 0))
    (>= --ecc-auto-response--accumulation-count
        --ecc-auto-response-accumulation-threshold)))


;; 10. Response detection functions
;; ----------------------------------------

(defun --ecc-auto-response--already-sent-p ()
  "Check if we've already sent a response near current position."
  (let ((current-pos (point-max))
        (threshold 100))  ; Consider positions within 100 chars as "same"
    (let ((result (cl-some (lambda (pos-time)
                             (< (abs (- current-pos (car pos-time))) threshold))
                           --ecc-auto-response--sent-positions)))
      (--ecc-debug-message "Already sent check: pos=%d, sent-positions=%s, result=%s" 
                           current-pos --ecc-auto-response--sent-positions result)
      result)))


;; 11. Response sending functions
;; ----------------------------------------

(defun --ecc-auto-response--send-response (state buffer)
  "Send appropriate response for STATE in BUFFER."
  (let ((response (cdr (assq state --ecc-auto-response-responses))))
    (when response
      (--ecc-auto-response--send-to-buffer buffer response)
      (--ecc-auto-response--update-tracking state)
      (when (fboundp '--ecc-notification-notify)
        (--ecc-notification-notify state buffer))
      ;; Trigger auto-periodical check if available
      (when (fboundp 'ecc-auto-periodical-setup-hook)
        (with-current-buffer buffer
          (ecc-auto-periodical-setup-hook))))))

(defun --ecc-auto-response--update-tracking (state)
  "Update tracking variables for STATE."
  (let ((current-time (float-time)))
    (setq-local --ecc-auto-response--last-state state)
    (setq-local --ecc-auto-response--last-time current-time)
    (when (= --ecc-auto-response--accumulation-count 0)
      (setq-local --ecc-auto-response--accumulation-start-time
                  current-time))
    (setq-local --ecc-auto-response--accumulation-count
                (1+ --ecc-auto-response--accumulation-count))
    ;; Record the position where we sent the response
    (push (cons (point-max) current-time) --ecc-auto-response--sent-positions)
    ;; Clean up old sent positions (older than 60 seconds)
    (setq --ecc-auto-response--sent-positions
          (cl-remove-if (lambda (pos-time)
                          (> (- current-time (cdr pos-time)) 60))
                        --ecc-auto-response--sent-positions))))

(defun --ecc-auto-response--send-to-buffer (buffer text)
  "Send TEXT to BUFFER."
  (with-current-buffer buffer
    (cond
     ((derived-mode-p 'vterm-mode)
      (when (fboundp 'vterm-send-string)
        (sit-for --ecc-auto-response-safe-interval)        
        (vterm-send-string text)
        (sit-for --ecc-auto-response-safe-interval)
        (vterm-send-return)))
     ((derived-mode-p 'comint-mode)
      (goto-char (point-max))
      (sit-for --ecc-auto-response-safe-interval)              
      (insert text)
      (sit-for --ecc-auto-response-safe-interval)              
      (comint-send-input))
     (t
      (goto-char (point-max))
      (insert text)
      (sit-for --ecc-auto-response-safe-interval)
      (insert "\n")        
      (sit-for --ecc-auto-response-safe-interval)       )))
  (--ecc-debug-message "Sent response to %s: %s" (buffer-name buffer)
                       text)
  ;; Allow buffer to update before next check
  (sit-for --ecc-auto-response-safe-interval))



;; 12. Mode-line functions
;; ----------------------------------------

(defun --ecc-auto-response--update-mode-line ()
  "Update mode-line to show auto-response status."
  (if --ecc-auto-response--enabled
      ;; Add AUTO indicator to mode-line
      (progn
        ;; Store the current mode-line-format before modifying
        (unless (local-variable-p '--ecc-auto-response--original-mode-line)
          (setq-local --ecc-auto-response--original-mode-line
                      (if (local-variable-p 'mode-line-format)
                          mode-line-format
                        (default-value 'mode-line-format))))
        
        ;; Create a face for the indicator
        (face-spec-set 'ecc-auto-indicator-face
                       `((t :background ,--ecc-auto-response-mode-line-color
                            :foreground "#ffffff"
                            :weight bold))
                       'face-defface-spec)
        
        (unless (and (listp mode-line-format)
                     (member '(:eval (when --ecc-auto-response--enabled " [AUTO] ")) mode-line-format))
          ;; Use :eval to dynamically show/hide based on buffer-local variable
          (let ((indicator '(:eval (when --ecc-auto-response--enabled
                                     (propertize " [AUTO] "
                                                 'face 'ecc-auto-indicator-face
                                                 'help-echo "Auto-response is active")))))
            ;; Use the stored original format
            (let ((original --ecc-auto-response--original-mode-line))
              (if (listp original)
                  (let ((new-format (copy-sequence original))
                        (buffer-id-pos (cl-position 'mode-line-buffer-identification original)))
                    (if buffer-id-pos
                        ;; Insert after buffer identification
                        (setq mode-line-format
                              (append (cl-subseq new-format 0 (1+ buffer-id-pos))
                                      (list indicator)
                                      (cl-subseq new-format (1+ buffer-id-pos))))
                      ;; If no buffer-id found, prepend
                      (setq mode-line-format (cons indicator new-format))))
                ;; If original is not a list, make it one
                (setq mode-line-format (list indicator original))))))
    ;; Remove AUTO indicator and restore original
    (when (local-variable-p '--ecc-auto-response--original-mode-line)
      (setq mode-line-format --ecc-auto-response--original-mode-line)
      (kill-local-variable '--ecc-auto-response--original-mode-line)))
  (force-mode-line-update)))

(defun --ecc-auto-response-refresh-all-mode-lines ()
  "Refresh mode-lines for all buffers with auto-response enabled."
  (interactive)
  ;; First, ensure the face is properly defined with current color
  (face-spec-set 'ecc-auto-indicator-face
                 `((t :background ,--ecc-auto-response-mode-line-color
                      :foreground "#ffffff"
                      :weight bold))
                 'face-defface-spec)
  ;; Refresh all registered buffers
  (dolist (buffer (--ecc-auto-response-get-registered-buffers))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when --ecc-auto-response--enabled
          ;; Reset mode-line to original first
          (when (local-variable-p '--ecc-auto-response--original-mode-line)
            (setq mode-line-format --ecc-auto-response--original-mode-line)
            (kill-local-variable '--ecc-auto-response--original-mode-line))
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
      (message "Face background should be: %s" --ecc-auto-response-mode-line-color))))

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
  (dolist (buffer (--ecc-auto-response-get-registered-buffers))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when --ecc-auto-response--enabled
          (if --ecc-auto-response-periodic-enabled
              (--ecc-auto-response--start-periodic-timer buffer)
            (--ecc-auto-response--stop-periodic-timer))))))
  (message "Periodic return sending %s"
           (if --ecc-auto-response-periodic-enabled "enabled" "disabled")))


(provide 'ecc-auto-response)

(when
    (not load-file-name)
  (message "ecc-auto-response.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))