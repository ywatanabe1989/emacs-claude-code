;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-03-20 08:15:49>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-auto-response-retry.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)


;;; Commentary:
;;; Retry and verification logic for ecc-auto-response.
;;; Separated from core to keep each module focused.
;;; Implements multi-tier retry: immediate verify, delayed retry, watchdog fallback.

;; 1. Dependencies
;; ----------------------------------------

(require 'ecc-debug)
(require 'ecc-state-detection)

;; Function stubs
(declare-function --ecc-auto-response--show-encouragement
		          "ecc-auto-response-ui" (buffer text))

;; 2. Configuration
;; ----------------------------------------

(defcustom --ecc-auto-response-accumulation-max 1
  "Maximum times a response may appear in buffer tail before suppressing.
When the response text already appears this many times in the last
`--ecc-state-detection-buffer-size' characters, sending is skipped.
Prevents queuing identical commands when the CLI has not consumed them."
  :type 'integer
  :group 'ecc)

(defcustom --ecc-auto-response-send-retry-max 8
  "Maximum retries for :waiting/:initial-waiting states."
  :type 'integer
  :group 'ecc)

(defcustom --ecc-auto-response-send-verify-delay 2.0
  "Seconds to wait before verifying if sent command was accepted."
  :type 'float
  :group 'ecc)

(defcustom --ecc-auto-response-permission-retry-max 1
  "Maximum retries for permission prompts (:y/y/n, :y/n).
Fewer than :waiting retries since these need full text re-send."
  :type 'integer
  :group 'ecc)

(defcustom --ecc-auto-response-permission-retry-delay 1.5
  "Seconds between permission prompt retries."
  :type 'float
  :group 'ecc)

;; 3. Variables
;; ----------------------------------------

(defvar --ecc-auto-response-responses nil
  "Stub; real value defined in ecc-auto-response.el.")

(defvar --ecc-auto-response-safe-interval 1.0
  "Stub; real value defined in ecc-auto-response.el.")

;; 4. Main Entry Points
;; ----------------------------------------

(defun --ecc-auto-response--verify-send (buffer original-state)
  "Verify that BUFFER accepted the sent command by checking state change.
Multi-tier retry:
  - Permission prompts (:y/y/n :y/n): re-send full text + return
  - Waiting states: re-send just return
  - Other states: no retry"
  (when (and (buffer-live-p buffer)
             (memq original-state '(:y/y/n :y/n :suggestion)))
    (let* ((is-permission (memq original-state '(:y/y/n :y/n)))
           (max-retries (if is-permission
                            --ecc-auto-response-permission-retry-max
                          --ecc-auto-response-send-retry-max))
           (delay (if is-permission
                      --ecc-auto-response-permission-retry-delay
                    --ecc-auto-response-send-verify-delay))
           (retries 0))
      (while (< retries max-retries)
        (sit-for delay)
        (if (not (buffer-live-p buffer))
            (setq retries max-retries)
          (let ((new-state (with-current-buffer buffer
                             (--ecc-state-detection-detect))))
            (if (not (eq new-state original-state))
                (progn
                  (--ecc-debug-message
                   "Send verified: state changed %s -> %s"
                   original-state new-state)
                  (setq retries max-retries))
              (setq retries (1+ retries))
              (--ecc-debug-message
               "Send retry %d/%d: state still %s, resending"
               retries max-retries original-state)
              (with-current-buffer buffer
                (--ecc-auto-response--retry-send original-state)))))))))

;; 5. Accumulation Detection
;; ----------------------------------------

(defun --ecc-auto-response--response-accumulated-p (buffer response)
  "Return t if RESPONSE already appears in BUFFER tail too many times.
Counts occurrences in the last `--ecc-state-detection-buffer-size'
characters.  Returns t when count >= `--ecc-auto-response-accumulation-max'."
  (when (and (buffer-live-p buffer) response)
    (with-current-buffer buffer
      (let* ((text (buffer-substring-no-properties
                    (max (point-min)
                         (- (point-max)
                            --ecc-state-detection-buffer-size))
                    (point-max)))
             (normalized (--ecc-state-detection--normalize-text text))
             (pattern (regexp-quote
                       (--ecc-state-detection--normalize-text response)))
             (count 0)
             (start 0))
        (while (string-match pattern normalized start)
          (setq count (1+ count)
                start (1+ (match-beginning 0))))
        (when (>= count --ecc-auto-response-accumulation-max)
          (--ecc-debug-message
           "Accumulation detected: \"%s\" appears %d times (max %d)"
           response count --ecc-auto-response-accumulation-max)
          t)))))

;; 6. Core Functions
;; ----------------------------------------

(defun --ecc-auto-response--retry-send (state)
  "Retry sending the appropriate response for STATE in current buffer.
Permission prompts: re-send full text + return.
Other states: re-send just return."
  (let ((response (cdr (assq state --ecc-auto-response-responses))))
    (cond
     ;; Permission prompts: re-send the actual response text + return
     ((and (memq state '(:y/y/n :y/n)) response)
      (--ecc-auto-response--send-text-and-return response))
     ;; Other states: just send return
     (t
      (--ecc-auto-response--send-return)))))

(defun --ecc-auto-response--send-text-and-return (text)
  "Send TEXT followed by return in current buffer.
Respects buffer mode (vterm, comint, etc.)."
  (cond
   ((derived-mode-p 'vterm-mode)
    (when (fboundp 'vterm-send-string)
      (vterm-send-string text)
      (sit-for 0.5)
      (when (fboundp 'vterm-send-return)
        (vterm-send-return))))
   ((derived-mode-p 'comint-mode)
    (goto-char (point-max))
    (insert text)
    (comint-send-input))
   (t
    (goto-char (point-max))
    (insert text "\n"))))

(defun --ecc-auto-response--send-return ()
  "Send just a return/enter in current buffer."
  (cond
   ((derived-mode-p 'vterm-mode)
    (when (fboundp 'vterm-send-return)
      (vterm-send-return)))
   ((derived-mode-p 'comint-mode)
    (comint-send-input))
   (t
    (insert "\n"))))

(defun --ecc-auto-response--send-to-buffer (buffer text state)
  "Send TEXT to BUFFER for STATE.
Rechecks state before sending to avoid interrupting user typing."
  (with-current-buffer buffer
    (sit-for --ecc-auto-response-safe-interval)
    ;; Recheck: abort if user started typing during the delay
    (let ((current-state (--ecc-state-detection-detect)))
      (when (eq current-state :user-typing)
        (--ecc-debug-message
         "Aborting send: user started typing during delay")
        (throw 'abort-send nil))
      (--ecc-auto-response--send-text-and-return text)
      (sit-for --ecc-auto-response-safe-interval)
      (--ecc-auto-response--show-encouragement buffer text)))
  (--ecc-debug-message "Sent response to %s: %s"
                       (buffer-name buffer) text))

;; 6. Provide
;; ----------------------------------------

(when (not load-file-name)
  (message "ecc-auto-response-retry.el loaded."))


(provide 'ecc-auto-response-retry)

(when
    (not load-file-name)
  (message "ecc-auto-response-retry.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))