;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-03-20 04:07:48>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-state-speaking-flash-feedback.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)


;;; Commentary:
;;; Separate system from state detection.
;;; Scans vterm buffer for speaking patterns (e.g., MCP audio_speak)
;;; and flashes the mode-line green for a fixed duration.

;; 1. Dependencies
;; ----------------------------------------

(require 'ecc-debug)
(require 'ecc-state-detection)

;; Function stubs
(declare-function --ecc-auto-response-get-registered-buffers
		          "ecc-auto-response" ())

;; 2. Configuration
;; ----------------------------------------

(defcustom ecc-speaking-flash-patterns
  '("scitex - audio_speak")
  "Patterns in the vterm buffer that indicate the agent is speaking.
When any of these appear at a new position in the buffer tail,
the mode-line flashes green for `ecc-speaking-flash-duration' seconds."
  :type '(repeat string)
  :group 'ecc)

(defcustom ecc-speaking-flash-duration 7.0
  "Seconds to flash the mode-line green when speaking is detected."
  :type 'float
  :group 'ecc)

(defcustom ecc-speaking-flash-scan-size 512
  "Number of characters from buffer end to scan for speaking patterns."
  :type 'integer
  :group 'ecc)

;; 3. Variables
;; ----------------------------------------

(defvar-local ecc-speaking--flash-active nil
  "Non-nil when the mode-line is flashing green for speaking.")

(defvar-local ecc-speaking--last-match-pos nil
  "Buffer position of last speaking pattern match.
Prevents re-triggering on the same occurrence.")

(defvar-local ecc-speaking--flash-timer nil
  "Timer to clear the speaking flash after duration expires.")

;; 4. Core Detection
;; ----------------------------------------

(defun ecc-speaking--scan-buffer ()
  "Scan current buffer tail for speaking patterns.
Returns the match position if a NEW occurrence is found, nil otherwise."
  (let* ((scan-end (point-max))
         (scan-start (max (point-min)
                          (- scan-end ecc-speaking-flash-scan-size)))
         (text (buffer-substring-no-properties scan-start scan-end))
         (normalized (--ecc-state-detection--normalize-text text))
         (found-pos nil))
    (dolist (pattern ecc-speaking-flash-patterns)
      (unless found-pos
        (let ((norm-pattern
               (regexp-quote
                (--ecc-state-detection--normalize-text pattern))))
          (when (string-match norm-pattern normalized)
            (let ((abs-pos (+ scan-start (match-beginning 0))))
              (unless (and ecc-speaking--last-match-pos
                           (= abs-pos ecc-speaking--last-match-pos))
                (setq found-pos abs-pos)))))))
    found-pos))

(defun ecc-speaking--trigger-flash (buffer pos)
  "Trigger speaking flash in BUFFER, recording match at POS."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local ecc-speaking--last-match-pos pos)
      (setq-local ecc-speaking--flash-active t)
      (force-mode-line-update)
      (--ecc-debug-message "Speaking flash triggered in %s"
                           (buffer-name buffer))
      ;; Cancel existing timer if any
      (when (and ecc-speaking--flash-timer
                 (timerp ecc-speaking--flash-timer))
        (cancel-timer ecc-speaking--flash-timer))
      ;; Set timer to clear flash
      (setq-local ecc-speaking--flash-timer
                  (run-with-timer
                   ecc-speaking-flash-duration nil
                   (lambda (buf)
                     (when (buffer-live-p buf)
                       (with-current-buffer buf
                         (setq-local ecc-speaking--flash-active nil)
                         (force-mode-line-update)
                         (--ecc-debug-message
                          "Speaking flash cleared in %s"
                          (buffer-name buf)))))
                   buffer)))))

;; 5. Integration with Pulse Timer
;; ----------------------------------------

(defun ecc-speaking--check-all-buffers ()
  "Check all registered buffers for speaking patterns.
Called from the pulse timer in ecc-auto-response-ui."
  (dolist (buffer (--ecc-auto-response-get-registered-buffers))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (unless ecc-speaking--flash-active
          (let ((pos (ecc-speaking--scan-buffer)))
            (when pos
              (ecc-speaking--trigger-flash buffer pos))))))))

;; 6. Mode-Line Face
;; ----------------------------------------

(defun ecc-speaking--mode-line-face (pulse-state)
  "Return face for mode-line when speaking flash is active.
PULSE-STATE alternates for the pulsing effect."
  (if pulse-state
      '(:background "#005500" :foreground "#ffffff" :weight bold)
    '(:background "#003300" :foreground "#aaffaa" :weight bold)))

;; 7. Provide
;; ----------------------------------------

(when (not load-file-name)
  (message "ecc-state-speaking-flash-feedback.el loaded."))


(provide 'ecc-state-speaking-flash-feedback)

(when
    (not load-file-name)
  (message "ecc-state-speaking-flash-feedback.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))