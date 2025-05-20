;;; -*- coding: utf-8; lexical-binding: t -*-
;;; ecc-state-detection-improved.el --- Improved Claude prompt state detection
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 20:00:00>

;;; Commentary:
;;; Enhanced Claude prompt state detection functionality.
;;; This module provides a clean, well-organized implementation
;;; of the state detection logic for Claude prompts in buffers.

;;; Code:

(require 'ecc-variables)

;;;; Customization

(defgroup ecc-state-detection nil
  "Settings for Claude prompt state detection."
  :group 'ecc
  :prefix "ecc-state-detection-")

(defcustom ecc-state-detection-buffer-size 2000
  "Number of characters to check for basic prompt detection.
This sets the maximum number of characters from the end of the buffer
that will be searched when using basic state detection methods."
  :type 'integer
  :group 'ecc-state-detection)

(defcustom ecc-state-detection-line-count 256
  "Number of lines to check for line-based prompt detection.
A larger number increases detection accuracy but may impact performance
with very large buffers. The default value of 256 is a balance between
thorough detection and performance."
  :type 'integer
  :group 'ecc-state-detection)

;;;; Core detection functions

(defun ecc-detect-state (&optional buffer)
  "Detect Claude prompt state in BUFFER (or current buffer).
Returns one of: :y/y/n, :y/n, :waiting, :initial-waiting, or nil.

This is the main function that should be used for state detection.
It automatically uses the best available detection method, prioritizing
line-based detection for accuracy when available."
  (with-current-buffer (or buffer (current-buffer))
    (or
     ;; First try line-based detection for better accuracy
     (ecc-detect-prompt-in-last-lines ecc-state-detection-line-count)
     ;; Fall back to basic detection if line detection finds nothing
     (ecc-detect-basic-state))))

(defun ecc-detect-basic-state ()
  "Basic detection of Claude prompt state using buffer content matching.
Returns :y/y/n, :y/n, :waiting, :initial-waiting, or nil."
  (let ((buffer-text (buffer-substring-no-properties 
                    (max (- (point-max) ecc-state-detection-buffer-size) (point-min))
                    (point-max))))
    (ecc-analyze-buffer-text-for-state buffer-text)))

(defun ecc-detect-prompt-in-last-lines (&optional n-lines)
  "Detect Claude prompts in the last N-LINES of the current buffer.
If N-LINES is nil, use `ecc-state-detection-line-count'.
Returns one of: :y/y/n, :y/n, :waiting, :initial-waiting, or nil."
  (let* ((lines (or n-lines ecc-state-detection-line-count))
         (buffer-lines (count-lines (point-min) (point-max)))
         (start-line (max 1 (- buffer-lines lines)))
         (start-pos (save-excursion
                      (goto-char (point-min))
                      (forward-line (1- start-line))
                      (point)))
         (buffer-text (buffer-substring-no-properties 
                      start-pos
                      (point-max))))
    (ecc-analyze-buffer-text-for-state buffer-text)))

(defun ecc-detect-prompt-in-region (start end)
  "Detect Claude prompts in region between START and END.
Returns one of: :y/y/n, :y/n, :waiting, :initial-waiting, or nil."
  (let ((buffer-text (buffer-substring-no-properties start end)))
    (ecc-analyze-buffer-text-for-state buffer-text)))

;;;; Analysis functions

(defun ecc-analyze-buffer-text-for-state (buffer-text)
  "Analyze BUFFER-TEXT to detect Claude prompt state.
Returns :y/y/n, :y/n, :waiting, :initial-waiting, or nil."
  (cond
   ;; Check for y/y/n prompts using customized pattern
   ((and (boundp 'ecc-state-prompt-y/y/n)
         ecc-state-prompt-y/y/n
         (string-match-p (regexp-quote ecc-state-prompt-y/y/n) buffer-text))
    :y/y/n)
   
   ;; Check for y/n prompts using customized pattern
   ((and (boundp 'ecc-state-prompt-y/n)
         ecc-state-prompt-y/n
         (string-match-p (regexp-quote ecc-state-prompt-y/n) buffer-text))
    :y/n)
   
   ;; Check for waiting prompts using customized patterns
   ((and (boundp 'ecc-state-prompt-waiting)
         ecc-state-prompt-waiting
         (string-match-p (regexp-quote ecc-state-prompt-waiting) buffer-text))
    :waiting)
   
   ;; Check for initial prompts
   ((and (boundp 'ecc-state-prompt-initial-waiting)
         ecc-state-prompt-initial-waiting
         (string-match-p (regexp-quote ecc-state-prompt-initial-waiting) buffer-text))
    :initial-waiting)
   
   ;; If no custom patterns match, try alternative initial waiting patterns
   ((ecc-detect-alternative-initial-waiting buffer-text)
    :initial-waiting)
   
   ;; Fallback to common patterns
   ((string-match-p "\\[Y/y/n\\]" buffer-text) :y/y/n)
   ((string-match-p "\\[y/n\\]\\|\\[Y/n\\]" buffer-text) :y/n)
   ((string-match-p "continue>\\|Continue>" buffer-text) :waiting)
   
   (t nil)))

(defun ecc-detect-alternative-initial-waiting (buffer-text)
  "Check if BUFFER-TEXT contains any alternative initial waiting patterns.
Returns t if a match is found, nil otherwise."
  (when (boundp 'ecc-state-prompt-initial-waiting-alternatives)
    (cl-some (lambda (pattern)
               (string-match-p pattern buffer-text))
             ecc-state-prompt-initial-waiting-alternatives)))

;;;; Utility functions

(defun ecc-state-get-name (state)
  "Convert STATE symbol to a human-readable name."
  (cond
   ((eq state :y/y/n) "Y/Y/N")
   ((eq state :y/n) "Y/N")
   ((eq state :waiting) "Continue")
   ((eq state :initial-waiting) "Initial-Waiting")
   (t (format "%s" state))))

;;;; Notification interface

(defun ecc-state-notify-if-prompt-detected (buffer)
  "Check if BUFFER contains a Claude prompt and notify if appropriate.
Returns the detected state if a prompt is found, nil otherwise."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((state (ecc-detect-state)))
        (when (and state
                  (boundp 'ecc-auto-notify-on-claude-prompt)
                  ecc-auto-notify-on-claude-prompt
                  (fboundp 'ecc-auto-notify-check-state))
          (ecc-auto-notify-check-state state))
        state))))

;;;; Backwards compatibility

;; Maintain backward compatibility with existing code
(defalias 'ecc-detect-simple-state 'ecc-detect-state
  "Alias for backwards compatibility with existing code.")

(defalias 'ecc-detect-enhanced-state 'ecc-detect-state
  "Alias for backwards compatibility with existing code.")

(defalias 'ecc-detect-prompt-state 'ecc-detect-state
  "Alias for backwards compatibility with existing code.")

(define-obsolete-function-alias 'ecc-state-detect-prompt
  'ecc-detect-state "May 2025")

(provide 'ecc-state-detection-improved)
;;; ecc-state-detection-improved.el ends here