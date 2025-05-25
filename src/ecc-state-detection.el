;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 16:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-state-detection.el

;;; Commentary:
;;; Centralized Claude prompt state detection functionality.
;;; This module consolidates all state detection logic in one place to eliminate duplication
;;; and ensure consistent behavior across different parts of the system.
;;;
;;; The core functionality includes:
;;; - Detection of various Claude prompt states (Y/N, Y/Y/N, waiting, etc.)
;;; - Multiple detection strategies for different contexts
;;; - Buffer-wide, region-based, and line-based detection options
;;; - Notification integration for detected states
;;;
;;; IMPORTANT: Claude's prompt patterns use non-breaking spaces (character 160, \u00A0)
;;; in specific locations. These must be preserved exactly for pattern matching to work.
;;; See ecc-variables.el for the exact patterns.
;;;
;;; This module provides a comprehensive interface for all state detection needs.

(require 'ecc-variables)
(require 'cl-lib)

;;; Code:

;; State symbols and documentation
;;
;; The following state symbols are used throughout this module:
;; - :y/n - Represents a yes/no prompt (typically "1. Yes / 2. No")
;; - :y/y/n - Represents a yes/yes+/no prompt (typically with a second "yes" option)
;; - :waiting - Represents when Claude is waiting for the user to continue
;; - :initial-waiting - Represents Claude's initial prompt at the start of a session
;; - nil - Represents no recognized prompt state

;;;; Core detection functions

;;;###autoload
(defun ecc-detect-state (&optional buffer)
  "Detect Claude prompt state in BUFFER (or current buffer).
Returns one of: :y/y/n, :y/n, :waiting, :initial-waiting, or nil.

This is the main function that should be used for state detection.
It automatically uses the best available detection method, prioritizing
line-based detection for accuracy when available."
  (with-current-buffer (or buffer (current-buffer))
    (ecc-debug-message "ecc-detect-state called for buffer: %s" (buffer-name))
    (let ((line-result (ecc-detect-prompt-in-last-lines ecc-state-detection-line-count))
          (basic-result (ecc-detect-basic-state)))
      (ecc-debug-message "Line-based detection result: %s" line-result)
      (ecc-debug-message "Basic detection result: %s" basic-result)
      (or line-result basic-result))))

;;;###autoload
(defun ecc-detect-basic-state ()
  "Basic detection of Claude prompt state using buffer content matching.
Returns :y/y/n, :y/n, :waiting, :initial-waiting, or nil."
  (let ((buffer-text (buffer-substring-no-properties 
                    (max (- (point-max) ecc-state-detection-buffer-size) (point-min))
                    (point-max))))
    (ecc-analyze-buffer-text-for-state buffer-text)))

;;;###autoload
(defun ecc-detect-prompt-in-last-lines (&optional n-lines)
  "Detect Claude prompts in the last N-LINES of the current buffer.
If N-LINES is nil, use `ecc-state-detection-line-count'.
Returns one of: :y/y/n, :y/n, :waiting, :initial-waiting, or nil."
  (interactive)
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

;;;###autoload
(defun ecc-detect-prompt-in-region (start end)
  "Detect Claude prompts in region between START and END.
Returns one of: :y/y/n, :y/n, :waiting, :initial-waiting, or nil."
  (interactive "r")
  (let ((buffer-text (buffer-substring-no-properties start end)))
    (ecc-analyze-buffer-text-for-state buffer-text)))

;;;; Analysis functions

(defun ecc-analyze-buffer-text-for-state (buffer-text)
  "Analyze BUFFER-TEXT to detect Claude prompt state.
Returns :y/y/n, :y/n, :waiting, :initial-waiting, or nil."
  (ecc-debug-message "ecc-analyze-buffer-text-for-state called")
  (ecc-debug-message "Buffer text length: %s" (length buffer-text))
  (when (< (length buffer-text) 200)
    (ecc-debug-message "Buffer text: %s" buffer-text))
  
  (let ((result
         (cond
          ;; Check for y/y/n prompts using customized pattern
          ((and (boundp 'ecc-state-prompt-y/y/n)
                ecc-state-prompt-y/y/n
                (string-match-p (regexp-quote ecc-state-prompt-y/y/n) buffer-text))
           (ecc-debug-message "Matched Y/Y/N pattern: %s" ecc-state-prompt-y/y/n)
           :y/y/n)
          
          ;; Check for y/n prompts using customized pattern
          ((and (boundp 'ecc-state-prompt-y/n)
                ecc-state-prompt-y/n
                (string-match-p (regexp-quote ecc-state-prompt-y/n) buffer-text))
           (ecc-debug-message "Matched Y/N pattern: %s" ecc-state-prompt-y/n)
           :y/n)
          
          ;; Check for waiting prompts using customized patterns
          ((and (boundp 'ecc-state-prompt-waiting)
                ecc-state-prompt-waiting
                (string-match-p (regexp-quote ecc-state-prompt-waiting) buffer-text))
           (ecc-debug-message "Matched waiting pattern: %s" ecc-state-prompt-waiting)
           :waiting)
          
          ;; Check for initial prompts
          ((and (boundp 'ecc-state-prompt-initial-waiting)
                ecc-state-prompt-initial-waiting
                (string-match-p (regexp-quote ecc-state-prompt-initial-waiting) buffer-text))
           (ecc-debug-message "Matched initial waiting pattern: %s" ecc-state-prompt-initial-waiting)
           :initial-waiting)
          
          ;; If no custom patterns match, try alternative initial waiting patterns
          ((ecc-detect-alternative-initial-waiting buffer-text)
           (ecc-debug-message "Matched alternative initial waiting pattern")
           :initial-waiting)
          
          ;; Fallback to common patterns
          ((string-match-p "\\[Y/y/n\\]" buffer-text) 
           (ecc-debug-message "Matched fallback Y/Y/N pattern")
           :y/y/n)
          ((string-match-p "\\[y/n\\]\\|\\[Y/n\\]" buffer-text) 
           (ecc-debug-message "Matched fallback Y/N pattern")
           :y/n)
          ((string-match-p "continue>\\|Continue>" buffer-text) 
           (ecc-debug-message "Matched fallback continue pattern")
           :waiting)
          
          (t 
           (ecc-debug-message "No pattern matched")
           nil))))
    (ecc-debug-message "Final detection result: %s" result)
    result))

(defun ecc-detect-alternative-initial-waiting (buffer-text)
  "Check if BUFFER-TEXT contains any alternative initial waiting patterns.
Returns t if a match is found, nil otherwise."
  (when (boundp 'ecc-state-prompt-initial-waiting-alternatives)
    (cl-some (lambda (pattern)
               (string-match-p pattern buffer-text))
             ecc-state-prompt-initial-waiting-alternatives)))

;;;; Utility functions

;;;###autoload
(defun ecc-state-get-name (state)
  "Convert STATE symbol to a human-readable name."
  (cond
   ((eq state :y/y/n) "Y/Y/N")
   ((eq state :y/n) "Y/N")
   ((eq state :waiting) "Continue")
   ((eq state :initial-waiting) "Initial-Waiting")
   (t (format "%s" state))))

;;;###autoload
(defun ecc-state-symbols ()
  "Return a list of all known state symbols used in state detection.
This is useful for iterating over all possible states."
  '(:y/y/n :y/n :waiting :initial-waiting))

;;;; Notification interface

;;;###autoload
(defun ecc-state-notify-if-prompt-detected (buffer)
  "Check if BUFFER contains a Claude prompt and notify if appropriate.
Returns the detected state if a prompt is found, nil otherwise."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((state (ecc-detect-state)))
        (when (and state
                  (boundp 'ecc-auto-notify-on-claude-prompt)
                  ecc-auto-notify-on-claude-prompt
                  (fboundp 'ecc-notification-check-state))
          ;; Use the notification system to ring bell and other alerts
          (ecc-notification-check-state state))
        state))))

;;;; Debugging helpers

;;;###autoload
(defun ecc-state-detection-debug-info (&optional buffer)
  "Return debug information about state detection in BUFFER or current buffer.
Returns a string with details about the buffer and detected state."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((state (ecc-detect-state))
           (basic-state (ecc-detect-basic-state))
           (line-state (ecc-detect-prompt-in-last-lines))
           (last-lines (min 3 (count-lines (point-min) (point-max))))
           (last-text (buffer-substring-no-properties
                       (save-excursion
                         (goto-char (point-max))
                         (forward-line (- last-lines))
                         (point))
                       (point-max))))
      (format "State Detection Debug Info:
Buffer: %s
Detected State: %s
  via basic method: %s
  via line method: %s
Buffer size: %d chars, %d lines
Last %d lines: %s"
              (buffer-name)
              (ecc-state-get-name state)
              (ecc-state-get-name basic-state)
              (ecc-state-get-name line-state)
              (buffer-size)
              (count-lines (point-min) (point-max))
              last-lines
              (replace-regexp-in-string "\n" "\\\\n" last-text)))))

;;;; Background detection integration

;;;###autoload
(defun ecc-state-detect-buffer-bg (buffer)
  "Detect the state of BUFFER in background processing.
Returns the detected state if a prompt is found, nil otherwise."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (ecc-detect-state))))

;;;; Buffer state integration

;;;###autoload
(defun ecc-state-update-buffer-state (buffer state)
  "Update the buffer-local state in BUFFER with detected STATE.
Returns the STATE argument for convenience in chaining."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (fboundp 'ecc-buffer-state-set)
        (ecc-buffer-state-set 'prompt-state state))
      state)))

;;;; Backward compatibility functions and aliases

;;;###autoload
(defalias 'ecc-detect-prompt-state 'ecc-detect-state
  "Alias for backwards compatibility with existing code.")

;;;###autoload
(define-obsolete-function-alias 'ecc-state-detect-prompt
  'ecc-detect-state "May 2025")

;; Provide both the standard name and compatibility names
(provide 'ecc-state-detection)
(provide 'ecc-state-detection)
(provide 'ecc-state-detection-improved)
(provide 'ecc-state-detect-prompt)

;;; ecc-state-detection.el ends here