;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 16:45:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-state-detection.el

;;; Commentary:
;;; Centralized Claude prompt state detection functionality.
;;; This module consolidates all state detection logic in one place to eliminate duplication
;;; and ensure consistent behavior across different parts of the system.

(require 'ecc-variables)

;;; Code:

;; Customization for state detection
(defgroup ecc-state-detection nil
  "Settings for Claude prompt state detection."
  :group 'ecc
  :prefix "ecc-state-detection-")

(defcustom ecc-state-detection-buffer-size 2000
  "Number of characters to check from the end of buffer for basic prompt detection."
  :type 'integer
  :group 'ecc-state-detection)

(defcustom ecc-state-detection-line-count 50
  "Number of lines to check from the end of buffer for line-based prompt detection."
  :type 'integer
  :group 'ecc-state-detection)

;; Main state detection functions

;;;###autoload
(defun ecc-detect-state (&optional buffer)
  "Detect Claude prompt state in BUFFER (or current buffer).
Returns one of: :y/y/n, :y/n, :waiting, :initial-waiting, or nil.

This is the main function that should be used for state detection.
It automatically uses the best available detection method."
  (with-current-buffer (or buffer (current-buffer))
    (cond
     ;; Use the enhanced detection if available
     ((fboundp 'ecc-detect-enhanced-state)
      (ecc-detect-enhanced-state buffer))
     
     ;; Fall back to line-based detection if available
     ((fboundp 'ecc-detect-prompt-in-last-lines)
      (ecc-detect-prompt-in-last-lines ecc-state-detection-line-count))
     
     ;; Fall back to basic detection
     (t (ecc-detect-basic-state)))))

;;;###autoload
(defun ecc-detect-basic-state ()
  "Basic detection of Claude prompt state using buffer content matching.
Returns :y/y/n, :y/n, :waiting, :initial-waiting, or nil."
  (let ((buffer-text (buffer-substring-no-properties 
                     (max (- (point-max) ecc-state-detection-buffer-size) (point-min))
                     (point-max))))
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
     
     (t nil))))

(defun ecc-detect-alternative-initial-waiting (buffer-text)
  "Check if BUFFER-TEXT contains any alternative initial waiting patterns.
Returns t if a match is found, nil otherwise."
  (when (boundp 'ecc-state-prompt-initial-waiting-alternatives)
    (catch 'found
      (dolist (pattern ecc-state-prompt-initial-waiting-alternatives)
        (when (string-match-p pattern buffer-text)
          (throw 'found t)))
      nil)))

;; Backwards compatibility functions

;;;###autoload
(defalias 'ecc-detect-simple-state 'ecc-detect-state
  "Alias for backwards compatibility with existing code.")

;; Compatibility with enhanced state detection

;;;###autoload
(defun ecc-detect-enhanced-state (&optional buffer)
  "Enhanced detection of Claude prompt state in BUFFER (or current buffer).
This is a wrapper that forwards to `ecc-detect-state` or appropriate function.
Returns one of: :y/y/n, :y/n, :waiting, :initial-waiting, or nil."
  (if (fboundp 'ecc-detect-prompt-in-last-lines)
      ;; Use line-based detection if available
      (with-current-buffer (or buffer (current-buffer))
        (ecc-detect-prompt-in-last-lines ecc-state-detection-line-count))
    ;; Fall back to basic detection
    (with-current-buffer (or buffer (current-buffer))
      (ecc-detect-basic-state))))

;; Notification interface

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
                   (fboundp 'ecc-auto-notify-check-state))
          (ecc-auto-notify-check-state state))
        state))))

;; Utility function to get human-readable state name

;;;###autoload
(defun ecc-state-get-name (state)
  "Convert STATE symbol to a human-readable name."
  (cond
   ((eq state :y/y/n) "Y/Y/N")
   ((eq state :y/n) "Y/N")
   ((eq state :waiting) "Continue")
   ((eq state :initial-waiting) "Initial-Waiting")
   (t (format "%s" state))))

(provide 'ecc-state-detection)

;;; ecc-state-detection.el ends here