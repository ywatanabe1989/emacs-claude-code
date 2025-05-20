;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 23:00:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-auto-detect.el

;;; Commentary:
;;; Centralized detection system for Claude prompt states.
;;; This module provides a unified interface for detecting Claude prompt states
;;; in buffers, supporting both global and buffer-local configurations.
;;;
;;; This is a clean consolidation of detection logic previously spread across
;;; ecc-state-detection.el, ecc-buffer-state.el, and other modules.

(require 'ecc-variables)

;;; Code:

;; Customization options
(defgroup ecc-auto-detect nil
  "Settings for Claude prompt detection."
  :group 'ecc
  :prefix "ecc-auto-detect-")

(defcustom ecc-auto-detect-buffer-size 2000
  "Number of characters to check from end of buffer for prompt detection."
  :type 'integer
  :group 'ecc-auto-detect)

(defcustom ecc-auto-detect-line-count 256
  "Number of lines to check from end of buffer for line-based detection."
  :type 'integer
  :group 'ecc-auto-detect)

;; Core detection functions

;;;###autoload
(defun ecc-auto-detect-prompt (&optional buffer)
  "Detect Claude prompt state in BUFFER (or current buffer).
Returns one of: :y/y/n, :y/n, :waiting, :initial-waiting, or nil.

This is the main function that should be used for prompt detection.
It uses the most accurate method available for the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (or
     ;; First try line-based detection for better accuracy
     (ecc-auto-detect-in-lines (or ecc-auto-detect-line-count 256))
     ;; Fall back to basic detection if line detection finds nothing
     (ecc-auto-detect-in-chars))))

(defun ecc-auto-detect-in-chars ()
  "Detect Claude prompt state using buffer content matching.
Returns :y/y/n, :y/n, :waiting, :initial-waiting, or nil."
  (let ((buffer-text (buffer-substring-no-properties 
                     (max (- (point-max) ecc-auto-detect-buffer-size) (point-min))
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
     
     ;; Try alternative initial waiting patterns
     ((ecc-auto-detect-alternative-waiting buffer-text)
      :initial-waiting)
     
     ;; Fallback to common patterns
     ((string-match-p "\\[Y/y/n\\]" buffer-text) :y/y/n)
     ((string-match-p "\\[y/n\\]\\|\\[Y/n\\]" buffer-text) :y/n)
     ((string-match-p "continue>\\|Continue>" buffer-text) :waiting)
     
     (t nil))))

(defun ecc-auto-detect-in-lines (&optional n-lines)
  "Detect Claude prompts in the last N-LINES of the current buffer.
If N-LINES is nil, use `ecc-auto-detect-line-count'.
Returns one of: :y/y/n, :y/n, :waiting, :initial-waiting, or nil."
  (let* ((lines (or n-lines ecc-auto-detect-line-count))
         (buffer-lines (count-lines (point-min) (point-max)))
         (start-line (max 1 (- buffer-lines lines)))
         (start-pos (save-excursion
                      (goto-char (point-min))
                      (forward-line (1- start-line))
                      (point)))
         (buffer-text (buffer-substring-no-properties 
                       start-pos
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
     
     ;; Try alternative initial waiting patterns
     ((ecc-auto-detect-alternative-waiting buffer-text)
      :initial-waiting)
     
     ;; Fallback to common patterns
     ((string-match-p "\\[Y/y/n\\]" buffer-text) :y/y/n)
     ((string-match-p "\\[y/n\\]\\|\\[Y/n\\]" buffer-text) :y/n)
     ((string-match-p "continue>\\|Continue>" buffer-text) :waiting)
     
     (t nil))))

;;;###autoload
(defun ecc-auto-detect-in-region (start end)
  "Detect Claude prompts in region between START and END.
Returns one of: :y/y/n, :y/n, :waiting, :initial-waiting, or nil."
  (interactive "r")
  (let ((buffer-text (buffer-substring-no-properties start end)))
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
     
     ;; Try alternative initial waiting patterns
     ((ecc-auto-detect-alternative-waiting buffer-text)
      :initial-waiting)
      
     ;; Fallback to common patterns
     ((string-match-p "\\[Y/y/n\\]" buffer-text) :y/y/n)
     ((string-match-p "\\[y/n\\]\\|\\[Y/n\\]" buffer-text) :y/n)
     ((string-match-p "continue>\\|Continue>" buffer-text) :waiting)
     
     (t nil))))

(defun ecc-auto-detect-alternative-waiting (buffer-text)
  "Check if BUFFER-TEXT contains any alternative initial waiting patterns.
Returns t if a match is found, nil otherwise."
  (when (boundp 'ecc-state-prompt-initial-waiting-alternatives)
    (catch 'found
      (dolist (pattern ecc-state-prompt-initial-waiting-alternatives)
        (when (string-match-p pattern buffer-text)
          (throw 'found t)))
      nil)))

;; Utility functions

;;;###autoload
(defun ecc-auto-detect-name (state)
  "Convert STATE symbol to a human-readable name."
  (cond
   ((eq state :y/y/n) "Y/Y/N")
   ((eq state :y/n) "Y/N")
   ((eq state :waiting) "Continue")
   ((eq state :initial-waiting) "Initial-Waiting")
   (t (format "%s" state))))

;; Predicates for checking specific states

;;;###autoload
(defun ecc-auto-detect-y/n-p (&optional buffer)
  "Return non-nil if BUFFER has a Y/N prompt state.
If BUFFER is nil, use current buffer."
  (eq (ecc-auto-detect-prompt buffer) :y/n))

;;;###autoload
(defun ecc-auto-detect-y/y/n-p (&optional buffer)
  "Return non-nil if BUFFER has a Y/Y/N prompt state.
If BUFFER is nil, use current buffer."
  (eq (ecc-auto-detect-prompt buffer) :y/y/n))

;;;###autoload
(defun ecc-auto-detect-waiting-p (&optional buffer)
  "Return non-nil if BUFFER has a waiting prompt state.
If BUFFER is nil, use current buffer."
  (eq (ecc-auto-detect-prompt buffer) :waiting))

;;;###autoload
(defun ecc-auto-detect-initial-waiting-p (&optional buffer)
  "Return non-nil if BUFFER has an initial waiting prompt state.
If BUFFER is nil, use current buffer."
  (eq (ecc-auto-detect-prompt buffer) :initial-waiting))

;; Backwards compatibility
;;;###autoload
(defalias 'ecc-detect-state 'ecc-auto-detect-prompt
  "Compatibility alias for `ecc-auto-detect-prompt'.")

;;;###autoload
(defalias 'ecc-detect-prompt-in-last-lines 'ecc-auto-detect-in-lines
  "Compatibility alias for `ecc-auto-detect-in-lines'.")

;;;###autoload
(defalias 'ecc-detect-prompt-in-region 'ecc-auto-detect-in-region
  "Compatibility alias for `ecc-auto-detect-in-region'.")

;;;###autoload
(defalias 'ecc-state-get-name 'ecc-auto-detect-name
  "Compatibility alias for `ecc-auto-detect-name'.")

(provide 'ecc-auto-detect)

;;; ecc-auto-detect.el ends here