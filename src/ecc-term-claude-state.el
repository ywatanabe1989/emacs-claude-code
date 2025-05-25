;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 23:55:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-term-claude-state.el

;;; Commentary:
;;; Centralized Claude prompt state detection functionality.
;;; This module provides a unified interface for detecting Claude prompt states
;;; in terminal buffers, consolidating duplicated logic from multiple files
;;; into a single, maintainable system.

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
(defun ecc-term-claude-get-state (&optional buffer)
  "Get the current Claude prompt state for BUFFER or current buffer.
Returns one of: :y/y/n, :y/n, :waiting, :initial-waiting, or nil.

This is the main function that should be used for state detection
in term-claude mode. It automatically uses the best available
detection method, falling back to simpler methods if necessary."
  (with-current-buffer (or buffer (current-buffer))
    (cond
     ;; First try enhanced detection from state-detection module if available
     ((and (featurep 'ecc-state-detection) 
           (fboundp 'ecc-detect-state))
      (ecc-detect-state))
     
     ;; Next try alternative detection module if available
     ((and (featurep 'ecc-state-detect-prompt)
           (fboundp 'ecc-detect-prompt-in-last-lines))
      (ecc-detect-prompt-in-last-lines 20))
     
     ;; Fall back to basic detection
     (t (ecc-term-claude-detect-basic-state)))))

;;;###autoload
(defun ecc-term-claude-detect-basic-state ()
  "Basic detection of Claude prompt state in the current buffer.
Returns :y/y/n, :y/n, :waiting, :initial-waiting, or nil.

This is a self-contained function that does not depend on
any other modules. It serves as a fallback when enhanced
detection is not available."
  (let ((buffer-text (buffer-substring-no-properties 
                     (max (- (point-max) 1000) (point-min))
                     (point-max))))
    (cond
     ;; Basic patterns for y/y/n prompts
     ((string-match-p "\\[Y/y/n\\]" buffer-text) :y/y/n)
     
     ;; Basic patterns for y/n prompts  
     ((string-match-p "\\[y/n\\]\\|\\[Y/n\\]" buffer-text) :y/n)
     
     ;; Basic patterns for waiting prompts
     ((string-match-p "continue>\\|Continue>" buffer-text) :waiting)
     
     ;; Use custom patterns if defined
     ((and (boundp 'ecc-state-prompt-y/y/n)
           ecc-state-prompt-y/y/n
           (string-match-p (regexp-quote ecc-state-prompt-y/y/n) buffer-text))
      :y/y/n)
     
     ((and (boundp 'ecc-state-prompt-y/n)
           ecc-state-prompt-y/n
           (string-match-p (regexp-quote ecc-state-prompt-y/n) buffer-text))
      :y/n)
     
     ((and (boundp 'ecc-state-prompt-waiting)
           ecc-state-prompt-waiting
           (string-match-p (regexp-quote ecc-state-prompt-waiting) buffer-text))
      :waiting)
     
     ((and (boundp 'ecc-state-prompt-initial-waiting)
           ecc-state-prompt-initial-waiting
           (string-match-p (regexp-quote ecc-state-prompt-initial-waiting) buffer-text))
      :initial-waiting)
     
     (t nil))))

;;;; Utility functions

;;;###autoload
(defun ecc-term-claude-state-name (state)
  "Convert STATE symbol to a human-readable name.
STATE should be one of: :y/y/n, :y/n, :waiting, :initial-waiting, or nil."
  (cond
   ((eq state :y/y/n) "Y/Y/N")
   ((eq state :y/n) "Y/N")
   ((eq state :waiting) "Continue")
   ((eq state :initial-waiting) "Initial-Waiting")
   (t (format "%s" state))))

;;;###autoload
(defun ecc-term-claude-state-symbols ()
  "Return a list of all known Claude prompt state symbols.
This is useful for iterating over all possible states."
  '(:y/y/n :y/n :waiting :initial-waiting))

;;;###autoload
(defun ecc-term-claude-check-state ()
  "Check and update the state of the Claude VTERM buffer.
Detects the current prompt state (if any) using the state detection
system and forces a mode line update to reflect the current state.

Returns the detected state or nil if no prompt state is detected."
  (interactive)
  (when (or (eq major-mode 'ecc-term-claude-mode)
            (eq major-mode 'vterm-mode))
    (let ((state (ecc-term-claude-get-state)))
      (force-mode-line-update)
      state)))

;;;; Backward compatibility aliases

;; For compatibility with existing code
(defalias 'ecc-detect-state 'ecc-term-claude-get-state)

(provide 'ecc-term-claude-state)

;;; ecc-term-claude-state.el ends here