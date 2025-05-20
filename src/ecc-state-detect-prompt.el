;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 08:15:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-state/ecc-state-detect-prompt.el

;;; Commentary:
;;; Functions for detecting Claude prompts in buffers.

(require 'ecc-variables)

(defgroup ecc-state-detect nil
  "Claude prompt detection settings."
  :group 'ecc
  :prefix "ecc-state-detect-")

(defcustom ecc-state-detect-line-count 20
  "Number of lines to check from the end of buffer for prompt detection."
  :type 'integer
  :group 'ecc-state-detect)

;;;###autoload
(defun ecc-detect-prompt-in-last-lines (&optional n-lines)
  "Detect Claude prompts in the last N-LINES of the current buffer.
If N-LINES is nil, use `ecc-state-detect-line-count'.
Returns one of: :y/y/n, :y/n, :waiting, :initial-waiting, or nil."
  (interactive)
  (let* ((lines (or n-lines ecc-state-detect-line-count))
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
     ((and ecc-state-prompt-y/y/n
           (string-match-p (regexp-quote ecc-state-prompt-y/y/n) buffer-text))
      :y/y/n)
     
     ;; Check for y/n prompts using customized pattern
     ((and ecc-state-prompt-y/n
           (string-match-p (regexp-quote ecc-state-prompt-y/n) buffer-text))
      :y/n)
     
     ;; Check for waiting prompts using customized patterns
     ((and ecc-state-prompt-waiting
           (string-match-p (regexp-quote ecc-state-prompt-waiting) buffer-text))
      :waiting)
     
     ;; Check for initial prompts
     ((and ecc-state-prompt-initial-waiting
           (string-match-p (regexp-quote ecc-state-prompt-initial-waiting) buffer-text))
      :initial-waiting)
     
     ;; Fallback to common patterns
     ((string-match-p "\\[Y/y/n\\]" buffer-text) :y/y/n)
     ((string-match-p "\\[y/n\\]\\|\\[Y/n\\]" buffer-text) :y/n)
     ((string-match-p "continue>\\|Continue>" buffer-text) :waiting)
     
     (t nil))))

;;;###autoload
(defun ecc-detect-prompt-in-region (start end)
  "Detect Claude prompts in region between START and END.
Returns one of: :y/y/n, :y/n, :waiting, :initial-waiting, or nil."
  (interactive "r")
  (let ((buffer-text (buffer-substring-no-properties start end)))
    (cond
     ;; Check for y/y/n prompts using customized pattern
     ((and ecc-state-prompt-y/y/n
           (string-match-p (regexp-quote ecc-state-prompt-y/y/n) buffer-text))
      :y/y/n)
     
     ;; Check for y/n prompts using customized pattern
     ((and ecc-state-prompt-y/n
           (string-match-p (regexp-quote ecc-state-prompt-y/n) buffer-text))
      :y/n)
     
     ;; Check for waiting prompts using customized patterns
     ((and ecc-state-prompt-waiting
           (string-match-p (regexp-quote ecc-state-prompt-waiting) buffer-text))
      :waiting)
     
     ;; Check for initial prompts
     ((and ecc-state-prompt-initial-waiting
           (string-match-p (regexp-quote ecc-state-prompt-initial-waiting) buffer-text))
      :initial-waiting)
     
     ;; Fallback to common patterns
     ((string-match-p "\\[Y/y/n\\]" buffer-text) :y/y/n)
     ((string-match-p "\\[y/n\\]\\|\\[Y/n\\]" buffer-text) :y/n)
     ((string-match-p "continue>\\|Continue>" buffer-text) :waiting)
     
     (t nil))))

;;;###autoload
(defun ecc-detect-prompt-state (&optional buffer n-lines)
  "Detect Claude prompt state in BUFFER (or current buffer) checking N-LINES.
Return the detected state as a symbol: :y/y/n, :y/n, :waiting, :initial-waiting, or nil."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (ecc-detect-prompt-in-last-lines n-lines)))

;; Replacement for the original simple detection function
;;;###autoload
(defun ecc-detect-enhanced-state (&optional buffer n-lines)
  "Enhanced detection of Claude prompt state in BUFFER, checking N-LINES.
This function checks both standard patterns and custom patterns
defined in ecc-variables. Returns :y/y/n, :y/n, :waiting, :initial-waiting, or nil."
  (with-current-buffer (or buffer (current-buffer))
    (ecc-detect-prompt-in-last-lines n-lines)))

(provide 'ecc-state-detect-prompt)

;;; ecc-state-detect-prompt.el ends here