;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-09 09:25:29>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/ecc-state/ecc-state.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'pulse)

;; Detectors
;; ------------------------------

(defun --ecc-state-waiting-p ()
  "Detect waiting prompt in Claude buffer."
  (--ecc-state-detect-prompt ecc-prompt-waiting))

(defun --ecc-state-initial-waiting-p ()
  "Detect y/n prompt in Claude buffer."
  (--ecc-state-detect-prompt ecc-prompt-initial-waiting))

(defun --ecc-state-y/n-p ()
  "Detect y/n prompt in Claude buffer."
  (and
   (--ecc-state-detect-prompt ecc-prompt-y/n)
   (not (--ecc-state-y/y/n-p))))

(defun --ecc-state-y/y/n-p ()
  "Detect y/n prompt in Claude buffer."
  (--ecc-state-detect-prompt ecc-prompt-y/y/n))

(defun --ecc-state-thinking-p ()
  "Detect if Claude is thinking."
  (--ecc-state-detect-prompt ecc-prompt-thinking))

(defun --ecc-state-loading-p ()
  "Detect if Claude is loading."
  (--ecc-state-detect-prompt ecc-prompt-loading))

(defun --ecc-state-running-p ()
  "Detect if Claude is in the middle of generating a response."
  (or (--ecc-state-thinking-p)
      (--ecc-state-loading-p)))

;; Sub-functions
;; ------------------------------

(defun ecc-state-get ()
  "Detect the current state of Claude prompt.
Returns one of: :y/y/n, :y/n, :waiting, :initial-waiting, :running, or nil."
  (cond
   ((--ecc-state-y/y/n-p)
    :y/y/n)
   ((--ecc-state-y/n-p)
    :y/n)
   ((--ecc-state-waiting-p)
    :waiting)
   ((--ecc-state-initial-waiting-p)
    :initial-waiting)
   ((--ecc-state-running-p)
    :running)
   (t nil)))

(defun --ecc-state-detect-prompt (prompt-text &optional n-lines)
  "Find prompt in active Claude buffer using PROMPT-TEXT within N-LINES lines.
Supports both vterm and standard buffers."
  (interactive)
  (let ((n-lines (or n-lines 50))
        (active-buffer (or (and (boundp 'ecc-active-buffer) ecc-active-buffer)
                           (and (boundp 'ecc-buffer-current-buffer) ecc-buffer-current-buffer))))
    (if (and prompt-text (buffer-live-p active-buffer))
        (with-current-buffer active-buffer
          (save-excursion
            (goto-char (point-max))
            (skip-chars-backward " \t\n\r")
            (let* ((current-line (line-number-at-pos))
                   (min-line (line-number-at-pos (point-min)))
                   (search-end-line
                    (max min-line (- current-line n-lines)))
                   (search-end (save-excursion
                                 (goto-char (point-min))
                                 (forward-line (1- search-end-line))
                                 (point))))
              (let ((found (search-backward prompt-text search-end t)))
                (when found
                  ;; Clear screen if in vterm mode and vterm is available
                  (when (and (boundp 'ecc-claude-vterm--vterm-available) 
                             ecc-claude-vterm--vterm-available
                             (eq major-mode 'ecc-claude-vterm-mode))
                    (condition-case nil
                        (vterm-clear)
                      (error nil)))
                  
                  ;; Highlight the found prompt
                  (condition-case nil
                      (save-excursion
                        (goto-char found)
                        (beginning-of-line)
                        (let ((line-start (point)))
                          (end-of-line)
                          (pulse-momentary-highlight-region line-start
                                                            (point))))
                    (error nil)))
                found))))
      nil)))


;; Provide both features to maintain backward compatibility
(provide 'ecc-state)
(provide 'ecc-state/ecc-state)

(when
    (not load-file-name)
  (message "ecc-state.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))