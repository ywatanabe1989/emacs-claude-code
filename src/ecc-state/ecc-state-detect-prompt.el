;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-14 08:22:34>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-state/ecc-state-detect-prompt.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ecc-state-variables)

;; Detectors
;; ------------------------------

(defun --ecc-state-detect-prompt-initial-waiting ()
  "Detect initial waiting prompt in Claude buffer."
  (--ecc-state-detect-prompt ecc-state-prompt-initial-waiting))

(defun --ecc-state-detect-prompt-waiting ()
  "Detect waiting prompt in Claude buffer."
  (--ecc-state-detect-prompt ecc-state-prompt-waiting))

(defun --ecc-state-detect-prompt-y/n ()
  "Detect y/n prompt in Claude buffer."
  (and
   (--ecc-state-detect-prompt ecc-state-prompt-y/n)
   (not (--ecc-state-detect-prompt ecc-state-prompt-y/y/n))))

(defun --ecc-state-detect-prompt-y/y/n ()
  "Detect Y/y/n prompt in Claude buffer."
  (--ecc-state-detect-prompt ecc-state-prompt-y/y/n))

;; Sub-functions
;; ------------------------------

(defun --ecc-state-detect-prompt (prompt-text &optional n-lines)
  "Find prompt in current buffer using PROMPT-TEXT within N-LINES lines."
  (interactive)
  (let ((n-lines (or n-lines 50)))
    (if (get-buffer ecc-buffer-name)
        (save-current-buffer
          (set-buffer ecc-buffer-name)
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
                found))))
      nil)))

;; Regex-based detection functions
;; ------------------------------

(defun --ecc-state-detect-prompt-regex (pattern &optional n-lines)
  "Find prompt in current buffer using regex PATTERN within N-LINES lines."
  (let ((n-lines (or n-lines 50)))
    (if (get-buffer ecc-buffer-name)
        (save-current-buffer
          (set-buffer ecc-buffer-name)
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
              (re-search-backward pattern search-end t))))
      nil)))

(defun --ecc-state-detect-initial-waiting-regex ()
  "Detect initial waiting prompt using regex."
  (--ecc-state-detect-prompt-regex ecc-state-prompt-pattern-initial-waiting))

(defun --ecc-state-detect-waiting-regex ()
  "Detect waiting prompt using regex."
  (--ecc-state-detect-prompt-regex ecc-state-prompt-pattern-waiting))

(defun --ecc-state-detect-y/n-regex ()
  "Detect y/n prompt using regex."
  (and
   (--ecc-state-detect-prompt-regex ecc-state-prompt-pattern-y/n)
   (not (--ecc-state-detect-prompt-regex ecc-state-prompt-pattern-y/y/n))))

(defun --ecc-state-detect-y/y/n-regex ()
  "Detect Y/y/n prompt using regex."
  (--ecc-state-detect-prompt-regex ecc-state-prompt-pattern-y/y/n))

(provide 'ecc-state-detect-prompt)

(when
    (not load-file-name)
  (message "ecc-state-detect-prompt.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))