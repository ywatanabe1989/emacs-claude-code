;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 18:00:23>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-state/ecc-state-detect-prompt.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;; Detectors
;; ------------------------------

(defun --ecc-detect-prompt-initial-waiting ()
  "Detect y/n prompt in Claude buffer."
  (--ecc-detect-prompt ecc-prompt-initial-waiting))

(defun --ecc-detect-prompt-waiting ()
  "Detect waiting prompt in Claude buffer."
  (--ecc-detect-prompt ecc-prompt-waiting))

(defun --ecc-detect-prompt-y/n ()
  "Detect y/n prompt in Claude buffer."
  (and
   (--ecc-detect-prompt ecc-prompt-y/n)
   (not (--ecc-detect-prompt-y/y/n))))

(defun --ecc-detect-prompt-y/y/n ()
  "Detect y/n prompt in Claude buffer."
  (--ecc-detect-prompt ecc-prompt-y/y/n))
;; Sub-functions
;; ------------------------------

(defun --ecc-detect-prompt (prompt-text &optional n-lines)
  "Find prompt in current buffer using PROMPT-TEXT within N-LINES lines."
  (interactive)
  (let ((n-lines (or n-lines 50)))
    (if (get-buffer ecc-buffer-name)
        (with-current-buffer ecc-buffer-name
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
              (let
                  ((found (search-backward prompt-text search-end t)))
                found))))
      nil)))


(provide 'ecc-state-detect-prompt)

(when
    (not load-file-name)
  (message "ecc-state-detect-prompt.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))