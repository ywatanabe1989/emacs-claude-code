;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-12 21:39:09>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-hello-world/ehw-utils/ehw-utils.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; ehw-utils.el --- Utility functions for emacs-hello-world  -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides utility functions for the emacs-hello-world package.

;;; Code:

;; 1. Main entry point
;; ----------------------------------------

(defun ehw-utils-format (text)
  "Format TEXT with hello world styling (uppercase with prefix)."
  (concat ">> " (upcase text) " <<"))

(defun ehw-utils-count-words (text)
  "Count the number of words in TEXT."
  (length (split-string text "\\W+" t)))

;; 2. Core functions
;; ----------------------------------------

(defun --ehw-utils-internal-helper (text)
  "Internal helper function for TEXT processing."
  (replace-regexp-in-string "\\." "!" text))

;; 3. Helper functions
;; ----------------------------------------

(defun ehw-utils-trim (text)
  "Trim whitespace from both ends of TEXT."
  (string-trim text))

;;; ehw-utils.el ends here


(provide 'ehw-utils)

(when
    (not load-file-name)
  (message "ehw-utils.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))