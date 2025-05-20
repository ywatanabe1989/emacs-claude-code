;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-12 21:46:07>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-hello-world/ehw-prep/ehw-prep-variables.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; ehw-prep-variables.el --- Variable definitions for emacs-hello-world preparation

;;; Commentary:
;; This file provides variable definitions used in the preparation phase
;; of the emacs-hello-world package.

;;; Code:

(defvar --ehw-prep-parent-dir-names
  '()
  "List of parent directory names for the emacs-hello-world package.")

(defvar --ehw-prep-dep-packages-src
  '()
  "List of dependency packages required for source code.")

(defvar --ehw-prep-dep-packages-test
  '()
  "List of dependency packages required for testing.")

(provide 'ehw-prep-variables)

(when (not load-file-name)
  (message "ehw-prep-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

;;; ehw-prep-variables.el ends here