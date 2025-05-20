;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-12 21:38:20>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-hello-world/ehw-prep/ehw-prep-install-deps.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; ehw-prep-install-deps.el --- Install dependencies for emacs-hello-world

;;; Commentary:
;; This file provides functions to install dependencies
;; required by the emacs-hello-world package.

;;; Code:

(require 'package)
(require 'ehw-prep-variables)

(defun ehw-prep-install-dependencies ()
  "Install dependencies for emacs-hello-world."
  (interactive)
  (message "Installing dependencies for emacs-hello-world...")
  (dolist (pkg --ehw-prep-dep-packages-src)
    (unless (package-installed-p pkg)
      (package-install pkg)))
  (message "No additional dependencies required for emacs-hello-world."))

(defun ehw-prep-install-test-dependencies ()
  "Install test dependencies for emacs-hello-world."
  (interactive)
  (message "Installing test dependencies for emacs-hello-world...")
  (dolist (pkg --ehw-prep-dep-packages-test)
    (unless (package-installed-p pkg)
      (package-install pkg)))
  (message "No additional test dependencies required for emacs-hello-world."))

(provide 'ehw-prep-install-deps)

(when (not load-file-name)
  (message "ehw-prep-install-deps.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

;;; ehw-prep-install-deps.el ends here