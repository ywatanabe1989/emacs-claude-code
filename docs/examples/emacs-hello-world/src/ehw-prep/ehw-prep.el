;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-12 21:37:10>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-hello-world/ehw-prep/ehw-prep.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; ehw-prep.el --- Preparation module for emacs-hello-world

;;; Commentary:
;; This file serves as the entry point for the preparation phase
;; of the emacs-hello-world package.  It handles tasks like setting
;; up the environment, adding to load path, and installing dependencies.

;;; Code:

;; Core preparation components
(require 'ehw-prep-variables)
(require 'ehw-prep-add-path)
(require 'ehw-prep-install-deps)

(defun ehw-prep-setup ()
  "Set up the environment for the emacs-hello-world package.
This includes adding directories to load-path and installing dependencies."
  (interactive)
  (message "Setting up emacs-hello-world environment...")
  
  ;; Install dependencies
  (ehw-prep-install-dependencies)
  
  (message "emacs-hello-world environment setup complete."))

(provide 'ehw-prep)

(when (not load-file-name)
  (message "ehw-prep.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

;;; ehw-prep.el ends here