;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-12 15:45:25>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/emacs-claude-code.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(add-to-list 'load-path "~/.emacs.d/lisp/emacs-claude-code/src/")
(add-to-list 'load-path
             "~/.emacs.d/lisp/emacs-claude-code/src/ecc-buffer")
(add-to-list 'load-path
             "~/.emacs.d/lisp/emacs-claude-code/src/ecc-state")
(add-to-list 'load-path
             "~/.emacs.d/lisp/emacs-claude-code/src/ecc-template")
(add-to-list 'load-path
             "~/.emacs.d/lisp/emacs-claude-code/src/ecc-term")

(require 'ecc-auto)
(require 'ecc-bindings)
(require 'ecc-buffer)
(require 'ecc-buffer-manager)
(require 'ecc-buffer-auto-switch)
(require 'ecc-buffer-current)
(require 'ecc-buffer)
(require 'ecc-buffer-navigation)
(require 'ecc-buffer-registry)
(require 'ecc-buffer-stale)
(require 'ecc-buffer-state)
(require 'ecc-buffer-timestamp)
(require 'ecc-buffer-variables)
(require 'ecc-buffer-verification)

(require 'ecc-large-buffer)
(require 'ecc-command)
(require 'ecc-compat)
(require 'ecc-dired)
(require 'ecc-elisp-test)
(require 'ecc-history)
(require 'ecc-integration)

(require 'ecc-mode)
(require 'ecc-repository)
(require 'ecc-repository-view)
(require 'ecc-run)
(require 'ecc-run-vterm)
(require 'ecc-send)
(require 'ecc-state)
(require 'ecc-state-engine)
(require 'ecc-claude-vterm-mode)
(require 'ecc-update-mode-line)
(require 'ecc-variables)
(require 'ecc-vterm)
(require 'ecc-update-mode-line)
(require 'ecc-bindings)
(require 'ecc-mode)

(require 'ecc-template-cache)
(require 'ecc-template)
(require 'ecc-template-mode)
(require 'ecc-claude-vterm-mode)
(require 'ecc-run-vterm)
(require 'ecc-state-detect)
(require 'ecc-state)


(provide 'emacs-claude-code)

(when
    (not load-file-name)
  (message "emacs-claude-code.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))