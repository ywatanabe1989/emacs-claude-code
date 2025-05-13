;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 20:35:31>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-term/ecc-term.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ecc-term-variables)

;; Forward declare functions to avoid circular requires
(declare-function ecc-term-run-claude "ecc-term-run")
(declare-function ecc-term-run-help "ecc-term-run")
(declare-function ecc-term-vterm "ecc-term-vterm-mode")

;; Only require legacy modules directly
(require 'ecc-term-vterm-mode-legacy)


(provide 'ecc-term)

(when
    (not load-file-name)
  (message "ecc-term.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))