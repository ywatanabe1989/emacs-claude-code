;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-14 00:27:57>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-term/ecc-term.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ecc-term-variables)
(require 'ecc-term-run)
(require 'ecc-term-vterm)
(require 'ecc-term-mode-legacy)

(require 'ecc-term-mode)

(provide 'ecc-term)

(when
    (not load-file-name)
  (message "ecc-term.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
