;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 08:19:32>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-term/ecc-term.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ecc-term-claude-mode)
(require 'ecc-term-run)
(require 'ecc-term-send-accept)
(require 'ecc-term-send-general)
(require 'ecc-term-vterm)


(provide 'ecc-term)

(when
    (not load-file-name)
  (message "ecc-term.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))