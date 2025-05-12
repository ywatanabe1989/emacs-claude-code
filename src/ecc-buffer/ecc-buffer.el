;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 06:44:17>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-buffer/ecc-buffer.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ecc-buffer-variables)
(require 'ecc-buffer-verification)
(require 'ecc-buffer-registry)
(require 'ecc-buffer-current)
(require 'ecc-buffer-state)
(require 'ecc-buffer-stale)
(require 'ecc-buffer-navigation)
(require 'ecc-buffer-timestamp)
(require 'ecc-buffer-auto-switch)

;; Register this feature
(provide 'ecc-buffer)


(when
    (not load-file-name)
  (message "ecc-buffer.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))