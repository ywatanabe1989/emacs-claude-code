;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 00:13:40>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/ecc-buffer/ecc-buffer.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ecc-buffer/ecc-buffer-variables)
(require 'ecc-buffer/ecc-buffer-verification)
(require 'ecc-buffer/ecc-buffer-registry)
(require 'ecc-buffer/ecc-buffer-current)
(require 'ecc-buffer/ecc-buffer-state)
(require 'ecc-buffer/ecc-buffer-stale)
(require 'ecc-buffer/ecc-buffer-navigation)
(require 'ecc-buffer/ecc-buffer-timestamp)


(provide 'ecc-buffer/ecc-buffer)

(when
    (not load-file-name)
  (message "ecc-buffer.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))