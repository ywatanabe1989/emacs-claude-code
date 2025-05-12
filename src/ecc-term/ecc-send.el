;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 09:55:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-term/ecc-send.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;; This file provides backward compatibility for the ecc-send module
;; that was split into ecc-term-send-general and ecc-term-send-accept 
;; during the refactoring.

;; First load the real implementation files
(require 'ecc-term-send-general)
(require 'ecc-term-send-accept)

;; For backward compatibility, define most common functions
;; This is needed for tests that check if the module is available
(unless (fboundp 'ecc-send-accept)
  (defalias 'ecc-send-accept 'ecc-term-send-accept))

(unless (fboundp 'ecc-send-waiting)
  (defalias 'ecc-send-waiting 'ecc-term-send-waiting))

(provide 'ecc-send)

(when (not load-file-name)
  (message "ecc-send.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))