;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 18:09:08>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-send/ecc-send.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ecc-send-general)
(require 'ecc-send-accept)


;; Backward compatibility with old function names
(defalias 'ecc-send-interrupt 'ecc-term-send-interrupt)
(defalias 'ecc-send-clear-history 'ecc-term-send-clear-history)
(defalias 'ecc-send-compact-history 'ecc-term-send-compact-history)
(defalias 'ecc-send-edit-memory 'ecc-term-send-edit-memory)
(defalias 'ecc-send-region 'ecc-term-send-region)
(defalias 'ecc-send-paste-to-active-buffer 'ecc-term-send-paste-to-active-buffer)
(defalias 'ecc-send-template 'ecc-term-send-template)
(defalias 'ecc-send-accept 'ecc-term-send-accept)

(defalias '--ecc-auto-send-1-y/n '--ecc-term-auto-send-1-y/n)
(defalias '--ecc-auto-send-2-y/y/n '--ecc-term-auto-send-2-y/y/n)
(defalias '--ecc-auto-send-3 '--ecc-term-auto-send-3)
(defalias '--ecc-auto-send-custom '--ecc-term-auto-send-custom)
(defalias '--ecc-auto-send-continue-on-y/y/n '--ecc-term-auto-send-continue-on-y/y/n)
(defalias '--ecc-auto-send-skip '--ecc-term-auto-send-skip)
(defalias '--ecc-send-string '--ecc-term-send-string)
(defalias '--ecc-send-by-state '--ecc-term-send-by-state)
(defalias 'ecc-auto-send-y 'ecc-term-auto-send-y)
(defalias 'ecc-auto-send-yy 'ecc-term-auto-send-yy)
(defalias 'ecc-auto-send-continue 'ecc-term-auto-send-continue)

;; Provide both the new and old feature names for backward compatibility
(provide 'ecc-term-send)
(provide 'ecc-send)

(when
    (not load-file-name)
  (message "ecc-term-send.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))