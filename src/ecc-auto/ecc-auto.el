;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 09:22:34>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-auto/ecc-auto.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ecc-auto-monitor)
(require 'ecc-auto-toggle-not-used)
(require 'ecc-auto-mode) ;; Backward compatibility module
(require 'ecc-ui-notification nil t) ;; Optional load for notifications

;; Provide standard aliases to ensure compatibility between old and new code
(defalias 'ecc-auto-toggle 'ecc-auto-toggle)
(defalias 'ecc-auto-enable 'ecc-auto-enable)
(defalias 'ecc-auto-disable 'ecc-auto-disable)
(defalias 'ecc-auto-check-and-restart 'ecc-auto-check-and-restart)


(provide 'ecc-auto)

(when
    (not load-file-name)
  (message "ecc-auto.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))