;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 18:09:08>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-send/ecc-send.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ecc-send-general)
(require 'ecc-send-accept)


(provide 'ecc-send)

(when
    (not load-file-name)
  (message "ecc-send.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))