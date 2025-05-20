;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-12 23:16:48>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-hello-world/src/ehw-core/ehw-core.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ehw-core-greet)
(require 'ehw-core-hello-world)


(provide 'ehw-core)

(when
    (not load-file-name)
  (message "ehw-core.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))