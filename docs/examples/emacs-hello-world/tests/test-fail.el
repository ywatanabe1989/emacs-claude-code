;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 00:08:05>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-hello-world/tests/test-fail.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ert)

;; Test main package loadability
(ert-deftest test-this-must-raise-error ()
  ""
  (should nil))


(provide 'test-fail)

(when
    (not load-file-name)
  (message "test-fail.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))