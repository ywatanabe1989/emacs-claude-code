;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 00:07:44>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-hello-world/tests/test-emacs-hello-world.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; Commentary:
;; Main entry point for testing the emacs-hello-world package

;;; Code:

(require 'ert)

;; Test main package loadability
(ert-deftest test-emacs-hello-world-loadable ()
  "Test if emacs-hello-world is loadable."
  (require 'emacs-hello-world)
  (should (featurep 'emacs-hello-world)))

;;; test-emacs-hello-world.el ends here


(provide 'test-emacs-hello-world)

(when
    (not load-file-name)
  (message "test-emacs-hello-world.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))