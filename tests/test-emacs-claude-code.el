;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-28 07:10:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/test-emacs-claude-code.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'emacs-claude-code)

(ert-deftest test-emacs-claude-code-loadable ()
  "Test that emacs-claude-code loads correctly."
  (should (featurep 'emacs-claude-code)))

(ert-deftest test-ecc-loadable ()
  "Test that ecc module loads correctly."
  (should (featurep 'ecc)))

(provide 'test-emacs-claude-code)

(when (not load-file-name)
  (message "test-emacs-claude-code.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))