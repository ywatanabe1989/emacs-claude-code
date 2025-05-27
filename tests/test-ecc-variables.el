;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-28 07:23:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-variables.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'ecc-variables)

(ert-deftest test-ecc-variables-loadable ()
  "Test that ecc-variables loads correctly."
  (should (featurep 'ecc-variables)))

(ert-deftest test-ecc-version-exists ()
  "Test that ecc version variable exists."
  (should (boundp '--ecc-version)))

(ert-deftest test-ecc-version-is-string ()
  "Test that ecc version is a string."
  (should (stringp --ecc-version)))

(provide 'test-ecc-variables)

(when (not load-file-name)
  (message "test-ecc-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))