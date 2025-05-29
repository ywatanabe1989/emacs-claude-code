;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-28 07:11:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-debug.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'ecc-debug)

(ert-deftest test-ecc-debug-feature-loads-without-error ()
  "Test that ecc-debug feature loads successfully without errors."
  (should (featurep 'ecc-debug)))

(ert-deftest test-ecc-debug-toggle-exists ()
  "Test that ecc-debug-toggle function exists."
  (should (functionp 'ecc-debug-toggle)))

(ert-deftest test-ecc-debug-toggle-interactive ()
  "Test that ecc-debug-toggle is interactive."
  (should (commandp 'ecc-debug-toggle)))

(ert-deftest test-ecc-debug-initially-disabled ()
  "Test that debug mode is disabled by default on startup."
  (should-not --ecc-debug-enabled))

(ert-deftest test-ecc-debug-message-function-exists ()
  "Test that --ecc-debug-message function exists."
  (should (functionp '--ecc-debug-message)))

(ert-deftest test-ecc-debug-prefix-variable-exists ()
  "Test that debug prefix variable exists."
  (should (boundp '--ecc-debug-prefix)))

(provide 'test-ecc-debug)

(when (not load-file-name)
  (message "test-ecc-debug.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))