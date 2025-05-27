;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-28 07:20:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-notification.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'ecc-notification)

(ert-deftest test-ecc-notification-loadable ()
  "Test that ecc-notification loads correctly."
  (should (featurep 'ecc-notification)))

(ert-deftest test-ecc-notification-toggle-exists ()
  "Test that ecc-notification-toggle function exists."
  (should (functionp 'ecc-notification-toggle)))

(ert-deftest test-ecc-notification-toggle-interactive ()
  "Test that ecc-notification-toggle is interactive."
  (should (commandp 'ecc-notification-toggle)))

(ert-deftest test-ecc-notification-enabled-variable-exists ()
  "Test that notification enabled variable exists."
  (should (boundp '--ecc-notification-enabled)))

(ert-deftest test-ecc-notification-methods-variable-exists ()
  "Test that notification methods variable exists."
  (should (boundp '--ecc-notification-methods)))

(ert-deftest test-ecc-notification-methods-is-list ()
  "Test that notification methods is a list."
  (should (listp --ecc-notification-methods)))

(provide 'test-ecc-notification)

(when (not load-file-name)
  (message "test-ecc-notification.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))