;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-28 07:13:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-auto-response.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'ecc-auto-response)

(ert-deftest test-ecc-auto-response-loadable ()
  "Test that ecc-auto-response loads correctly."
  (should (featurep 'ecc-auto-response)))

(ert-deftest test-ecc-auto-toggle-exists ()
  "Test that ecc-auto-toggle alias exists."
  (should (functionp 'ecc-auto-toggle)))

(ert-deftest test-ecc-auto-toggle-interactive ()
  "Test that ecc-auto-toggle is interactive."
  (should (commandp 'ecc-auto-toggle)))

(ert-deftest test-ecc-auto-response-interval-is-number ()
  "Test that auto-response interval is a number."
  (should (numberp --ecc-auto-response-interval)))

(ert-deftest test-ecc-auto-response-interval-positive ()
  "Test that auto-response interval is positive."
  (should (> --ecc-auto-response-interval 0)))

(ert-deftest test-ecc-auto-response-safe-interval-is-number ()
  "Test that safe interval is a number."
  (should (numberp --ecc-auto-response-safe-interval)))

(ert-deftest test-ecc-auto-response-safe-interval-positive ()
  "Test that safe interval is positive."
  (should (> --ecc-auto-response-safe-interval 0)))

(ert-deftest test-ecc-auto-response-throttle-duration-is-number ()
  "Test that throttle duration is a number."
  (should (numberp --ecc-auto-response-throttle-duration)))

(ert-deftest test-ecc-auto-response-throttle-duration-positive ()
  "Test that throttle duration is positive."
  (should (> --ecc-auto-response-throttle-duration 0)))

(ert-deftest test-ecc-auto-response-responses-is-list ()
  "Test that response alist is a list."
  (should (listp --ecc-auto-response-responses)))

(ert-deftest test-ecc-auto-response-responses-has-yn ()
  "Test that response alist contains :y/n key."
  (should (assq :y/n --ecc-auto-response-responses)))

(ert-deftest test-ecc-auto-response-responses-has-waiting ()
  "Test that response alist contains :waiting key."
  (should (assq :waiting --ecc-auto-response-responses)))

(ert-deftest test-ecc-auto-response-registry-exists ()
  "Test that buffer registry exists."
  (should (hash-table-p --ecc-auto-response--registered-buffers)))

(provide 'test-ecc-auto-response)

(when (not load-file-name)
  (message "test-ecc-auto-response.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))