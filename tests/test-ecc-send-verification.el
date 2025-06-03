;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-06-04 07:12:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-send-verification.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'ecc-send-verification)

(ert-deftest test-ecc-send-verification-loadable ()
  "Test if ecc-send-verification is loadable."
  (should (featurep 'ecc-send-verification)))

(ert-deftest test-ecc-send-verification-timeout-exists ()
  "Test that timeout variable exists and is a number."
  (should (boundp 'ecc-send-verification-timeout))
  (should (numberp ecc-send-verification-timeout))
  (should (> ecc-send-verification-timeout 0)))

(ert-deftest test-ecc-send-verification-retry-count-exists ()
  "Test that retry count variable exists and is positive."
  (should (boundp 'ecc-send-verification-retry-count))
  (should (integerp ecc-send-verification-retry-count))
  (should (> ecc-send-verification-retry-count 0)))

(ert-deftest test-ecc-send-verification-check-interval-exists ()
  "Test that check interval variable exists and is positive."
  (should (boundp 'ecc-send-verification-check-interval))
  (should (numberp ecc-send-verification-check-interval))
  (should (> ecc-send-verification-check-interval 0)))

(ert-deftest test-ecc-get-buffer-end-content ()
  "Test buffer end content extraction."
  (with-temp-buffer
    (insert "This is a test buffer with some content")
    (should (string-match-p "content$" (ecc--get-buffer-end-content)))))

(ert-deftest test-ecc-buffer-content-advanced-p ()
  "Test content advancement detection."
  (should (ecc--buffer-content-advanced-p "hello" "hello world"))
  (should-not (ecc--buffer-content-advanced-p "hello" "hello"))
  (should-not (ecc--buffer-content-advanced-p "hello world" "hello")))

(ert-deftest test-ecc-send-verify-command-exists ()
  "Test that main verification function exists."
  (should (fboundp 'ecc-send-verify-command)))

(ert-deftest test-ecc-verify-prompt-ready-exists ()
  "Test that prompt verification function exists."
  (should (fboundp 'ecc-verify-prompt-ready)))

(ert-deftest test-ecc-send-with-verification-exists ()
  "Test that integration helper exists."
  (should (fboundp 'ecc-send-with-verification)))

(provide 'test-ecc-send-verification)

(when (not load-file-name)
  (message "test-ecc-send-verification.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))