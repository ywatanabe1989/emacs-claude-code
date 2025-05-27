;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-28 07:14:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-state-detection.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'ecc-state-detection)

(ert-deftest test-ecc-state-detection-loadable ()
  "Test that ecc-state-detection loads correctly."
  (should (featurep 'ecc-state-detection)))

(ert-deftest test-ecc-state-detection-patterns-is-list ()
  "Test that state detection patterns is a list."
  (should (listp --ecc-state-detection-patterns)))

(ert-deftest test-ecc-state-detection-patterns-not-empty ()
  "Test that state detection patterns list is not empty."
  (should (> (length --ecc-state-detection-patterns) 0)))

(ert-deftest test-ecc-state-detection-buffer-size-is-integer ()
  "Test that buffer size is an integer."
  (should (integerp --ecc-state-detection-buffer-size)))

(ert-deftest test-ecc-state-detection-buffer-size-positive ()
  "Test that buffer size is positive."
  (should (> --ecc-state-detection-buffer-size 0)))

(ert-deftest test-ecc-state-detection-yn-pattern ()
  "Test Y/N state detection."
  (with-temp-buffer
    (insert "Some text [y/n] prompt")
    (should (eq (--ecc-state-detection-detect) :y/n))))

(ert-deftest test-ecc-state-detection-yyn-pattern ()
  "Test Y/Y/N state detection."
  (with-temp-buffer
    (insert "Some text [Y/y/n] prompt")
    (should (eq (--ecc-state-detection-detect) :y/y/n))))

(ert-deftest test-ecc-state-detection-waiting-pattern ()
  "Test waiting state detection."
  (with-temp-buffer
    (insert "│ >                            ")
    (should (eq (--ecc-state-detection-detect) :waiting))))

(ert-deftest test-ecc-state-detection-no-match ()
  "Test that nil is returned when no pattern matches."
  (with-temp-buffer
    (insert "Just regular text without any patterns")
    (should-not (--ecc-state-detection-detect))))

(ert-deftest test-ecc-state-detection-get-name ()
  "Test state name conversion."
  (should (string= (--ecc-state-detection-get-name :y/n) "Y/N"))
  (should (string= (--ecc-state-detection-get-name :waiting) "Continue")))

(provide 'test-ecc-state-detection)

(when (not load-file-name)
  (message "test-ecc-state-detection.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))