;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-28 07:12:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-list.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'ecc-list)

(ert-deftest test-ecc-list-loadable ()
  "Test that ecc-list loads correctly."
  (should (featurep 'ecc-list)))

(ert-deftest test-ecc-list-buffers-exists ()
  "Test that ecc-list-buffers function exists."
  (should (functionp 'ecc-list-buffers)))

(ert-deftest test-ecc-list-buffers-interactive ()
  "Test that ecc-list-buffers is interactive."
  (should (commandp 'ecc-list-buffers)))

(ert-deftest test-ecc-list-mode-map-exists ()
  "Test that buffer list mode map exists."
  (should (keymapp --ecc-buffer-list-mode-map)))

(ert-deftest test-ecc-list-auto-refresh-default ()
  "Test that auto-refresh is enabled by default."
  (should --ecc-buffer-list-auto-refresh))

(ert-deftest test-ecc-list-refresh-interval-is-number ()
  "Test that refresh interval is a number."
  (should (numberp --ecc-buffer-list-refresh-interval)))

(ert-deftest test-ecc-list-refresh-interval-positive ()
  "Test that refresh interval is positive."
  (should (> --ecc-buffer-list-refresh-interval 0)))

(provide 'test-ecc-list)

(when (not load-file-name)
  (message "test-ecc-list.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))