;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-10-24 18:50:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-anti-flicker.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'ecc-anti-flicker)

;; Basic loading tests
(ert-deftest test-ecc-anti-flicker-feature-loads ()
  "Test that ecc-anti-flicker feature loads successfully."
  (should (featurep 'ecc-anti-flicker)))

(ert-deftest test-ecc-anti-flicker-scrollback-size-is-integer ()
  "Test that scrollback size is an integer."
  (should (integerp --ecc-anti-flicker-scrollback-size)))

(ert-deftest test-ecc-anti-flicker-scrollback-size-positive ()
  "Test that scrollback size is positive."
  (should (> --ecc-anti-flicker-scrollback-size 0)))

(ert-deftest test-ecc-anti-flicker-context-threshold-valid ()
  "Test that context threshold is between 0 and 1."
  (should (and (>= --ecc-anti-flicker-context-threshold 0)
               (<= --ecc-anti-flicker-context-threshold 1))))

;; Configuration tests
(ert-deftest test-ecc-anti-flicker-default-scrollback-100k ()
  "Test that default scrollback is 100,000 (vterm max)."
  (should (= --ecc-anti-flicker-scrollback-size 100000)))

(ert-deftest test-ecc-anti-flicker-default-threshold-50percent ()
  "Test that default threshold is 0.5 (50%)."
  (should (= --ecc-anti-flicker-context-threshold 0.5)))

;; Buffer-local variables tests
(ert-deftest test-ecc-anti-flicker-variables-are-buffer-local ()
  "Test that key variables are buffer-local."
  (should (local-variable-p '--ecc-anti-flicker--enabled))
  (should (local-variable-p '--ecc-anti-flicker--check-timer))
  (should (local-variable-p '--ecc-anti-flicker--last-warning-time)))

;; Status function tests
(ert-deftest test-ecc-anti-flicker-status-function-exists ()
  "Test that status function is defined."
  (should (fboundp '--ecc-anti-flicker-status)))

;; Toggle function tests
(ert-deftest test-ecc-anti-flicker-toggle-function-exists ()
  "Test that toggle function is defined."
  (should (fboundp '--ecc-anti-flicker-toggle)))

;; Hook management tests
(ert-deftest test-ecc-anti-flicker-hook-functions-exist ()
  "Test that hook management functions exist."
  (should (fboundp '--ecc-anti-flicker-setup-vterm-hook))
  (should (fboundp '--ecc-anti-flicker-remove-vterm-hook)))

;; Font configuration tests
(ert-deftest test-ecc-anti-flicker-font-config-exists ()
  "Test that font configuration function exists."
  (should (fboundp '--ecc-anti-flicker-configure-fonts)))

(ert-deftest test-ecc-anti-flicker-default-font-is-string ()
  "Test that default font family is a string."
  (should (stringp --ecc-anti-flicker-font-family)))

;; Warning cooldown tests
(ert-deftest test-ecc-anti-flicker-warning-cooldown-is-positive ()
  "Test that warning cooldown is positive."
  (with-temp-buffer
    (should (> --ecc-anti-flicker--warning-cooldown 0))))

;; Monitoring tests
(ert-deftest test-ecc-anti-flicker-monitoring-functions-exist ()
  "Test that monitoring functions are defined."
  (should (fboundp '--ecc-anti-flicker--start-monitoring))
  (should (fboundp '--ecc-anti-flicker--stop-monitoring))
  (should (fboundp '--ecc-anti-flicker--check-buffer-size)))

(provide 'test-ecc-anti-flicker)

(when (not load-file-name)
  (message "test-ecc-anti-flicker.el loaded."))
