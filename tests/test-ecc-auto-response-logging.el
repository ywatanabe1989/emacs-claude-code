;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-06-04 07:13:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-auto-response-logging.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'ecc-auto-response-logging)

(ert-deftest test-ecc-auto-response-logging-loadable ()
  "Test if ecc-auto-response-logging is loadable."
  (should (featurep 'ecc-auto-response-logging)))

(ert-deftest test-ecc-auto-response-log-buffer-name-exists ()
  "Test that log buffer name variable exists."
  (should (boundp 'ecc-auto-response-log-buffer-name))
  (should (stringp ecc-auto-response-log-buffer-name)))

(ert-deftest test-ecc-auto-response-log-level-exists ()
  "Test that log level variable exists with valid value."
  (should (boundp 'ecc-auto-response-log-level))
  (should (memq ecc-auto-response-log-level '(debug info warn error))))

(ert-deftest test-ecc-auto-response-log-max-entries-exists ()
  "Test that max entries variable exists and is positive."
  (should (boundp 'ecc-auto-response-log-max-entries))
  (should (integerp ecc-auto-response-log-max-entries))
  (should (> ecc-auto-response-log-max-entries 0)))

(ert-deftest test-ecc-auto-response-log-function-exists ()
  "Test that main log function exists."
  (should (fboundp 'ecc-auto-response-log)))

(ert-deftest test-ecc-auto-response-log-adds-entry ()
  "Test that logging adds entries."
  (let ((--ecc-auto-response-log-entries nil))
    (ecc-auto-response-log 'info "Test message")
    (should (= (length --ecc-auto-response-log-entries) 1))
    (should (string-match-p "Test message" 
                           (plist-get (car --ecc-auto-response-log-entries) :message)))))

(ert-deftest test-ecc-auto-response-log-respects-level ()
  "Test that logging respects log level."
  (let ((--ecc-auto-response-log-entries nil)
        (ecc-auto-response-log-level 'warn))
    ;; Debug message should not be logged
    (ecc-auto-response-log 'debug "Debug message")
    (should (= (length --ecc-auto-response-log-entries) 0))
    ;; Error message should be logged
    (ecc-auto-response-log 'error "Error message")
    (should (= (length --ecc-auto-response-log-entries) 1))))

(ert-deftest test-ecc-auto-response-show-log-interactive ()
  "Test that show log is an interactive command."
  (should (commandp 'ecc-auto-response-show-log)))

(ert-deftest test-ecc-auto-response-clear-log-interactive ()
  "Test that clear log is an interactive command."
  (should (commandp 'ecc-auto-response-clear-log)))

(ert-deftest test-ecc-auto-response-export-log-interactive ()
  "Test that export log is an interactive command."
  (should (commandp 'ecc-auto-response-export-log)))

(ert-deftest test-ecc-specialized-logging-functions ()
  "Test that specialized logging functions exist."
  (should (fboundp 'ecc-auto-response-log-state-detection))
  (should (fboundp 'ecc-auto-response-log-send-attempt))
  (should (fboundp 'ecc-auto-response-log-send-success))
  (should (fboundp 'ecc-auto-response-log-send-failure))
  (should (fboundp 'ecc-auto-response-log-throttle)))

(provide 'test-ecc-auto-response-logging)

(when (not load-file-name)
  (message "test-ecc-auto-response-logging.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))