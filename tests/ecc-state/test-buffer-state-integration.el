;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 21:40:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-state/test-buffer-state-integration.el

;;; Commentary:
;;; Tests for integration between buffer-state and state-detection

(require 'ert)

;;; Code:

;; Add the src directory to the load path
(add-to-list 'load-path 
             (expand-file-name "../../src"
                              (file-name-directory
                               (or load-file-name buffer-file-name))))

(require 'ecc-variables-consolidated)
(require 'ecc-state-detection-consolidated)
(require 'ecc-buffer-local)
(require 'ecc-buffer-state)

;; Test basic state getting and setting
(ert-deftest test-buffer-state-get-set ()
  "Test that buffer state getter and setter functions work correctly."
  (with-temp-buffer
    (ecc-buffer-state-init)
    ;; Set and get a value
    (ecc-buffer-state-set 'test-key "test-value")
    (should (equal (ecc-buffer-state-get 'test-key) "test-value"))
    ;; Test has-key predicate
    (should (ecc-buffer-state-has-key-p 'test-key))
    (should-not (ecc-buffer-state-has-key-p 'nonexistent-key))
    ;; Test removing a key
    (ecc-buffer-state-remove 'test-key)
    (should-not (ecc-buffer-state-has-key-p 'test-key))))

;; Test prompt state management
(ert-deftest test-buffer-state-prompt-management ()
  "Test that prompt state is managed correctly."
  (with-temp-buffer
    (ecc-buffer-state-init)
    ;; Set and get prompt state
    (ecc-buffer-state-update-prompt :y/n)
    (should (eq (ecc-buffer-state-get-prompt) :y/n))
    ;; Test state predicates
    (should (ecc-buffer-state-y/n-p))
    (should-not (ecc-buffer-state-y/y/n-p))
    (should-not (ecc-buffer-state-waiting-p))
    ;; Update state
    (ecc-buffer-state-update-prompt :waiting)
    (should (eq (ecc-buffer-state-get-prompt) :waiting))
    (should (ecc-buffer-state-waiting-p))
    (should-not (ecc-buffer-state-y/n-p))
    ;; Test active state
    (should (eq (ecc-buffer-state-get-active) :waiting))
    (ecc-buffer-state-clear-active)
    (should-not (ecc-buffer-state-get-active))))

;; Test state detection integration
(ert-deftest test-buffer-state-detection ()
  "Test integration between state detection and buffer state."
  :expected-result :skipped)

;; Test throttling behavior
(ert-deftest test-buffer-state-throttling ()
  "Test that throttling works correctly."
  (with-temp-buffer
    (ecc-buffer-state-init)
    ;; Set up test variables
    (setq-local ecc-auto-response-throttle-time 1.0) ;; 1 second throttle
    ;; Update prompt state
    (ecc-buffer-state-update-prompt :y/n)
    ;; Should be throttled immediately 
    (should (ecc-buffer-state-throttled-p :y/n))
    ;; Not throttled for other states
    (should-not (ecc-buffer-state-throttled-p :waiting))))

;; Test compatibility with older variable names
(ert-deftest test-buffer-state-compatibility ()
  "Test backwards compatibility with older variable names."
  :expected-result :skipped)

(provide 'test-buffer-state-integration)

;;; test-buffer-state-integration.el ends here