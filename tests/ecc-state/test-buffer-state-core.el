;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 21:55:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-state/test-buffer-state-refactored.el

;;; Commentary:
;;; Tests for refactored buffer state module

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
(ert-deftest test-buffer-state-get-set-refactored ()
  "Test that buffer state getter and setter functions work correctly."
  (with-temp-buffer
    (ecc-buffer-state-init)
    ;; Set and get a regular value
    (ecc-buffer-state-set 'test-key "test-value")
    (should (equal (ecc-buffer-state-get 'test-key) "test-value"))
    ;; Test has-key predicate
    (should (ecc-buffer-state-has-key-p 'test-key))
    (should-not (ecc-buffer-state-has-key-p 'nonexistent-key))
    ;; Test removing a key
    (ecc-buffer-state-remove 'test-key)
    (should-not (ecc-buffer-state-has-key-p 'test-key))
    ;; Test using constants
    (ecc-buffer-state-set ecc-buffer-state-key-prompt :y/n)
    (should (equal (ecc-buffer-state-get ecc-buffer-state-key-prompt) :y/n))))

;; Test prompt state predicates
(ert-deftest test-buffer-state-predicates-refactored ()
  "Test that prompt state predicates work correctly."
  (with-temp-buffer
    (ecc-buffer-state-init)
    ;; Test generic prompt state predicate
    (ecc-buffer-state-set ecc-buffer-state-key-prompt :y/n)
    (should (ecc-buffer-state-has-prompt-p :y/n))
    (should-not (ecc-buffer-state-has-prompt-p :waiting))
    ;; Test specific state predicates
    (should (ecc-buffer-state-y/n-p))
    (should-not (ecc-buffer-state-y/y/n-p))
    (should-not (ecc-buffer-state-waiting-p))
    (should-not (ecc-buffer-state-initial-waiting-p))
    ;; Change state and test again
    (ecc-buffer-state-set ecc-buffer-state-key-prompt :waiting)
    (should-not (ecc-buffer-state-y/n-p))
    (should (ecc-buffer-state-waiting-p))))

;; Test throttling behavior
(ert-deftest test-buffer-state-throttling-refactored ()
  "Test that throttling functions work correctly."
  (with-temp-buffer
    (ecc-buffer-state-init)
    ;; Set up test variables and state
    (setq-local ecc-auto-response-throttle-time 1.0) ;; 1 second throttle
    (ecc-buffer-state-update-prompt :y/n)
    ;; Test active state duplication
    (should (ecc-buffer-state-duplicate-active-p :y/n))
    (should-not (ecc-buffer-state-duplicate-active-p :waiting))
    ;; Test time-based throttling
    (should (ecc-buffer-state-time-throttled-p :y/n))
    ;; Test combined throttling
    (should (ecc-buffer-state-throttled-p :y/n))
    (should-not (ecc-buffer-state-throttled-p :waiting))))

;; Test state update and export/import
(ert-deftest test-buffer-state-update-export-refactored ()
  "Test that state update and export/import functions work correctly."
  (with-temp-buffer
    ;; Set up older variables
    (setq-local ecc-buffer-state :y/n)
    (setq-local ecc-buffer-last-state-time (float-time))
    (setq-local ecc-buffer-active-state :y/n)
    ;; Initialize buffer state (should import existing variables)
    (ecc-buffer-state-init)
    ;; Check that state was properly imported
    (should (eq (ecc-buffer-state-get-prompt) :y/n))
    (should (eq (ecc-buffer-state-get-active) :y/n))
    ;; Update state in new system
    (ecc-buffer-state-update-prompt :waiting)
    ;; Export back to old variables
    (ecc-buffer-state-export-standard)
    ;; Check that old variables got updated
    (should (eq ecc-buffer-state :waiting))
    (should (eq ecc-buffer-active-state :waiting))))

;; Test debug info
(ert-deftest test-buffer-state-debug-info-refactored ()
  "Test that debug info function works correctly."
  (with-temp-buffer
    (ecc-buffer-state-init)
    (ecc-buffer-state-update-prompt :y/n)
    (let ((debug-info (ecc-buffer-state-debug-info)))
      (should (stringp debug-info))
      (should (string-match-p "Current State: :y/n" debug-info))
      (should (string-match-p "Active State: :y/n" debug-info)))))

(provide 'test-buffer-state-refactored)

;;; test-buffer-state-refactored.el ends here