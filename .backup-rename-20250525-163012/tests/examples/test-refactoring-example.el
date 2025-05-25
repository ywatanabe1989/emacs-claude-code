;;; -*- coding: utf-8; lexical-binding: t -*-
;;; test-refactoring-example.el --- Example of refactored tests following best practices
;;; Author: Claude
;;; Timestamp: <2025-05-25>

;;; Commentary:
;;; This file demonstrates how to refactor tests to follow testing best practices:
;;; - One assertion per test
;;; - Self-explanatory test names
;;; - Clear AAA (Arrange-Act-Assert) structure
;;; - Test isolation

(require 'ert)
(require 'ecc-auto-response)

;;; Test Helpers (Arrange)
(defun test-helper-create-claude-buffer (name)
  "Create a test buffer with NAME for Claude interaction."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (setq ecc-auto-response-notify t))
    buffer))

(defun test-helper-capture-notification ()
  "Create a function that captures notification messages."
  (let ((captured nil))
    (lambda (format-string &rest args)
      (setq captured (apply #'format format-string args))
      captured)))

;;; Before: Multiple assertions in one test
;;; BAD EXAMPLE - DO NOT COPY
;; (ert-deftest test-auto-response-multiple-buffer-names ()
;;   "Test that each buffer's auto-response shows its own name."
;;   ;; This test has 6 assertions testing different behaviors!
;;   ...)

;;; After: Single assertion per test
;;; GOOD EXAMPLES - FOLLOW THIS PATTERN

(ert-deftest test-auto-response-should-include-buffer-name-claude-project-a-in-yn-notification ()
  "Auto-response Y/N notification includes buffer name CLAUDE-PROJECT-A."
  ;; Arrange
  (let* ((buffer-name "*CLAUDE-PROJECT-A*")
         (buffer (test-helper-create-claude-buffer buffer-name))
         (capture-fn (test-helper-capture-notification))
         captured-message)
    
    (unwind-protect
        (progn
          ;; Act
          (cl-letf (((symbol-function 'message) capture-fn))
            (ecc-auto-response--send-to-buffer buffer "1" "Y/N")
            (setq captured-message (funcall capture-fn "" nil)))
          
          ;; Assert - Single assertion
          (should (string-match-p "\\[\\*CLAUDE-PROJECT-A\\*\\] Auto-response to Y/N: 1" 
                                 captured-message)))
      
      ;; Cleanup
      (kill-buffer buffer))))

(ert-deftest test-auto-response-should-include-buffer-name-claude-project-a-in-continue-notification ()
  "Auto-response Continue notification includes buffer name CLAUDE-PROJECT-A."
  ;; Arrange
  (let* ((buffer-name "*CLAUDE-PROJECT-A*")
         (buffer (test-helper-create-claude-buffer buffer-name))
         (capture-fn (test-helper-capture-notification))
         captured-message)
    
    (unwind-protect
        (progn
          ;; Act
          (cl-letf (((symbol-function 'message) capture-fn))
            (ecc-auto-response--send-to-buffer buffer "/user:auto" "Continue")
            (setq captured-message (funcall capture-fn "" nil)))
          
          ;; Assert - Single assertion
          (should (string-match-p "\\[\\*CLAUDE-PROJECT-A\\*\\] Auto-response to Continue: /user:auto" 
                                 captured-message)))
      
      ;; Cleanup
      (kill-buffer buffer))))

;;; State Reset Tests - Before and After

;;; Before: Multiple state checks in one test
;;; BAD EXAMPLE
;; (ert-deftest test-auto-core-reset-state ()
;;   "Test resetting auto-core state."
;;   (ecc-auto-core-reset-state)
;;   (should-not ecc-auto-core--last-state)
;;   (should (= ecc-auto-core--last-response-time 0))
;;   (should (= ecc-auto-core--initial-check-count 0)))

;;; After: Separate test for each state value
;;; GOOD EXAMPLES

(ert-deftest test-reset-state-should-clear-last-state-to-nil ()
  "Reset state clears last-state to nil."
  ;; Arrange
  (setq ecc-auto-core--last-state :y/n)
  
  ;; Act
  (ecc-auto-core-reset-state)
  
  ;; Assert
  (should-not ecc-auto-core--last-state))

(ert-deftest test-reset-state-should-clear-last-response-time-to-zero ()
  "Reset state clears last-response-time to 0."
  ;; Arrange
  (setq ecc-auto-core--last-response-time (float-time))
  
  ;; Act
  (ecc-auto-core-reset-state)
  
  ;; Assert
  (should (= ecc-auto-core--last-response-time 0)))

(ert-deftest test-reset-state-should-clear-initial-check-count-to-zero ()
  "Reset state clears initial-check-count to 0."
  ;; Arrange
  (setq ecc-auto-core--initial-check-count 5)
  
  ;; Act
  (ecc-auto-core-reset-state)
  
  ;; Assert
  (should (= ecc-auto-core--initial-check-count 0)))

;;; Special Character Tests - Parameterized Test Pattern

(defun test-helper-verify-buffer-name-in-notification (buffer-name expected-pattern)
  "Verify that BUFFER-NAME appears as EXPECTED-PATTERN in notification."
  (let* ((buffer (test-helper-create-claude-buffer buffer-name))
         (capture-fn (test-helper-capture-notification))
         captured-message)
    
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'message) capture-fn))
            (ecc-auto-response--send-to-buffer buffer "1" "Y/N")
            (setq captured-message (funcall capture-fn "" nil)))
          
          ;; Return whether pattern matches
          (string-match-p expected-pattern captured-message))
      
      (kill-buffer buffer))))

(ert-deftest test-auto-response-should-escape-angle-brackets-in-buffer-name ()
  "Auto-response correctly escapes < and > in buffer names."
  (should (test-helper-verify-buffer-name-in-notification 
           "*CLAUDE<2>*" 
           "\\[\\*CLAUDE<2>\\*\\]")))

(ert-deftest test-auto-response-should-escape-pipe-character-in-buffer-name ()
  "Auto-response correctly escapes | in buffer names."
  (should (test-helper-verify-buffer-name-in-notification 
           "*claude|test*" 
           "\\[\\*claude|test\\*\\]")))

(ert-deftest test-auto-response-should-handle-at-symbol-in-buffer-name ()
  "Auto-response correctly handles @ in buffer names."
  (should (test-helper-verify-buffer-name-in-notification 
           "*claude@server*" 
           "\\[\\*claude@server\\*\\]")))

(ert-deftest test-auto-response-should-escape-square-brackets-in-buffer-name ()
  "Auto-response correctly escapes [ and ] in buffer names."
  (should (test-helper-verify-buffer-name-in-notification 
           "*claude [workspace]*" 
           "\\[\\*claude \\[workspace\\]\\*\\]")))

;;; Test Documentation

;;; Each test now:
;;; 1. Tests exactly ONE behavior
;;; 2. Has a descriptive name following: should-[behavior]-when-[condition]
;;; 3. Follows AAA pattern with clear sections
;;; 4. Is independent of other tests
;;; 5. Uses helper functions to reduce duplication
;;; 6. Cleans up resources properly

;;; Benefits of this approach:
;;; - When a test fails, you know EXACTLY what broke
;;; - Tests serve as documentation of expected behavior
;;; - Tests are easier to understand and maintain
;;; - Debugging is faster - no need to figure out which assertion failed
;;; - Tests can run in any order without side effects

(provide 'test-refactoring-example)
;;; test-refactoring-example.el ends here