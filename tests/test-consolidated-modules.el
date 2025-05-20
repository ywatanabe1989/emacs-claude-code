;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 17:50:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-consolidated-modules.el

;;; Commentary:
;;; Tests for consolidated Claude Code modules.
;;; 
;;; This file contains tests for the consolidated modules created during
;;; the clean code refactoring process. It ensures that the modules have
;;; maintained their functionality while improving their structure.

(require 'ert)
(require 'ecc-variables)
(require 'ecc-state-detection)
(require 'ecc-auto-core)
(require 'ecc-debug-utils)

;;; Code:

;;;; Test helpers

(defmacro with-test-buffer (&rest body)
  "Create a temporary buffer and evaluate BODY there.
The buffer is killed after BODY is evaluated."
  (declare (indent 0) (debug t))
  `(let ((temp-buffer (generate-new-buffer "*ecc-test*")))
     (unwind-protect
         (with-current-buffer temp-buffer
           ,@body)
       (kill-buffer temp-buffer))))

(defun insert-claude-prompt-y-n ()
  "Insert a Claude Y/N prompt into the current buffer."
  (erase-buffer)
  (insert "Some text before prompt\n")
  (insert (concat "Would you like to continue? " ecc-state-prompt-y/n "\n"))
  (insert "❯ 1. Yes\n  2. No\n"))

(defun insert-claude-prompt-y-y-n ()
  "Insert a Claude Y/Y/N prompt into the current buffer."
  (erase-buffer)
  (insert "Some text before prompt\n")
  (insert "Would you like to continue and apply these changes?\n")
  (insert (concat "  1. Yes\n" ecc-state-prompt-y/y/n "\n  3. No\n")))

(defun insert-claude-prompt-waiting ()
  "Insert a Claude waiting prompt into the current buffer."
  (erase-buffer)
  (insert "Some text before prompt\n")
  (insert ecc-state-prompt-waiting))

(defun insert-claude-prompt-initial-waiting ()
  "Insert a Claude initial waiting prompt into the current buffer."
  (erase-buffer)
  (insert "Some text before prompt\n")
  (insert ecc-state-prompt-initial-waiting))

(defun test-consolidated-feature-provided-p (feature)
  "Test if FEATURE is provided.
Returns t if the feature is available, nil otherwise."
  (featurep feature))

;;;; Variables module tests

(ert-deftest test-ecc-variables-customization-groups ()
  "Test that customization groups are properly defined."
  (should (get 'emacs-claude-code 'custom-group))
  (should (get 'ecc-buffers 'custom-group))
  (should (get 'ecc-auto-response 'custom-group))
  (should (get 'ecc-state-detection 'custom-group))
  (should (get 'ecc-vterm 'custom-group))
  (should (get 'ecc-notification 'custom-group))
  (should (get 'ecc-debug 'custom-group)))

(ert-deftest test-ecc-variables-custom-options ()
  "Test that key custom options are defined."
  (should (boundp 'ecc-auto-response-y/n))
  (should (boundp 'ecc-auto-response-y/y/n))
  (should (boundp 'ecc-auto-response-waiting))
  (should (boundp 'ecc-auto-response-initial-waiting))
  (should (boundp 'ecc-state-prompt-y/n))
  (should (boundp 'ecc-state-prompt-y/y/n))
  (should (boundp 'ecc-state-prompt-waiting))
  (should (boundp 'ecc-state-prompt-initial-waiting)))

;;;; State detection module tests

(ert-deftest test-ecc-state-detection-y-n ()
  "Test detection of Y/N prompts."
  (with-test-buffer
    (insert-claude-prompt-y-n)
    (should (eq (ecc-detect-state) :y/n))))

(ert-deftest test-ecc-state-detection-y-y-n ()
  "Test detection of Y/Y/N prompts."
  (with-test-buffer
    (insert-claude-prompt-y-y-n)
    (should (eq (ecc-detect-state) :y/y/n))))

(ert-deftest test-ecc-state-detection-waiting ()
  "Test detection of waiting prompts."
  (with-test-buffer
    (insert-claude-prompt-waiting)
    (should (eq (ecc-detect-state) :waiting))))

(ert-deftest test-ecc-state-detection-initial-waiting ()
  "Test detection of initial waiting prompts."
  (with-test-buffer
    (insert-claude-prompt-initial-waiting)
    (should (eq (ecc-detect-state) :initial-waiting))))

(ert-deftest test-ecc-state-detection-in-region ()
  "Test detection of prompts in specific region."
  (with-test-buffer
    (insert "Start of buffer\n")
    (let ((start (point)))
      (insert-claude-prompt-y-n)
      (let ((end (point)))
        (insert "\nEnd of buffer")
        (should (eq (ecc-detect-prompt-in-region start end) :y/n))))))

(ert-deftest test-ecc-state-get-name ()
  "Test conversion of state symbols to human-readable names."
  (should (string= (ecc-state-get-name :y/n) "Y/N"))
  (should (string= (ecc-state-get-name :y/y/n) "Y/Y/N"))
  (should (string= (ecc-state-get-name :waiting) "Continue"))
  (should (string= (ecc-state-get-name :initial-waiting) "Initial-Waiting")))

;;;; Consolidated wrapper modules tests

(ert-deftest test-state-detection-consolidated-provided ()
  "Test that the state-detection-consolidated feature is provided."
  (unwind-protect
      (progn
        ;; Unload first to ensure clean test
        (when (featurep 'ecc-state-detection-consolidated)
          (unload-feature 'ecc-state-detection-consolidated t))
        
        ;; Now require and verify
        (require 'ecc-state-detection-consolidated)
        (should (test-consolidated-feature-provided-p 'ecc-state-detection-consolidated)))
    ;; Cleanup - ensure we reload for other tests
    (when (featurep 'ecc-state-detection-consolidated)
      (unload-feature 'ecc-state-detection-consolidated t))))

(ert-deftest test-state-detection-functions-available ()
  "Test that core functions are available through the consolidated module."
  (unwind-protect
      (progn
        ;; Load through the wrapper
        (require 'ecc-state-detection-consolidated)
        
        ;; Verify core functions are available
        (should (fboundp 'ecc-detect-state))
        (should (fboundp 'ecc-detect-basic-state))
        (should (fboundp 'ecc-detect-prompt-in-last-lines))
        (should (fboundp 'ecc-detect-prompt-in-region))
        
        ;; Verify backward compatibility functions
        (should (fboundp 'ecc-detect-simple-state))
        (should (fboundp 'ecc-detect-enhanced-state))
        (should (fboundp 'ecc-detect-prompt-state)))
    
    ;; Cleanup
    (when (featurep 'ecc-state-detection-consolidated)
      (unload-feature 'ecc-state-detection-consolidated t))))

;;;; Test other consolidated modules (can be expanded as more modules are consolidated)

(ert-deftest test-debug-utils-consolidated-provided ()
  "Test that the debug-utils-consolidated feature is provided (if available)."
  (skip-unless (locate-library "ecc-debug-utils-consolidated"))
  
  (unwind-protect
      (progn
        ;; Unload first to ensure clean test
        (when (featurep 'ecc-debug-utils-consolidated)
          (unload-feature 'ecc-debug-utils-consolidated t))
        
        ;; Now require and verify
        (require 'ecc-debug-utils-consolidated)
        (should (test-consolidated-feature-provided-p 'ecc-debug-utils-consolidated)))
    
    ;; Cleanup
    (when (featurep 'ecc-debug-utils-consolidated)
      (unload-feature 'ecc-debug-utils-consolidated t))))

(ert-deftest test-term-claude-mode-consolidated-provided ()
  "Test that the term-claude-mode-consolidated feature is provided (if available)."
  (skip-unless (locate-library "ecc-term-claude-mode-consolidated"))
  
  (unwind-protect
      (progn
        ;; Unload first to ensure clean test
        (when (featurep 'ecc-term-claude-mode-consolidated)
          (unload-feature 'ecc-term-claude-mode-consolidated t))
        
        ;; Now require and verify
        (require 'ecc-term-claude-mode-consolidated)
        (should (test-consolidated-feature-provided-p 'ecc-term-claude-mode-consolidated)))
    
    ;; Cleanup
    (when (featurep 'ecc-term-claude-mode-consolidated)
      (unload-feature 'ecc-term-claude-mode-consolidated t))))

;;;; Auto core module tests

(ert-deftest test-ecc-auto-core-buffer-registration ()
  "Test buffer registration and cleanup."
  (let ((buffer1 (generate-new-buffer "*ecc-test-1*"))
        (buffer2 (generate-new-buffer "*ecc-test-2*")))
    (unwind-protect
        (progn
          ;; Register buffers
          (should (eq (ecc-auto-core-register-buffer buffer1) buffer1))
          (should (eq (ecc-auto-core-register-buffer buffer2) buffer2))
          
          ;; Check registration
          (should (member buffer1 (ecc-auto-core-registered-buffers)))
          (should (member buffer2 (ecc-auto-core-registered-buffers)))
          
          ;; Unregister one buffer
          (ecc-auto-core-unregister-buffer buffer1)
          (should-not (member buffer1 (ecc-auto-core-registered-buffers)))
          (should (member buffer2 (ecc-auto-core-registered-buffers)))
          
          ;; Kill buffer and check cleanup
          (kill-buffer buffer2)
          (should-not (member buffer2 (ecc-auto-core-registered-buffers))))
      
      ;; Cleanup
      (when (buffer-live-p buffer1) (kill-buffer buffer1))
      (when (buffer-live-p buffer2) (kill-buffer buffer2)))))

(ert-deftest test-ecc-auto-core-throttling ()
  "Test throttling of auto-responses."
  ;; Reset state for testing
  (ecc-auto-core-reset-state)
  
  ;; Initially not throttled
  (should-not (ecc-auto-core-throttled-p :y/n))
  
  ;; Update state and check throttling
  (ecc-auto-core-update-state :y/n)
  (should (ecc-auto-core-throttled-p :y/n))
  
  ;; Different state should not be throttled
  (should-not (ecc-auto-core-throttled-p :waiting))
  
  ;; Reset state and verify no throttling
  (ecc-auto-core-reset-state)
  (should-not (ecc-auto-core-throttled-p :y/n)))

(ert-deftest test-ecc-auto-core-timer ()
  "Test timer management functions."
  ;; Ensure timer is stopped at the beginning
  (ecc-auto-core-timer-stop)
  (should-not (ecc-auto-core-timer-active-p))
  
  ;; Start timer and verify it's active
  (ecc-auto-core-timer-start (lambda () nil))
  (should (ecc-auto-core-timer-active-p))
  
  ;; Stop timer and verify it's inactive
  (ecc-auto-core-timer-stop)
  (should-not (ecc-auto-core-timer-active-p)))

;;;; Debug utils module tests

(ert-deftest test-ecc-debug-utils-basic ()
  "Test basic functionality of debug utilities."
  ;; Test global debug toggle
  (let ((ecc-debug-enabled nil))
    (ecc-debug-toggle-global)
    (should ecc-debug-enabled)
    (ecc-debug-toggle-global)
    (should-not ecc-debug-enabled)))

(ert-deftest test-ecc-debug-utils-categories ()
  "Test debug category management."
  ;; Setup: make sure we have categories defined
  (let ((ecc-debug-categories '(test1 test2 test3))
        (ecc-debug-enabled-categories nil))
    
    ;; By default all categories are enabled
    (should (ecc-debug--category-enabled-p 'test1))
    (should (ecc-debug--category-enabled-p 'test2))
    
    ;; Enable specific categories
    (setq ecc-debug-enabled-categories '(test1))
    (should (ecc-debug--category-enabled-p 'test1))
    (should-not (ecc-debug--category-enabled-p 'test2))
    
    ;; Enable all categories
    (ecc-debug-enable-all-categories)
    (should (null ecc-debug-enabled-categories))
    (should (ecc-debug--category-enabled-p 'test1))
    (should (ecc-debug--category-enabled-p 'test2))))

(ert-deftest test-ecc-debug-utils-buffer-local ()
  "Test buffer-local debug functionality."
  (with-test-buffer
    ;; Initially defined but nil
    (should (boundp 'ecc-debug-buffer-enabled))
    (should-not ecc-debug-buffer-enabled)
    
    ;; Toggle and check
    (ecc-debug-toggle-buffer)
    (should ecc-debug-buffer-enabled)
    
    ;; Toggle again and check
    (ecc-debug-toggle-buffer)
    (should-not ecc-debug-buffer-enabled)))

(ert-deftest test-ecc-debug-utils-factory-function ()
  "Test debug function factory."
  ;; Create various debug functions
  (let* ((ecc-debug-enabled t)
         (global-fn (ecc-debug-make-debug-fn))
         (category-fn (ecc-debug-make-debug-fn nil 'test-category))
         (test-buffer (generate-new-buffer "*debug-test*")))
    
    (unwind-protect
        (with-current-buffer test-buffer
          ;; Enable buffer-local debug
          (setq ecc-debug-buffer-enabled t)
          
          ;; Test buffer function
          (let ((buffer-fn (ecc-debug-make-debug-fn test-buffer)))
            (should (functionp buffer-fn))))
      
      ;; Cleanup
      (kill-buffer test-buffer))))

;;;; Run all tests

(defun ecc-test-run-all-consolidated-tests ()
  "Run all tests for consolidated modules."
  (interactive)
  (ert-run-tests-interactively "^test-ecc-"))

(provide 'test-consolidated-modules)

;;; test-consolidated-modules.el ends here