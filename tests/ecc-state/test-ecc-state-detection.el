;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 17:55:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-state/test-ecc-state-detection.el

;;; Commentary:
;;; Tests for the consolidated state detection module.

(require 'ert)
(require 'ecc-variables)
(require 'ecc-state-detection)

;;; Code:

;; Test the main state detection function
(ert-deftest test-ecc-detect-state ()
  "Test the main state detection function."
  
  ;; Test with different buffer contents
  (with-temp-buffer
    ;; No prompt
    (insert "This is a regular message without prompts.\n")
    (insert "No prompts here either.\n")
    (should-not (ecc-detect-state))
    
    ;; Y/N prompt
    (erase-buffer)
    (insert "This is some text.\n")
    (insert "Would you like to continue? [y/n]\n")
    (should (eq (ecc-detect-state) :y/n))
    
    ;; Y/Y/N prompt
    (erase-buffer)
    (insert "This is some text.\n")
    (insert "Would you like to see more? [Y/y/n]\n")
    (should (eq (ecc-detect-state) :y/y/n))
    
    ;; Continue prompt
    (erase-buffer)
    (insert "This is some text.\n")
    (insert "Type 'continue>' to continue\n")
    (should (eq (ecc-detect-state) :waiting))))

;; Test the basic state detection function
(ert-deftest test-ecc-detect-basic-state ()
  "Test basic buffer-based detection."
  
  (with-temp-buffer
    ;; No prompt
    (insert "This is a regular message without prompts.\n")
    (should-not (ecc-detect-basic-state))
    
    ;; Y/N prompt
    (erase-buffer)
    (insert "Would you like to continue? [y/n]\n")
    (should (eq (ecc-detect-basic-state) :y/n))
    
    ;; Y/Y/N prompt
    (erase-buffer)
    (insert "Would you like to see more? [Y/y/n]\n")
    (should (eq (ecc-detect-basic-state) :y/y/n))))

;; Test detection in last lines
(ert-deftest test-ecc-detect-prompt-in-last-lines ()
  "Test detection in last N lines."
  
  (with-temp-buffer
    ;; Test with large buffer
    (dotimes (i 50)
      (insert (format "Line %d of test content\n" (1+ i))))
    (insert "Would you like to continue? [y/n]\n")
    
    ;; Should detect with default line count
    (should (eq (ecc-detect-prompt-in-last-lines) :y/n))
    
    ;; Should detect with smaller line count
    (should (eq (ecc-detect-prompt-in-last-lines 10) :y/n))))

;; Test detection in region
(ert-deftest test-ecc-detect-prompt-in-region ()
  "Test detection in specific region."
  
  (with-temp-buffer
    (insert "Some text before\n")
    (let ((start (point)))
      (insert "Would you like to continue? [y/n]\n")
      (let ((end (point)))
        (insert "Some text after\n")
        
        ;; Should detect in the region
        (should (eq (ecc-detect-prompt-in-region start end) :y/n))
        
        ;; Should not detect in wrong region
        (should-not (ecc-detect-prompt-in-region (point-min) start))
        (should-not (ecc-detect-prompt-in-region end (point-max)))))))

;; Test alternative initial waiting patterns
(ert-deftest test-ecc-detect-alternative-initial-waiting ()
  "Test detection of alternative initial waiting patterns."
  
  (let ((ecc-state-prompt-initial-waiting-alternatives 
         '("Hello, I'm Claude" "I'm Claude, an AI assistant")))
    
    ;; Test with matching text
    (should (ecc-detect-alternative-initial-waiting 
             "Hello, I'm Claude. What can I help you with today?"))
    
    ;; Test with another matching text
    (should (ecc-detect-alternative-initial-waiting 
             "I'm Claude, an AI assistant by Anthropic"))
    
    ;; Test with non-matching text
    (should-not (ecc-detect-alternative-initial-waiting 
                 "This text doesn't contain a pattern"))))

;; Test state notification function
(ert-deftest test-ecc-state-notify-if-prompt-detected ()
  "Test the notification interface."
  
  (let ((ecc-auto-notify-on-claude-prompt t)
        (notification-called nil)
        (notification-state nil))
    
    ;; Mock the notification function
    (cl-letf (((symbol-function 'ecc-notification-check-state)
               (lambda (state) 
                 (setq notification-called t
                       notification-state state))))
      
      (with-temp-buffer
        ;; Insert a prompt
        (insert "Would you like to continue? [y/n]\n")
        
        ;; Test notification is triggered
        (should (eq (ecc-state-notify-if-prompt-detected (current-buffer)) :y/n))
        (should notification-called)
        (should (eq notification-state :y/n))
        
        ;; Test with no prompt
        (setq notification-called nil
              notification-state nil)
        (erase-buffer)
        (insert "No prompt here\n")
        (should-not (ecc-state-notify-if-prompt-detected (current-buffer)))
        (should-not notification-called)))))

;; Test state name utility
(ert-deftest test-ecc-state-get-name ()
  "Test getting human-readable state names."
  
  (should (string= (ecc-state-get-name :y/n) "Y/N"))
  (should (string= (ecc-state-get-name :y/y/n) "Y/Y/N"))
  (should (string= (ecc-state-get-name :waiting) "Continue"))
  (should (string= (ecc-state-get-name :initial-waiting) "Initial-Waiting"))
  (should (string= (ecc-state-get-name :other) ":other")))

;; Test backwards compatibility
(ert-deftest test-ecc-state-backward-compatibility ()
  "Test backward compatibility with old function names."
  
  (with-temp-buffer
    (insert "Would you like to continue? [y/n]\n")
    
    ;; Test aliases
    (should (eq (ecc-detect-state) :y/n))
    (should (eq (ecc-detect-state) :y/n))
    (should (eq (ecc-detect-prompt-state) :y/n))))

(provide 'test-ecc-state-detection)

;;; test-ecc-state-detection.el ends here