;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 10:15:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-state/test-ecc-state-detect-prompt.el

;;; Commentary:
;;; Tests for the Claude prompt state detection functionality

(require 'ert)
(require 'ecc-variables-consolidated)

;; Load the files under test
(when (file-exists-p "/home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-state-detect-prompt.el")
  (load-file "/home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-state-detect-prompt.el"))

;; Test the basic prompt detection functions
(ert-deftest test-ecc-detect-prompt-in-last-lines ()
  "Test the function to detect Claude prompts in last N lines."
  ;; Skip if function doesn't exist
  (skip-unless (fboundp 'ecc-detect-prompt-in-last-lines))
  
  ;; Test with different buffer contents
  (with-temp-buffer
    ;; No prompt
    (erase-buffer)
    (insert "This is a regular message without prompts.\n")
    (insert "No prompts here either.\n")
    (should-not (ecc-detect-prompt-in-last-lines))
    
    ;; Y/N prompt
    (erase-buffer)
    (insert "This is some text.\n")
    (insert "Would you like to continue? [y/n]\n")
    (should (eq (ecc-detect-prompt-in-last-lines) :y/n))
    
    ;; Y/Y/N prompt
    (erase-buffer)
    (insert "This is some text.\n")
    (insert "Would you like to see more? [Y/y/n]\n")
    (should (eq (ecc-detect-prompt-in-last-lines) :y/y/n))
    
    ;; Continue prompt
    (erase-buffer)
    (insert "This is some text.\n")
    (insert "Type 'continue>' to continue\n")
    (should (eq (ecc-detect-prompt-in-last-lines) :waiting))
    
    ;; Custom prompts
    (let ((ecc-state-prompt-y/n "❯ 1. Yes"))
      (erase-buffer)
      (insert "This is some text.\n")
      (insert "1. Yes\n")
      (insert "2. No\n")
      (insert "❯ 1. Yes\n")
      (should (eq (ecc-detect-prompt-in-last-lines) :y/n)))
    
    (let ((ecc-state-prompt-y/y/n " 2. Yes, and"))
      (erase-buffer)
      (insert "This is some text.\n")
      (insert "1. Yes\n")
      (insert " 2. Yes, and\n")
      (insert "3. No\n")
      (should (eq (ecc-detect-prompt-in-last-lines) :y/y/n)))
    
    (let ((ecc-state-prompt-waiting "│ >                            "))
      (erase-buffer)
      (insert "This is some text.\n")
      (insert "│ >                            \n")
      (should (eq (ecc-detect-prompt-in-last-lines) :waiting)))
    
    (let ((ecc-state-prompt-initial-waiting "│ > Try "))
      (erase-buffer)
      (insert "This is some text.\n")
      (insert "│ > Try typing something\n")
      (should (eq (ecc-detect-prompt-in-last-lines) :initial-waiting)))))

;; Test the region-based prompt detection
(ert-deftest test-ecc-detect-prompt-in-region ()
  "Test detecting prompts in a specific region."
  ;; Skip if function doesn't exist
  (skip-unless (fboundp 'ecc-detect-prompt-in-region))
  
  ;; Test with different regions
  (with-temp-buffer
    (insert "This is the beginning text.\n")
    (insert "Would you like to continue? [y/n]\n")
    (insert "This is the end text.\n")
    
    ;; Region with Y/N prompt
    (should (eq (ecc-detect-prompt-in-region (point-min) (point-max)) :y/n))
    
    ;; Region without prompt
    (erase-buffer)
    (insert "This is text without prompts.\n")
    (insert "Still no prompts here.\n")
    (should-not (ecc-detect-prompt-in-region (point-min) (point-max)))
    
    ;; Multiple prompts in region (should detect first match in priority order)
    (erase-buffer)
    (insert "Would you like to see more? [Y/y/n]\n")
    (insert "Would you like to continue? [y/n]\n")
    (should (eq (ecc-detect-prompt-in-region (point-min) (point-max)) :y/y/n))))

;; Test buffer-based prompt detection
(ert-deftest test-ecc-detect-prompt-state ()
  "Test detecting prompt state in a buffer."
  ;; Skip if function doesn't exist
  (skip-unless (fboundp 'ecc-detect-prompt-state))
  
  ;; Test with different buffers
  (with-temp-buffer
    (insert "Would you like to continue? [y/n]\n")
    
    ;; Same buffer
    (should (eq (ecc-detect-prompt-state) :y/n))
    
    ;; Different buffer
    (with-temp-buffer
      (insert "Type 'continue>' to continue\n")
      (let ((test-buffer (current-buffer)))
        ;; Test with specified buffer
        (should (eq (ecc-detect-prompt-state test-buffer) :waiting))))))

;; Test the enhanced state detection function
(ert-deftest test-ecc-detect-enhanced-state ()
  "Test the enhanced state detection function."
  ;; Skip if function doesn't exist
  (skip-unless (fboundp 'ecc-detect-enhanced-state))
  
  ;; Test with different buffers
  (with-temp-buffer
    (insert "Would you like to continue? [y/n]\n")
    
    ;; Test enhanced detection
    (should (eq (ecc-detect-enhanced-state) :y/n))
    
    ;; Test with custom prompt patterns
    (let ((ecc-state-prompt-y/n "❯ 1. Yes"))
      (erase-buffer)
      (insert "❯ 1. Yes\n")
      (insert "2. No\n")
      (should (eq (ecc-detect-enhanced-state) :y/n)))
    
    ;; Test with line count parameter
    (erase-buffer)
    (insert (make-string 100 ?\n)) ; 100 blank lines
    (insert "Would you like to continue? [y/n]\n")
    
    ;; With default line count (should detect)
    (should (eq (ecc-detect-enhanced-state) :y/n))
    
    ;; With very small line count (shouldn't detect)
    (should-not (ecc-detect-enhanced-state nil 1)))))

;; Test with complex mixed content
(ert-deftest test-ecc-detect-prompt-complex-content ()
  "Test detection with complex mixed content typical of Claude interactions."
  ;; Skip if function doesn't exist
  (skip-unless (fboundp 'ecc-detect-prompt-in-last-lines))
  
  ;; Test with realistic Claude output
  (with-temp-buffer
    ;; Code block then Y/N prompt
    (erase-buffer)
    (insert "Here's a Python function to calculate factorials:\n\n")
    (insert "```python\n")
    (insert "def factorial(n):\n")
    (insert "    if n == 0 or n == 1:\n")
    (insert "        return 1\n")
    (insert "    else:\n")
    (insert "        return n * factorial(n-1)\n")
    (insert "```\n\n")
    (insert "Would you like me to explain how this works? [Y/n]\n")
    (should (eq (ecc-detect-prompt-in-last-lines) :y/n))
    
    ;; Complex Claude conversation with Y/Y/N prompt
    (erase-buffer)
    (insert "I analyzed your code and found several potential optimizations:\n\n")
    (insert "1. The loop on line 27 could be replaced with a list comprehension\n")
    (insert "2. The function on line 42 is repeatedly calculating the same values\n")
    (insert "3. There are unused imports on lines 5-8\n\n")
    (insert "Would you like me to:\n")
    (insert "1. Explain these issues in more detail\n")
    (insert "2. Fix them and show you the improved code\n")
    (insert "3. Move on to the next part of your project\n\n")
    (insert "[Y/y/n]\n")
    (should (eq (ecc-detect-prompt-in-last-lines) :y/y/n))))

;; Test line count parameter
(ert-deftest test-ecc-detect-prompt-line-count ()
  "Test that the line count parameter works correctly."
  ;; Skip if function doesn't exist
  (skip-unless (fboundp 'ecc-detect-prompt-in-last-lines))
  
  (with-temp-buffer
    ;; Create a buffer with 100 lines of text and prompt at end
    (dotimes (i 99)
      (insert (format "Line %d of placeholder text\n" (1+ i))))
    (insert "Would you like to continue? [y/n]\n")
    
    ;; Default line count should work
    (should (eq (ecc-detect-prompt-in-last-lines) :y/n))
    
    ;; Small line count should work
    (should (eq (ecc-detect-prompt-in-last-lines 10) :y/n))
    
    ;; Very small line count should not find the prompt
    (should-not (ecc-detect-prompt-in-last-lines 1))))

;; Test customization
(ert-deftest test-ecc-state-detect-customization ()
  "Test that customization variables work correctly."
  ;; Skip if group doesn't exist
  (skip-unless (get 'ecc-state-detect 'custom-group))
  
  ;; Check line count customization variable
  (should (boundp 'ecc-state-detect-line-count))
  
  ;; Test with custom line count
  (let ((ecc-state-detect-line-count 5))
    (with-temp-buffer
      ;; Create buffer with 10 lines and prompt at end
      (dotimes (i 9)
        (insert (format "Line %d of placeholder text\n" (1+ i))))
      (insert "Would you like to continue? [y/n]\n")
      
      ;; Should detect with default parameters 
      ;; (using customized ecc-state-detect-line-count)
      (should (eq (ecc-detect-prompt-in-last-lines) :y/n)))))

(provide 'test-ecc-state-detect-prompt)

;;; test-ecc-state-detect-prompt.el ends here