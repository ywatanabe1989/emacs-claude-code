;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 08:20:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-state/test-ecc-detect-prompt.el

;;; Commentary:
;;; Tests for ecc-detect-prompt functions.

(require 'ert)
(require 'ecc-variables-consolidated)
(require 'ecc-state-detect-prompt)

;; Test detection in last n lines
(ert-deftest test-ecc-detect-prompt-in-last-lines ()
  "Test detection of prompts in last n lines."
  
  ;; Setup test buffer
  (with-temp-buffer
    ;; Add some content to make buffer larger
    (dotimes (_ 50)
      (insert "This is a line of test content\n"))
    
    ;; Test for y/n prompt
    (let ((ecc-state-prompt-y/n "❯ 1. Yes"))
      (erase-buffer)
      (dotimes (_ 50)
        (insert "This is a line of test content\n"))
      (insert "Some text before the prompt\n")
      (insert "❯ 1. Yes\n")
      (insert "❯ 2. No\n")
      (should (eq (ecc-detect-prompt-in-last-lines 5) :y/n)))
    
    ;; Test for y/y/n prompt
    (let ((ecc-state-prompt-y/y/n " 2. Yes, and"))
      (erase-buffer)
      (dotimes (_ 50)
        (insert "This is a line of test content\n"))
      (insert "Some text before the prompt\n")
      (insert "❯ 1. Yes\n")
      (insert " 2. Yes, and\n")
      (insert "❯ 3. No\n")
      (should (eq (ecc-detect-prompt-in-last-lines 5) :y/y/n)))
    
    ;; Test for waiting prompt
    (let ((ecc-state-prompt-waiting "│ >                            "))
      (erase-buffer)
      (dotimes (_ 50)
        (insert "This is a line of test content\n"))
      (insert "Some text before the prompt\n")
      (insert "│ >                            \n")
      (should (eq (ecc-detect-prompt-in-last-lines 5) :waiting)))
    
    ;; Test for initial waiting prompt
    (let ((ecc-state-prompt-initial-waiting "│ > Try "))
      (erase-buffer)
      (dotimes (_ 50)
        (insert "This is a line of test content\n"))
      (insert "Some text before the prompt\n")
      (insert "│ > Try some command here\n")
      (should (eq (ecc-detect-prompt-in-last-lines 5) :initial-waiting)))
    
    ;; Test for no prompt
    (erase-buffer)
    (dotimes (_ 50)
      (insert "This is a line of test content\n"))
    (insert "Some text with no prompt\n")
    (should (eq (ecc-detect-prompt-in-last-lines 5) nil))
    
    ;; Test with prompt outside of n-lines window
    (let ((ecc-state-prompt-y/n "❯ 1. Yes"))
      (erase-buffer)
      (insert "❯ 1. Yes\n") ;; This is at the top
      (dotimes (_ 50)
        (insert "This is a line of test content\n"))
      (should (eq (ecc-detect-prompt-in-last-lines 5) nil)))))

;; Test detection in region
(ert-deftest test-ecc-detect-prompt-in-region ()
  "Test detection of prompts in a specified region."
  
  ;; Setup test buffer
  (with-temp-buffer
    ;; Test for y/n prompt
    (let ((ecc-state-prompt-y/n "❯ 1. Yes"))
      (erase-buffer)
      (insert "Some text before\n")
      (let ((start (point)))
        (insert "❯ 1. Yes\n")
        (insert "❯ 2. No\n")
        (let ((end (point)))
          (insert "Some text after\n")
          (should (eq (ecc-detect-prompt-in-region start end) :y/n)))))
    
    ;; Test for y/y/n prompt
    (let ((ecc-state-prompt-y/y/n " 2. Yes, and"))
      (erase-buffer)
      (insert "Some text before\n")
      (let ((start (point)))
        (insert "❯ 1. Yes\n")
        (insert " 2. Yes, and\n")
        (insert "❯ 3. No\n")
        (let ((end (point)))
          (insert "Some text after\n")
          (should (eq (ecc-detect-prompt-in-region start end) :y/y/n)))))
    
    ;; Test for waiting prompt
    (let ((ecc-state-prompt-waiting "│ >                            "))
      (erase-buffer)
      (insert "Some text before\n")
      (let ((start (point)))
        (insert "│ >                            \n")
        (let ((end (point)))
          (insert "Some text after\n")
          (should (eq (ecc-detect-prompt-in-region start end) :waiting)))))
    
    ;; Test for initial waiting prompt
    (let ((ecc-state-prompt-initial-waiting "│ > Try "))
      (erase-buffer)
      (insert "Some text before\n")
      (let ((start (point)))
        (insert "│ > Try some command\n")
        (let ((end (point)))
          (insert "Some text after\n")
          (should (eq (ecc-detect-prompt-in-region start end) :initial-waiting)))))
    
    ;; Test for no prompt in region
    (erase-buffer)
    (insert "Some text before\n")
    (let ((start (point)))
      (insert "This has no prompt\n")
      (let ((end (point)))
        (insert "❯ 1. Yes\n") ;; Prompt outside region
        (should (eq (ecc-detect-prompt-in-region start end) nil))))))

;; Test the enhanced detection
(ert-deftest test-ecc-detect-enhanced-state ()
  "Test the enhanced state detection function."
  
  ;; Setup test buffer
  (with-temp-buffer
    ;; Test with custom patterns
    (let ((ecc-state-prompt-y/n "❯ 1. Yes")
          (ecc-state-prompt-y/y/n " 2. Yes, and")
          (ecc-state-prompt-waiting "│ >                            ")
          (ecc-state-prompt-initial-waiting "│ > Try "))
      
      ;; Test y/n detection
      (erase-buffer)
      (insert "Test buffer content\n")
      (insert "❯ 1. Yes\n")
      (should (eq (ecc-detect-enhanced-state) :y/n))
      
      ;; Test y/y/n detection
      (erase-buffer)
      (insert "Test buffer content\n")
      (insert " 2. Yes, and\n")
      (should (eq (ecc-detect-enhanced-state) :y/y/n))
      
      ;; Test waiting detection
      (erase-buffer)
      (insert "Test buffer content\n")
      (insert "│ >                            \n")
      (should (eq (ecc-detect-enhanced-state) :waiting))
      
      ;; Test initial waiting detection
      (erase-buffer)
      (insert "Test buffer content\n")
      (insert "│ > Try some command\n")
      (should (eq (ecc-detect-enhanced-state) :initial-waiting)))
    
    ;; Test fallback patterns
    (let ((ecc-state-prompt-y/n nil)
          (ecc-state-prompt-y/y/n nil)
          (ecc-state-prompt-waiting nil)
          (ecc-state-prompt-initial-waiting nil))
      
      ;; Test y/n detection
      (erase-buffer)
      (insert "Test buffer content\n")
      (insert "[y/n] ")
      (should (eq (ecc-detect-enhanced-state) :y/n))
      
      ;; Test y/y/n detection
      (erase-buffer)
      (insert "Test buffer content\n")
      (insert "[Y/y/n] ")
      (should (eq (ecc-detect-enhanced-state) :y/y/n))
      
      ;; Test waiting detection
      (erase-buffer)
      (insert "Test buffer content\n")
      (insert "continue> ")
      (should (eq (ecc-detect-enhanced-state) :waiting)))))

(provide 'test-ecc-detect-prompt)

;;; test-ecc-detect-prompt.el ends here