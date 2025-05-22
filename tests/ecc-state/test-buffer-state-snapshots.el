;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 20:15:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-state/test-buffer-state-snapshots.el

;;; Commentary:
;;; Snapshot tests for buffer state detection.
;;; These tests verify that the state detection system correctly identifies
;;; states from realistic buffer content snapshots.

(require 'ert)
(require 'ecc-variables-consolidated)
(require 'ecc-api)
(require 'ecc-buffer-local)
(require 'ecc-api)
(require 'ecc-state-detection-consolidated)
(require 'ecc-api)
(require 'ecc-buffer-api)
(require 'ecc-api)

;; Test buffer for snapshots
(defvar ecc-snapshot-test-buffer nil "Buffer for snapshot tests.")

;; Common test infrastructure
(defun ecc-snapshot-setup ()
  "Set up snapshot test environment."
  (setq ecc-snapshot-test-buffer (generate-new-buffer "*ecc-snapshot*"))
  (ecc-buffer-register ecc-snapshot-test-buffer))

(defun ecc-snapshot-teardown ()
  "Clean up snapshot test environment."
  (when (buffer-live-p ecc-snapshot-test-buffer)
    (kill-buffer ecc-snapshot-test-buffer))
  (setq ecc-snapshot-test-buffer nil))

(defun ecc-load-snapshot (snapshot-content expected-state)
  "Load SNAPSHOT-CONTENT into test buffer and verify it's detected as EXPECTED-STATE."
  (with-current-buffer ecc-snapshot-test-buffer
    (erase-buffer)
    (insert snapshot-content)
    (let ((detected-state (ecc-buffer-state-detect)))
      (should (eq detected-state expected-state))
      (should (eq ecc-buffer-state expected-state)))))

;; Snapshot content for different states
(defconst ecc-snapshot-y-n
  "│ Claude Model: Claude 3 Opus
│ 
│ I'd be happy to help you with that decision. Let's compare the different options to determine which one might be best for your situation. I'll focus on the key differences and trade-offs.
│ 
│ A custom built NAS solution generally offers:
│ - More flexibility and customization
│ - Potentially lower costs for equivalent storage capacity
│ - Ability to upgrade individual components over time
│ - Complete control over software and hardware
│ 
│ Pre-built NAS solutions like Synology offer:
│ - Easier setup and maintenance
│ - User-friendly software interfaces
│ - Regular software updates and support
│ - More polished experience overall
│ 
│ Would you like me to explore either option in more detail?
│ 
│ ❯ 1. Yes
│   2. No"
  "Snapshot of a Y/N prompt.")

(defconst ecc-snapshot-y-y-n
  "│ Claude Model: Claude 3 Opus
│ 
│ I'll help you create a Python function to calculate compound interest. Here's a simple implementation:
│ 
│ ```python
│ def compound_interest(principal, rate, time, compounds_per_year=1):
│     \"\"\"
│     Calculate compound interest.
│     
│     Args:
│         principal: Initial investment amount
│         rate: Annual interest rate (decimal)
│         time: Time in years
│         compounds_per_year: Number of times interest is compounded per year
│         
│     Returns:
│         The final amount after compound interest
│     \"\"\"
│     return principal * (1 + rate/compounds_per_year)**(compounds_per_year*time)
│ ```
│ 
│ Would you like me to explain how this function works, or would you like to see examples of how to use it?
│ 
│  1. No
│ ❯ 2. Yes, and show me examples
│  3. Yes, but don't show examples"
  "Snapshot of a Y/Y/N prompt.")

(defconst ecc-snapshot-waiting
  "│ Claude Model: Claude 3 Opus
│ 
│ # Python Data Analysis Libraries
│ 
│ Here's an overview of the most essential Python libraries for data analysis:
│ 
│ ## 1. NumPy
│ 
│ NumPy is the foundation for numerical computing in Python. It provides:
│ 
│ - N-dimensional array objects
│ - Sophisticated broadcasting functions
│ - Tools for integrating C/C++ code
│ - Linear algebra, Fourier transform, and random number capabilities
│ 
│ Basic usage:
│ ```python
│ import numpy as np
│ 
│ # Create arrays
│ arr = np.array([1, 2, 3, 4, 5])
│ matrix = np.array([[1, 2, 3], [4, 5, 6], [7, 8, 9]])
│ 
│ # Array operations
│ print(arr * 2)  # Element-wise multiplication
│ print(matrix.shape)  # Get dimensions
│ print(matrix.T)  # Transpose
│ ```
│ 
│ ## 2. Pandas
│ 
│ Pandas is built on top of NumPy and provides:
│ 
│ I have more information about Python data analysis libraries to share. Would you like me to continue?
│ 
│ │ >                            "
  "Snapshot of a waiting prompt.")

(defconst ecc-snapshot-initial-waiting
  "│ Claude Model: Claude 3 Opus
│ Conversation ID: [redacted]
│ Time: May 20, 2025, 12:28 PM PDT
│ 
│ Your conversation will show up here. Claude may take a while to think and respond.
│ 
│ Legal disclaimer: output may be inaccurate, especially with proper names, domain knowledge, calculations, and in non-English languages. Limit is 5 messages per conversation and character count up to 100,000 per organization. Anthropic does not collect your data or use your inputs or outputs to train Claude. If you notice inappropriate or unsafe outputs, please report them to us.
│ 
│ > Try 'Explain zero-shot learning to me'
│ "
  "Snapshot of an initial waiting prompt.")

(defconst ecc-snapshot-regular-content
  "│ Claude Model: Claude 3 Opus
│ 
│ # Introduction to Machine Learning
│ 
│ Machine learning is a subfield of artificial intelligence that focuses on developing systems that can learn from and make decisions based on data. Instead of explicitly programming rules, machine learning algorithms build models based on sample data to make predictions or decisions without being explicitly programmed to do so.
│ 
│ ## Key Concepts
│ 
│ ### Supervised Learning
│ 
│ In supervised learning, the algorithm is trained on labeled data, meaning the input comes with the correct output. The goal is to learn a mapping function from the input to the output. Examples include:
│ 
│ - Classification: Predicting a categorical label (e.g., spam detection, image recognition)
│ - Regression: Predicting a continuous value (e.g., house prices, temperature forecasting)
│ 
│ ### Unsupervised Learning
│ 
│ Unsupervised learning works with unlabeled data and tries to find patterns or structure. Common techniques include:
│ 
│ - Clustering: Grouping similar instances (e.g., customer segmentation)
│ - Dimensionality Reduction: Reducing the number of variables (e.g., PCA, t-SNE)
│ - Association: Finding rules that describe associations between variables (e.g., market basket analysis)
│ 
│ ### Reinforcement Learning
│ 
│ Reinforcement learning involves an agent learning to make decisions by taking actions in an environment to maximize some notion of reward. It's used in:
│ 
│ - Game playing (e.g., AlphaGo, chess)
│ - Robotics
│ - Autonomous systems
│ - Resource management
│ "
  "Snapshot of regular content (no prompt).")

;; Snapshot tests
(ert-deftest ecc-test-snapshot-y-n ()
  "Test Y/N prompt snapshot detection."
  (ecc-snapshot-setup)
  (unwind-protect
      (ecc-load-snapshot ecc-snapshot-y-n :y/n)
    (ecc-snapshot-teardown)))

(ert-deftest ecc-test-snapshot-y-y-n ()
  "Test Y/Y/N prompt snapshot detection."
  (ecc-snapshot-setup)
  (unwind-protect
      (ecc-load-snapshot ecc-snapshot-y-y-n :y/y/n)
    (ecc-snapshot-teardown)))

(ert-deftest ecc-test-snapshot-waiting ()
  "Test waiting prompt snapshot detection."
  (ecc-snapshot-setup)
  (unwind-protect
      (ecc-load-snapshot ecc-snapshot-waiting :waiting)
    (ecc-snapshot-teardown)))

(ert-deftest ecc-test-snapshot-initial-waiting ()
  "Test initial waiting prompt snapshot detection."
  (ecc-snapshot-setup)
  (unwind-protect
      (ecc-load-snapshot ecc-snapshot-initial-waiting :initial-waiting)
    (ecc-snapshot-teardown)))

(ert-deftest ecc-test-snapshot-regular-content ()
  "Test regular content (no prompt) detection."
  (ecc-snapshot-setup)
  (unwind-protect
      (ecc-load-snapshot ecc-snapshot-regular-content nil)
    (ecc-snapshot-teardown)))

(ert-deftest ecc-test-snapshots-with-buffer-specific-patterns ()
  "Test detection with buffer-specific patterns."
  (ecc-snapshot-setup)
  (unwind-protect
      (progn
        ;; Set custom patterns for this buffer
        (with-current-buffer ecc-snapshot-test-buffer
          (setq-local ecc-state-prompt-y/n "Option 1")
          (setq-local ecc-state-prompt-initial-waiting "Let's begin")
          
          ;; Content that matches custom patterns
          (erase-buffer)
          (insert "Some content with Option 1 visible")
          (should (eq (ecc-buffer-state-detect) :y/n))
          
          (erase-buffer)
          (insert "Content with Let's begin prompt")
          (should (eq (ecc-buffer-state-detect) :initial-waiting))))
    (ecc-snapshot-teardown)))

(ert-deftest ecc-test-snapshot-with-alternative-patterns ()
  "Test detection with alternative initial waiting patterns."
  (ecc-snapshot-setup)
  (unwind-protect
      (progn
        ;; Standard pattern should be detected
        (ecc-load-snapshot ecc-snapshot-initial-waiting :initial-waiting)
        
        ;; Alternative patterns should also be detected
        (with-current-buffer ecc-snapshot-test-buffer
          (dolist (pattern ecc-state-prompt-initial-waiting-alternatives)
            (erase-buffer)
            (insert "Content with alternative pattern: " pattern)
            (should (eq (ecc-buffer-state-detect) :initial-waiting)))))
    (ecc-snapshot-teardown)))

(provide 'test-buffer-state-snapshots)

;;; test-buffer-state-snapshots.el ends here