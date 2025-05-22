;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 15:05:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-state/test-buffer-state-basic.el

;;; Commentary:
;;; Tests for buffer-local state functionality.
;;; Follows TDD best practices with single-assertion tests
;;; and descriptive names explaining expected behavior.

(require 'ert)
(require 'ecc-buffer-state)
(require 'ecc-state-detection-consolidated)

;; Test buffer-local state initialization
(ert-deftest test-buffer-state-init-creates-buffer-local-variables ()
  "Test that buffer-state-init creates required buffer-local variables."
  (let ((test-buffer (generate-new-buffer "*test-buffer-state*")))
    (unwind-protect
        (progn
          ;; Arrange: Create buffer without state
          
          ;; Act: Initialize buffer state
          (with-current-buffer test-buffer
            (ecc-buffer-state-init))
          
          ;; Assert: Buffer-local variables should be created
          (with-current-buffer test-buffer
            (should (boundp 'ecc-buffer-state-prompt))))
      
      ;; Clean up
      (kill-buffer test-buffer))))

(ert-deftest test-buffer-state-update-prompt-changes-stored-value ()
  "Test that update-prompt changes the stored prompt state value."
  (let ((test-buffer (generate-new-buffer "*test-buffer-state*")))
    (unwind-protect
        (progn
          ;; Arrange: Initialize buffer with state
          (with-current-buffer test-buffer
            (ecc-buffer-state-init))
          
          ;; Act: Update prompt state
          (with-current-buffer test-buffer
            (ecc-buffer-state-update-prompt :y/n))
          
          ;; Assert: Stored value should match
          (with-current-buffer test-buffer
            (should (eq (ecc-buffer-state-get-prompt) :y/n))))
      
      ;; Clean up
      (kill-buffer test-buffer))))

(ert-deftest test-buffer-state-set-and-get-stores-arbitrary-state ()
  "Test that set and get functions store and retrieve arbitrary state values."
  (let ((test-buffer (generate-new-buffer "*test-buffer-state*")))
    (unwind-protect
        (progn
          ;; Arrange: Initialize buffer with state
          (with-current-buffer test-buffer
            (ecc-buffer-state-init))
          
          ;; Act: Set arbitrary state
          (with-current-buffer test-buffer
            (ecc-buffer-state-set 'active-state :y/n))
          
          ;; Assert: Retrieved value should match
          (with-current-buffer test-buffer
            (should (eq (ecc-buffer-state-get 'active-state) :y/n))))
      
      ;; Clean up
      (kill-buffer test-buffer))))

;; Test that state is buffer-local
(ert-deftest test-buffer-state-remains-independent-between-buffers ()
  "Test that state changes in one buffer do not affect other buffers."
  (let ((buffer-a (generate-new-buffer "*test-buffer-a*"))
        (buffer-b (generate-new-buffer "*test-buffer-b*")))
    
    (unwind-protect
        (progn
          ;; Arrange: Initialize both buffers with state
          (with-current-buffer buffer-a
            (ecc-buffer-state-init))
          (with-current-buffer buffer-b
            (ecc-buffer-state-init))
          
          ;; Act: Set different states in each buffer
          (with-current-buffer buffer-a
            (ecc-buffer-state-update-prompt :y/n))
          (with-current-buffer buffer-b
            (ecc-buffer-state-update-prompt :waiting))
          
          ;; Assert: Each buffer maintains its own state
          (with-current-buffer buffer-a
            (should (eq (ecc-buffer-state-get-prompt) :y/n)))
          (with-current-buffer buffer-b
            (should (eq (ecc-buffer-state-get-prompt) :waiting))))
      
      ;; Clean up
      (kill-buffer buffer-a)
      (kill-buffer buffer-b))))

;; Run tests if in batch mode
(when noninteractive
  (ert-run-tests-batch-and-exit))

(provide 'test-buffer-state-basic)

;;; test-buffer-state-basic.el ends here