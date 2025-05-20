;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 15:05:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-state/test-buffer-state-basic.el

;;; Commentary:
;;; Basic tests for buffer-local state functionality.

(require 'ert)
(require 'ecc-buffer-state)
(require 'ecc-state-detection)

;; Test buffer-local state operations
(ert-deftest test-buffer-state-basic ()
  "Test basic buffer-local state operations."
  (let ((test-buffer (generate-new-buffer "*test-buffer-state*")))
    (unwind-protect
        (progn
          ;; Initialize buffer state
          (with-current-buffer test-buffer
            (ecc-buffer-state-init)
            
            ;; Test setting and getting state
            (ecc-buffer-state-update-prompt :y/n)
            (should (eq (ecc-buffer-state-get-prompt) :y/n))
            
            ;; Test getting active state
            (ecc-buffer-state-set 'active-state :y/n)
            (should (eq (ecc-buffer-state-get 'active-state) :y/n))))
      
      ;; Clean up
      (kill-buffer test-buffer))))

;; Test that state is buffer-local
(ert-deftest test-buffer-state-isolation ()
  "Test that state is isolated between buffers."
  (let ((buffer-a (generate-new-buffer "*test-buffer-a*"))
        (buffer-b (generate-new-buffer "*test-buffer-b*")))
    
    (unwind-protect
        (progn
          ;; Initialize both buffers
          (with-current-buffer buffer-a
            (ecc-buffer-state-init))
          (with-current-buffer buffer-b
            (ecc-buffer-state-init))
          
          ;; Set different states
          (with-current-buffer buffer-a
            (ecc-buffer-state-update-prompt :y/n))
          (with-current-buffer buffer-b
            (ecc-buffer-state-update-prompt :waiting))
          
          ;; Verify states are independent
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