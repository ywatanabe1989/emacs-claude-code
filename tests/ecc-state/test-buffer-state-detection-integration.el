;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 15:15:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-state/test-buffer-state-detection-integration.el

;;; Commentary:
;;; Integration tests for buffer-local state with background detection.
;;; These tests verify that the buffer-local state and background detection
;;; systems work together correctly to provide independent state tracking
;;; across multiple buffers without relying on cursor position.

(require 'ert)
(require 'ecc-state-detection-consolidated)
(require 'ecc-buffer-state)
(require 'ecc-background-detection)
(require 'ecc-vterm-utils)
(require 'ecc-debug-utils)

;;; Code:

;; Test fixtures
(defvar test-integration-buffer-a nil "First test buffer.")
(defvar test-integration-buffer-b nil "Second test buffer.")

(defun test-integration-setup ()
  "Set up test environment for integration tests."
  ;; Create test buffers
  (setq test-integration-buffer-a (generate-new-buffer "*test-integration-a*"))
  (setq test-integration-buffer-b (generate-new-buffer "*test-integration-b*"))
  
  ;; Initialize buffer state
  (with-current-buffer test-integration-buffer-a
    (ecc-buffer-state-init))
  (with-current-buffer test-integration-buffer-b
    (ecc-buffer-state-init))
  
  ;; Fill with different content
  (with-current-buffer test-integration-buffer-a
    (erase-buffer)
    (insert "Some content in buffer A\n")
    (insert "│ > Try \n"))  ;; initial-waiting pattern
  
  (with-current-buffer test-integration-buffer-b
    (erase-buffer)
    (insert "Different content in buffer B\n")
    (insert "[Y/n]\n"))  ;; y/n pattern
  
  ;; Register with background detection
  (ecc-background-detection-register-buffer test-integration-buffer-a)
  (ecc-background-detection-register-buffer test-integration-buffer-b))

(defun test-integration-teardown ()
  "Clean up test environment after integration tests."
  ;; Stop background detection if active
  (when (and (boundp 'ecc-background-detection-active)
             ecc-background-detection-active)
    (ecc-background-detection-stop))
  
  ;; Kill test buffers
  (when (buffer-live-p test-integration-buffer-a)
    (kill-buffer test-integration-buffer-a))
  (when (buffer-live-p test-integration-buffer-b)
    (kill-buffer test-integration-buffer-b))
  
  ;; Reset variables
  (setq test-integration-buffer-a nil)
  (setq test-integration-buffer-b nil))

(ert-deftest test-buffer-state-detection-integration ()
  "Test that buffer-local state tracking works correctly."
  (test-integration-setup)
  (unwind-protect
      (progn
        ;; Manually detect states in each buffer
        (with-current-buffer test-integration-buffer-a
          (let ((state (ecc-detect-state)))
            (should (eq state :initial-waiting))
            (ecc-buffer-state-update-prompt state)))
        
        (with-current-buffer test-integration-buffer-b
          (let ((state (ecc-detect-state)))
            (should (eq state :y/n))
            (ecc-buffer-state-update-prompt state)))
        
        ;; Verify states were set correctly in each buffer
        (with-current-buffer test-integration-buffer-a
          (should (eq (ecc-buffer-state-get-prompt) :initial-waiting)))
        
        (with-current-buffer test-integration-buffer-b
          (should (eq (ecc-buffer-state-get-prompt) :y/n)))
        
        ;; Change content in one buffer
        (with-current-buffer test-integration-buffer-a
          (erase-buffer)
          (insert "Changed content\n")
          (insert "continue>\n")) ;; waiting pattern
        
        ;; Detect new state
        (with-current-buffer test-integration-buffer-a
          (let ((state (ecc-detect-state)))
            (should (eq state :waiting))
            (ecc-buffer-state-update-prompt state)))
        
        ;; Verify states remain independent
        (with-current-buffer test-integration-buffer-a
          (should (eq (ecc-buffer-state-get-prompt) :waiting)))
        
        (with-current-buffer test-integration-buffer-b
          (should (eq (ecc-buffer-state-get-prompt) :y/n))))
    (test-integration-teardown)))

(ert-deftest test-background-detection-cursor-independence ()
  "Test that detection works regardless of cursor position."
  (test-integration-setup)
  (unwind-protect
      (progn
        ;; Place cursor at beginning of buffer
        (with-current-buffer test-integration-buffer-a
          (goto-char (point-min))
          ;; Remember the cursor position
          (let ((original-point (point)))
            ;; Detect state - should work even with cursor at beginning
            (should (eq (ecc-detect-state) :initial-waiting))
            ;; Verify cursor position was not changed by detection
            (should (= (point) original-point))))
        
        ;; Test with cursor in the middle
        (with-current-buffer test-integration-buffer-b
          (goto-char (/ (point-max) 2))
          ;; Remember the cursor position
          (let ((original-point (point)))
            ;; Detect state - should work even with cursor in middle
            (should (eq (ecc-detect-state) :y/n))
            ;; Verify cursor position was not changed by detection
            (should (= (point) original-point)))))
    (test-integration-teardown)))

(ert-deftest test-buffer-state-independence ()
  "Test that buffer states remain independent."
  (test-integration-setup)
  (unwind-protect
      (progn
        ;; Set different states in each buffer manually
        (with-current-buffer test-integration-buffer-a
          (ecc-buffer-state-update-prompt :initial-waiting))
        (with-current-buffer test-integration-buffer-b
          (ecc-buffer-state-update-prompt :y/n))
        
        ;; Verify states are independent
        (with-current-buffer test-integration-buffer-a
          (should (eq (ecc-buffer-state-get-prompt) :initial-waiting)))
        (with-current-buffer test-integration-buffer-b
          (should (eq (ecc-buffer-state-get-prompt) :y/n)))
        
        ;; Change one buffer's content and update state
        (with-current-buffer test-integration-buffer-a
          (erase-buffer)
          (insert "Changed to Y/Y/N pattern\n")
          (insert "[Y/y/n]\n")
          ;; Update state manually
          (let ((state (ecc-detect-state)))
            (should (eq state :y/y/n))
            (ecc-buffer-state-update-prompt state)))
        
        ;; Verify one buffer's state changed while the other remained the same
        (with-current-buffer test-integration-buffer-a
          (should (eq (ecc-buffer-state-get-prompt) :y/y/n)))
        (with-current-buffer test-integration-buffer-b
          (should (eq (ecc-buffer-state-get-prompt) :y/n))))
    (test-integration-teardown)))

(ert-deftest test-vterm-utils-send-string-buffer-locality ()
  "Test that vterm utilities handle buffer locality correctly."
  (test-integration-setup)
  (unwind-protect
      (progn
        ;; Mock vterm functions for testing
        (cl-letf (((symbol-function 'vterm-send-string)
                   (lambda (str) str))
                  ((symbol-function 'vterm-send-return)
                   (lambda () t)))
          
          ;; Set up debug counter to track debug message calls
          (let ((debug-calls-a 0)
                (debug-calls-b 0))
            
            ;; Create buffer-specific debug functions
            (cl-letf (((symbol-function 'ecc-debug-utils-make-debug-fn)
                       (lambda (&optional buffer)
                         (if (eq buffer test-integration-buffer-a)
                             (lambda (&rest args)
                               (setq debug-calls-a (1+ debug-calls-a)))
                           (lambda (&rest args)
                             (setq debug-calls-b (1+ debug-calls-b)))))))
              
              ;; Send to both buffers
              (ecc-vterm-utils-send-string 
               test-integration-buffer-a "test-a"
               (ecc-debug-utils-make-debug-fn test-integration-buffer-a))
              
              (ecc-vterm-utils-send-string 
               test-integration-buffer-b "test-b"
               (ecc-debug-utils-make-debug-fn test-integration-buffer-b))
              
              ;; Verify debug calls were correctly routed to each buffer
              (should (> debug-calls-a 0))
              (should (> debug-calls-b 0))))))
    (test-integration-teardown)))

(provide 'test-buffer-state-detection-integration)

;;; test-buffer-state-detection-integration.el ends here