;;; -*- coding: utf-8; lexical-binding: t -*-
;;; test-buffer-state-detection.el --- Tests for state detection
;;; Commentary:
;;; Comprehensive tests for the buffer state detection system.
;;; These tests follow clean code principles with small, focused test functions,
;;; descriptive names, and proper separation of concerns.

;;; Code:
(require 'ert)
(require 'ecc-variables)
(require 'ecc-state-detection)

;;;; Utility functions

(defun test-state-setup-buffer (content)
  "Create a temp buffer with CONTENT and return it."
  (let ((buffer (generate-new-buffer "*ecc-test-buffer*")))
    (with-current-buffer buffer
      (insert content))
    buffer))

(defun test-state-cleanup-buffer (buffer)
  "Kill the test BUFFER."
  (when (buffer-live-p buffer)
    (kill-buffer buffer)))

;;;; Basic state detection tests

(ert-deftest test-buffer-state-detection-basic ()
  "Test basic state detection functionality."
  (let ((buffer (test-state-setup-buffer "")))
    (unwind-protect
        (progn
          ;; Initial state should be nil
          (should-not (ecc-detect-state buffer))
          
          ;; Test y/n state
          (with-current-buffer buffer
            (erase-buffer)
            (insert "Would you like to continue? [y/n]"))
          (should (eq (ecc-detect-state buffer) :y/n))
          
          ;; Test y/y/n state
          (with-current-buffer buffer
            (erase-buffer)
            (insert "Would you like to see more options? [Y/y/n]"))
          (should (eq (ecc-detect-state buffer) :y/y/n))
          
          ;; Test waiting state
          (with-current-buffer buffer
            (erase-buffer)
            (insert "Type 'continue>' to continue"))
          (should (eq (ecc-detect-state buffer) :waiting)))
      
      ;; Clean up
      (test-state-cleanup-buffer buffer))))

(ert-deftest test-buffer-state-detection-name-formatting ()
  "Test state name formatting returns proper string representations."
  ;; Verify we get string output for each state type
  (should (stringp (ecc-state-get-name :y/n)))
  (should (stringp (ecc-state-get-name :y/y/n)))
  (should (stringp (ecc-state-get-name :waiting)))
  (should (stringp (ecc-state-get-name :initial-waiting)))
  (should (stringp (ecc-state-get-name :custom))))

;;;; Detection in complex buffer conditions

(ert-deftest test-buffer-state-detection-complex-content ()
  "Test detection works with large amounts of content."
  (let ((buffer (test-state-setup-buffer "")))
    (unwind-protect
        (progn
          ;; Create a buffer with significant content
          (with-current-buffer buffer
            (dotimes (i 10)
              (insert (format "Line %d of test content\n" (1+ i))))
            
            ;; Add prompt in the middle of content
            (insert "[y/n] prompt in the middle\n")
            
            (dotimes (i 5)
              (insert (format "Line %d after prompt\n" (1+ i))))
            
            ;; Verify prompt is detected
            (should (eq (ecc-detect-state buffer) :y/n)))
      
      ;; Clean up
      (test-state-cleanup-buffer buffer))))

;;;; Custom patterns tests

(ert-deftest test-buffer-state-detection-custom-patterns ()
  "Test detection works with custom prompt patterns."
  (let ((buffer (test-state-setup-buffer ""))
        (original-y/n ecc-state-prompt-y/n))
    
    ;; Set a custom pattern
    (setq ecc-state-prompt-y/n "Custom Y/N pattern")
    
    (unwind-protect
        (progn
          ;; Test custom pattern is detected
          (with-current-buffer buffer
            (insert "This contains a Custom Y/N pattern here"))
          (should (eq (ecc-detect-state buffer) :y/n)))
      
      ;; Restore original state
      (setq ecc-state-prompt-y/n original-y/n)
      
      ;; Clean up
      (test-state-cleanup-buffer buffer))))

;;;; Notification integration tests

(ert-deftest test-buffer-state-detection-notification ()
  "Test integration with notification system."
  (let ((buffer (test-state-setup-buffer ""))
        (ecc-auto-notify-on-claude-prompt t)
        (notification-called nil)
        (notification-state nil))
    
    ;; Mock the notification function
    (cl-letf (((symbol-function 'ecc-auto-notify-check-state)
               (lambda (state) 
                 (setq notification-called t)
                 (setq notification-state state))))
      
      (unwind-protect
          (progn
            ;; Insert a prompt and trigger notification
            (with-current-buffer buffer
              (insert "Would you like to continue? [y/n]"))
            
            ;; Check notification is triggered correctly
            (should (eq (ecc-state-notify-if-prompt-detected buffer) :y/n))
            (should notification-called)
            (should (eq notification-state :y/n)))
      
      ;; Clean up
      (test-state-cleanup-buffer buffer))))

(provide 'test-buffer-state-detection)
;;; test-buffer-state-detection.el ends here