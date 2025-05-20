;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 00:15:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-auto/test-ecc-auto-core.el

;;; Commentary:
;;; Tests for the auto-response core infrastructure (ecc-auto-core.el).

(require 'ert)
(require 'ecc-variables)
(require 'ecc-auto-core)

;;; Code:

;; Helper functions for testing
(defun test-ecc-auto-core--mock-buffer ()
  "Create a temporary buffer for testing."
  (generate-new-buffer "*ecc-test*"))

(defun test-ecc-auto-core--cleanup-buffer (buffer)
  "Clean up test BUFFER."
  (when (buffer-live-p buffer)
    (kill-buffer buffer)))

;; Basic tests
(ert-deftest test-ecc-auto-core-timer-operations ()
  "Test timer operations."
  ;; Ensure timer is initially off
  (ecc-auto-core-timer-stop)
  (should-not (ecc-auto-core-timer-active-p))
  
  ;; Start timer
  (ecc-auto-core-timer-start (lambda () nil))
  (should (ecc-auto-core-timer-active-p))
  
  ;; Stop timer
  (ecc-auto-core-timer-stop)
  (should-not (ecc-auto-core-timer-active-p)))

(ert-deftest test-ecc-auto-core-throttling ()
  "Test throttling functions."
  ;; Reset state
  (ecc-auto-core-reset-state)
  
  ;; Should not be throttled initially
  (should-not (ecc-auto-core-throttled-p))
  
  ;; Update time and check throttling
  (ecc-auto-core-update-time)
  (should (ecc-auto-core-throttled-p))
  
  ;; Reset state and check again
  (ecc-auto-core-reset-state)
  (should-not (ecc-auto-core-throttled-p)))

(ert-deftest test-ecc-auto-core-buffer-registration ()
  "Test buffer registration and management."
  (let ((buffer (test-ecc-auto-core--mock-buffer)))
    (unwind-protect
        (progn
          ;; Register buffer
          (should (eq (ecc-auto-core-register-buffer buffer) buffer))
          (should (memq buffer (ecc-auto-core-registered-buffers)))
          
          ;; Unregister buffer
          (ecc-auto-core-unregister-buffer buffer)
          (should-not (memq buffer (ecc-auto-core-registered-buffers)))
          
          ;; Register again
          (ecc-auto-core-register-buffer buffer)
          (should (memq buffer (ecc-auto-core-registered-buffers)))
          
          ;; Clean up buffers should remove killed buffers
          (kill-buffer buffer)
          (should (eq (length (ecc-auto-core-cleanup-buffers)) 0)))
      
      ;; Clean up in case test fails
      (test-ecc-auto-core--cleanup-buffer buffer))))

(ert-deftest test-ecc-auto-core-processing ()
  "Test buffer processing functions."
  (let ((buffer (test-ecc-auto-core--mock-buffer))
        (processed nil)
        (callback (lambda (buf) (setq processed buf))))
    
    (unwind-protect
        (progn
          ;; Process buffer should call callback
          (ecc-auto-core-process-buffer buffer callback)
          (should (eq processed buffer))
          
          ;; Reset for next test
          (setq processed nil)
          
          ;; Register buffer
          (ecc-auto-core-register-buffer buffer)
          
          ;; Process all should call for registered buffers
          (ecc-auto-core-process-all-buffers callback)
          (should (eq processed buffer)))
      
      ;; Clean up
      (test-ecc-auto-core--cleanup-buffer buffer))))

(ert-deftest test-ecc-auto-core-initialize-shutdown ()
  "Test system initialization and shutdown."
  (let ((buffer (test-ecc-auto-core--mock-buffer)))
    (unwind-protect
        (progn
          ;; Register buffer and start timer
          (ecc-auto-core-register-buffer buffer)
          (ecc-auto-core-timer-start (lambda () nil))
          
          ;; Initialize should keep timer but clean state
          (ecc-auto-core-initialize)
          (should (ecc-auto-core-timer-active-p))
          (should-not (ecc-auto-core-throttled-p))
          
          ;; Shutdown should stop everything
          (ecc-auto-core-shutdown)
          (should-not (ecc-auto-core-timer-active-p))
          (should (eq (length (ecc-auto-core-registered-buffers)) 0)))
      
      ;; Clean up
      (test-ecc-auto-core--cleanup-buffer buffer))))

(ert-deftest test-ecc-auto-core-debug-status ()
  "Test debug status reporting."
  ;; Reset state
  (ecc-auto-core-shutdown)
  
  ;; Check status string
  (let ((status (ecc-auto-core-debug-status)))
    (should (stringp status))
    (should (string-match-p "Timer Active: No" status))
    (should (string-match-p "Registered Buffers: 0" status))))

(provide 'test-ecc-auto-core)

;;; test-ecc-auto-core.el ends here