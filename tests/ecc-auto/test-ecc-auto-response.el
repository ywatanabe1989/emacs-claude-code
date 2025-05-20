;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 00:20:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-auto/test-ecc-auto-response.el

;;; Commentary:
;;; Tests for the main auto-response module (ecc-auto-response.el).

(require 'ert)
(require 'ecc-variables)
(require 'ecc-auto-core)
(require 'ecc-auto-detect)
(require 'ecc-auto-response)

;;; Code:

;; Helper functions for testing
(defun test-ecc-auto-response--mock-buffer (content)
  "Create a temporary buffer with CONTENT for testing."
  (let ((buffer (generate-new-buffer "*ecc-test*")))
    (with-current-buffer buffer
      (insert content))
    buffer))

(defun test-ecc-auto-response--cleanup-buffer (buffer)
  "Clean up test BUFFER."
  (when (buffer-live-p buffer)
    (kill-buffer buffer)))

;; Mock sending function to capture the response
(defvar test-ecc-auto-response--last-response nil)
(defvar test-ecc-auto-response--last-type nil)

(defun test-ecc-auto-response--mock-send-message (buffer response type)
  "Mock the send message function for testing.
Captures RESPONSE and TYPE for verification."
  (setq test-ecc-auto-response--last-response response)
  (setq test-ecc-auto-response--last-type type)
  t)

;; Tests for auto-response functionality
(ert-deftest test-ecc-auto-response-send ()
  "Test sending responses based on state."
  (let ((buffer (test-ecc-auto-response--mock-buffer "Test content"))
        (ecc-auto-response-enabled t)
        (ecc-auto-response-y/n "test-yes")
        (ecc-auto-response-y/y/n "test-yes-plus")
        (ecc-auto-response-waiting "test-continue")
        (ecc-auto-response-initial-waiting "test-initial"))
    
    ;; Temporarily override send function
    (cl-letf (((symbol-function 'ecc-auto-response--send-message)
               #'test-ecc-auto-response--mock-send-message))
      
      (unwind-protect
          (progn
            ;; Test Y/N response
            (ecc-auto-response-send buffer :y/n)
            (should (string= test-ecc-auto-response--last-response "test-yes"))
            (should (string= test-ecc-auto-response--last-type "Y/N"))
            
            ;; Test Y/Y/N response
            (ecc-auto-response-send buffer :y/y/n)
            (should (string= test-ecc-auto-response--last-response "test-yes-plus"))
            (should (string= test-ecc-auto-response--last-type "Y/Y/N"))
            
            ;; Test waiting response
            (ecc-auto-response-send buffer :waiting)
            (should (string= test-ecc-auto-response--last-response "test-continue"))
            (should (string= test-ecc-auto-response--last-type "Continue"))
            
            ;; Test initial waiting response
            (ecc-auto-response-send buffer :initial-waiting)
            (should (string= test-ecc-auto-response--last-response "test-initial"))
            (should (string= test-ecc-auto-response--last-type "Initial-Waiting")))
        
        (test-ecc-auto-response--cleanup-buffer buffer)))))

(ert-deftest test-ecc-auto-response-check-and-respond ()
  "Test checking and responding to prompt states."
  (let* ((content "❯ 1. Yes\n  2. No\n")
         (buffer (test-ecc-auto-response--mock-buffer content))
         (ecc-auto-response-enabled t)
         (ecc-auto-response-y/n "test-yes"))
    
    ;; Temporarily override send function
    (cl-letf (((symbol-function 'ecc-auto-response--send-message)
               #'test-ecc-auto-response--mock-send-message))
      
      (unwind-protect
          (progn
            ;; Should detect Y/N and respond
            (should (ecc-auto-response-check-and-respond buffer))
            (should (string= test-ecc-auto-response--last-response "test-yes"))
            (should (string= test-ecc-auto-response--last-type "Y/N")))
        
        (test-ecc-auto-response--cleanup-buffer buffer)))))

(ert-deftest test-ecc-auto-response-disabled-skips ()
  "Test that disabled auto-response skips processing."
  (let* ((content "❯ 1. Yes\n  2. No\n")
         (buffer (test-ecc-auto-response--mock-buffer content))
         (ecc-auto-response-enabled nil))
    
    (unwind-protect
        (progn
          ;; Disable auto-response and verify it skips
          (setq test-ecc-auto-response--last-response nil)
          (setq test-ecc-auto-response--last-type nil)
          
          (should-not (ecc-auto-response-check-and-respond buffer))
          (should-not test-ecc-auto-response--last-response)
          (should-not test-ecc-auto-response--last-type))
      
      (test-ecc-auto-response--cleanup-buffer buffer))))

;; Test the convenience functions
(ert-deftest test-ecc-auto-response-convenience-functions ()
  "Test convenience functions for specific response types."
  (let ((buffer (test-ecc-auto-response--mock-buffer "Test content"))
        (ecc-auto-response-y/n "test-yes")
        (ecc-auto-response-y/y/n "test-yes-plus")
        (ecc-auto-response-waiting "test-continue"))
    
    ;; Temporarily override send function
    (cl-letf (((symbol-function 'ecc-auto-response--send-message)
               #'test-ecc-auto-response--mock-send-message))
      
      (unwind-protect
          (progn
            ;; Test Y/N convenience function
            (ecc-auto-response-yes buffer)
            (should (string= test-ecc-auto-response--last-response "test-yes"))
            (should (string= test-ecc-auto-response--last-type "Y/N"))
            
            ;; Test Y/Y/N convenience function
            (ecc-auto-response-yes-plus buffer)
            (should (string= test-ecc-auto-response--last-response "test-yes-plus"))
            (should (string= test-ecc-auto-response--last-type "Y/Y/N"))
            
            ;; Test continue convenience function
            (ecc-auto-response-continue buffer)
            (should (string= test-ecc-auto-response--last-response "test-continue"))
            (should (string= test-ecc-auto-response--last-type "Continue"))
            
            ;; Test custom response
            (ecc-auto-response-custom "test-custom")
            (should (string= test-ecc-auto-response--last-response "test-custom"))
            (should (string-match-p "Custom" test-ecc-auto-response--last-type)))
        
        (test-ecc-auto-response--cleanup-buffer buffer)))))

;; Test system start/stop
(ert-deftest test-ecc-auto-response-system-control ()
  "Test starting and stopping the auto-response system."
  ;; Make sure system is stopped
  (ecc-auto-response-stop)
  (should-not ecc-auto-response-enabled)
  (should-not (ecc-auto-core-timer-active-p))
  
  ;; Start system
  (ecc-auto-response-start)
  (should ecc-auto-response-enabled)
  (should (ecc-auto-core-timer-active-p))
  
  ;; Stop system
  (ecc-auto-response-stop)
  (should-not ecc-auto-response-enabled)
  (should-not (ecc-auto-core-timer-active-p))
  
  ;; Toggle system
  (ecc-auto-response-toggle)
  (should ecc-auto-response-enabled)
  (should (ecc-auto-core-timer-active-p))
  
  ;; Clean up
  (ecc-auto-response-stop))

;; Test buffer registration
(ert-deftest test-ecc-auto-response-register-buffer ()
  "Test registering buffers for auto-response."
  (let ((buffer (test-ecc-auto-response--mock-buffer "Test content")))
    (unwind-protect
        (progn
          ;; Register buffer
          (should (eq (ecc-auto-response-register-buffer buffer) buffer))
          (should (memq buffer (ecc-auto-core-registered-buffers))))
      
      (test-ecc-auto-response--cleanup-buffer buffer))))

(provide 'test-ecc-auto-response)

;;; test-ecc-auto-response.el ends here