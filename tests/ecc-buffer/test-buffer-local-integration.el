;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 19:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-buffer/test-buffer-local-integration.el

;;; Commentary:
;;; Integration tests for buffer-local state tracking and auto-response.
;;; Tests the interaction between multiple Claude buffers with different configurations.

(require 'ert)
(require 'ecc-variables-consolidated)
(require 'ecc-api)
(require 'ecc-buffer-local)
(require 'ecc-api)
(require 'ecc-buffer-api)
(require 'ecc-api)
(require 'ecc-auto-response-buffer-local)
(require 'ecc-api)

;; Test fixtures
(defvar ecc-test-buffer-a nil "First test buffer.")
(defvar ecc-test-buffer-b nil "Second test buffer.")

(defun ecc-integration-test-setup-buffers ()
  "Set up test buffers for integration tests."
  ;; Create test buffers
  (setq ecc-test-buffer-a (generate-new-buffer "*ecc-integration-a*"))
  (setq ecc-test-buffer-b (generate-new-buffer "*ecc-integration-b*"))
  
  ;; Register them as Claude buffers with buffer-local config
  (ecc-buffer-register ecc-test-buffer-a)
  (ecc-buffer-register ecc-test-buffer-b)
  
  ;; Set up different content to simulate different states
  (with-current-buffer ecc-test-buffer-a
    (erase-buffer)
    (insert "Some content here\n")
    (insert "│ > Try \n"))  ;; initial-waiting pattern
  
  (with-current-buffer ecc-test-buffer-b
    (erase-buffer)
    (insert "Other content\n")
    (insert "[Y/n]\n")))  ;; y/n pattern

(defun ecc-integration-test-teardown-buffers ()
  "Clean up test buffers."
  (when (buffer-live-p ecc-test-buffer-a)
    (kill-buffer ecc-test-buffer-a))
  (when (buffer-live-p ecc-test-buffer-b)
    (kill-buffer ecc-test-buffer-b))
  (setq ecc-test-buffer-a nil)
  (setq ecc-test-buffer-b nil))

(ert-deftest ecc-test-multi-buffer-independent-config ()
  "Test that multiple buffers can have independent configurations."
  (ecc-integration-test-setup-buffers)
  (unwind-protect
      (progn
        ;; Set different configurations for each buffer
        (with-current-buffer ecc-test-buffer-a
          (ecc-buffer-auto-response-set-y/n "a-yes")
          (ecc-buffer-auto-response-set-initial "/a-start"))
        
        (with-current-buffer ecc-test-buffer-b
          (ecc-buffer-auto-response-set-y/n "b-yes")
          (ecc-buffer-auto-response-set-initial "/b-start"))
        
        ;; Verify configurations are independent
        (with-current-buffer ecc-test-buffer-a
          (should (string= ecc-buffer-auto-response-y/n "a-yes"))
          (should (string= ecc-buffer-auto-response-initial-waiting "/a-start")))
        
        (with-current-buffer ecc-test-buffer-b
          (should (string= ecc-buffer-auto-response-y/n "b-yes"))
          (should (string= ecc-buffer-auto-response-initial-waiting "/b-start"))))
    (ecc-integration-test-teardown-buffers)))

(ert-deftest ecc-test-multi-buffer-independent-state-tracking ()
  "Test that state tracking is independent across buffers."
  (ecc-integration-test-setup-buffers)
  (unwind-protect
      (progn
        ;; Detect states in each buffer
        (with-current-buffer ecc-test-buffer-a
          (should (eq (ecc-buffer-state-detect) :initial-waiting)))
        
        (with-current-buffer ecc-test-buffer-b
          (should (eq (ecc-buffer-state-detect) :y/n)))
        
        ;; Verify that buffer-local state is tracked independently
        (with-current-buffer ecc-test-buffer-a
          (should (eq ecc-buffer-state :initial-waiting)))
        
        (with-current-buffer ecc-test-buffer-b
          (should (eq ecc-buffer-state :y/n))))
    (ecc-integration-test-teardown-buffers)))

(ert-deftest ecc-test-multi-buffer-independent-auto-response ()
  "Test that auto-response works independently across buffers."
  (ecc-integration-test-setup-buffers)
  (unwind-protect
      (progn
        ;; Set up tracking variables
        (defvar ecc-test-response-a nil)
        (defvar ecc-test-response-b nil)
        
        ;; Set different configurations
        (with-current-buffer ecc-test-buffer-a
          (setq-local ecc-buffer-auto-response-initial-waiting "/a-start")
          (setq-local ecc-buffer-auto-response-enabled t))
        
        (with-current-buffer ecc-test-buffer-b
          (setq-local ecc-buffer-auto-response-y/n "b-yes")
          (setq-local ecc-buffer-auto-response-enabled t))
        
        ;; Mock the send function to capture responses
        (cl-letf (((symbol-function 'ecc-buffer-send-response)
                   (lambda (response type)
                     (cond
                      ((eq (current-buffer) ecc-test-buffer-a)
                       (setq ecc-test-response-a response))
                      ((eq (current-buffer) ecc-test-buffer-b)
                       (setq ecc-test-response-b response))))))
          
          ;; Run auto-response check in both buffers
          (ecc-auto-response-buffer-local-check ecc-test-buffer-a)
          (ecc-auto-response-buffer-local-check ecc-test-buffer-b)
          
          ;; Verify the responses were correct and buffer-specific
          (should (string= ecc-test-response-a "/a-start"))
          (should (string= ecc-test-response-b "b-yes"))))
    (ecc-integration-test-teardown-buffers)))

(ert-deftest ecc-test-multi-buffer-independent-throttling ()
  "Test that throttling is independent across buffers."
  (ecc-integration-test-setup-buffers)
  (unwind-protect
      (progn
        ;; Enable auto-response for both buffers
        (with-current-buffer ecc-test-buffer-a
          (setq-local ecc-buffer-auto-response-enabled t)
          (setq-local ecc-buffer-auto-response-initial-waiting "/a-start"))
        
        (with-current-buffer ecc-test-buffer-b
          (setq-local ecc-buffer-auto-response-enabled t)
          (setq-local ecc-buffer-auto-response-y/n "b-yes"))
        
        ;; Set up throttling for buffer A only
        (with-current-buffer ecc-test-buffer-a
          (ecc-buffer-local-update-time :initial-waiting)
          (setq-local ecc-buffer-active-state :initial-waiting))
        
        ;; Set up response tracking
        (defvar ecc-test-throttle-response-a nil)
        (defvar ecc-test-throttle-response-b nil)
        
        ;; Mock the send function to track responses
        (cl-letf (((symbol-function 'ecc-buffer-send-response)
                   (lambda (response type)
                     (cond
                      ((eq (current-buffer) ecc-test-buffer-a)
                       (setq ecc-test-throttle-response-a response))
                      ((eq (current-buffer) ecc-test-buffer-b)
                       (setq ecc-test-throttle-response-b response))))))
          
          ;; Run auto-response check in both buffers
          (ecc-auto-response-buffer-local-check ecc-test-buffer-a)
          (ecc-auto-response-buffer-local-check ecc-test-buffer-b)
          
          ;; Buffer A should be throttled, Buffer B should respond
          (should (null ecc-test-throttle-response-a))
          (should (string= ecc-test-throttle-response-b "b-yes"))))
    (ecc-integration-test-teardown-buffers)))

(ert-deftest ecc-test-buffer-api-integration ()
  "Test that the API properly handles buffer-local settings."
  (ecc-integration-test-setup-buffers)
  (unwind-protect
      (progn
        ;; Use API to set buffer-specific settings
        (ecc-buffer-auto-response-set-y/n "api-yes" ecc-test-buffer-a)
        (ecc-buffer-auto-response-set-waiting "/api-continue" ecc-test-buffer-b)
        
        ;; Verify settings through the API
        (should (string= (ecc-buffer-settings-get 'ecc-buffer-auto-response-y/n ecc-test-buffer-a)
                         "api-yes"))
        
        (should (string= (ecc-buffer-settings-get 'ecc-buffer-auto-response-waiting ecc-test-buffer-b)
                         "/api-continue"))
        
        ;; Use API to enable/disable auto-response
        (ecc-buffer-auto-response-enable ecc-test-buffer-a)
        (ecc-buffer-auto-response-disable ecc-test-buffer-b)
        
        ;; Verify auto-response state 
        (should (ecc-buffer-settings-get 'ecc-buffer-auto-response-enabled ecc-test-buffer-a))
        (should-not (ecc-buffer-settings-get 'ecc-buffer-auto-response-enabled ecc-test-buffer-b)))
    (ecc-integration-test-teardown-buffers)))

(provide 'test-buffer-local-integration)

;;; test-buffer-local-integration.el ends here