;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 15:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-auto-response.el

;;; Commentary:
;;; Tests for the auto-response module.
;;; These tests verify the functionality of the auto-response system,
;;; including global mode, buffer-local mode, and different response types.

(require 'ert)
(require 'ecc-auto-core)
(require 'ecc-auto-response)
(require 'ecc-state-detection-consolidated)

;;; Code:

;;;; Test utilities

(defvar auto-response-test-sent-string nil
  "String sent during testing.")

(defvar auto-response-test-sent-buffer nil
  "Buffer to which string was sent during testing.")

(defvar auto-response-test-notification nil
  "Notification message displayed during testing.")

(defmacro with-temp-buffer-fixture (content &rest body)
  "Create a temporary buffer with CONTENT and evaluate BODY.
Ensures proper cleanup of test resources."
  (declare (indent 1))
  `(let ((temp-buffer (generate-new-buffer "*auto-response-test*")))
     (unwind-protect
         (progn
           (with-current-buffer temp-buffer
             (insert ,content)
             (goto-char (point-max)))
           (setq auto-response-test-sent-string nil
                 auto-response-test-sent-buffer nil
                 auto-response-test-notification nil)
           ,@body)
       (when (buffer-live-p temp-buffer)
         (kill-buffer temp-buffer)))))

;; Mock state detection function for controlled testing
(defvar auto-response-test-mock-state nil
  "State to return from the mock state detection function.")

(defun auto-response-test-mock-detect-state ()
  "Mock implementation of state detection.
Returns `auto-response-test-mock-state'."
  auto-response-test-mock-state)

;; Mock buffer process functions for testing
(defun auto-response-test-mock-vterm-send-string (buffer string)
  "Mock implementation of vterm-send-string for BUFFER and STRING.
Records the string and buffer for testing."
  (setq auto-response-test-sent-string string
        auto-response-test-sent-buffer buffer))

(defun auto-response-test-mock-message (&rest args)
  "Mock implementation of message for ARGS.
Records the message for testing."
  (setq auto-response-test-notification (apply #'format args)))

;; Mock buffer-local variables for testing
(defvar-local ecc-buffer-auto-response-enabled nil
  "Mock buffer-local auto-response enabled state.")

(defvar-local ecc-buffer-auto-response-y/n "1"
  "Mock buffer-local response for Y/N prompt.")

(defvar-local ecc-buffer-auto-response-y/y/n "2"
  "Mock buffer-local response for Y/Y/N prompt.")

(defvar-local ecc-buffer-auto-response-waiting "/auto"
  "Mock buffer-local response for waiting prompt.")

(defvar-local ecc-buffer-auto-response-initial-waiting "/user:understand-guidelines"
  "Mock buffer-local response for initial waiting prompt.")

(defvar-local ecc-buffer-auto-notify-completions t
  "Mock buffer-local auto-notify setting.")

;; Test buffer state system
(defun auto-response-test-mock-buffer-state-throttled-p (state)
  "Mock implementation of buffer state throttling test for STATE.
Always returns nil for testing (no throttling)."
  nil)

(defun auto-response-test-mock-buffer-state-get (key)
  "Mock implementation of buffer state get for KEY.
Returns a test value for KEY."
  (cond
   ((eq key 'prompt-state) auto-response-test-mock-state)
   (t nil)))

(defun auto-response-test-mock-buffer-state-set (key value)
  "Mock implementation of buffer state set for KEY and VALUE.
Does nothing in testing."
  nil)

;;;; Global mode tests

(ert-deftest test-auto-response-start-global ()
  "Test starting auto-response in global mode."
  (with-temp-buffer-fixture "Test content"
    ;; Set global mode
    (setq ecc-auto-response-buffer-local-default nil)
    
    ;; Mock core functions
    (cl-letf (((symbol-function 'ecc-auto-core-register-buffer) #'ignore)
              ((symbol-function 'ecc-auto-core-timer-start) #'ignore)
              ((symbol-function 'ecc-auto-core-initial-check) #'ignore))
      
      ;; Start auto-response
      (ecc-auto-response-start)
      
      ;; Check if auto-response was enabled
      (should ecc-auto-response-enabled)
      (should ecc-auto-response--registered-callback))))

(ert-deftest test-auto-response-stop-global ()
  "Test stopping auto-response in global mode."
  (with-temp-buffer-fixture "Test content"
    ;; Set global mode and enabled state
    (setq ecc-auto-response-buffer-local-default nil
          ecc-auto-response-enabled t
          ecc-auto-response--registered-callback (lambda (buffer state) nil))
    
    ;; Mock core functions
    (cl-letf (((symbol-function 'ecc-auto-core-timer-stop) #'ignore)
              ((symbol-function 'ecc-auto-core-reset-state) #'ignore))
      
      ;; Stop auto-response
      (ecc-auto-response-stop)
      
      ;; Check if auto-response was disabled
      (should-not ecc-auto-response-enabled)
      (should-not ecc-auto-response--registered-callback))))

(ert-deftest test-auto-response-toggle-global ()
  "Test toggling auto-response in global mode."
  (with-temp-buffer-fixture "Test content"
    ;; Set global mode
    (setq ecc-auto-response-buffer-local-default nil)
    
    ;; Mock dependencies
    (cl-letf (((symbol-function 'ecc-auto-response-start) 
               (lambda () (setq ecc-auto-response-enabled t)))
              ((symbol-function 'ecc-auto-response-stop)
               (lambda () (setq ecc-auto-response-enabled nil))))
      
      ;; Start from disabled state
      (setq ecc-auto-response-enabled nil)
      (ecc-auto-response-toggle)
      (should ecc-auto-response-enabled)
      
      ;; Toggle to disabled state
      (ecc-auto-response-toggle)
      (should-not ecc-auto-response-enabled))))

(ert-deftest test-auto-response-send ()
  "Test sending response to Claude prompt."
  (with-temp-buffer-fixture "Test content with [y/n] prompt"
    ;; Set global mode and enabled state
    (setq ecc-auto-response-buffer-local-default nil
          ecc-auto-response-enabled t
          ecc-auto-response-yes "TEST_YES"
          ecc-auto-response-notify t)
    
    ;; Replace dependencies with mocks
    (cl-letf (((symbol-function 'ecc-detect-state) #'auto-response-test-mock-detect-state)
              ((symbol-function 'vterm-send-string) 
               (lambda (text) 
                 (setq auto-response-test-sent-string text
                       auto-response-test-sent-buffer (current-buffer))))
              ((symbol-function 'vterm-send-return) #'ignore)
              ((symbol-function 'message) #'auto-response-test-mock-message))
      
      ;; Test y/n prompt
      (setq auto-response-test-mock-state :y/n)
      (with-current-buffer temp-buffer
        (setq major-mode 'vterm-mode)) ;; Mock vterm mode
      
      ;; Send response
      (ecc-auto-response-send temp-buffer)
      
      ;; Check response was sent correctly
      (should (string= auto-response-test-sent-string "TEST_YES"))
      (should (eq auto-response-test-sent-buffer temp-buffer))
      (should auto-response-test-notification)
      
      ;; Test disabled auto-response
      (setq ecc-auto-response-enabled nil
            auto-response-test-sent-string nil)
      (ecc-auto-response-send temp-buffer)
      
      ;; Check no response was sent
      (should-not auto-response-test-sent-string))))

(ert-deftest test-auto-response-dispatch-response ()
  "Test dispatching responses to different terminal types."
  (with-temp-buffer-fixture "Test content"
    (cl-letf (((symbol-function 'vterm-send-string) 
               (lambda (text) 
                 (setq auto-response-test-sent-string text
                       auto-response-test-sent-buffer (current-buffer))))
              ((symbol-function 'vterm-send-return) #'ignore)
              ((symbol-function 'message) #'auto-response-test-mock-message))
      
      ;; Test vterm mode
      (with-current-buffer temp-buffer
        (setq major-mode 'vterm-mode))
      
      (ecc-auto-response--dispatch-response temp-buffer "TEST_RESP" "TEST_TYPE")
      
      ;; Check response was sent via vterm
      (should (string= auto-response-test-sent-string "TEST_RESP"))
      (should (eq auto-response-test-sent-buffer temp-buffer))
      
      ;; Test notification
      (should (string-match-p "TEST_TYPE" auto-response-test-notification)))))

;;;; Buffer-local mode tests

(ert-deftest test-auto-response-buffer-local-init ()
  "Test buffer-local auto-response initialization."
  (with-temp-buffer-fixture "Test content"
    ;; Mock dependencies
    (cl-letf (((symbol-function 'ecc-buffer-state-init) #'ignore)
              ((symbol-function 'ecc-buffer-local-init) #'ignore)
              ((symbol-function 'ecc-auto-core-register-buffer-local) #'ignore))
      
      ;; Initialize buffer-local auto-response
      (ecc-auto-response-buffer-local-init temp-buffer)
      
      ;; Check buffer-local variables set
      (with-current-buffer temp-buffer
        (should (boundp 'ecc-buffer-auto-response-enabled))
        (should (equal ecc-buffer-auto-response-y/n ecc-auto-response-yes))
        (should (equal ecc-buffer-auto-response-y/y/n ecc-auto-response-yes-plus))
        (should (equal ecc-buffer-auto-response-waiting ecc-auto-response-continue))
        (should (equal ecc-buffer-auto-response-initial-waiting ecc-auto-response-initial-waiting))
        (should (equal ecc-buffer-auto-notify-completions ecc-auto-response-notify))))))

(ert-deftest test-auto-response-buffer-local-check ()
  "Test buffer-local state checking and response."
  (with-temp-buffer-fixture "Test content with [y/n] prompt"
    ;; Set up buffer-local state
    (with-current-buffer temp-buffer
      (setq ecc-buffer-auto-response-enabled t
            ecc-buffer-auto-response-y/n "TEST_BUFFER_YES"))
    
    ;; Replace dependencies with mocks
    (cl-letf (((symbol-function 'ecc-detect-state) #'auto-response-test-mock-detect-state)
              ((symbol-function 'ecc-buffer-state-throttled-p) #'auto-response-test-mock-buffer-state-throttled-p)
              ((symbol-function 'ecc-buffer-state-get) #'auto-response-test-mock-buffer-state-get)
              ((symbol-function 'ecc-buffer-state-set) #'auto-response-test-mock-buffer-state-set)
              ((symbol-function 'ecc-auto-response-buffer-local-send-message)
               (lambda (buffer response type)
                 (setq auto-response-test-sent-string response
                       auto-response-test-sent-buffer buffer))))
      
      ;; Test with y/n state
      (setq auto-response-test-mock-state :y/n)
      
      ;; Check state and send response
      (ecc-auto-response-buffer-local-check temp-buffer)
      
      ;; Verify response was sent
      (should (string= auto-response-test-sent-string "TEST_BUFFER_YES"))
      (should (eq auto-response-test-sent-buffer temp-buffer))
      
      ;; Test disabled buffer-local auto-response
      (with-current-buffer temp-buffer
        (setq ecc-buffer-auto-response-enabled nil))
      
      (setq auto-response-test-sent-string nil)
      (ecc-auto-response-buffer-local-check temp-buffer)
      
      ;; Check no response was sent
      (should-not auto-response-test-sent-string))))

;;;; Convenience function tests

(ert-deftest test-auto-response-yes ()
  "Test convenience function for sending Y response."
  (with-temp-buffer-fixture "Test content"
    ;; Set global mode
    (setq ecc-auto-response-buffer-local-default nil
          ecc-auto-response-yes "TEST_GLOBAL_YES")
    
    ;; Replace dependencies with mocks
    (cl-letf (((symbol-function 'ecc-auto-response--dispatch-response)
               (lambda (buffer response type)
                 (setq auto-response-test-sent-string response
                       auto-response-test-sent-buffer buffer))))
      
      ;; Send yes response
      (ecc-auto-response-yes temp-buffer)
      
      ;; Verify response was sent
      (should (string= auto-response-test-sent-string "TEST_GLOBAL_YES"))
      (should (eq auto-response-test-sent-buffer temp-buffer))
      
      ;; Test buffer-local mode
      (setq ecc-auto-response-buffer-local-default t)
      (with-current-buffer temp-buffer
        (setq ecc-buffer-auto-response-enabled t
              ecc-buffer-auto-response-y/n "TEST_BUFFER_YES"))
      
      (cl-letf (((symbol-function 'buffer-local-value) 
                 (lambda (var buffer) 
                   (if (eq buffer temp-buffer)
                       (buffer-local-value var temp-buffer)
                     nil)))
                ((symbol-function 'ecc-auto-response-buffer-local-send-message)
                 (lambda (buffer response type)
                   (setq auto-response-test-sent-string response
                         auto-response-test-sent-buffer buffer))))
        
        ;; Reset and send in buffer-local mode
        (setq auto-response-test-sent-string nil)
        (ecc-auto-response-yes temp-buffer)
        
        ;; Verify buffer-local response was sent
        (should (string= auto-response-test-sent-string "TEST_BUFFER_YES"))
        (should (eq auto-response-test-sent-buffer temp-buffer))))))

(ert-deftest test-auto-response-custom ()
  "Test sending custom response text."
  (with-temp-buffer-fixture "Test content"
    ;; Set global mode
    (setq ecc-auto-response-buffer-local-default nil)
    
    ;; Replace dependencies with mocks
    (cl-letf (((symbol-function 'ecc-auto-response--dispatch-response)
               (lambda (buffer response type)
                 (setq auto-response-test-sent-string response
                       auto-response-test-sent-buffer buffer))))
      
      ;; Send custom response
      (with-current-buffer temp-buffer
        (ecc-auto-response-custom "TEST_CUSTOM"))
      
      ;; Verify response was sent
      (should (string= auto-response-test-sent-string "TEST_CUSTOM"))
      (should (eq auto-response-test-sent-buffer temp-buffer)))))

;;;; Backward compatibility tests

(ert-deftest test-auto-response-legacy-functions ()
  "Test backward compatibility functions."
  (with-temp-buffer-fixture "Test content"
    ;; Mock dependencies
    (cl-letf (((symbol-function 'ecc-auto-response-start) 
               (lambda () (setq ecc-auto-response-enabled t)))
              ((symbol-function 'ecc-auto-response-stop)
               (lambda () (setq ecc-auto-response-enabled nil)))
              ((symbol-function 'ecc-auto-response-toggle)
               (lambda () (setq ecc-auto-response-enabled (not ecc-auto-response-enabled)))))
      
      ;; Test legacy start function
      (setq ecc-auto-response-enabled nil)
      (ecc-start-auto-response)
      (should ecc-auto-response-enabled)
      
      ;; Test legacy stop function
      (ecc-stop-auto-response)
      (should-not ecc-auto-response-enabled)
      
      ;; Test legacy toggle function
      (ecc-toggle-auto-response)
      (should ecc-auto-response-enabled))))

(provide 'test-ecc-auto-response)

;;; test-ecc-auto-response.el ends here