;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 20:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-regression/test-buffer-local-regression.el

;;; Commentary:
;;; Regression tests to ensure the buffer-local implementation maintains
;;; compatibility with the original functionality.

(require 'ert)
(require 'ecc-variables)
(require 'ecc-buffer-local)
(require 'ecc-buffer-api)
(require 'ecc-auto-response-unified)
(require 'ecc-auto-response-buffer-local)
(require 'ecc-state-detection)

;; Test buffers
(defvar ecc-regression-buffer nil "Buffer for regression testing.")

;; Mocked original functions for comparison
(defvar ecc-regression-original-detect-state-result nil
  "Result from mocked original detect-state function.")

(defvar ecc-regression-original-auto-response-params nil
  "Parameters sent to mocked original auto-response function.")

;; Test setup/teardown
(defun ecc-regression-setup ()
  "Set up regression test environment."
  (setq ecc-regression-buffer (generate-new-buffer "*ecc-regression*"))
  (ecc-buffer-register ecc-regression-buffer)
  
  ;; Reset test tracking variables
  (setq ecc-regression-original-detect-state-result nil)
  (setq ecc-regression-original-auto-response-params nil))

(defun ecc-regression-teardown ()
  "Clean up regression test environment."
  (when (buffer-live-p ecc-regression-buffer)
    (kill-buffer ecc-regression-buffer))
  (setq ecc-regression-buffer nil))

;; Mock original functions for comparison
(defun ecc-regression-mock-original-detect-state (buffer content state)
  "Mock the original detect state function.
BUFFER is the buffer to use, CONTENT is the content to insert,
STATE is the result to return."
  (with-current-buffer buffer
    (erase-buffer)
    (insert content)
    (setq ecc-regression-original-detect-state-result state)))

(defun ecc-regression-mock-original-auto-response (buffer response type)
  "Mock the original auto-response function.
BUFFER is the buffer to use, RESPONSE is the response string,
TYPE is the response type."
  (setq ecc-regression-original-auto-response-params
        (list :buffer buffer :response response :type type)))

;; Regression tests
(ert-deftest ecc-test-regression-state-detection ()
  "Test that buffer-local state detection matches original behavior."
  (ecc-regression-setup)
  (unwind-protect
      (let ((test-cases '(("Content with [Y/n]" :y/n)
                         ("Content with [Y/y/n]" :y/y/n)
                         ("Content with continue>" :waiting)
                         ("Content with │ > Try " :initial-waiting)
                         ("Regular content" nil))))
        
        (dolist (test-case test-cases)
          (let ((content (car test-case))
                (expected-state (cadr test-case)))
            
            ;; Test with mock original function
            (ecc-regression-mock-original-detect-state 
             ecc-regression-buffer content expected-state)
            
            ;; Test with buffer-local implementation
            (with-current-buffer ecc-regression-buffer
              (erase-buffer)
              (insert content)
              (let ((detected-state (ecc-buffer-state-detect)))
                ;; Verify both produce the same result
                (should (eq detected-state ecc-regression-original-detect-state-result)))))))
    (ecc-regression-teardown)))

(ert-deftest ecc-test-regression-auto-response ()
  "Test that buffer-local auto-response matches original behavior."
  (ecc-regression-setup)
  (unwind-protect
      (let ((test-cases '((":y/n" "1" "Y/N")
                         (":y/y/n" "2" "Y/Y/N")
                         (":waiting" "/auto" "Continue")
                         (":initial-waiting" "/user:understand-guidelines" "Initial-Waiting"))))
        
        (dolist (test-case test-cases)
          (let ((state (intern (car test-case)))
                (response (cadr test-case))
                (type (caddr test-case)))
            
            ;; Configure buffer with original-like settings
            (with-current-buffer ecc-regression-buffer
              (setq-local ecc-buffer-auto-response-y/n "1")
              (setq-local ecc-buffer-auto-response-y/y/n "2")
              (setq-local ecc-buffer-auto-response-waiting "/auto")
              (setq-local ecc-buffer-auto-response-initial-waiting 
                          "/user:understand-guidelines"))
            
            ;; Mock original auto-response function
            (ecc-regression-mock-original-auto-response
             ecc-regression-buffer response type)
            
            ;; Test with buffer-local implementation
            (with-current-buffer ecc-regression-buffer
              ;; Temporarily mock the actual send function
              (cl-letf (((symbol-function 'ecc-buffer-send-response)
                         (lambda (resp typ)
                           ;; Verify parameters match original
                           (should (string= resp response))
                           (should (string= typ type)))))
                
                ;; Process the state
                (ecc-auto-response-buffer-local-process-state state))))))
    (ecc-regression-teardown)))

(ert-deftest ecc-test-regression-api-backward-compatibility ()
  "Test that buffer-local API maintains backwards compatibility."
  (ecc-regression-setup)
  (unwind-protect
      (progn
        ;; Original API should still work with buffer-local system
        
        ;; 1. Global settings should initialize buffer-local settings
        (setq ecc-auto-response-y/n "global-1")
        (setq ecc-auto-response-waiting "/global-continue")
        
        ;; Register a new buffer with these globals
        (let ((new-buffer (generate-new-buffer "*ecc-regression-api*")))
          (unwind-protect
              (progn
                (ecc-buffer-register new-buffer)
                
                ;; Verify global settings were inherited
                (with-current-buffer new-buffer
                  (should (string= ecc-buffer-auto-response-y/n "global-1"))
                  (should (string= ecc-buffer-auto-response-waiting "/global-continue"))))
            (kill-buffer new-buffer)))
        
        ;; 2. Original API state functions should still work
        (with-current-buffer ecc-regression-buffer
          (erase-buffer)
          (insert "Content with [Y/n]")
          
          ;; Both detection methods should return same result
          (should (eq (ecc-detect-state) :y/n))
          (should (eq (ecc-buffer-state-detect) :y/n)))
        
        ;; 3. Buffer management API should be compatible
        (ecc-buffer-set-current ecc-regression-buffer)
        (should (eq (ecc-buffer-current) ecc-regression-buffer)))
    (ecc-regression-teardown)))

(ert-deftest ecc-test-regression-global-fallback ()
  "Test that buffer-local system falls back to global settings properly."
  (ecc-regression-setup)
  (unwind-protect
      (progn
        ;; Set global settings
        (setq ecc-auto-response-y/n "global-y/n")
        (setq ecc-auto-response-y/y/n "global-y/y/n")
        (setq ecc-auto-response-waiting "global-waiting")
        (setq ecc-auto-response-initial-waiting "global-initial")
        
        ;; Create a new buffer without explicit local settings
        (let ((fallback-buffer (generate-new-buffer "*ecc-regression-fallback*")))
          (unwind-protect
              (progn
                (ecc-buffer-register fallback-buffer)
                
                ;; Test fallback to global settings when locals aren't set
                (with-current-buffer fallback-buffer
                  ;; Initialize buffer but don't set locals explicitly
                  (makunbound 'ecc-buffer-auto-response-y/n)
                  
                  ;; Mock the send function and process a state
                  (let ((received-response nil))
                    (cl-letf (((symbol-function 'ecc-buffer-send-response)
                               (lambda (response type)
                                 (setq received-response response))))
                      
                      ;; Process state
                      (ecc-auto-response-buffer-local-process-state :y/n)
                      
                      ;; Verify global setting was used
                      (should (string= received-response "global-y/n"))))))
            (kill-buffer fallback-buffer))))
    (ecc-regression-teardown)))

(ert-deftest ecc-test-regression-throttling-behavior ()
  "Test that buffer-local throttling behaves like original throttling."
  (ecc-regression-setup)
  (unwind-protect
      (progn
        ;; Configure buffer
        (with-current-buffer ecc-regression-buffer
          (setq-local ecc-buffer-auto-response-y/n "test-y/n")
          (setq-local ecc-buffer-auto-response-enabled t))
        
        ;; Test initial non-throttled response
        (let ((response-sent nil))
          (with-current-buffer ecc-regression-buffer
            ;; Mock send function
            (cl-letf (((symbol-function 'ecc-buffer-send-response)
                       (lambda (response type)
                         (setq response-sent response))))
              
              ;; First response should go through
              (should-not (ecc-buffer-local-throttled-p :y/n))
              (ecc-auto-response-buffer-local-process-state :y/n)
              (should (string= response-sent "test-y/n"))
              
              ;; Immediate second attempt should be throttled
              (setq response-sent nil)
              (should (ecc-buffer-local-throttled-p :y/n))
              (ecc-auto-response-buffer-local-process-state :y/n)
              (should-not response-sent)))))
    (ecc-regression-teardown)))

(provide 'test-buffer-local-regression)

;;; test-buffer-local-regression.el ends here