;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 11:45:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-term/test-ecc-term-claude-integration.el

;;; Commentary:
;;; Integration tests for the term-claude mode modules.
;;; These tests verify that the different modules work correctly together.

(require 'ert)
(require 'ecc-term-claude-state)
(require 'ecc-term-claude-auto)
(require 'ecc-term-claude-setup)
(require 'ecc-term-claude-buffer)
(require 'ecc-term-claude-interaction)
(require 'ecc-term-claude-mode-v2)

;;; Code:

;; Mock functions
(defvar ecc-test-last-string-sent nil
  "Last string sent to vterm in tests.")

(defvar ecc-test-return-pressed nil
  "Whether return was pressed in tests.")

(defvar ecc-test-clear-called nil
  "Whether vterm-clear was called in tests.")

;; Test environment setup
(defun ecc-test-setup-mock-functions ()
  "Set up mock functions for testing."
  (setq ecc-test-last-string-sent nil
        ecc-test-return-pressed nil
        ecc-test-clear-called nil)
  
  (fset 'vterm-send-string 
        (lambda (str) (setq ecc-test-last-string-sent str)))
  
  (fset 'vterm-send-return
        (lambda () (setq ecc-test-return-pressed t)))
  
  (fset 'vterm-clear
        (lambda () (setq ecc-test-clear-called t))))

(defun ecc-test-teardown-mock-functions ()
  "Restore original functions after testing."
  (when (fboundp 'vterm-send-string-original)
    (fset 'vterm-send-string (symbol-function 'vterm-send-string-original))
    (fmakunbound 'vterm-send-string-original))
  
  (when (fboundp 'vterm-send-return-original)
    (fset 'vterm-send-return (symbol-function 'vterm-send-return-original))
    (fmakunbound 'vterm-send-return-original))
  
  (when (fboundp 'vterm-clear-original)
    (fset 'vterm-clear (symbol-function 'vterm-clear-original))
    (fmakunbound 'vterm-clear-original)))

(defmacro with-mock-environment (&rest body)
  "Execute BODY with mock environment."
  (declare (indent 0))
  `(progn
     (when (fboundp 'vterm-send-string)
       (fset 'vterm-send-string-original (symbol-function 'vterm-send-string)))
     (when (fboundp 'vterm-send-return)
       (fset 'vterm-send-return-original (symbol-function 'vterm-send-return)))
     (when (fboundp 'vterm-clear)
       (fset 'vterm-clear-original (symbol-function 'vterm-clear)))
     
     (ecc-test-setup-mock-functions)
     (unwind-protect
         (progn ,@body)
       (ecc-test-teardown-mock-functions))))

(defmacro with-test-buffer (&rest body)
  "Execute BODY with a test buffer."
  (declare (indent 0))
  `(let ((ecc-buffer-registered-buffers-alist nil)
         (ecc-buffer-current-buffer nil)
         (ecc-term-claude-auto-mode nil)
         (ecc-term-claude-update-functions nil)
         (ecc-vterm-always-follow-bottom nil)
         (ecc-auto-response-y/n "y")
         (ecc-auto-response-y/y/n "y")
         (ecc-auto-response-waiting "")
         (ecc-auto-response-initial-waiting "")
         (test-buffer (generate-new-buffer "*claude-test*")))
     (with-mock-environment
       (unwind-protect
           (with-current-buffer test-buffer
             (setq major-mode 'vterm-mode)  ;; Fake vterm-mode for testing
             ,@body)
         (when (buffer-live-p test-buffer)
           (kill-buffer test-buffer))))))

;; Integration tests

(ert-deftest test-integration-detect-state-and-auto-respond ()
  "Test state detection and auto-response integration."
  (with-test-buffer
    ;; Insert test content
    (insert "Some content with [y/n] prompt")
    
    ;; Detect state
    (let ((state (ecc-term-claude-get-state)))
      (should (eq state :y/n))
      
      ;; Send auto-response based on state
      (ecc-term-claude-auto-send state)
      (should (string= ecc-test-last-string-sent "y"))
      (should ecc-test-return-pressed))))

(ert-deftest test-integration-setup-and-buffer-management ()
  "Test setup and buffer management integration."
  (with-test-buffer
    ;; Register buffer
    (ecc-term-claude-register-buffer)
    (should (ecc-term-claude-buffer-p (current-buffer)))
    
    ;; Apply setup to buffer
    (ecc-term-claude-setup-common (current-buffer))
    
    ;; Verify it's registered as current buffer
    (should (eq (ecc-term-claude-get-current-buffer) (current-buffer)))
    
    ;; Should be in list of buffers
    (should (memq (current-buffer) (ecc-term-claude-list-buffers)))))

(ert-deftest test-integration-auto-mode-send-accept ()
  "Test auto-mode and auto-response integration."
  (with-test-buffer
    ;; Insert test content
    (insert "Some content with [y/n] prompt")
    
    ;; Enable auto-mode
    (ecc-term-claude-toggle-auto-mode)
    (should ecc-term-claude-auto-mode)
    (should (memq 'ecc-term-claude-auto-send-accept ecc-term-claude-update-functions))
    
    ;; Verify auto-response works
    (ecc-term-claude-auto-send-accept)
    (should (string= ecc-test-last-string-sent "y"))
    (should ecc-test-return-pressed)))

(ert-deftest test-integration-state-buffer-interaction ()
  "Test state detection and interaction integration."
  (with-test-buffer
    ;; Insert test content for different states
    (erase-buffer)
    (insert "Some content with [y/n] prompt")
    (should (eq (ecc-term-claude-get-state) :y/n))
    
    ;; Send 'yes' response for this state
    (ecc-term-claude-send-yes)
    (should (string= ecc-test-last-string-sent "y"))
    (should ecc-test-return-pressed)
    
    ;; Change content to test another state
    (erase-buffer)
    (insert "Some content with [Y/y/n] prompt")
    (should (eq (ecc-term-claude-get-state) :y/y/n))
    
    ;; Send 'no' response for this state
    (setq ecc-test-last-string-sent nil)
    (setq ecc-test-return-pressed nil)
    (ecc-term-claude-send-no)
    (should (string= ecc-test-last-string-sent "n"))
    (should ecc-test-return-pressed)))

(ert-deftest test-integration-mode-line-indicator ()
  "Test mode line state indicator integration."
  (with-test-buffer
    ;; Test different states and their indicators
    (erase-buffer)
    (insert "Some content with [y/n] prompt")
    (should (string= (ecc-term-claude-mode-line-state-indicator) " [Y/N]"))
    
    (erase-buffer)
    (insert "Some content with [Y/y/n] prompt")
    (should (string= (ecc-term-claude-mode-line-state-indicator) " [Y/Y/N]"))
    
    (erase-buffer)
    (insert "Some content with continue> prompt")
    (should (string= (ecc-term-claude-mode-line-state-indicator) " [Waiting]"))
    
    (erase-buffer)
    (insert "No prompt here")
    (should (string= (ecc-term-claude-mode-line-state-indicator) ""))))

(ert-deftest test-integration-cleanup ()
  "Test cleanup integration."
  (with-test-buffer
    ;; Set up buffer
    (ecc-term-claude-register-buffer)
    (should (ecc-term-claude-buffer-p (current-buffer)))
    
    ;; Set timer
    (setq ecc-term-claude-state-timer (run-with-idle-timer 100 nil #'ignore))
    (should ecc-term-claude-state-timer)
    
    ;; Cleanup
    (ecc-term-claude-cleanup-buffer)
    
    ;; Verify cleanup
    (should-not ecc-term-claude-state-timer)
    (should-not (ecc-term-claude-buffer-p (current-buffer)))))

(provide 'test-ecc-term-claude-integration)

;;; test-ecc-term-claude-integration.el ends here