;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 20:00:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-system/test-buffer-local-system.el

;;; Commentary:
;;; System-level tests for the buffer-local configuration functionality.
;;; These tests verify that the complete system works correctly from end to end.

(require 'ert)
(require 'ecc-variables)
(require 'ecc-api)
(require 'ecc-buffer-local)
(require 'ecc-api)
(require 'ecc-buffer-api)
(require 'ecc-api)
(require 'ecc-auto-response)
(require 'ecc-buffer-local)
(require 'ecc-api)
(require 'ecc-state-detection)
(require 'ecc-api)

;; Test fixtures
(defvar ecc-system-test-buffer-a nil "First test buffer.")
(defvar ecc-system-test-buffer-b nil "Second test buffer.")

(defun ecc-system-test-setup ()
  "Set up system test environment."
  ;; Create test buffers
  (setq ecc-system-test-buffer-a (generate-new-buffer "*ecc-system-a*"))
  (setq ecc-system-test-buffer-b (generate-new-buffer "*ecc-system-b*"))
  
  ;; Initialize vterm mode simulation for buffers
  (with-current-buffer ecc-system-test-buffer-a
    (set (make-local-variable 'major-mode) 'vterm-mode))
  
  (with-current-buffer ecc-system-test-buffer-b
    (set (make-local-variable 'major-mode) 'vterm-mode))
  
  ;; Register buffers with the system
  (ecc-buffer-register ecc-system-test-buffer-a)
  (ecc-buffer-register ecc-system-test-buffer-b)
  
  ;; Fill with different content
  (with-current-buffer ecc-system-test-buffer-a
    (erase-buffer)
    (insert "Some content here\n")
    (insert "│ > Try \n"))  ;; initial-waiting pattern
  
  (with-current-buffer ecc-system-test-buffer-b
    (erase-buffer)
    (insert "Other content\n")
    (insert "[Y/n]\n")))  ;; y/n pattern

(defun ecc-system-test-teardown ()
  "Clean up system test environment."
  (when (buffer-live-p ecc-system-test-buffer-a)
    (kill-buffer ecc-system-test-buffer-a))
  (when (buffer-live-p ecc-system-test-buffer-b)
    (kill-buffer ecc-system-test-buffer-b))
  (setq ecc-system-test-buffer-a nil)
  (setq ecc-system-test-buffer-b nil))

;; Test response tracking
(defvar ecc-system-test-responses-sent nil
  "List of responses sent during testing.")

;; Mock vterm functions for testing
(defun ecc-system-test-mock-vterm-functions ()
  "Mock vterm functions for testing."
  ;; Save original functions if they exist
  (when (fboundp 'vterm-send-string)
    (fset 'ecc-system-test-original-vterm-send-string 
          (symbol-function 'vterm-send-string)))
  (when (fboundp 'vterm-send-return)
    (fset 'ecc-system-test-original-vterm-send-return 
          (symbol-function 'vterm-send-return)))
  
  ;; Mock functions to track responses
  (fset 'vterm-send-string 
        (lambda (str) 
          (push str ecc-system-test-responses-sent)
          str))
  (fset 'vterm-send-return 
        (lambda () 
          (push "<RETURN>" ecc-system-test-responses-sent)
          t))
  
  ;; Reset response tracking
  (setq ecc-system-test-responses-sent nil))

(defun ecc-system-test-restore-vterm-functions ()
  "Restore original vterm functions after testing."
  ;; Restore original functions if they were saved
  (if (fboundp 'ecc-system-test-original-vterm-send-string)
      (progn
        (fset 'vterm-send-string 
              (symbol-function 'ecc-system-test-original-vterm-send-string))
        (fmakunbound 'ecc-system-test-original-vterm-send-string))
    (fmakunbound 'vterm-send-string))
  
  (if (fboundp 'ecc-system-test-original-vterm-send-return)
      (progn
        (fset 'vterm-send-return 
              (symbol-function 'ecc-system-test-original-vterm-send-return))
        (fmakunbound 'ecc-system-test-original-vterm-send-return))
    (fmakunbound 'vterm-send-return))
  
  ;; Clear response tracking
  (setq ecc-system-test-responses-sent nil))

(ert-deftest ecc-test-system-full-buffer-independence ()
  "Test full system independence between buffers."
  (ecc-system-test-setup)
  (ecc-system-test-mock-vterm-functions)
  (unwind-protect
      (progn
        ;; Configure auto-response
        (with-current-buffer ecc-system-test-buffer-a
          (setq-local ecc-buffer-auto-response-initial-waiting "/custom-a")
          (setq-local ecc-buffer-auto-response-y/n "a-yes")
          (setq-local ecc-buffer-auto-response-enabled t)
          
          ;; Detect and respond
          (should (eq (ecc-buffer-state-detect) :initial-waiting))
          (ecc-auto-response--process-buffer-global (current-buffer)))
        
        (with-current-buffer ecc-system-test-buffer-b
          (setq-local ecc-buffer-auto-response-initial-waiting "/custom-b")
          (setq-local ecc-buffer-auto-response-y/n "b-yes")
          (setq-local ecc-buffer-auto-response-enabled t)
          
          ;; Detect and respond
          (should (eq (ecc-buffer-state-detect) :y/n))
          (ecc-auto-response--process-buffer-global (current-buffer)))
        
        ;; Verify each buffer received its own distinct response
        ;; Responses are captured in ecc-system-test-responses-sent
        (let ((responses (reverse ecc-system-test-responses-sent)))
          ;; Should have 4 entries: "/custom-a", "<RETURN>", "b-yes", "<RETURN>"
          (should (>= (length responses) 4))
          (should (member "/custom-a" responses))
          (should (member "b-yes" responses)))
        
        ;; Verify throttling state is independent
        (with-current-buffer ecc-system-test-buffer-a
          (should (eq ecc-buffer-active-state :initial-waiting))
          (should (> (alist-get :initial-waiting ecc-buffer-last-time-alist 0.0) 0.0)))
        
        (with-current-buffer ecc-system-test-buffer-b
          (should (eq ecc-buffer-active-state :y/n))
          (should (> (alist-get :y/n ecc-buffer-last-time-alist 0.0) 0.0))))
    (ecc-system-test-restore-vterm-functions)
    (ecc-system-test-teardown)))

(ert-deftest ecc-test-system-api-interactions ()
  "Test system-level API interactions with buffer-local configuration."
  (ecc-system-test-setup)
  (unwind-protect
      (progn
        ;; Test buffer registration through API
        (let ((buf-c (generate-new-buffer "*ecc-system-c*")))
          (unwind-protect
              (progn
                (ecc-buffer-register buf-c)
                (should (member buf-c (ecc-buffer-list)))
                
                ;; Test current buffer setting
                (ecc-buffer-set-current buf-c)
                (should (eq (ecc-buffer-current) buf-c))
                
                ;; Test buffer-specific settings
                (ecc-buffer-auto-response-set-y/n "c-yes" buf-c)
                (should (string= (ecc-buffer-settings-get 'ecc-buffer-auto-response-y/n buf-c)
                                "c-yes")))
            (kill-buffer buf-c))))
        
        ;; Test buffer-specific toggle functions
        (with-current-buffer ecc-system-test-buffer-a
          (ecc-buffer-auto-response-enable)
          (should ecc-buffer-auto-response-enabled)
          
          (ecc-buffer-auto-response-toggle)
          (should-not ecc-buffer-auto-response-enabled)
          
          (ecc-buffer-auto-response-toggle)
          (should ecc-buffer-auto-response-enabled))
        
        ;; Test debug toggle
        (with-current-buffer ecc-system-test-buffer-b
          (should-not ecc-buffer-debug-enabled)
          (ecc-buffer-debug-toggle)
          (should ecc-buffer-debug-enabled))
    (ecc-system-test-teardown)))

(ert-deftest ecc-test-system-buffer-state-lifecycle ()
  "Test the full lifecycle of buffer state detection and tracking."
  (ecc-system-test-setup)
  (unwind-protect
      (progn
        ;; Initial state detection
        (with-current-buffer ecc-system-test-buffer-a
          (should (eq (ecc-buffer-state-detect) :initial-waiting))
          (should (eq ecc-buffer-state :initial-waiting))
          (should (> ecc-buffer-last-state-time 0.0)))
        
        ;; Change buffer content and detect new state
        (with-current-buffer ecc-system-test-buffer-a
          (erase-buffer)
          (insert "New content\n")
          (insert "[Y/n]\n")  ;; y/n pattern
          
          ;; Check if state updates
          (should (eq (ecc-buffer-state-detect) :y/n))
          (should (eq ecc-buffer-state :y/n)))
        
        ;; Test predicates
        (with-current-buffer ecc-system-test-buffer-a
          (should (ecc-buffer-state-y/n-p))
          (should-not (ecc-buffer-state-waiting-p)))
        
        ;; Test parallel state independence
        (with-current-buffer ecc-system-test-buffer-b
          (should (ecc-buffer-state-y/n-p))
          (should-not (ecc-buffer-state-initial-waiting-p)))
        
        ;; Change second buffer's content too
        (with-current-buffer ecc-system-test-buffer-b
          (erase-buffer)
          (insert "Continue content\n")
          (insert "continue>\n")  ;; waiting pattern
          
          ;; Check if state updates independently
          (should (eq (ecc-buffer-state-detect) :waiting))
          (should (eq ecc-buffer-state :waiting))))
    (ecc-system-test-teardown)))

(provide 'test-buffer-local-system)

;;; test-buffer-local-system.el ends here