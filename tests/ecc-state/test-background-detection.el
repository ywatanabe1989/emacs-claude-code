;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 22:00:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-state/test-background-detection.el

;;; Commentary:
;;; Tests for the background detection system.

(require 'ert)
(require 'ecc-variables)
(require 'ecc-state-detection)
(require 'ecc-buffer-state)
(require 'ecc-background-detection)

;; Test fixtures
(defvar ecc-background-test-buffer-a nil "First test buffer.")
(defvar ecc-background-test-buffer-b nil "Second test buffer.")
(defvar ecc-background-test-callback-called nil "Whether callback was called.")
(defvar ecc-background-test-callback-buffer nil "Buffer passed to callback.")
(defvar ecc-background-test-callback-state nil "State detected in callback.")

(defun ecc-background-test-setup ()
  "Set up test buffers for background detection tests."
  ;; Reset callback variables
  (setq ecc-background-test-callback-called nil)
  (setq ecc-background-test-callback-buffer nil)
  (setq ecc-background-test-callback-state nil)
  
  ;; Create test buffers
  (setq ecc-background-test-buffer-a (generate-new-buffer "*ecc-bg-test-a*"))
  (setq ecc-background-test-buffer-b (generate-new-buffer "*ecc-bg-test-b*"))
  
  ;; Initialize buffer state
  (with-current-buffer ecc-background-test-buffer-a
    (ecc-buffer-state-init))
  (with-current-buffer ecc-background-test-buffer-b
    (ecc-buffer-state-init))
  
  ;; Fill with different content
  (with-current-buffer ecc-background-test-buffer-a
    (erase-buffer)
    (insert "Some content here\n")
    (insert "│ > Try \n"))  ;; initial-waiting pattern
  
  (with-current-buffer ecc-background-test-buffer-b
    (erase-buffer)
    (insert "Other content\n")
    (insert "[Y/n]\n")))  ;; y/n pattern

(defun ecc-background-test-teardown ()
  "Clean up test environment."
  ;; Stop background detection if running
  (when (and (boundp 'ecc-background-detection-active)
             ecc-background-detection-active)
    (ecc-background-detection-stop))
  
  ;; Kill test buffers
  (when (buffer-live-p ecc-background-test-buffer-a)
    (kill-buffer ecc-background-test-buffer-a))
  (when (buffer-live-p ecc-background-test-buffer-b)
    (kill-buffer ecc-background-test-buffer-b))
  
  ;; Reset variables
  (setq ecc-background-test-buffer-a nil)
  (setq ecc-background-test-buffer-b nil)
  (setq ecc-background-test-callback-called nil)
  (setq ecc-background-test-callback-buffer nil)
  (setq ecc-background-test-callback-state nil))

(defun ecc-background-test-callback (buffer state)
  "Test callback for background detection.
Stores BUFFER and STATE for inspection."
  (setq ecc-background-test-callback-called t)
  (setq ecc-background-test-callback-buffer buffer)
  (setq ecc-background-test-callback-state state))

(ert-deftest ecc-test-background-detection-buffer-registration ()
  "Test buffer registration and management."
  (ecc-background-test-setup)
  (unwind-protect
      (progn
        ;; Ensure clean state
        (setq ecc-background-detection-registered-buffers nil)
        
        ;; Test registration
        (should (eq (ecc-background-detection-register-buffer
                     ecc-background-test-buffer-a)
                    ecc-background-test-buffer-a))
        (should (= (length ecc-background-detection-registered-buffers) 1))
        
        ;; Test multiple registrations
        (ecc-background-detection-register-buffer ecc-background-test-buffer-b)
        (should (= (length ecc-background-detection-registered-buffers) 2))
        (should (member ecc-background-test-buffer-a 
                       ecc-background-detection-registered-buffers))
        (should (member ecc-background-test-buffer-b
                       ecc-background-detection-registered-buffers))
        
        ;; Test unregistration
        (ecc-background-detection-unregister-buffer ecc-background-test-buffer-a)
        (should (= (length ecc-background-detection-registered-buffers) 1))
        (should-not (member ecc-background-test-buffer-a
                           ecc-background-detection-registered-buffers))
        
        ;; Test getting registered buffers
        (let ((buffers (ecc-background-detection-registered-buffers)))
          (should (= (length buffers) 1))
          (should (member ecc-background-test-buffer-b buffers))))
    (ecc-background-test-teardown)))

(ert-deftest ecc-test-background-detection-check-buffer ()
  "Test background detection in a single buffer."
  (ecc-background-test-setup)
  (unwind-protect
      (progn
        ;; Set up callback
        (setq ecc-background-detection-callback #'ecc-background-test-callback)
        
        ;; Test detection without cursor
        (with-current-buffer ecc-background-test-buffer-a
          ;; Move cursor to beginning (away from prompt)
          (goto-char (point-min))
          
          ;; Should still detect state
          (should (eq (ecc-background-detect-state-in-buffer
                       ecc-background-test-buffer-a)
                      :initial-waiting)))
        
        ;; Test callback
        (ecc-background-detection-check-buffer ecc-background-test-buffer-a)
        (should ecc-background-test-callback-called)
        (should (eq ecc-background-test-callback-buffer ecc-background-test-buffer-a))
        (should (eq ecc-background-test-callback-state :initial-waiting))
        
        ;; Test state tracking was updated
        (with-current-buffer ecc-background-test-buffer-a
          (should (eq (ecc-buffer-state-get-prompt) :initial-waiting))))
    (ecc-background-test-teardown)))

(ert-deftest ecc-test-background-detection-process-buffers ()
  "Test processing multiple buffers in background."
  (ecc-background-test-setup)
  (unwind-protect
      (progn
        ;; Set flags for testing
        (setq ecc-background-detection-active t)
        (setq ecc-background-detection-processing nil)
        (setq ecc-background-detection-callback #'ecc-background-test-callback)
        (setq ecc-background-detection-registered-buffers nil)
        
        ;; Register both buffers
        (ecc-background-detection-register-buffer ecc-background-test-buffer-a)
        (ecc-background-detection-register-buffer ecc-background-test-buffer-b)
        
        ;; Process buffers
        (ecc-background-detection-process-buffers)
        
        ;; Check that the callback was called for at least one buffer
        (should ecc-background-test-callback-called)
        (should (or (eq ecc-background-test-callback-buffer ecc-background-test-buffer-a)
                   (eq ecc-background-test-callback-buffer ecc-background-test-buffer-b)))
        (should (or (eq ecc-background-test-callback-state :initial-waiting)
                   (eq ecc-background-test-callback-state :y/n)))
        
        ;; Check that both buffers have stored states
        (with-current-buffer ecc-background-test-buffer-a
          (should (eq (ecc-buffer-state-get-prompt) :initial-waiting)))
        (with-current-buffer ecc-background-test-buffer-b
          (should (eq (ecc-buffer-state-get-prompt) :y/n))))
    (ecc-background-test-teardown)))

(ert-deftest ecc-test-background-detection-timer-management ()
  "Test background detection timer management."
  (ecc-background-test-setup)
  (unwind-protect
      (progn
        ;; Ensure timers are cleared
        (setq ecc-background-detection-timer nil)
        (setq ecc-background-detection-idle-timer nil)
        
        ;; Start timer
        (ecc-background-detection-timer-start)
        (should (timerp ecc-background-detection-timer))
        (should (timerp ecc-background-detection-idle-timer))
        
        ;; Stop timer
        (ecc-background-detection-timer-stop)
        (should-not ecc-background-detection-timer)
        (should-not ecc-background-detection-idle-timer))
    (ecc-background-test-teardown)))

(ert-deftest ecc-test-background-detection-api ()
  "Test public API for background detection."
  (ecc-background-test-setup)
  (unwind-protect
      (progn
        ;; Test start function
        (ecc-background-detection-start #'ecc-background-test-callback)
        (should ecc-background-detection-active)
        (should (eq ecc-background-detection-callback #'ecc-background-test-callback))
        (should (timerp ecc-background-detection-timer))
        
        ;; Test stop function
        (ecc-background-detection-stop)
        (should-not ecc-background-detection-active)
        (should-not ecc-background-detection-callback)
        (should-not ecc-background-detection-timer)
        
        ;; Test toggle function
        (ecc-background-detection-toggle #'ecc-background-test-callback)
        (should ecc-background-detection-active)
        (ecc-background-detection-toggle)
        (should-not ecc-background-detection-active)
        
        ;; Test buffer API
        (ecc-background-detection-add-buffer ecc-background-test-buffer-a)
        (should (member ecc-background-test-buffer-a
                       ecc-background-detection-registered-buffers))
        (ecc-background-detection-remove-buffer ecc-background-test-buffer-a)
        (should-not (member ecc-background-test-buffer-a
                           ecc-background-detection-registered-buffers)))
    (ecc-background-test-teardown)))

(ert-deftest ecc-test-background-detection-integration ()
  "Test integration between background detection and buffer state."
  (ecc-background-test-setup)
  (unwind-protect
      (progn
        ;; Initialize with registration
        (with-current-buffer ecc-background-test-buffer-a
          (ecc-buffer-state-init))
        (with-current-buffer ecc-background-test-buffer-b
          (ecc-buffer-state-init))
        
        ;; Register buffers
        (ecc-background-detection-register-buffer ecc-background-test-buffer-a)
        (ecc-background-detection-register-buffer ecc-background-test-buffer-b)
        
        ;; Test detection updates state correctly
        (ecc-background-detect-state-in-buffer ecc-background-test-buffer-a)
        (with-current-buffer ecc-background-test-buffer-a
          (should (eq (ecc-buffer-state-get-prompt) :initial-waiting)))
        
        ;; Change buffer content and test detection again
        (with-current-buffer ecc-background-test-buffer-a
          (erase-buffer)
          (insert "New content\n")
          (insert "[Y/y/n]\n")  ;; y/y/n pattern
          ;; Move cursor away from prompt
          (goto-char (point-min)))
        
        ;; Should still detect new state
        (ecc-background-detect-state-in-buffer ecc-background-test-buffer-a)
        (with-current-buffer ecc-background-test-buffer-a
          (should (eq (ecc-buffer-state-get-prompt) :y/y/n))))
    (ecc-background-test-teardown)))

(provide 'test-background-detection)

;;; test-background-detection.el ends here