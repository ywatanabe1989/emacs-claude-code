;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 16:05:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-auto-notify.el

;;; Commentary:
;;; Tests for the consolidated auto-notify module.

;;; Code:
(require 'ert)
(require 'ecc-variables-consolidated)
(require 'ecc-auto-notify)

;; Mocks
(defvar test-ecc-auto-notify-message-log nil
  "Log of messages for testing.")

(defvar test-ecc-auto-notify-bell-called nil
  "Whether the bell function was called.")

(defvar test-ecc-auto-notify-flash-called nil
  "Whether the flash function was called.")

(defun test-ecc-auto-notify-setup ()
  "Set up test environment."
  (setq test-ecc-auto-notify-message-log nil)
  (setq test-ecc-auto-notify-bell-called nil)
  (setq test-ecc-auto-notify-flash-called nil)
  
  ;; Mock functions
  (advice-add 'message :around
              (lambda (orig-fun &rest args)
                (when (and args (stringp (car args)))
                  (push (apply #'format args) test-ecc-auto-notify-message-log))
                (apply orig-fun args)))
  
  (advice-add 'ecc-auto-notify-ring-bell :override
              (lambda () (setq test-ecc-auto-notify-bell-called t)))
  
  (advice-add 'ecc-auto-notify-flash-mode-line :override
              (lambda () (setq test-ecc-auto-notify-flash-called t)))
  
  ;; Set global notification settings
  (setq ecc-auto-notify-on-claude-prompt t)
  (setq ecc-auto-notify-bell t)
  (setq ecc-auto-notify-flash t)
  (setq ecc-auto-notify-prompt-types '(:initial-waiting :waiting :y/n :y/y/n))
  (setq ecc-auto-notify-interval 0.1))

(defun test-ecc-auto-notify-teardown ()
  "Clean up test environment."
  (advice-remove 'message (lambda (orig-fun &rest args)
                            (when (and args (stringp (car args)))
                              (push (apply #'format args) test-ecc-auto-notify-message-log))
                            (apply orig-fun args)))
  
  (advice-remove 'ecc-auto-notify-ring-bell
                 (lambda () (setq test-ecc-auto-notify-bell-called t)))
  
  (advice-remove 'ecc-auto-notify-flash-mode-line
                 (lambda () (setq test-ecc-auto-notify-flash-called t))))

;; Global Mode Tests

(ert-deftest test-ecc-auto-notify-check-state ()
  "Test basic notification state checking."
  (test-ecc-auto-notify-setup)
  (unwind-protect
      (progn
        ;; Test with valid state
        (should (ecc-auto-notify-check-state :y/n))
        (should test-ecc-auto-notify-bell-called)
        (should test-ecc-auto-notify-flash-called)
        (should (string-match-p "Claude prompt detected: yes/no prompt" 
                                (car test-ecc-auto-notify-message-log)))
        
        ;; Reset mocks
        (setq test-ecc-auto-notify-message-log nil)
        (setq test-ecc-auto-notify-bell-called nil)
        (setq test-ecc-auto-notify-flash-called nil)
        
        ;; Test with invalid state
        (should-not (ecc-auto-notify-check-state :invalid-state))
        (should-not test-ecc-auto-notify-bell-called)
        (should-not test-ecc-auto-notify-flash-called)
        (should-not test-ecc-auto-notify-message-log)
        
        ;; Test with notifications disabled
        (setq ecc-auto-notify-on-claude-prompt nil)
        (should-not (ecc-auto-notify-check-state :y/n))
        (should-not test-ecc-auto-notify-bell-called)
        (should-not test-ecc-auto-notify-flash-called))
    (test-ecc-auto-notify-teardown)))

(ert-deftest test-ecc-auto-notify-throttling ()
  "Test notification throttling."
  (test-ecc-auto-notify-setup)
  (unwind-protect
      (progn
        ;; First notification should go through
        (should (ecc-auto-notify-check-state :y/n))
        (should test-ecc-auto-notify-bell-called)
        (should test-ecc-auto-notify-flash-called)
        
        ;; Reset mocks
        (setq test-ecc-auto-notify-message-log nil)
        (setq test-ecc-auto-notify-bell-called nil)
        (setq test-ecc-auto-notify-flash-called nil)
        
        ;; Immediate repeat should be throttled
        (should-not (ecc-auto-notify-check-state :y/n))
        (should-not test-ecc-auto-notify-bell-called)
        (should-not test-ecc-auto-notify-flash-called)
        
        ;; Different state should go through
        (should (ecc-auto-notify-check-state :y/y/n))
        (should test-ecc-auto-notify-bell-called)
        (should test-ecc-auto-notify-flash-called))
    (test-ecc-auto-notify-teardown)))

(ert-deftest test-ecc-auto-notify-toggle ()
  "Test toggling notification settings."
  (test-ecc-auto-notify-setup)
  (unwind-protect
      (progn
        ;; Start with notifications enabled
        (setq ecc-auto-notify-on-claude-prompt t)
        
        ;; Toggle notifications off
        (ecc-auto-notify-toggle)
        (should-not ecc-auto-notify-on-claude-prompt)
        (should (string-match-p "notifications disabled" 
                                (car test-ecc-auto-notify-message-log)))
        
        ;; Reset message log
        (setq test-ecc-auto-notify-message-log nil)
        
        ;; Toggle notifications on
        (ecc-auto-notify-toggle)
        (should ecc-auto-notify-on-claude-prompt)
        (should (string-match-p "notifications enabled" 
                                (car test-ecc-auto-notify-message-log))))
    (test-ecc-auto-notify-teardown)))

(ert-deftest test-ecc-auto-notify-bell-methods ()
  "Test different bell methods."
  (test-ecc-auto-notify-setup)
  (unwind-protect
      (progn
        ;; Remove mock for this test
        (advice-remove 'ecc-auto-notify-ring-bell
                       (lambda () (setq test-ecc-auto-notify-bell-called t)))
        
        ;; Test with audible method (should not error)
        (setq ecc-auto-notify-bell-method 'audible)
        (should-not (condition-case err
                        (progn (ecc-auto-notify-ring-bell) nil)
                      (error t)))
        
        ;; Test with visible method (should not error)
        (setq ecc-auto-notify-bell-method 'visible)
        (should-not (condition-case err
                        (progn (ecc-auto-notify-ring-bell) nil)
                      (error t)))
        
        ;; Test with both method (should not error)
        (setq ecc-auto-notify-bell-method 'both)
        (should-not (condition-case err
                        (progn (ecc-auto-notify-ring-bell) nil)
                      (error t)))
        
        ;; Test with external method (with no command set)
        (setq ecc-auto-notify-bell-method 'external)
        (setq ecc-auto-notify-bell-external-command nil)
        (should-not (condition-case err
                        (progn (ecc-auto-notify-ring-bell) nil)
                      (error t)))
        
        ;; Test with invalid method (should default to audible)
        (setq ecc-auto-notify-bell-method 'invalid-method)
        (should-not (condition-case err
                        (progn (ecc-auto-notify-ring-bell) nil)
                      (error t))))
    (test-ecc-auto-notify-teardown)))

;; Buffer-Local Mode Tests

(ert-deftest test-ecc-auto-notify-buffer-local-init ()
  "Test initializing buffer-local notification settings."
  (test-ecc-auto-notify-setup)
  (unwind-protect
      (let ((test-buffer (generate-new-buffer "*test-buffer*")))
        (unwind-protect
            (with-current-buffer test-buffer
              ;; Initialize buffer-local settings
              (setq ecc-auto-notify-default-enabled t)
              (ecc-auto-notify-buffer-local-init)
              
              ;; Check if variables were properly set
              (should (local-variable-p 'ecc-buffer-auto-notify-enabled))
              (should (local-variable-p 'ecc-buffer-auto-notify-bell))
              (should (local-variable-p 'ecc-buffer-auto-notify-flash))
              
              ;; Check if they have the right values
              (should ecc-buffer-auto-notify-enabled)
              (should (eq ecc-buffer-auto-notify-bell ecc-auto-notify-bell))
              (should (eq ecc-buffer-auto-notify-flash ecc-auto-notify-flash)))
          (when (buffer-live-p test-buffer)
            (kill-buffer test-buffer))))
    (test-ecc-auto-notify-teardown)))

(ert-deftest test-ecc-auto-notify-buffer-local-toggle ()
  "Test toggling buffer-local notification settings."
  (test-ecc-auto-notify-setup)
  (unwind-protect
      (let ((test-buffer (generate-new-buffer "*test-buffer*")))
        (unwind-protect
            (with-current-buffer test-buffer
              ;; Initialize buffer-local settings
              (setq ecc-auto-notify-default-enabled t)
              (ecc-auto-notify-buffer-local-init)
              
              ;; Toggle notifications off
              (ecc-auto-notify-buffer-local-toggle)
              (should-not ecc-buffer-auto-notify-enabled)
              (should (string-match-p "Buffer-local notifications disabled" 
                                      (car test-ecc-auto-notify-message-log)))
              
              ;; Reset message log
              (setq test-ecc-auto-notify-message-log nil)
              
              ;; Toggle notifications on
              (ecc-auto-notify-buffer-local-toggle)
              (should ecc-buffer-auto-notify-enabled)
              (should (string-match-p "Buffer-local notifications enabled" 
                                      (car test-ecc-auto-notify-message-log))))
          (when (buffer-live-p test-buffer)
            (kill-buffer test-buffer))))
    (test-ecc-auto-notify-teardown)))

(ert-deftest test-ecc-auto-notify-buffer-local-check-state ()
  "Test buffer-local notification state checking."
  (test-ecc-auto-notify-setup)
  (unwind-protect
      (let ((test-buffer (generate-new-buffer "*test-buffer*")))
        (unwind-protect
            (with-current-buffer test-buffer
              ;; Initialize buffer-local settings
              (setq ecc-auto-notify-default-enabled t)
              (ecc-auto-notify-buffer-local-init)
              
              ;; Test with valid state
              (should (ecc-auto-notify-buffer-local-check-state :y/n))
              (should test-ecc-auto-notify-bell-called)
              (should test-ecc-auto-notify-flash-called)
              (should (string-match-p "Claude prompt in \\*test-buffer\\*: yes/no prompt" 
                                     (car test-ecc-auto-notify-message-log)))
              
              ;; Reset mocks
              (setq test-ecc-auto-notify-message-log nil)
              (setq test-ecc-auto-notify-bell-called nil)
              (setq test-ecc-auto-notify-flash-called nil)
              
              ;; Test with invalid state
              (should-not (ecc-auto-notify-buffer-local-check-state :invalid-state))
              (should-not test-ecc-auto-notify-bell-called)
              (should-not test-ecc-auto-notify-flash-called)
              
              ;; Test with notifications disabled
              (setq-local ecc-buffer-auto-notify-enabled nil)
              (should-not (ecc-auto-notify-buffer-local-check-state :y/n))
              (should-not test-ecc-auto-notify-bell-called)
              (should-not test-ecc-auto-notify-flash-called))
          (when (buffer-live-p test-buffer)
            (kill-buffer test-buffer))))
    (test-ecc-auto-notify-teardown)))

(ert-deftest test-ecc-auto-notify-unified-check ()
  "Test unified checking function that handles both global and buffer-local modes."
  (test-ecc-auto-notify-setup)
  (unwind-protect
      (let ((test-buffer (generate-new-buffer "*test-buffer*")))
        (unwind-protect
            (progn
              ;; Test with global settings
              (setq ecc-auto-notify-buffer-local-default nil)
              (setq ecc-auto-notify-on-claude-prompt t)
              
              (with-current-buffer test-buffer
                ;; We'll mock ecc-detect-state for this test
                (cl-letf (((symbol-function 'ecc-detect-state)
                           (lambda () :y/n)))
                  (ecc-auto-notify-check-unified test-buffer)
                  (should test-ecc-auto-notify-bell-called)
                  (should test-ecc-auto-notify-flash-called)
                  (should (string-match-p "Claude prompt detected: yes/no prompt" 
                                          (car test-ecc-auto-notify-message-log)))))
              
              ;; Reset mocks
              (setq test-ecc-auto-notify-message-log nil)
              (setq test-ecc-auto-notify-bell-called nil)
              (setq test-ecc-auto-notify-flash-called nil)
              
              ;; Test with buffer-local settings
              (setq ecc-auto-notify-buffer-local-default t)
              
              (with-current-buffer test-buffer
                (ecc-auto-notify-buffer-local-init)
                
                ;; We'll mock ecc-detect-state for this test
                (cl-letf (((symbol-function 'ecc-detect-state)
                           (lambda () :y/n)))
                  (ecc-auto-notify-check-unified test-buffer)
                  (should test-ecc-auto-notify-bell-called)
                  (should test-ecc-auto-notify-flash-called)
                  (should (string-match-p "Claude prompt in \\*test-buffer\\*: yes/no prompt" 
                                          (car test-ecc-auto-notify-message-log))))))
          (when (buffer-live-p test-buffer)
            (kill-buffer test-buffer))))
    (test-ecc-auto-notify-teardown)))

(provide 'test-ecc-auto-notify)

;;; test-ecc-auto-notify.el ends here