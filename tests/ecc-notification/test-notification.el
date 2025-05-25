;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 22:45:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-notification/test-notification.el

;;; Commentary:
;;; Tests for ecc-notification.el

(require 'ert)

;;; Code:

;; Add the src directory to the load path
(add-to-list 'load-path 
             (expand-file-name "../../src"
                              (file-name-directory
                               (or load-file-name buffer-file-name))))

;; Require the notification module
(require 'ecc-notification)

;; Test state description function
(ert-deftest test-notification-state-description ()
  "Test the state description function."
  (should (stringp (ecc-notification--state-description :y/n)))
  (should (string= (ecc-notification--state-description :y/n) "yes/no prompt"))
  (should (string= (ecc-notification--state-description :waiting) "waiting for input"))
  (should (string= (ecc-notification--state-description :unknown-state) "unknown-state")))

;; Test throttling logic
(ert-deftest test-notification-throttling ()
  "Test notification throttling logic."
  ;; Ensure notifications are enabled for testing
  (let ((ecc-notification-enabled t)
        (ecc-notification-states '(:y/n :waiting))
        (ecc-notification-throttle-interval 1.0)
        (ecc-notification--last-time 0)
        (ecc-notification--last-state nil))
    
    ;; Should notify for new state
    (should (ecc-notification--should-notify-p :y/n))
    
    ;; Update tracking variables
    (ecc-notification--update-state :y/n)
    
    ;; Should not notify for same state too soon
    (should-not (ecc-notification--should-notify-p :y/n))
    
    ;; Should notify for different state
    (should (ecc-notification--should-notify-p :waiting))
    
    ;; Test throttling after time passes
    (setq ecc-notification--last-time (- (float-time) 2.0))
    (should (ecc-notification--should-notify-p :y/n))))

;; Test notification dispatch
(ert-deftest test-notification-dispatch ()
  "Test notification dispatch function."
  ;; Mock notification methods for testing
  (let ((bell-called nil)
        (flash-called nil)
        (message-called nil)
        (ecc-notification-enabled t)
        (ecc-notification-states '(:y/n))
        (ecc-notification-methods '(bell flash message))
        (ecc-notification--last-time 0)
        (ecc-notification--last-state nil))
    
    ;; Override notification methods with test versions
    (cl-letf (((symbol-function 'ecc-notification-ring-bell)
               (lambda () (setq bell-called t)))
              ((symbol-function 'ecc-notification-flash-mode-line)
               (lambda () (setq flash-called t)))
              ((symbol-function 'ecc-notification-display-message)
               (lambda (state &optional buffer) (setq message-called t))))
      
      ;; Test notification dispatch
      (should (ecc-notification-dispatch :y/n))
      
      ;; All methods should be called
      (should bell-called)
      (should flash-called)
      (should message-called)
      
      ;; State should be updated
      (should (eq ecc-notification--last-state :y/n)))))

;; Test notification method toggling
(ert-deftest test-notification-toggle-methods ()
  "Test toggling notification methods."
  ;; Start with all methods
  (let ((ecc-notification-methods '(bell flash message)))
    
    ;; Test bell toggle
    (ecc-notification-toggle-bell)
    (should-not (memq 'bell ecc-notification-methods))
    (should (memq 'flash ecc-notification-methods))
    
    ;; Toggle back on
    (ecc-notification-toggle-bell)
    (should (memq 'bell ecc-notification-methods))
    
    ;; Test flash toggle
    (ecc-notification-toggle-flash)
    (should-not (memq 'flash ecc-notification-methods))))

;; Test backward compatibility aliases
(ert-deftest test-notification-backward-compatibility ()
  "Test backward compatibility aliases."
  ;; Test that aliases are defined
  (should (fboundp 'ecc-notification-toggle))
  (should (fboundp 'ecc-notification-toggle-bell))
  (should (fboundp 'ecc-notification-check-state))
  (should (fboundp 'ecc-notification-dispatch))
  
  ;; Test that aliases point to the right functions
  (should (functionp (symbol-function 'ecc-notification-toggle)))
  (should (functionp (symbol-function 'ecc-notification-toggle-bell)))
  
  ;; Call aliases and check they run without errors
  (let ((ecc-notification-enabled t))
    (ecc-notification-toggle)
    (should-not ecc-notification-enabled)
    (ecc-notification-toggle)
    (should ecc-notification-enabled)))

;; Test buffer name in notifications
(ert-deftest test-notification-buffer-name-display ()
  "Test that notifications include buffer name when buffer is provided."
  (let ((ecc-notification-enabled t)
        (ecc-notification-methods '(message))
        (ecc-notification-states '(:y/n))
        (ecc-notification--last-time 0)
        (ecc-notification--last-state nil)
        (test-buffer-name "*test-notification-buffer*")
        (captured-message nil))
    
    ;; Create a test buffer
    (with-temp-buffer
      (rename-buffer test-buffer-name)
      
      ;; Mock message function to capture output
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq captured-message (apply #'format format-string args)))))
        
        ;; Test with buffer parameter
        (ecc-notification-dispatch :y/n (current-buffer))
        (should captured-message)
        (should (string-match-p (regexp-quote test-buffer-name) captured-message))
        (should (string-match-p "\\[.*\\]" captured-message))
        
        ;; Reset for next test
        (setq captured-message nil)
        (setq ecc-notification--last-state nil)
        (setq ecc-notification--last-time 0)
        
        ;; Test without buffer parameter
        (ecc-notification-dispatch :y/n)
        (should captured-message)
        (should-not (string-match-p "\\[.*\\]" captured-message))))))

(provide 'test-notification)

;;; test-notification.el ends here