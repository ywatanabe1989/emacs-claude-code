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
(ert-deftest test-notification-state-description-should-return-string-for-y/n-state ()
  "Test that state description returns a string for y/n state."
  ;; Act
  (let ((result (ecc-notification--state-description :y/n)))
    ;; Assert
    (should (stringp result))))

(ert-deftest test-notification-state-description-should-return-yes-no-prompt-for-y/n-state ()
  "Test that state description returns 'yes/no prompt' for y/n state."
  ;; Act
  (let ((result (ecc-notification--state-description :y/n)))
    ;; Assert
    (should (string= result "yes/no prompt"))))

(ert-deftest test-notification-state-description-should-return-waiting-for-input-for-waiting-state ()
  "Test that state description returns 'waiting for input' for waiting state."
  ;; Act
  (let ((result (ecc-notification--state-description :waiting)))
    ;; Assert
    (should (string= result "waiting for input"))))

(ert-deftest test-notification-state-description-should-return-state-name-for-unknown-state ()
  "Test that state description returns state name for unknown state."
  ;; Act
  (let ((result (ecc-notification--state-description :unknown-state)))
    ;; Assert
    (should (string= result "unknown-state"))))

;; Test throttling logic
(ert-deftest test-notification-should-notify-for-new-state ()
  "Test that notification is allowed for a new state."
  ;; Arrange
  (let ((ecc-notification-enabled t)
        (ecc-notification-states '(:y/n :waiting))
        (ecc-notification-throttle-interval 1.0)
        (ecc-notification--last-time 0)
        (ecc-notification--last-state nil))
    
    ;; Act & Assert
    (should (ecc-notification--should-notify-p :y/n))))

(ert-deftest test-notification-should-not-notify-for-same-state-too-soon ()
  "Test that notification is throttled for repeated same state."
  ;; Arrange
  (let ((ecc-notification-enabled t)
        (ecc-notification-states '(:y/n :waiting))
        (ecc-notification-throttle-interval 1.0)
        (ecc-notification--last-time 0)
        (ecc-notification--last-state nil))
    
    ;; First notification should work
    (ecc-notification--update-state :y/n)
    
    ;; Act & Assert - immediate repeat should be throttled
    (should-not (ecc-notification--should-notify-p :y/n))))

(ert-deftest test-notification-should-notify-for-different-state ()
  "Test that notification is allowed when state changes."
  ;; Arrange
  (let ((ecc-notification-enabled t)
        (ecc-notification-states '(:y/n :waiting))
        (ecc-notification-throttle-interval 1.0)
        (ecc-notification--last-time (float-time))
        (ecc-notification--last-state :y/n))
    
    ;; Act & Assert - different state should notify
    (should (ecc-notification--should-notify-p :waiting))))

(ert-deftest test-notification-should-notify-after-throttle-interval ()
  "Test that notification is allowed after throttle interval passes."
  ;; Arrange
  (let ((ecc-notification-enabled t)
        (ecc-notification-states '(:y/n :waiting))
        (ecc-notification-throttle-interval 1.0)
        (ecc-notification--last-time (- (float-time) 2.0)) ; 2 seconds ago
        (ecc-notification--last-state :y/n))
    
    ;; Act & Assert - enough time has passed
    (should (ecc-notification--should-notify-p :y/n))))

(ert-deftest test-notification-should-update-tracking-state ()
  "Test that notification tracking state is updated."
  ;; Arrange
  (let ((ecc-notification-enabled t)
        (ecc-notification-states '(:y/n :waiting))
        (ecc-notification--last-time 0)
        (ecc-notification--last-state nil))
    
    ;; Act
    (ecc-notification--update-state :y/n)
    
    ;; Assert
    (should (eq ecc-notification--last-state :y/n))
    (should (> ecc-notification--last-time 0))))

;; Test notification dispatch
(ert-deftest test-notification-dispatch-should-return-true-when-notified ()
  "Test that notification dispatch returns true when notification occurs."
  ;; Arrange
  (let ((ecc-notification-enabled t)
        (ecc-notification-states '(:y/n))
        (ecc-notification-methods '(message))
        (ecc-notification--last-time 0)
        (ecc-notification--last-state nil))
    
    (cl-letf (((symbol-function 'ecc-notification-display-message) #'ignore))
      
      ;; Act & Assert
      (should (ecc-notification-dispatch :y/n)))))

(ert-deftest test-notification-should-call-bell-method-when-enabled ()
  "Test that bell method is called when included in methods list."
  ;; Arrange
  (let ((bell-called nil)
        (ecc-notification-enabled t)
        (ecc-notification-states '(:y/n))
        (ecc-notification-methods '(bell))
        (ecc-notification--last-time 0)
        (ecc-notification--last-state nil))
    
    (cl-letf (((symbol-function 'ecc-notification-ring-bell)
               (lambda () (setq bell-called t))))
      
      ;; Act
      (ecc-notification-dispatch :y/n)
      
      ;; Assert
      (should bell-called))))

(ert-deftest test-notification-should-call-flash-method-when-enabled ()
  "Test that flash method is called when included in methods list."
  ;; Arrange
  (let ((flash-called nil)
        (ecc-notification-enabled t)
        (ecc-notification-states '(:y/n))
        (ecc-notification-methods '(flash))
        (ecc-notification--last-time 0)
        (ecc-notification--last-state nil))
    
    (cl-letf (((symbol-function 'ecc-auto-notify-flash-mode-line)
               (lambda (&optional buffer) (setq flash-called t))))
      
      ;; Act
      (ecc-notification-dispatch :y/n)
      
      ;; Assert
      (should flash-called))))

(ert-deftest test-notification-should-call-message-method-when-enabled ()
  "Test that message method is called when included in methods list."
  ;; Arrange
  (let ((message-called nil)
        (ecc-notification-enabled t)
        (ecc-notification-states '(:y/n))
        (ecc-notification-methods '(message))
        (ecc-notification--last-time 0)
        (ecc-notification--last-state nil))
    
    (cl-letf (((symbol-function 'ecc-notification-display-message)
               (lambda (state &optional buffer) (setq message-called t))))
      
      ;; Act
      (ecc-notification-dispatch :y/n)
      
      ;; Assert
      (should message-called))))

(ert-deftest test-notification-should-update-last-state-after-dispatch ()
  "Test that last state is updated after successful dispatch."
  ;; Arrange
  (let ((ecc-notification-enabled t)
        (ecc-notification-states '(:y/n))
        (ecc-notification-methods '(message))
        (ecc-notification--last-time 0)
        (ecc-notification--last-state nil))
    
    (cl-letf (((symbol-function 'ecc-notification-display-message) #'ignore))
      
      ;; Act
      (ecc-notification-dispatch :y/n)
      
      ;; Assert
      (should (eq ecc-notification--last-state :y/n)))))

;; Test notification method toggling
(ert-deftest test-notification-toggle-bell-should-disable-when-enabled ()
  "Test that bell toggle disables bell when it is enabled."
  ;; Arrange
  (let ((ecc-notification-methods '(bell flash message)))
    ;; Act
    (ecc-notification-toggle-bell)
    ;; Assert
    (should-not (memq 'bell ecc-notification-methods))))

(ert-deftest test-notification-toggle-bell-should-enable-when-disabled ()
  "Test that bell toggle enables bell when it is disabled."
  ;; Arrange
  (let ((ecc-notification-methods '(flash message)))
    ;; Act
    (ecc-notification-toggle-bell)
    ;; Assert
    (should (memq 'bell ecc-notification-methods))))

(ert-deftest test-notification-toggle-bell-should-preserve-other-methods ()
  "Test that bell toggle preserves other notification methods."
  ;; Arrange
  (let ((ecc-notification-methods '(bell flash message)))
    ;; Act
    (ecc-notification-toggle-bell)
    ;; Assert
    (should (memq 'flash ecc-notification-methods))
    (should (memq 'message ecc-notification-methods))))

(ert-deftest test-notification-toggle-flash-should-disable-when-enabled ()
  "Test that flash toggle disables flash when it is enabled."
  ;; Arrange
  (let ((ecc-notification-methods '(bell flash message)))
    ;; Act
    (ecc-notification-toggle-flash)
    ;; Assert
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