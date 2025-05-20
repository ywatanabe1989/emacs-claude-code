;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 00:45:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-auto/test-ecc-auto-response-consolidated.el

;;; Commentary:
;;; Tests for the consolidated auto-response system.
;;; These tests cover both global and buffer-local auto-response functionality.

(require 'ert)
(require 'ecc-variables-consolidated)
(require 'ecc-state-detection-consolidated)
(require 'ecc-auto-core-consolidated)
(require 'ecc-auto-response-consolidated)

;;; Code:

;;; Global Mode Tests

(ert-deftest test-ecc-auto-response-consolidated-toggle-global ()
  "Test that auto-response toggle correctly flips the enabled state in global mode."
  ;; Store original states
  (let ((orig-enabled ecc-auto-response-enabled)
        (orig-buffer-local-default ecc-auto-response-buffer-local-default))
    (unwind-protect
        (progn
          ;; Mock timer function to prevent actual timer creation
          (cl-letf (((symbol-function 'ecc-auto-core-timer-start) (lambda (callback) t))
                    ((symbol-function 'ecc-auto-core-timer-stop) (lambda () t))
                    ((symbol-function 'ecc-auto-core-initial-check) (lambda (buffer callback) t)))
            
            ;; Set to use global mode
            (setq ecc-auto-response-buffer-local-default nil)
            
            ;; Set initial state to disabled
            (setq ecc-auto-response-enabled nil)
            
            ;; Toggle should enable
            (ecc-auto-response-toggle)
            (should ecc-auto-response-enabled)
            
            ;; Toggle again should disable
            (ecc-auto-response-toggle)
            (should-not ecc-auto-response-enabled)))
      
      ;; Restore original state
      (setq ecc-auto-response-enabled orig-enabled
            ecc-auto-response-buffer-local-default orig-buffer-local-default))))

(ert-deftest test-ecc-auto-response-consolidated-start-stop-global ()
  "Test that auto-response start and stop functions work correctly in global mode."
  ;; Store original states
  (let ((orig-enabled ecc-auto-response-enabled)
        (orig-callback ecc-auto-response--registered-callback)
        (orig-buffer-local-default ecc-auto-response-buffer-local-default))
    (unwind-protect
        (progn
          ;; Mock timer function to prevent actual timer creation
          (cl-letf (((symbol-function 'ecc-auto-core-timer-start) (lambda (callback) t))
                    ((symbol-function 'ecc-auto-core-timer-stop) (lambda () t))
                    ((symbol-function 'ecc-auto-core-initial-check) (lambda (buffer callback) t))
                    ((symbol-function 'ecc-auto-core-register-buffer) (lambda (buffer) buffer)))
            
            ;; Set to use global mode
            (setq ecc-auto-response-buffer-local-default nil)
            
            ;; Start the auto-response
            (ecc-auto-response-start)
            
            ;; Check state is enabled and callback is set
            (should ecc-auto-response-enabled)
            (should ecc-auto-response--registered-callback)
            
            ;; Stop the auto-response
            (ecc-auto-response-stop)
            
            ;; Check state is disabled and callback is cleared
            (should-not ecc-auto-response-enabled)
            (should-not ecc-auto-response--registered-callback)))
      
      ;; Restore original state
      (setq ecc-auto-response-enabled orig-enabled
            ecc-auto-response--registered-callback orig-callback
            ecc-auto-response-buffer-local-default orig-buffer-local-default))))

(ert-deftest test-ecc-auto-response-consolidated-register-buffer-global ()
  "Test that buffer registration works correctly in global mode."
  (with-temp-buffer
    (let ((test-buffer (current-buffer))
          (orig-buffer-local-default ecc-auto-response-buffer-local-default))
      (unwind-protect
          (progn
            ;; Set to use global mode
            (setq ecc-auto-response-buffer-local-default nil)
            
            ;; Mock the core register function
            (cl-letf (((symbol-function 'ecc-auto-core-register-buffer)
                       (lambda (buffer) 
                         (should (eq buffer test-buffer))
                         buffer)))
              
              ;; Test the register buffer function
              (should (eq (ecc-auto-response-register-buffer test-buffer) test-buffer))))
        
        ;; Restore original state
        (setq ecc-auto-response-buffer-local-default orig-buffer-local-default)))))

(ert-deftest test-ecc-auto-response-consolidated-send-global ()
  "Test that auto-response send function works correctly in global mode."
  ;; Store original states
  (let ((orig-enabled ecc-auto-response-enabled)
        (send-message-called nil)
        (send-message-buffer nil)
        (send-message-response nil)
        (send-message-type nil)
        (orig-buffer-local-default ecc-auto-response-buffer-local-default))
    (unwind-protect
        (progn
          ;; Set to use global mode
          (setq ecc-auto-response-buffer-local-default nil)
          
          ;; Set up a mock environment
          (setq ecc-auto-response-enabled t
                ecc-auto-response-yes "1"
                ecc-auto-response-yes-plus "2"
                ecc-auto-response-continue "/auto"
                ecc-auto-response-initial-waiting "/user:understand-guidelines")
          
          ;; Mock the send message function and state detection
          (cl-letf (((symbol-function 'ecc-auto-response--dispatch-response)
                     (lambda (buffer response type)
                       (setq send-message-called t
                             send-message-buffer buffer
                             send-message-response response
                             send-message-type type)
                       t))
                    ((symbol-function 'ecc-detect-state)
                     (lambda () :waiting)))
            
            ;; Test send with auto-detected state
            (with-temp-buffer
              (let ((test-buffer (current-buffer)))
                (ecc-auto-response-send test-buffer)
                
                ;; Check that the message was sent with correct parameters
                (should send-message-called)
                (should (eq send-message-buffer test-buffer))
                (should (string= send-message-response "/auto"))
                (should (string= send-message-type "Continue"))
                
                ;; Reset for next test
                (setq send-message-called nil)
                
                ;; Test send with explicit state
                (ecc-auto-response-send test-buffer :y/n)
                
                ;; Check that the message was sent with correct parameters
                (should send-message-called)
                (should (eq send-message-buffer test-buffer))
                (should (string= send-message-response "1"))
                (should (string= send-message-type "Y/N"))))))
      
      ;; Restore original state
      (setq ecc-auto-response-enabled orig-enabled
            ecc-auto-response-buffer-local-default orig-buffer-local-default))))

;;; Buffer-Local Mode Tests

(ert-deftest test-ecc-auto-response-consolidated-buffer-local-init ()
  "Test buffer-local initialization."
  (with-temp-buffer
    (let ((test-buffer (current-buffer))
          (orig-buffer-local-default ecc-auto-response-buffer-local-default)
          (orig-default-enabled ecc-auto-response-default-enabled))
      (unwind-protect
          (progn
            ;; Set up test environment
            (setq ecc-auto-response-buffer-local-default t
                  ecc-auto-response-default-enabled t
                  ecc-auto-response-yes "1"
                  ecc-auto-response-yes-plus "2"
                  ecc-auto-response-continue "/auto"
                  ecc-auto-response-initial-waiting "/user:understand-guidelines"
                  ecc-auto-response-notify t)
            
            ;; Mock necessary functions
            (cl-letf (((symbol-function 'ecc-buffer-state-init)
                       (lambda () t))
                      ((symbol-function 'ecc-buffer-local-init)
                       (lambda () t))
                      ((symbol-function 'ecc-auto-core-register-buffer-local)
                       (lambda (buffer) 
                         (should (eq buffer test-buffer))
                         t)))
              
              ;; Initialize buffer-local auto-response
              (ecc-auto-response-buffer-local-init test-buffer)
              
              ;; Check that buffer-local variables were set correctly
              (should (boundp 'ecc-buffer-auto-response-enabled))
              (should ecc-buffer-auto-response-enabled)
              (should (string= ecc-buffer-auto-response-y/n "1"))
              (should (string= ecc-buffer-auto-response-y/y/n "2"))
              (should (string= ecc-buffer-auto-response-waiting "/auto"))
              (should (string= ecc-buffer-auto-response-initial-waiting "/user:understand-guidelines"))
              (should ecc-buffer-auto-notify-completions))))
        
        ;; Restore original state
        (setq ecc-auto-response-buffer-local-default orig-buffer-local-default
              ecc-auto-response-default-enabled orig-default-enabled)))))

(ert-deftest test-ecc-auto-response-consolidated-buffer-local-start-stop ()
  "Test buffer-local start and stop functions."
  (with-temp-buffer
    (let ((orig-buffer-local-default ecc-auto-response-buffer-local-default))
      (unwind-protect
          (progn
            ;; Set up test environment
            (setq ecc-auto-response-buffer-local-default t)
            
            ;; Set up buffer-local state
            (setq-local ecc-buffer-auto-response-enabled nil)
            
            ;; Mock necessary functions
            (cl-letf (((symbol-function 'ecc-auto-response-buffer-local-init)
                       (lambda (&optional buffer) t))
                      ((symbol-function 'ecc-auto-core-timer-start-buffer-local)
                       (lambda (callback) 
                         (should (functionp callback))
                         t))
                      ((symbol-function 'ecc-auto-core-timer-stop-buffer-local)
                       (lambda () t)))
              
              ;; Start buffer-local auto-response
              (ecc-auto-response-buffer-local-start)
              
              ;; Check that it was enabled
              (should ecc-buffer-auto-response-enabled)
              
              ;; Stop buffer-local auto-response
              (ecc-auto-response-buffer-local-stop)
              
              ;; Check that it was disabled
              (should-not ecc-buffer-auto-response-enabled))))
        
        ;; Restore original state
        (setq ecc-auto-response-buffer-local-default orig-buffer-local-default)))))

(ert-deftest test-ecc-auto-response-consolidated-buffer-local-check ()
  "Test buffer-local state checking."
  (with-temp-buffer
    (let ((test-buffer (current-buffer))
          (send-message-called nil))
      
      ;; Set up buffer-local state
      (setq-local ecc-buffer-auto-response-enabled t
                  ecc-buffer-auto-response-y/n "1"
                  ecc-buffer-auto-response-y/y/n "2"
                  ecc-buffer-auto-response-waiting "/auto"
                  ecc-buffer-auto-response-initial-waiting "/user:understand-guidelines")
      
      ;; Mock necessary functions
      (cl-letf (((symbol-function 'ecc-detect-state)
                 (lambda () :y/n))
                ((symbol-function 'ecc-buffer-state-throttled-p)
                 (lambda (state) nil))
                ((symbol-function 'ecc-buffer-state-set)
                 (lambda (key value) t))
                ((symbol-function 'ecc-auto-response-buffer-local-send-message)
                 (lambda (buffer response type)
                   (setq send-message-called t)
                   (should (eq buffer test-buffer))
                   (should (string= response "1"))
                   (should (string= type "Y/N"))
                   t)))
        
        ;; Check buffer state
        (ecc-auto-response-buffer-local-check test-buffer)
        
        ;; Verify message was sent
        (should send-message-called)))))

;;; Convenience Function Tests

(ert-deftest test-ecc-auto-response-consolidated-convenience-global ()
  "Test convenience functions in global mode."
  (with-temp-buffer
    (let ((test-buffer (current-buffer))
          (orig-buffer-local-default ecc-auto-response-buffer-local-default)
          (dispatch-called nil)
          (dispatch-buffer nil)
          (dispatch-response nil)
          (dispatch-type nil))
      (unwind-protect
          (progn
            ;; Set to use global mode
            (setq ecc-auto-response-buffer-local-default nil
                  ecc-auto-response-yes "1"
                  ecc-auto-response-yes-plus "2"
                  ecc-auto-response-continue "/auto")
            
            ;; Mock dispatch function
            (cl-letf (((symbol-function 'ecc-auto-response--dispatch-response)
                       (lambda (buffer response type)
                         (setq dispatch-called t
                               dispatch-buffer buffer
                               dispatch-response response
                               dispatch-type type)
                         t))
                      ((symbol-function 'buffer-local-value)
                       (lambda (var buf) nil)))
              
              ;; Test yes convenience function
              (ecc-auto-response-yes test-buffer)
              (should dispatch-called)
              (should (eq dispatch-buffer test-buffer))
              (should (string= dispatch-response "1"))
              (should (string= dispatch-type "Y/N"))
              
              ;; Reset for next test
              (setq dispatch-called nil)
              
              ;; Test yes-plus convenience function
              (ecc-auto-response-yes-plus test-buffer)
              (should dispatch-called)
              (should (eq dispatch-buffer test-buffer))
              (should (string= dispatch-response "2"))
              (should (string= dispatch-type "Y/Y/N"))
              
              ;; Reset for next test
              (setq dispatch-called nil)
              
              ;; Test continue convenience function
              (ecc-auto-response-continue test-buffer)
              (should dispatch-called)
              (should (eq dispatch-buffer test-buffer))
              (should (string= dispatch-response "/auto"))
              (should (string= dispatch-type "Continue"))))
        
        ;; Restore original state
        (setq ecc-auto-response-buffer-local-default orig-buffer-local-default)))))

(ert-deftest test-ecc-auto-response-consolidated-convenience-buffer-local ()
  "Test convenience functions in buffer-local mode."
  (with-temp-buffer
    (let ((test-buffer (current-buffer))
          (orig-buffer-local-default ecc-auto-response-buffer-local-default)
          (send-called nil)
          (send-buffer nil)
          (send-response nil)
          (send-type nil))
      (unwind-protect
          (progn
            ;; Set to use buffer-local mode
            (setq ecc-auto-response-buffer-local-default t)
            
            ;; Set up buffer-local variables
            (setq-local ecc-buffer-auto-response-enabled t
                        ecc-buffer-auto-response-y/n "LOCAL-1"
                        ecc-buffer-auto-response-y/y/n "LOCAL-2"
                        ecc-buffer-auto-response-waiting "LOCAL-AUTO")
            
            ;; Mock functions
            (cl-letf (((symbol-function 'ecc-auto-response-buffer-local-send-message)
                       (lambda (buffer response type)
                         (setq send-called t
                               send-buffer buffer
                               send-response response
                               send-type type)
                         t))
                      ((symbol-function 'buffer-local-value)
                       (lambda (var buf) 
                         (cond
                          ((eq var 'ecc-buffer-auto-response-enabled) t)
                          ((eq var 'ecc-buffer-auto-response-y/n) "LOCAL-1")
                          ((eq var 'ecc-buffer-auto-response-y/y/n) "LOCAL-2")
                          ((eq var 'ecc-buffer-auto-response-waiting) "LOCAL-AUTO")
                          (t nil)))))
              
              ;; Test yes convenience function
              (ecc-auto-response-yes test-buffer)
              (should send-called)
              (should (eq send-buffer test-buffer))
              (should (string= send-response "LOCAL-1"))
              (should (string= send-type "Y/N"))
              
              ;; Reset for next test
              (setq send-called nil)
              
              ;; Test yes-plus convenience function
              (ecc-auto-response-yes-plus test-buffer)
              (should send-called)
              (should (eq send-buffer test-buffer))
              (should (string= send-response "LOCAL-2"))
              (should (string= send-type "Y/Y/N"))
              
              ;; Reset for next test
              (setq send-called nil)
              
              ;; Test continue convenience function
              (ecc-auto-response-continue test-buffer)
              (should send-called)
              (should (eq send-buffer test-buffer))
              (should (string= send-response "LOCAL-AUTO"))
              (should (string= send-type "Continue"))))
        
        ;; Restore original state
        (setq ecc-auto-response-buffer-local-default orig-buffer-local-default)))))

;;; Backward Compatibility Tests

(ert-deftest test-ecc-auto-response-consolidated-backward-compatibility ()
  "Test backward compatibility functions."
  ;; Store original state
  (let ((orig-enabled ecc-auto-response-enabled)
        (orig-callback ecc-auto-response--registered-callback)
        (orig-buffer-local-default ecc-auto-response-buffer-local-default))
    (unwind-protect
        (progn
          ;; Set to use global mode
          (setq ecc-auto-response-buffer-local-default nil)
          
          ;; Mock functions
          (cl-letf (((symbol-function 'ecc-auto-core-timer-start) (lambda (callback) t))
                    ((symbol-function 'ecc-auto-core-timer-stop) (lambda () t))
                    ((symbol-function 'ecc-auto-core-initial-check) (lambda (buffer callback) t))
                    ((symbol-function 'ecc-auto-core-register-buffer) (lambda (buffer) buffer))
                    ((symbol-function 'ecc-auto-core-process-all-buffers)
                     (lambda (callback) 
                       (should (eq callback ecc-auto-response--registered-callback)))))
            
            ;; Test old function names
            (setq ecc-auto-response-enabled nil)
            
            ;; Start with old function name
            (ecc-start-auto-response)
            (should ecc-auto-response-enabled)
            
            ;; Set callback for check-and-respond test
            (setq ecc-auto-response--registered-callback (lambda () 'test))
            
            ;; Test old check-and-respond function
            (ecc-check-and-respond)
            
            ;; Stop with old function name
            (ecc-stop-auto-response)
            (should-not ecc-auto-response-enabled)))
      
      ;; Restore original state
      (setq ecc-auto-response-enabled orig-enabled
            ecc-auto-response--registered-callback orig-callback
            ecc-auto-response-buffer-local-default orig-buffer-local-default))))

(provide 'test-ecc-auto-response-consolidated)

;;; test-ecc-auto-response-consolidated.el ends here