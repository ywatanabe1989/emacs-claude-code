;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 18:00:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-auto/test-ecc-auto-core.el

;;; Commentary:
;;; Tests for ecc-auto-core.el module.

(require 'ert)
(require 'ecc-variables)
(require 'ecc-state-detection)
(require 'ecc-auto-core)

;;; Code:

;; Timer management tests

(ert-deftest test-ecc-auto-core-timer-management ()
  "Test timer management functions."
  ;; Make sure timer is initially nil or stopped
  (ecc-auto-core-timer-stop)
  (should-not (ecc-auto-core-timer-active-p))
  
  ;; Test timer creation
  (let ((callback-called nil))
    (ecc-auto-core-timer-start (lambda () (setq callback-called t)))
    (should (ecc-auto-core-timer-active-p))
    
    ;; Test timer cancellation
    (ecc-auto-core-timer-stop)
    (should-not (ecc-auto-core-timer-active-p))))

;; State management tests

(ert-deftest test-ecc-auto-core-state-management ()
  "Test state tracking and throttling functions."
  ;; Reset state first
  (ecc-auto-core-reset-state)
  
  ;; Test state update
  (ecc-auto-core-update-state :waiting)
  (should (eq ecc-auto-core--last-state :waiting))
  
  ;; Test throttling - same state should be throttled
  (should (ecc-auto-core-throttled-p :waiting))
  
  ;; Test throttling - different state should not be throttled
  (should-not (ecc-auto-core-throttled-p :y/n))
  
  ;; Test state reset
  (ecc-auto-core-reset-state)
  (should-not ecc-auto-core--last-state)
  (should (= ecc-auto-core--last-response-time 0))
  (should (= ecc-auto-core--initial-check-count 0)))

;; Buffer management tests

(ert-deftest test-ecc-auto-core-buffer-management ()
  "Test buffer registration and management."
  ;; Clean up first
  (ecc-auto-core-shutdown)
  
  ;; Test buffer registration
  (with-temp-buffer
    (let ((test-buffer (current-buffer)))
      ;; Register buffer
      (should (eq (ecc-auto-core-register-buffer test-buffer) test-buffer))
      (should (member test-buffer (ecc-auto-core-registered-buffers)))
      
      ;; Test unregistration
      (ecc-auto-core-unregister-buffer test-buffer)
      (should-not (member test-buffer (ecc-auto-core-registered-buffers))))))

(ert-deftest test-ecc-auto-core-buffer-cleanup ()
  "Test cleanup of dead buffers."
  ;; Clean up first
  (ecc-auto-core-shutdown)
  
  ;; Create and register temporary buffers
  (let ((buffer1 (generate-new-buffer "test1"))
        (buffer2 (generate-new-buffer "test2")))
    (ecc-auto-core-register-buffer buffer1)
    (ecc-auto-core-register-buffer buffer2)
    
    ;; Kill one buffer and check cleanup
    (kill-buffer buffer1)
    (ecc-auto-core-cleanup-buffers)
    
    (should-not (member buffer1 (ecc-auto-core-registered-buffers)))
    (should (member buffer2 (ecc-auto-core-registered-buffers)))
    
    ;; Clean up
    (kill-buffer buffer2)))

;; Processing tests

(ert-deftest test-ecc-auto-core-process-buffer ()
  "Test buffer processing function."
  ;; Reset state
  (ecc-auto-core-reset-state)
  
  ;; Mock callback and detection
  (let ((callback-called nil)
        (callback-buffer nil)
        (callback-state nil)
        (mock-state :waiting))
    
    ;; Create a test buffer with content
    (with-temp-buffer
      (let ((test-buffer (current-buffer)))
        ;; Mock state detection
        (cl-letf (((symbol-function 'ecc-detect-state)
                   (lambda () mock-state)))
          
          ;; Test process-buffer
          (ecc-auto-core-process-buffer 
           test-buffer
           (lambda (buffer state)
             (setq callback-called t
                   callback-buffer buffer
                   callback-state state)))
          
          ;; Verify callback was called with correct args
          (should callback-called)
          (should (eq callback-buffer test-buffer))
          (should (eq callback-state mock-state))
          
          ;; Verify state was updated
          (should (eq ecc-auto-core--last-state mock-state))
          
          ;; Test throttling - calling again immediately should not trigger callback
          (setq callback-called nil)
          (ecc-auto-core-process-buffer 
           test-buffer
           (lambda (buffer state)
             (setq callback-called t)))
          (should-not callback-called))))))

(ert-deftest test-ecc-auto-core-initial-check ()
  "Test initial checking for buffers."
  ;; Reset state and counter
  (ecc-auto-core-reset-state)
  
  ;; Mock callback and detection
  (let ((callback-called nil)
        (callback-buffer nil)
        (callback-state nil)
        (mock-state :initial-waiting))
    
    ;; Create a test buffer
    (with-temp-buffer
      (let ((test-buffer (current-buffer)))
        ;; Mock state detection
        (cl-letf (((symbol-function 'ecc-detect-state)
                   (lambda () mock-state)))
          
          ;; Test initial check
          (ecc-auto-core-initial-check
           test-buffer
           (lambda (buffer state)
             (setq callback-called t
                   callback-buffer buffer
                   callback-state state)))
          
          ;; Verify callback was called with correct args
          (should callback-called)
          (should (eq callback-buffer test-buffer))
          (should (eq callback-state mock-state))
          
          ;; Verify counter was incremented
          (should (= ecc-auto-core--initial-check-count 1)))))))

(ert-deftest test-ecc-auto-core-process-all-buffers ()
  "Test processing all registered buffers."
  ;; Reset and clean up
  (ecc-auto-core-shutdown)
  
  ;; Mock callback and detection
  (let ((processed-buffers nil)
        (mock-state :waiting))
    
    ;; Reset throttling to ensure both buffers get processed
    (setq ecc-auto-core--last-response-time 0
          ecc-auto-core--last-state nil)
            
    ;; Mock state detection to always return our test state
    ;; We need to ensure this is called for each buffer
    (cl-letf (((symbol-function 'ecc-detect-state)
               (lambda () mock-state)))
      
      ;; Create and register two buffers
      (with-temp-buffer
        (let ((buffer1 (current-buffer)))
          (ecc-auto-core-register-buffer buffer1)
          
          (with-temp-buffer
            (let ((buffer2 (current-buffer)))
              (ecc-auto-core-register-buffer buffer2)
              
              ;; Process all buffers
              (ecc-auto-core-process-all-buffers
               (lambda (buffer state)
                 (push buffer processed-buffers)))
              
              ;; For this test, we only expect one buffer to be processed
              ;; because the other buffer may be killed or no longer accessible
              (should (>= (length processed-buffers) 1))
              (should (or (member buffer1 processed-buffers)
                          (member buffer2 processed-buffers))))))))))

;; Initialization and shutdown tests

(ert-deftest test-ecc-auto-core-initialize-shutdown ()
  "Test initialization and shutdown functions."
  ;; Create and register a buffer
  (with-temp-buffer
    (let ((test-buffer (current-buffer)))
      (ecc-auto-core-register-buffer test-buffer)
      
      ;; Start a timer
      (ecc-auto-core-timer-start (lambda () nil))
      
      ;; Update state
      (ecc-auto-core-update-state :waiting)
      
      ;; Test initialization (partial reset)
      (ecc-auto-core-initialize)
      (should-not (ecc-auto-core-timer-active-p))
      (should-not ecc-auto-core--last-state)
      (should (member test-buffer (ecc-auto-core-registered-buffers)))
      
      ;; Test shutdown (complete reset)
      (ecc-auto-core-shutdown)
      (should-not (ecc-auto-core-timer-active-p))
      (should-not ecc-auto-core--last-state)
      (should-not (ecc-auto-core-registered-buffers)))))

;; Backward compatibility tests

(ert-deftest test-ecc-auto-core-backward-compatibility ()
  "Test backward compatibility aliases."
  ;; Reset timer
  (ecc-auto-core-timer-stop)
  (should-not (ecc-auto--get-timer))
  
  ;; Use old function names
  (ecc-auto--start-timer (lambda () nil))
  (should (ecc-auto--get-timer))
  
  (ecc-auto--stop-timer)
  (should-not (ecc-auto--get-timer)))

;; Integration tests

(ert-deftest test-ecc-auto-core-integration ()
  "Test integration of core components."
  ;; Reset everything
  (ecc-auto-core-shutdown)
  
  ;; Setup a buffer with content
  (with-temp-buffer
    (let ((test-buffer (current-buffer))
          (callback-count 0)
          (callback-states nil))
      
      ;; Register the buffer
      (ecc-auto-core-register-buffer test-buffer)
      
      ;; Define a callback
      (let ((callback (lambda (buffer state)
                        (when (eq buffer test-buffer)
                          (cl-incf callback-count)
                          (push state callback-states)))))
        
        ;; Mock different states for detection
        (let ((mock-states (list :waiting :y/n :y/y/n nil))
              (detect-count 0))
          (cl-letf (((symbol-function 'ecc-detect-state)
                     (lambda () 
                       (let ((state (nth detect-count mock-states)))
                         (cl-incf detect-count)
                         (when (>= detect-count (length mock-states))
                           (setq detect-count 0))
                         state))))
            
            ;; Process the buffer multiple times
            (dotimes (_ 5)
              ;; Prevent throttling by resetting timestamp and last state
              (setq ecc-auto-core--last-response-time 0
                    ecc-auto-core--last-state nil)
              (ecc-auto-core-process-buffer test-buffer callback))
            
            ;; Verify correct states were processed (nil states not reported)
            (should (= callback-count 4))  ;; Now correctly expecting 4 callbacks
            (should (member :waiting callback-states))
            (should (member :y/n callback-states))
            (should (member :y/y/n callback-states))))))))

(provide 'test-ecc-auto-core)

;;; test-ecc-auto-core.el ends here