;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 14:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-auto-core.el

;;; Commentary:
;;; Tests for the auto-core module.
;;; These tests verify the functionality of the core infrastructure
;;; for the auto-response system, including timer management,
;;; state tracking, buffer management, and processing.

(require 'ert)
(require 'ecc-auto-core)
(require 'ecc-state-detection-consolidated)

;;; Code:

;;;; Test utilities

(defvar auto-test-response-received nil
  "Variable to track responses received during testing.")

(defvar auto-test-callback-called nil
  "Whether the test callback was called.")

(defvar auto-test-callback-buffer nil
  "The buffer passed to the test callback.")

(defvar auto-test-callback-state nil
  "The state passed to the test callback.")

(defun auto-test-callback (buffer state)
  "Test callback for auto-response.
Stores BUFFER and STATE in test variables."
  (setq auto-test-callback-called t
        auto-test-callback-buffer buffer
        auto-test-callback-state state))

(defmacro with-temp-buffer-fixture (content &rest body)
  "Create a temporary buffer with CONTENT and evaluate BODY.
Ensures proper cleanup of test resources."
  (declare (indent 1))
  `(let ((temp-buffer (generate-new-buffer "*auto-test*")))
     (unwind-protect
         (progn
           (with-current-buffer temp-buffer
             (insert ,content)
             (goto-char (point-max)))
           ,@body)
       (when (buffer-live-p temp-buffer)
         (kill-buffer temp-buffer)))))

;; Mock state detection function for controlled testing
(defvar auto-test-mock-state nil
  "State to return from the mock state detection function.")

(defun auto-test-mock-detect-state ()
  "Mock implementation of state detection.
Returns `auto-test-mock-state'."
  auto-test-mock-state)

;;;; Timer management tests

(ert-deftest test-auto-core-timer-start ()
  "Test starting the auto timer."
  (unwind-protect
      (progn
        ;; Start timer with dummy callback
        (ecc-auto-core-timer-start #'ignore)
        
        ;; Check that timer is active
        (should (ecc-auto-core-timer-active-p))
        (should (timerp ecc-auto-core--timer)))
    
    ;; Clean up
    (ecc-auto-core-timer-stop)))

(ert-deftest test-auto-core-timer-stop ()
  "Test stopping the auto timer."
  ;; First start a timer
  (ecc-auto-core-timer-start #'ignore)
  
  ;; Now stop it
  (ecc-auto-core-timer-stop)
  
  ;; Check that timer is inactive
  (should-not (ecc-auto-core-timer-active-p))
  (should-not ecc-auto-core--timer))

(ert-deftest test-auto-core-timer-restart ()
  "Test restarting the timer with new callback."
  (unwind-protect
      (let ((original-callback (lambda () (message "Original")))
            (new-callback (lambda () (message "New"))))
        
        ;; Start with original callback
        (ecc-auto-core-timer-start original-callback)
        (should (ecc-auto-core-timer-active-p))
        
        ;; Restart with new callback
        (ecc-auto-core-timer-start new-callback)
        (should (ecc-auto-core-timer-active-p)))
    
    ;; Clean up
    (ecc-auto-core-timer-stop)))

;;;; State management tests

(ert-deftest test-auto-core-update-state ()
  "Test updating state tracking."
  ;; Set initial values to ensure we're testing changes
  (setq ecc-auto-core--last-state nil
        ecc-auto-core--last-response-time 0)
  
  ;; Update with test state
  (ecc-auto-core-update-state :y/n)
  
  ;; Check state was updated
  (should (eq ecc-auto-core--last-state :y/n))
  (should (> ecc-auto-core--last-response-time 0)))

(ert-deftest test-auto-core-throttled-p ()
  "Test throttling logic."
  ;; Set up state with recent timestamp
  (setq ecc-auto-core--last-state :y/n
        ecc-auto-core--last-response-time (float-time))
  
  ;; Check if same state is throttled
  (should (ecc-auto-core-throttled-p :y/n))
  
  ;; Check if different state is not throttled
  (should-not (ecc-auto-core-throttled-p :waiting))
  
  ;; Set up state with old timestamp
  (setq ecc-auto-core--last-state :y/n
        ecc-auto-core--last-response-time 
        (- (float-time) (* 2 ecc-auto-core-throttle-time)))
  
  ;; Check if same state is not throttled after timeout
  (should-not (ecc-auto-core-throttled-p :y/n)))

(ert-deftest test-auto-core-reset-state ()
  "Test resetting state tracking."
  ;; Set up some state
  (setq ecc-auto-core--last-state :y/n
        ecc-auto-core--last-response-time (float-time)
        ecc-auto-core--initial-check-count 3)
  
  ;; Reset state
  (ecc-auto-core-reset-state)
  
  ;; Check all values were reset
  (should-not ecc-auto-core--last-state)
  (should (= ecc-auto-core--last-response-time 0))
  (should (= ecc-auto-core--initial-check-count 0)))

;;;; Buffer management tests

(ert-deftest test-auto-core-register-buffer ()
  "Test buffer registration."
  (with-temp-buffer-fixture "Test content"
    ;; Clear existing registrations
    (setq ecc-auto-core--registered-buffers nil)
    
    ;; Register test buffer
    (ecc-auto-core-register-buffer temp-buffer)
    
    ;; Check buffer was registered
    (should (memq temp-buffer ecc-auto-core--registered-buffers))))

(ert-deftest test-auto-core-unregister-buffer ()
  "Test buffer unregistration."
  (with-temp-buffer-fixture "Test content"
    ;; Clear existing registrations and register test buffer
    (setq ecc-auto-core--registered-buffers nil)
    (ecc-auto-core-register-buffer temp-buffer)
    
    ;; Unregister test buffer
    (ecc-auto-core-unregister-buffer temp-buffer)
    
    ;; Check buffer was unregistered
    (should-not (memq temp-buffer ecc-auto-core--registered-buffers))))

(ert-deftest test-auto-core-registered-buffers ()
  "Test retrieving registered buffers."
  (with-temp-buffer-fixture "Test content"
    (let ((temp-buffer-2 (generate-new-buffer "*auto-test-2*")))
      (unwind-protect
          (progn
            ;; Clear existing registrations
            (setq ecc-auto-core--registered-buffers nil)
            
            ;; Register test buffers
            (ecc-auto-core-register-buffer temp-buffer)
            (ecc-auto-core-register-buffer temp-buffer-2)
            
            ;; Get registered buffers
            (let ((buffers (ecc-auto-core-registered-buffers)))
              ;; Check both buffers are included
              (should (= (length buffers) 2))
              (should (memq temp-buffer buffers))
              (should (memq temp-buffer-2 buffers)))
            
            ;; Kill one buffer
            (kill-buffer temp-buffer-2)
            
            ;; Get registered buffers again
            (let ((buffers (ecc-auto-core-registered-buffers)))
              ;; Check only the live buffer is included
              (should (= (length buffers) 1))
              (should (memq temp-buffer buffers))
              (should-not (memq temp-buffer-2 buffers))))
        
        ;; Clean up
        (when (buffer-live-p temp-buffer-2)
          (kill-buffer temp-buffer-2))))))

(ert-deftest test-auto-core-cleanup-buffers ()
  "Test cleaning up dead buffers."
  (with-temp-buffer-fixture "Test content"
    (let ((temp-buffer-2 (generate-new-buffer "*auto-test-2*")))
      (unwind-protect
          (progn
            ;; Clear existing registrations
            (setq ecc-auto-core--registered-buffers nil)
            
            ;; Register test buffers
            (ecc-auto-core-register-buffer temp-buffer)
            (ecc-auto-core-register-buffer temp-buffer-2)
            
            ;; Kill one buffer
            (kill-buffer temp-buffer-2)
            
            ;; Clean up buffer registry
            (let ((result (ecc-auto-core-cleanup-buffers)))
              ;; Check only the live buffer remains
              (should (= (length result) 1))
              (should (memq temp-buffer result))
              (should-not (memq temp-buffer-2 ecc-auto-core--registered-buffers))))
        
        ;; Clean up
        (when (buffer-live-p temp-buffer-2)
          (kill-buffer temp-buffer-2))))))

;;;; Core processing tests

(ert-deftest test-auto-core-process-buffer ()
  "Test processing a buffer."
  (with-temp-buffer-fixture "Test content with [y/n] prompt"
    ;; Set up test variables
    (setq auto-test-callback-called nil
          auto-test-callback-buffer nil
          auto-test-callback-state nil)
    
    ;; Replace state detection with mock
    (cl-letf (((symbol-function 'ecc-detect-state) #'auto-test-mock-detect-state))
      ;; Set mock state
      (setq auto-test-mock-state :y/n)
      
      ;; Process the buffer
      (ecc-auto-core-process-buffer temp-buffer #'auto-test-callback)
      
      ;; Check callback was called with correct parameters
      (should auto-test-callback-called)
      (should (eq auto-test-callback-buffer temp-buffer))
      (should (eq auto-test-callback-state :y/n))
      
      ;; Reset and test with no state
      (setq auto-test-callback-called nil
            auto-test-mock-state nil)
      
      ;; Process the buffer again
      (ecc-auto-core-process-buffer temp-buffer #'auto-test-callback)
      
      ;; Check callback was not called
      (should-not auto-test-callback-called))))

(ert-deftest test-auto-core-initial-check ()
  "Test initial check functionality."
  (with-temp-buffer-fixture "Test content with initial prompt"
    ;; Set up test variables
    (setq auto-test-callback-called nil
          auto-test-callback-buffer nil
          auto-test-callback-state nil
          ecc-auto-core--initial-check-count 0)
    
    ;; Replace state detection with mock
    (cl-letf (((symbol-function 'ecc-detect-state) #'auto-test-mock-detect-state))
      ;; Set mock state to initial waiting
      (setq auto-test-mock-state :initial-waiting)
      
      ;; Perform initial check
      (ecc-auto-core-initial-check temp-buffer #'auto-test-callback)
      
      ;; Check callback was called with correct parameters
      (should auto-test-callback-called)
      (should (eq auto-test-callback-buffer temp-buffer))
      (should (eq auto-test-callback-state :initial-waiting))
      
      ;; Check counter was incremented
      (should (= ecc-auto-core--initial-check-count 1))
      
      ;; Reset and test with different state
      (setq auto-test-callback-called nil
            auto-test-mock-state :y/n
            ecc-auto-core--initial-check-count 0)
      
      ;; Perform initial check again
      (ecc-auto-core-initial-check temp-buffer #'auto-test-callback)
      
      ;; Check callback was not called (only works with :initial-waiting)
      (should-not auto-test-callback-called)
      
      ;; Check counter was still incremented
      (should (= ecc-auto-core--initial-check-count 1)))))

(ert-deftest test-auto-core-process-all-buffers ()
  "Test processing all registered buffers."
  (let ((temp-buffer-1 (generate-new-buffer "*auto-test-1*"))
        (temp-buffer-2 (generate-new-buffer "*auto-test-2*")))
    (unwind-protect
        (progn
          ;; Set up test variables
          (setq auto-test-callback-called nil
                ecc-auto-core--registered-buffers nil)
          
          ;; Register test buffers
          (ecc-auto-core-register-buffer temp-buffer-1)
          (ecc-auto-core-register-buffer temp-buffer-2)
          
          ;; Replace state detection with mock
          (cl-letf (((symbol-function 'ecc-detect-state) #'auto-test-mock-detect-state)
                    ((symbol-function 'ecc-auto-core-process-buffer)
                     (lambda (buffer callback)
                       (setq auto-test-callback-called t))))
            
            ;; Process all buffers
            (ecc-auto-core-process-all-buffers #'auto-test-callback)
            
            ;; Check callback was called
            (should auto-test-callback-called)))
      
      ;; Clean up
      (when (buffer-live-p temp-buffer-1)
        (kill-buffer temp-buffer-1))
      (when (buffer-live-p temp-buffer-2)
        (kill-buffer temp-buffer-2)))))

;;;; Lifecycle tests

(ert-deftest test-auto-core-initialize ()
  "Test initialization of auto-core system."
  (unwind-protect
      (progn
        ;; Set up some state to be reset
        (ecc-auto-core-timer-start #'ignore)
        (setq ecc-auto-core--last-state :y/n
              ecc-auto-core--last-response-time (float-time)
              ecc-auto-core--initial-check-count 3)
        
        ;; Initialize the system
        (ecc-auto-core-initialize)
        
        ;; Check everything was reset
        (should-not (ecc-auto-core-timer-active-p))
        (should-not ecc-auto-core--last-state)
        (should (= ecc-auto-core--last-response-time 0))
        (should (= ecc-auto-core--initial-check-count 0)))
    
    ;; Clean up
    (ecc-auto-core-timer-stop)))

(ert-deftest test-auto-core-shutdown ()
  "Test shutdown of auto-core system."
  (unwind-protect
      (progn
        ;; Set up some state
        (ecc-auto-core-timer-start #'ignore)
        (setq ecc-auto-core--last-state :y/n
              ecc-auto-core--last-response-time (float-time)
              ecc-auto-core--initial-check-count 3
              ecc-auto-core--registered-buffers '(dummy-buffer))
        
        ;; Shut down the system
        (ecc-auto-core-shutdown)
        
        ;; Check everything was reset
        (should-not (ecc-auto-core-timer-active-p))
        (should-not ecc-auto-core--last-state)
        (should (= ecc-auto-core--last-response-time 0))
        (should (= ecc-auto-core--initial-check-count 0))
        (should-not ecc-auto-core--registered-buffers))
    
    ;; Clean up
    (ecc-auto-core-timer-stop)))

;;;; Backward compatibility tests

(ert-deftest test-auto-core-legacy-functions ()
  "Test backward compatibility functions."
  (unwind-protect
      (progn
        ;; Test timer active check
        (ecc-auto-core-timer-start #'ignore)
        (should (ecc-auto--get-timer))
        
        ;; Test timer stop
        (ecc-auto--stop-timer)
        (should-not (ecc-auto-core-timer-active-p))
        
        ;; Test timer start
        (ecc-auto--start-timer #'ignore)
        (should (ecc-auto-core-timer-active-p)))
    
    ;; Clean up
    (ecc-auto-core-timer-stop)))

(provide 'test-ecc-auto-core)

;;; test-ecc-auto-core.el ends here