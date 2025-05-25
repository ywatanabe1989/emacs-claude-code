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
(require 'ecc-state-detection)

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

(ert-deftest test-auto-core-timer-start-activates-timer ()
  "Test that starting the timer makes it active."
  (with-clean-ecc-auto-core-state
    ;; Arrange: Timer is initially inactive
    
    ;; Act: Start timer with callback
    (ecc-auto-core-timer-start #'ignore)
    
    ;; Assert: Timer should be active
    (should (ecc-auto-core-timer-active-p))))

(ert-deftest test-auto-core-timer-start-creates-timer-object ()
  "Test that starting the timer creates a valid timer object."
  (with-clean-ecc-auto-core-state
    ;; Arrange: Timer reference is initially nil
    
    ;; Act: Start timer with callback
    (ecc-auto-core-timer-start #'ignore)
    
    ;; Assert: Timer object should be created
    (should (timerp ecc-auto-core--timer))))

(ert-deftest test-auto-core-timer-stop-deactivates-active-timer ()
  "Test that stopping the timer deactivates an active timer."
  (with-clean-ecc-auto-core-state
    ;; Arrange: Start a timer
    (ecc-auto-core-timer-start #'ignore)
    
    ;; Act: Stop the timer
    (ecc-auto-core-timer-stop)
    
    ;; Assert: Timer should be inactive
    (should-not (ecc-auto-core-timer-active-p))))

(ert-deftest test-auto-core-timer-stop-clears-timer-reference ()
  "Test that stopping the timer clears the internal timer reference."
  (with-clean-ecc-auto-core-state
    ;; Arrange: Start a timer
    (ecc-auto-core-timer-start #'ignore)
    
    ;; Act: Stop the timer
    (ecc-auto-core-timer-stop)
    
    ;; Assert: Timer reference should be nil
    (should-not ecc-auto-core--timer)))

(ert-deftest test-auto-core-timer-remains-active-after-restart ()
  "Test that timer remains active when restarted with new callback."
  (with-clean-ecc-auto-core-state
    (let ((original-callback (lambda () (message "Original")))
          (new-callback (lambda () (message "New"))))
      
      ;; Arrange: Start timer with original callback
      (ecc-auto-core-timer-start original-callback)
      
      ;; Act: Restart with new callback
      (ecc-auto-core-timer-start new-callback)
      
      ;; Assert: Timer should still be active
      (should (ecc-auto-core-timer-active-p)))))

(ert-deftest test-auto-core-timer-replaces-previous-timer-on-restart ()
  "Test that restarting timer replaces the previous timer instance."
  (with-clean-ecc-auto-core-state
    (let ((original-callback (lambda () (message "Original")))
          (new-callback (lambda () (message "New"))))
      
      ;; Arrange: Start timer and capture reference
      (ecc-auto-core-timer-start original-callback)
      (let ((first-timer ecc-auto-core--timer))
        
        ;; Act: Restart with new callback
        (ecc-auto-core-timer-start new-callback)
        
        ;; Assert: Timer instance should be different
        (should-not (eq first-timer ecc-auto-core--timer))))))

;;;; State management tests

(ert-deftest test-auto-core-update-state-stores-provided-state-value ()
  "Test that update-state stores the provided state value."
  (with-clean-ecc-auto-core-state
    ;; Arrange: Start with clean state
    (setq ecc-auto-core--last-state nil)
    
    ;; Act: Update with test state
    (ecc-auto-core-update-state :y/n)
    
    ;; Assert: State should be stored
    (should (eq ecc-auto-core--last-state :y/n))))

(ert-deftest test-auto-core-update-state-records-current-timestamp ()
  "Test that update-state records the current timestamp."
  (with-clean-ecc-auto-core-state
    ;; Arrange: Start with zero timestamp
    (setq ecc-auto-core--last-response-time 0)
    
    ;; Act: Update state
    (ecc-auto-core-update-state :y/n)
    
    ;; Assert: Timestamp should be updated to current time
    (should (> ecc-auto-core--last-response-time 0))))

(ert-deftest test-auto-core-throttled-p-blocks-duplicate-state-within-throttle-period ()
  "Test that throttled-p returns true for same state within throttle period."
  (with-clean-ecc-auto-core-state
    ;; Arrange: Set up state with recent timestamp
    (setq ecc-auto-core--last-state :y/n
          ecc-auto-core--last-response-time (float-time))
    
    ;; Act & Assert: Same state should be throttled
    (should (ecc-auto-core-throttled-p :y/n))))

(ert-deftest test-auto-core-throttled-p-allows-different-state-immediately ()
  "Test that throttled-p returns false for different state regardless of timing."
  (with-clean-ecc-auto-core-state
    ;; Arrange: Set up state with recent timestamp for :y/n
    (setq ecc-auto-core--last-state :y/n
          ecc-auto-core--last-response-time (float-time))
    
    ;; Act & Assert: Different state should not be throttled
    (should-not (ecc-auto-core-throttled-p :waiting))))

(ert-deftest test-auto-core-throttled-p-allows-same-state-after-throttle-period ()
  "Test that throttled-p returns false for same state after throttle period expires."
  (with-clean-ecc-auto-core-state
    ;; Arrange: Set up state with old timestamp (beyond throttle period)
    (setq ecc-auto-core--last-state :y/n
          ecc-auto-core--last-response-time 
          (- (float-time) (* 2 ecc-auto-core-throttle-time)))
    
    ;; Act & Assert: Same state should not be throttled after timeout
    (should-not (ecc-auto-core-throttled-p :y/n))))

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

;; Test isolation macro for clean state
(defmacro with-clean-ecc-auto-core-state (&rest body)
  "Execute BODY with clean ECC auto-core state."
  `(let ((ecc-auto-core--registered-buffers nil)
         (ecc-auto-core--last-state nil)
         (original-timer ecc-auto-core--timer))
     (unwind-protect
         (progn
           (setq ecc-auto-core--timer nil)
           ,@body)
       ;; Cleanup
       (ecc-auto-core-timer-stop)
       (setq ecc-auto-core--registered-buffers nil
             ecc-auto-core--last-state nil
             ecc-auto-core--timer original-timer))))

(ert-deftest test-auto-core-registered-buffers-includes-multiple-registered-buffers ()
  "Test that registered-buffers returns all registered live buffers."
  (with-clean-ecc-auto-core-state
    (with-temp-buffer-fixture "Test content"
      (let ((temp-buffer-2 (generate-new-buffer "*auto-test-2*")))
        (unwind-protect
            (progn
              ;; Arrange: Register two test buffers
              (ecc-auto-core-register-buffer temp-buffer)
              (ecc-auto-core-register-buffer temp-buffer-2)
              
              ;; Act: Get registered buffers
              (let ((buffers (ecc-auto-core-registered-buffers)))
                
                ;; Assert: Both buffers are included
                (should (= (length buffers) 2))
                (should (memq temp-buffer buffers))
                (should (memq temp-buffer-2 buffers))))
          
          ;; Clean up
          (when (buffer-live-p temp-buffer-2)
            (kill-buffer temp-buffer-2)))))))

(ert-deftest test-auto-core-registered-buffers-excludes-dead-buffers ()
  "Test that registered-buffers excludes dead buffers from results."
  (with-clean-ecc-auto-core-state
    (with-temp-buffer-fixture "Test content"
      (let ((temp-buffer-2 (generate-new-buffer "*auto-test-2*")))
        ;; Arrange: Register two buffers
        (ecc-auto-core-register-buffer temp-buffer)
        (ecc-auto-core-register-buffer temp-buffer-2)
        
        ;; Act: Kill one buffer
        (kill-buffer temp-buffer-2)
        
        ;; Get registered buffers after killing one
        (let ((buffers (ecc-auto-core-registered-buffers)))
          
          ;; Assert: Only live buffer is included
          (should (= (length buffers) 1))
          (should (memq temp-buffer buffers))
          (should-not (memq temp-buffer-2 buffers)))))))

(ert-deftest test-auto-core-registered-buffers-returns-empty-when-none-registered ()
  "Test that registered-buffers returns empty list when no buffers registered."
  (with-clean-ecc-auto-core-state
    ;; Arrange: Start with clean state (no registered buffers)
    
    ;; Act: Get registered buffers
    (let ((buffers (ecc-auto-core-registered-buffers)))
      
      ;; Assert: Empty list returned
      (should (= (length buffers) 0)))))

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

(ert-deftest test-auto-core-should-call-callback-when-initial-waiting-detected ()
  "Test that callback is called when initial waiting state is detected."
  (with-temp-buffer-fixture "Test content with initial prompt"
    ;; Arrange
    (setq auto-test-callback-called nil)
    (cl-letf (((symbol-function 'ecc-detect-state) (lambda () :initial-waiting)))
      
      ;; Act
      (ecc-auto-core-initial-check temp-buffer #'auto-test-callback)
      
      ;; Assert
      (should auto-test-callback-called))))

(ert-deftest test-auto-core-should-pass-buffer-to-callback ()
  "Test that correct buffer is passed to callback."
  (with-temp-buffer-fixture "Test content with initial prompt"
    ;; Arrange
    (setq auto-test-callback-buffer nil)
    (cl-letf (((symbol-function 'ecc-detect-state) (lambda () :initial-waiting)))
      
      ;; Act
      (ecc-auto-core-initial-check temp-buffer #'auto-test-callback)
      
      ;; Assert
      (should (eq auto-test-callback-buffer temp-buffer)))))

(ert-deftest test-auto-core-should-pass-state-to-callback ()
  "Test that detected state is passed to callback."
  (with-temp-buffer-fixture "Test content with initial prompt"
    ;; Arrange
    (setq auto-test-callback-state nil)
    (cl-letf (((symbol-function 'ecc-detect-state) (lambda () :initial-waiting)))
      
      ;; Act
      (ecc-auto-core-initial-check temp-buffer #'auto-test-callback)
      
      ;; Assert
      (should (eq auto-test-callback-state :initial-waiting)))))

(ert-deftest test-auto-core-should-increment-check-counter ()
  "Test that initial check counter is incremented."
  (with-temp-buffer-fixture "Test content with initial prompt"
    ;; Arrange
    (setq ecc-auto-core--initial-check-count 0)
    (cl-letf (((symbol-function 'ecc-detect-state) (lambda () :initial-waiting)))
      
      ;; Act
      (ecc-auto-core-initial-check temp-buffer #'auto-test-callback)
      
      ;; Assert
      (should (= ecc-auto-core--initial-check-count 1)))))

(ert-deftest test-auto-core-should-not-call-callback-for-non-initial-waiting ()
  "Test that callback is not called for non-initial-waiting states."
  (with-temp-buffer-fixture "Test content with y/n prompt"
    ;; Arrange
    (setq auto-test-callback-called nil)
    (cl-letf (((symbol-function 'ecc-detect-state) (lambda () :y/n)))
      
      ;; Act
      (ecc-auto-core-initial-check temp-buffer #'auto-test-callback)
      
      ;; Assert
      (should-not auto-test-callback-called))))

(ert-deftest test-auto-core-should-increment-counter-even-without-callback ()
  "Test that counter increments even when callback is not called."
  (with-temp-buffer-fixture "Test content with y/n prompt"
    ;; Arrange
    (setq ecc-auto-core--initial-check-count 0)
    (cl-letf (((symbol-function 'ecc-detect-state) (lambda () :y/n)))
      
      ;; Act
      (ecc-auto-core-initial-check temp-buffer #'auto-test-callback)
      
      ;; Assert
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

(ert-deftest test-auto-core-should-stop-existing-timer-on-initialize ()
  "Test that existing timer is stopped during initialization."
  (unwind-protect
      (progn
        ;; Arrange
        (ecc-auto-core-timer-start #'ignore)
        
        ;; Act
        (ecc-auto-core-initialize)
        
        ;; Assert
        (should-not (ecc-auto-core-timer-active-p)))
    ;; Clean up
    (ecc-auto-core-timer-stop)))

(ert-deftest test-auto-core-should-clear-last-state-on-initialize ()
  "Test that last state is cleared during initialization."
  (unwind-protect
      (progn
        ;; Arrange
        (setq ecc-auto-core--last-state :y/n)
        
        ;; Act
        (ecc-auto-core-initialize)
        
        ;; Assert
        (should-not ecc-auto-core--last-state))
    ;; Clean up
    (ecc-auto-core-timer-stop)))

(ert-deftest test-auto-core-should-reset-last-response-time-on-initialize ()
  "Test that last response time is reset during initialization."
  (unwind-protect
      (progn
        ;; Arrange
        (setq ecc-auto-core--last-response-time (float-time))
        
        ;; Act
        (ecc-auto-core-initialize)
        
        ;; Assert
        (should (= ecc-auto-core--last-response-time 0)))
    ;; Clean up
    (ecc-auto-core-timer-stop)))

(ert-deftest test-auto-core-should-reset-initial-check-count-on-initialize ()
  "Test that initial check count is reset during initialization."
  (unwind-protect
      (progn
        ;; Arrange
        (setq ecc-auto-core--initial-check-count 3)
        
        ;; Act
        (ecc-auto-core-initialize)
        
        ;; Assert
        (should (= ecc-auto-core--initial-check-count 0)))
    ;; Clean up
    (ecc-auto-core-timer-stop)))

(ert-deftest test-auto-core-should-stop-timer-on-shutdown ()
  "Test that timer is stopped during shutdown."
  (unwind-protect
      (progn
        ;; Arrange
        (ecc-auto-core-timer-start #'ignore)
        
        ;; Act
        (ecc-auto-core-shutdown)
        
        ;; Assert
        (should-not (ecc-auto-core-timer-active-p)))
    ;; Clean up
    (ecc-auto-core-timer-stop)))

(ert-deftest test-auto-core-should-clear-last-state-on-shutdown ()
  "Test that last state is cleared during shutdown."
  (unwind-protect
      (progn
        ;; Arrange
        (setq ecc-auto-core--last-state :y/n)
        
        ;; Act
        (ecc-auto-core-shutdown)
        
        ;; Assert
        (should-not ecc-auto-core--last-state))
    ;; Clean up
    (ecc-auto-core-timer-stop)))

(ert-deftest test-auto-core-should-reset-last-response-time-on-shutdown ()
  "Test that last response time is reset to 0 during shutdown."
  (unwind-protect
      (progn
        ;; Arrange
        (setq ecc-auto-core--last-response-time (float-time))
        
        ;; Act
        (ecc-auto-core-shutdown)
        
        ;; Assert
        (should (= ecc-auto-core--last-response-time 0)))
    ;; Clean up
    (ecc-auto-core-timer-stop)))

(ert-deftest test-auto-core-should-reset-initial-check-count-on-shutdown ()
  "Test that initial check count is reset to 0 during shutdown."
  (unwind-protect
      (progn
        ;; Arrange
        (setq ecc-auto-core--initial-check-count 3)
        
        ;; Act
        (ecc-auto-core-shutdown)
        
        ;; Assert
        (should (= ecc-auto-core--initial-check-count 0)))
    ;; Clean up
    (ecc-auto-core-timer-stop)))

(ert-deftest test-auto-core-should-clear-registered-buffers-on-shutdown ()
  "Test that registered buffers list is cleared during shutdown."
  (unwind-protect
      (progn
        ;; Arrange
        (setq ecc-auto-core--registered-buffers '(dummy-buffer))
        
        ;; Act
        (ecc-auto-core-shutdown)
        
        ;; Assert
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
        (should (ecc-auto-core-timer-active-p))
        
        ;; Test timer stop
        (ecc-auto-core-timer-stop)
        (should-not (ecc-auto-core-timer-active-p))
        
        ;; Test timer start
        (ecc-auto-core-timer-start #'ignore)
        (should (ecc-auto-core-timer-active-p)))
    
    ;; Clean up
    (ecc-auto-core-timer-stop)))

(provide 'test-ecc-auto-core)

;;; test-ecc-auto-core.el ends here