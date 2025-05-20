;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 19:20:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-auto/test-ecc-auto-response-fix.el

;;; Commentary:
;;; Tests for auto-response throttling and improvements.

(require 'ecc-variables)
(require 'ecc-auto-response)
(require 'ecc-auto-response-fix)
(require 'ert)

;; Load state detection module if available
(when (locate-library "ecc-state-detection")
  (require 'ecc-state-detection))

;; Test throttling of auto-responses
(ert-deftest test-ecc-auto-response-throttled-p ()
  "Test that auto-response throttling works correctly."
  ;; Setup variables we need
  (let ((orig-alist ecc-auto-response-last-time-alist)
        (orig-state ecc-auto-response-active-state)
        (orig-throttle ecc-auto-response-throttle-time))
    
    ;; Use a small throttle time for testing
    (setq ecc-auto-response-throttle-time 0.1)
    
    ;; Reset all timestamps
    (setq ecc-auto-response-last-time-alist
          '((:y/n . 0.0)
            (:y/y/n . 0.0)
            (:waiting . 0.0)
            (:initial-waiting . 0.0)))
    
    ;; Initially nothing should be throttled (no active state)
    (setq ecc-auto-response-active-state nil)
    (should-not (ecc-auto-response-throttled-p :waiting))
    
    ;; Update the time for waiting state to current time
    (ecc-auto-response-update-time :waiting)
    
    ;; Now waiting should be throttled but other states shouldn't
    (should (ecc-auto-response-throttled-p :waiting))
    (should-not (ecc-auto-response-throttled-p :y/n))
    
    ;; Test active state throttling
    (setq ecc-auto-response-active-state :y/n)
    (should (ecc-auto-response-throttled-p :y/n))
    
    ;; Restore original values
    (setq ecc-auto-response-last-time-alist orig-alist
          ecc-auto-response-active-state orig-state
          ecc-auto-response-throttle-time orig-throttle)))

;; Test that check-and-respond advised function works correctly without arguments
(ert-deftest test-ecc-check-and-respond-advised ()
  "Test that ecc-check-and-respond-advised can be called without arguments."
  (let ((orig-enabled ecc-buffer-auto-response-enabled)
        (orig-buffer ecc-buffer-current-buffer))
    (unwind-protect
        (progn
          ;; Set up a mock environment
          (setq ecc-buffer-auto-response-enabled t)
          (save-window-excursion
            (with-temp-buffer
              (setq ecc-buffer-current-buffer (current-buffer))
              ;; Mock the state detection function
              (cl-letf (((symbol-function 'ecc-detect-simple-state) (lambda () nil)))
                ;; This function should run without errors
                (should-not (ecc-check-and-respond-advised))))))
      ;; Restore original values
      (setq ecc-buffer-auto-response-enabled orig-enabled
            ecc-buffer-current-buffer orig-buffer))))

;; Test that state is reset when auto-response is stopped
(ert-deftest test-ecc-auto-response-stop-advised ()
  "Test that auto-response state is reset when stopped."
  ;; Setup variables we need
  (let ((orig-alist ecc-auto-response-last-time-alist)
        (orig-state ecc-auto-response-active-state))
    
    ;; Set some active state and non-zero timestamps
    (setq ecc-auto-response-active-state :waiting
          ecc-auto-response-last-time-alist
          '((:y/n . 123.0)
            (:y/y/n . 123.0)
            (:waiting . 123.0)
            (:initial-waiting . 123.0)))
    
    ;; Mock the original function
    (cl-letf (((symbol-function 'ecc-stop-auto-response-original)
               (lambda () t)))
      
      ;; Call the stop advice
      (ecc-auto-response-stop-advised 'ecc-stop-auto-response-original)
      
      ;; Check that timestamps were reset
      (should-not ecc-auto-response-active-state)
      (should (equal 0.0 (alist-get :waiting ecc-auto-response-last-time-alist)))
      (should (equal 0.0 (alist-get :y/n ecc-auto-response-last-time-alist))))
    
    ;; Restore original values
    (setq ecc-auto-response-last-time-alist orig-alist
          ecc-auto-response-active-state orig-state)))

;; Test auto-response with new state detection module
(ert-deftest test-ecc-auto-response-with-state-detection ()
  "Test that auto-response works with the new state detection module."
  (let ((orig-enabled ecc-buffer-auto-response-enabled)
        (orig-buffer ecc-buffer-current-buffer)
        (response-sent nil)
        (ecc-auto-response-waiting "/auto"))  ; Set the response value
    (unwind-protect
        (progn
          ;; Set up a mock environment
          (setq ecc-buffer-auto-response-enabled t)
          (save-window-excursion
            (with-temp-buffer
              (setq ecc-buffer-current-buffer (current-buffer))
              
              ;; Mock the state detection functions to return a specific state
              (cl-letf (((symbol-function 'ecc-detect-simple-state) 
                         (lambda () :waiting))
                        ((symbol-function 'ecc-detect-state) 
                         (lambda (&optional _) :waiting))
                        ((symbol-function 'ecc-auto--send-response) 
                         (lambda (buffer response type) 
                           (setq response-sent response)
                           (should (string= response "/auto"))
                           (should (string= type "Continue")))))
                
                ;; Temporarily disable throttling for this test
                (let ((ecc-auto-response-throttle-time 0.0))
                  ;; Call check-and-respond and verify it uses the state detection functions
                  (ecc-check-and-respond-advised)
                  
                  ;; Verify response was sent
                  (should (string= response-sent "/auto")))))))
      
      ;; Restore original values
      (setq ecc-buffer-auto-response-enabled orig-enabled
            ecc-buffer-current-buffer orig-buffer))))

;; Test initial check functionality
(ert-deftest test-ecc-auto-response-initial-check ()
  "Test that initial check for prompts works when auto-response starts."
  (let ((orig-enabled ecc-buffer-auto-response-enabled)
        (orig-buffer ecc-buffer-current-buffer)
        (check-and-respond-called nil))
    (unwind-protect
        (progn
          ;; Set up a mock environment
          (setq ecc-buffer-auto-response-enabled t
                ecc-debug-enabled nil) ; Ensure debugging is off for test
          
          (save-window-excursion
            (with-temp-buffer
              (setq ecc-buffer-current-buffer (current-buffer))
              
              ;; Insert some content resembling Claude's initial prompt
              (insert "Claude is ready\n")
              
              ;; Mock the relevant functions
              (cl-letf (((symbol-function 'ecc-detect-simple-state) 
                         (lambda () nil)) ; First detection returns nothing
                        ((symbol-function 'ecc-check-and-respond) 
                         (lambda () (setq check-and-respond-called t))))
                
                ;; Set up the list of alternative patterns
                (let ((ecc-state-prompt-initial-waiting-alternatives
                       '("Claude is ready" "Ready for your request" "How can I help")))
                
                  ;; Run the initial check function
                  (ecc-auto-response-initial-check)
                  
                  ;; Verify that alternate detection was used and check-and-respond was called
                  (should check-and-respond-called))))))
      
      ;; Restore original values
      (setq ecc-buffer-auto-response-enabled orig-enabled
            ecc-buffer-current-buffer orig-buffer))))

;; Test alternative initial waiting patterns detection
(ert-deftest test-ecc-auto-response-alternative-patterns ()
  "Test that alternative initial waiting patterns are detected."
  (with-temp-buffer
    ;; Set up the buffer with content that should match an alternative pattern
    (insert "Some content here\n")
    (insert "Claude is ready\n")
    
    ;; Make sure the variable is defined
    (let ((ecc-state-prompt-initial-waiting-alternatives
           '("Claude is ready" "Ready for your request" "How can I help")))
      
      ;; Test the function with our buffer text
      (should (ecc-detect-alternative-initial-waiting 
               (buffer-substring-no-properties (point-min) (point-max))))
      
      ;; Test with a non-matching pattern too
      (erase-buffer)
      (insert "This does not match any patterns\n")
      (should-not (ecc-detect-alternative-initial-waiting 
                   (buffer-substring-no-properties (point-min) (point-max)))))))

;; Test the throttling in the advised version of the auto-response send function
(ert-deftest test-ecc-auto-response-send-advised ()
  "Test that the advised version of ecc-auto-response-send handles throttling."
  (let ((orig-alist ecc-auto-response-last-time-alist)
        (orig-state ecc-auto-response-active-state)
        (orig-throttle ecc-auto-response-throttle-time)
        (orig-fn-called nil))
    
    ;; Use a very long throttle time for this test
    (setq ecc-auto-response-throttle-time 100.0)
    
    ;; Set recent timestamp for :waiting state
    (setq ecc-auto-response-last-time-alist
          '((:y/n . 0.0)
            (:y/y/n . 0.0)
            (:waiting . 9999999999.0) ; Very recent timestamp
            (:initial-waiting . 0.0)))
    
    ;; Create a mock buffer
    (save-window-excursion
      (with-temp-buffer
        (let ((test-buffer (current-buffer)))
          
          ;; Mock functions
          (cl-letf (((symbol-function 'ecc-detect-simple-state) 
                     (lambda () :waiting))
                    ((symbol-function 'orig-fun) 
                     (lambda (buffer &optional _) 
                       (setq orig-fn-called t)
                       buffer)))
            
            ;; Call the advised function with waiting state - should be throttled
            (ecc-auto-response-send-advised 'orig-fun test-buffer :waiting)
            
            ;; Original function should NOT be called due to throttling
            (should-not orig-fn-called)
            
            ;; Now try with a different state that isn't throttled
            (setq orig-fn-called nil)
            (ecc-auto-response-send-advised 'orig-fun test-buffer :y/n)
            
            ;; Original function SHOULD be called for unthrottled state
            (should orig-fn-called)))))
    
    ;; Restore original values
    (setq ecc-auto-response-last-time-alist orig-alist
          ecc-auto-response-active-state orig-state
          ecc-auto-response-throttle-time orig-throttle)))

(provide 'test-ecc-auto-response-fix)

;;; test-ecc-auto-response-fix.el ends here