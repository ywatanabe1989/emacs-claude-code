;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 14:48:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-auto/ecc-auto-response-test.el

;;; Commentary:
;;; Tests for auto-response functionality.

(require 'ert)
(require 'ecc-auto-response)
(require 'ecc-variables)

(when (featurep 'ecc-state-detect-prompt)
  (require 'ecc-state-detect-prompt))

(ert-deftest ecc-auto-response-test-first-interaction ()
  "Test that first interaction gets special handling."
  ;; Reset the interaction counter for the test
  (let ((ecc-interaction-counter 0)
        (ecc-interaction-timestamps nil)
        (ecc-debug-enabled nil)) ; Ensure debugging is off for test
    
    ;; Mock vterm-send-string for testing
    (cl-letf (((symbol-function 'vterm-send-string)
               (lambda (str) str))
              ((symbol-function 'vterm-send-return)
               (lambda () t))
              ((symbol-function 'sit-for)
               (lambda (seconds) t)))
      
      ;; Call the function we're testing
      (ecc-auto--send-vterm-response "test")
      
      ;; Check if interaction counter was incremented
      (should (= ecc-interaction-counter 1))
      
      ;; Check if timestamp was recorded
      (should (= (length ecc-interaction-timestamps) 1)))))

(ert-deftest ecc-auto-response-test-subsequent-interactions ()
  "Test that subsequent interactions use standard timing."
  ;; Set counter to indicate not the first interaction
  (let ((ecc-interaction-counter 5)
        (ecc-interaction-timestamps nil)
        (ecc-debug-enabled nil)) ; Ensure debugging is off for test
    
    ;; Mock vterm-send-string for testing
    (cl-letf (((symbol-function 'vterm-send-string)
               (lambda (str) str))
              ((symbol-function 'vterm-send-return)
               (lambda () t))
              ((symbol-function 'sit-for)
               (lambda (seconds) 
                 ;; Verify that normal delay is used
                 (should (= seconds 1.0))
                 t)))
      
      ;; Call the function we're testing
      (ecc-auto--send-vterm-response "test")
      
      ;; Counter should not be incremented on subsequent calls
      (should (= ecc-interaction-counter 5))
      
      ;; No timestamp should be recorded for non-first interactions
      (should (= (length ecc-interaction-timestamps) 0)))))

(ert-deftest ecc-auto-response-test-initial-waiting-detection ()
  "Test that initial-waiting state is properly detected."
  (with-temp-buffer
    ;; Set up a buffer with content that should match the initial waiting pattern
    (insert "Some content here\n")
    (insert "│ > Try \n")
    
    ;; Test standard detection function if available
    (when (fboundp 'ecc-detect-prompt-in-last-lines)
      (should (eq (ecc-detect-prompt-in-last-lines) :initial-waiting)))
    
    ;; Test the simple state detection used by auto-response
    (should (eq (ecc-detect-simple-state) :initial-waiting))))

(ert-deftest ecc-auto-response-test-initial-waiting-response ()
  "Test that auto-response sends correct response for initial-waiting state."
  (with-temp-buffer
    ;; Set up the buffer for auto-response
    (let ((ecc-buffer-current-buffer (current-buffer))
          (ecc-buffer-auto-response-enabled t)
          (ecc-auto-response-initial-waiting "/user:understand-guidelines")
          (response-sent nil))
      
      ;; Mock detection function to always return initial-waiting
      (cl-letf (((symbol-function 'ecc-detect-simple-state)
                 (lambda () :initial-waiting))
                ((symbol-function 'ecc-auto--send-response)
                 (lambda (buffer response type) 
                   (setq response-sent response)
                   (should (string= response "/user:understand-guidelines"))
                   (should (string= type "Initial-Waiting")))))
        
        ;; Call the check and respond function
        (ecc-check-and-respond)
        
        ;; Verify response was sent
        (should (string= response-sent "/user:understand-guidelines"))))))

(provide 'ecc-auto-response-test)

;;; ecc-auto-response-test.el ends here