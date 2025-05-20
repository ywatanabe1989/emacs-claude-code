;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 14:48:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-auto/ecc-auto-response-test.el

;;; Commentary:
;;; Tests for auto-response functionality.

(require 'ert)
(require 'ecc-auto-response)
(require 'ecc-variables)

(ert-deftest ecc-auto-response-test-first-interaction ()
  "Test that first interaction gets special handling."
  ;; Reset the interaction counter for the test
  (let ((ecc-interaction-counter 0)
        (ecc-interaction-timestamps nil))
    
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
        (ecc-interaction-timestamps nil))
    
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

(provide 'ecc-auto-response-test)

;;; ecc-auto-response-test.el ends here