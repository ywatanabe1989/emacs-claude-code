;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 10:05:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-auto/test-ecc-interaction-limiter.el

;;; Commentary:
;;; Tests for interaction limits functionality.

(require 'ecc-variables)
(require 'ecc-interaction-tracker)
(require 'ecc-interaction-limiter)
(require 'ert)

;; Test basic limit checking
(ert-deftest test-ecc-interaction-check-limits ()
  "Test basic interaction limit checking."
  ;; Disable limits initially
  (setq ecc-interaction-limit-enabled nil)
  (setq ecc-interaction-limit-per-time-enabled nil)
  
  ;; With limits disabled, should always allow
  (should (ecc-interaction-check-limits))
  
  ;; Enable limits but set high enough to pass
  (setq ecc-interaction-limit-enabled t)
  (setq ecc-interaction-limit-count 100)
  (setq ecc-interaction-counter 50)
  
  ;; Should allow since we're under the limit
  (should (ecc-interaction-check-limits))
  
  ;; Set counter over limit
  (setq ecc-interaction-counter 150)
  (setq ecc-interaction-limit-action 'block)
  
  ;; Should not allow since we're over the limit
  (should-not (ecc-interaction-check-limits))
  
  ;; Reset test state
  (setq ecc-interaction-limit-enabled nil)
  (setq ecc-interaction-counter 0))

;; Test time-based limits
(ert-deftest test-ecc-interaction-time-limits ()
  "Test time-based interaction limits."
  ;; Enable time-based limits
  (setq ecc-interaction-limit-enabled t)
  (setq ecc-interaction-limit-per-time-enabled t)
  (setq ecc-interaction-limit-per-hour 20)
  (setq ecc-interaction-limit-action 'block)
  
  ;; Set up empty timestamps list
  (setq ecc-interaction-timestamps nil)
  
  ;; Should allow with no recent timestamps
  (should (ecc-interaction-check-limits))
  
  ;; Add recent timestamps to simulate hourly activity
  (let ((now (float-time))
        (hour-ago (- (float-time) 3600)))
    ;; Add 25 timestamps within the last hour
    (dotimes (i 25)
      (push (- now (* i 60)) ecc-interaction-timestamps))
    
    ;; Should not allow since we have 25 timestamps in the last hour
    (should-not (ecc-interaction-check-limits))
    
    ;; Add some older timestamps outside the hourly window
    (dotimes (i 10)
      (push (- hour-ago (* i 300)) ecc-interaction-timestamps))
    
    ;; Should still not allow (older timestamps shouldn't affect the hourly count)
    (should-not (ecc-interaction-check-limits)))
  
  ;; Reset test state
  (setq ecc-interaction-limit-enabled nil)
  (setq ecc-interaction-limit-per-time-enabled nil)
  (setq ecc-interaction-timestamps nil))

;; Test limit reached handler
(ert-deftest test-ecc-interaction-limit-handler ()
  "Test that limit handler function is called."
  ;; Set up test handler and limit action
  (let ((handler-called nil)
        (limit-type-received nil)
        (count-received 0)
        (orig-handler ecc-interaction-limit-handler-function)
        (orig-action ecc-interaction-limit-action))
    
    ;; Set our test handler and limit action
    (setq ecc-interaction-limit-handler-function
          (lambda (limit-type count)
            (setq handler-called t
                  limit-type-received limit-type
                  count-received count)))
    (setq ecc-interaction-limit-action 'warn)
    
    ;; Call the handler function
    (ecc-interaction-handle-limit 'count 42)
    
    ;; Verify handler was called with correct arguments
    (should handler-called)
    (should (eq limit-type-received 'count))
    (should (= count-received 42))
    
    ;; Restore original settings
    (setq ecc-interaction-limit-handler-function orig-handler
          ecc-interaction-limit-action orig-action)))


(provide 'test-ecc-interaction-limiter)

;;; test-ecc-interaction-limiter.el ends here