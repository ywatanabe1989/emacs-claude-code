;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 18:57:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-auto/test-notification-format-updated.el

;;; Commentary:
;;; Test enhanced notification format in auto-response system.

(require 'ert)
(require 'ecc-variables-refactored)
(require 'ecc-auto-response-refactored)

;; Test improved notification format
(ert-deftest test-ecc-auto-notify-format ()
  "Test improved notification format with actual response string."
  (let ((type "Y/N")
        (response "1")
        (message-text nil))
    
    ;; Redirect messages to a variable
    (cl-letf (((symbol-function 'message)
              (lambda (fmt &rest args)
                (setq message-text (apply #'format fmt args)))))
      
      ;; Call the notification function with our test values
      (ecc-auto-response-display-notification type response)
      
      ;; Check the output contains both type and response
      (should (string-match-p (regexp-quote type) message-text))
      (should (string-match-p (regexp-quote response) message-text))
      (should (string-match-p "Auto-responded:" message-text)))))

;; Test response function passes both parameters correctly
(ert-deftest test-ecc-auto-send-response-notification ()
  "Test that ecc-auto-response-send-message passes both parameters to notify."
  (let ((buffer (current-buffer))
        (type "TEST")
        (response "test-response")
        (called-type nil)
        (called-response nil))
    
    ;; Mock functions to avoid actual sending
    (cl-letf (((symbol-function 'ecc-auto-response-send-to-vterm)
               (lambda (_) t))
              ((symbol-function 'ecc-auto-response-display-notification)
               (lambda (t r)
                 (setq called-type t)
                 (setq called-response r))))
      
      ;; Ensure notification is enabled
      (let ((ecc-auto-notify-completions t))
        ;; Call the function
        (ecc-auto-response-send-message buffer response type)
        
        ;; Check that notify was called with correct params
        (should (equal called-type type))
        (should (equal called-response response))))))

;; Run the tests when evaluating the file
(ert-run-tests-batch-and-exit)

(provide 'test-notification-format-updated)

;;; test-notification-format-updated.el ends here