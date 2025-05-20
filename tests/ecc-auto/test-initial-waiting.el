;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 15:35:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-auto/test-initial-waiting.el

;;; Commentary:
;;; Manual test script for initial-waiting detection and auto-response.

;; Load necessary files
(require 'ecc-variables)
(require 'ecc-auto-response)
(when (locate-library "ecc-state-detect-prompt")
  (require 'ecc-state-detect-prompt))
(require 'ecc-auto-response-fix)
(when (locate-library "ecc-auto-response-fix-improved")
  (require 'ecc-auto-response-fix-improved))

;; Set up variables for testing
(setq ecc-debug-enabled t)  ;; Enable debug output

;; Test detection with standard pattern
(defun ecc-test-initial-waiting-detection ()
  "Test detection of initial-waiting state with various patterns."
  (interactive)
  (message "Testing initial-waiting detection...")
  (with-temp-buffer
    ;; Test with standard pattern
    (erase-buffer)
    (insert "Some content here\n")
    (insert "│ > Try \n")
    (let ((state (ecc-detect-simple-state)))
      (message "Standard pattern test: %s (expected: :initial-waiting)" state))
    
    ;; Test with alternative patterns
    (when (boundp 'ecc-state-prompt-initial-waiting-alternatives)
      (dolist (pattern ecc-state-prompt-initial-waiting-alternatives)
        (erase-buffer)
        (insert "Some content here\n")
        (insert pattern)
        (let ((state (cond
                      ((fboundp 'ecc-check-and-respond-improved)
                       (let ((ecc-buffer-current-buffer (current-buffer))
                             (result nil))
                         (cl-letf (((symbol-function 'ecc-auto--send-response)
                                   (lambda (buffer response type)
                                     (setq result type))))
                           (ecc-check-and-respond-improved)
                           result)))
                      (t nil))))
          (message "Alternative pattern '%s' test: %s" pattern state))))))

;; Test auto-response for initial-waiting
(defun ecc-test-initial-waiting-response ()
  "Test auto-response for initial-waiting state."
  (interactive)
  (message "Testing initial-waiting auto-response...")
  (let ((test-buffer (generate-new-buffer "*ecc-test-initial-waiting*")))
    (with-current-buffer test-buffer
      ;; Set up the test buffer
      (insert "Some content here\n")
      (insert "│ > Try \n")
      
      ;; Set up variables for the test
      (setq ecc-buffer-current-buffer test-buffer)
      (setq ecc-buffer-auto-response-enabled t)
      (setq ecc-auto-response-initial-waiting "/user:understand-guidelines")
      
      ;; Mock the send function
      (cl-letf (((symbol-function 'ecc-auto--send-vterm-response)
                 (lambda (response)
                   (message "Would send response: %s" response)
                   (insert "Response sent: " response "\n"))))
        
        ;; Run the tests
        (message "Testing original check-and-respond...")
        (ecc-check-and-respond)
        
        ;; Test improved version if available
        (when (fboundp 'ecc-check-and-respond-improved)
          (message "Testing improved check-and-respond...")
          (ecc-check-and-respond-improved))))
    
    ;; Display results
    (switch-to-buffer test-buffer)
    (message "Test complete. See buffer for results.")))

;; Combined test
(defun ecc-test-initial-waiting-all ()
  "Run all initial-waiting tests."
  (interactive)
  (ecc-test-initial-waiting-detection)
  (ecc-test-initial-waiting-response))

(message "Initial waiting test script loaded. Run tests with M-x ecc-test-initial-waiting-all")

(provide 'test-initial-waiting)

;;; test-initial-waiting.el ends here