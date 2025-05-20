;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 19:00:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-auto/test-initial-waiting-updated.el

;;; Commentary:
;;; Manual test script for initial-waiting detection and auto-response.

;; Load necessary files
(require 'ecc-variables-refactored)
(require 'ecc-auto-response-refactored)
(require 'ecc-state-detection-refactored)

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
    (let ((state (ecc-detect-state)))
      (message "Standard pattern test: %s (expected: :initial-waiting)" state))
    
    ;; Test with alternative patterns
    (when (boundp 'ecc-state-prompt-initial-waiting-alternatives)
      (dolist (pattern ecc-state-prompt-initial-waiting-alternatives)
        (erase-buffer)
        (insert "Some content here\n")
        (insert pattern)
        (let ((state (let ((ecc-buffer-current-buffer (current-buffer))
                           (result nil))
                       (cl-letf (((symbol-function 'ecc-auto-response-send-message)
                                 (lambda (buffer response type)
                                   (setq result type))))
                         (ecc-auto-response-check)
                         result))))
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
      (cl-letf (((symbol-function 'ecc-auto-response-send-to-vterm)
                 (lambda (response)
                   (message "Would send response: %s" response)
                   (insert "Response sent: " response "\n"))))
        
        ;; Run the tests
        (message "Testing auto-response check...")
        (ecc-auto-response-check)))
    
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

(provide 'test-initial-waiting-updated)

;;; test-initial-waiting-updated.el ends here