;;; test-ecc-term-claude-auto.el --- Tests for term-claude auto-responses -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Created: 2025-05-21
;; Version: 1.0.0
;; Keywords: convenience, testing

;;; Commentary:
;; Unit tests for term-claude auto-response functions.
;; These tests verify that automatic responses are sent correctly
;; for various Claude prompt types and scenarios.

;;; Code:

(require 'ert)
(require 'ecc-term-claude-state)
(require 'ecc-variables)

;; Load the mock version of ecc-term-claude-auto.el for testing
(load-file (expand-file-name "mock-ecc-term-claude-auto.el" 
                             (file-name-directory load-file-name)))

;; Define mock mode for testing
(defvar vterm-mode-map (make-sparse-keymap))
(define-derived-mode vterm-mode fundamental-mode "VTerm"
  "Major mode for vterm terminal emulator.")
(defvar-local vterm--term nil)
(defun vterm-clear ()
  "Mock function to clear the terminal."
  (message "Terminal cleared"))

;;;; Test Utilities

(defun ecc-test-with-temp-buffer (content &rest body)
  "Create a temporary buffer with CONTENT and execute BODY in it.
Automatically cleans up buffer after execution."
  (let ((temp-buffer (generate-new-buffer "*ecc-test*")))
    (unwind-protect
        (progn
          (with-current-buffer temp-buffer
            (insert content)
            (setq major-mode 'vterm-mode)  ;; Simulate vterm-mode without actually running it
            (apply #'funcall body)))
      (kill-buffer temp-buffer))))

;;;; Response Mapping Tests

(ert-deftest test-ecc-term-claude-auto-response-map ()
  "Test that the auto-response map has the correct entries."
  (should (assq :y/n ecc-term-claude-auto-response-map))
  (should (assq :y/y/n ecc-term-claude-auto-response-map))
  (should (assq :waiting ecc-term-claude-auto-response-map))
  (should (assq :initial-waiting ecc-term-claude-auto-response-map))
  
  ;; Test the mapping values
  (should (eq (cdr (assq :y/n ecc-term-claude-auto-response-map)) 'ecc-auto-response-y/n))
  (should (eq (cdr (assq :y/y/n ecc-term-claude-auto-response-map)) 'ecc-auto-response-y/y/n))
  (should (eq (cdr (assq :waiting ecc-term-claude-auto-response-map)) 'ecc-auto-response-waiting))
  (should (eq (cdr (assq :initial-waiting ecc-term-claude-auto-response-map)) 'ecc-auto-response-initial-waiting)))

;;;; Auto-Send Tests

(ert-deftest test-ecc-term-claude-auto-send-y-n-vterm ()
  "Test auto-sending for Y/N prompts."
  (let* ((sent-string nil)
         (sent-return nil)
         ;; Mock functions for verification
         (vterm-send-string-orig (symbol-function 'vterm-send-string))
         (vterm-send-return-orig (symbol-function 'vterm-send-return)))
    
    (cl-letf (((symbol-function 'vterm-send-string)
               (lambda (str) (setq sent-string str)))
              ((symbol-function 'vterm-send-return)
               (lambda () (setq sent-return t))))
      
      ;; Test sending with the default response
      (let ((ecc-auto-response-y/n "y"))
        (should (ecc-term-claude-auto-send :y/n))
        (should (equal sent-string "y"))
        (should sent-return))
      
      ;; Test sending with a custom response
      (let ((ecc-auto-response-y/n "yep"))
        (setq sent-string nil)
        (setq sent-return nil)
        (should (ecc-term-claude-auto-send :y/n))
        (should (equal sent-string "yep"))
        (should sent-return)))
    
    ;; Restore original functions
    (fset 'vterm-send-string vterm-send-string-orig)
    (fset 'vterm-send-return vterm-send-return-orig)))

(ert-deftest test-ecc-term-claude-auto-send-y-y-n-vterm ()
  "Test auto-sending for Y/Y/N prompts."
  (let* ((sent-string nil)
         (sent-return nil)
         ;; Mock functions for verification
         (vterm-send-string-orig (symbol-function 'vterm-send-string))
         (vterm-send-return-orig (symbol-function 'vterm-send-return)))
    
    (cl-letf (((symbol-function 'vterm-send-string)
               (lambda (str) (setq sent-string str)))
              ((symbol-function 'vterm-send-return)
               (lambda () (setq sent-return t))))
      
      ;; Test sending with the default response
      (let ((ecc-auto-response-y/y/n "y"))
        (should (ecc-term-claude-auto-send :y/y/n))
        (should (equal sent-string "y"))
        (should sent-return)))
    
    ;; Restore original functions
    (fset 'vterm-send-string vterm-send-string-orig)
    (fset 'vterm-send-return vterm-send-return-orig)))

(ert-deftest test-ecc-term-claude-auto-send-waiting-vterm ()
  "Test auto-sending for waiting prompts."
  (let* ((sent-string nil)
         (sent-return nil)
         ;; Mock functions for verification
         (vterm-send-string-orig (symbol-function 'vterm-send-string))
         (vterm-send-return-orig (symbol-function 'vterm-send-return)))
    
    (cl-letf (((symbol-function 'vterm-send-string)
               (lambda (str) (setq sent-string str)))
              ((symbol-function 'vterm-send-return)
               (lambda () (setq sent-return t))))
      
      ;; Test sending with the default empty response
      ;; (typically just enter/return key)
      (let ((ecc-auto-response-waiting ""))
        (should (ecc-term-claude-auto-send :waiting))
        (should (equal sent-string ""))
        (should sent-return)))
    
    ;; Restore original functions
    (fset 'vterm-send-string vterm-send-string-orig)
    (fset 'vterm-send-return vterm-send-return-orig)))

(ert-deftest test-ecc-term-claude-auto-send-invalid-state-vterm ()
  "Test auto-sending with an invalid state."
  ;; Should raise an error for invalid states
  (should-error (ecc-term-claude-auto-send :invalid-state) :type 'error))

;;;; Auto-Mode Tests

(ert-deftest test-ecc-term-claude-toggle-auto-mode-vterm ()
  "Test toggling the auto-mode."
  ;; Ensure the mode is initially off
  (let ((ecc-term-claude-auto-mode nil)
        (ecc-term-claude-update-functions nil))
    
    ;; Toggle on
    (ecc-term-claude-toggle-auto-mode)
    (should ecc-term-claude-auto-mode)
    (should (member 'ecc-term-claude-auto-send-accept ecc-term-claude-update-functions))
    
    ;; Toggle off
    (ecc-term-claude-toggle-auto-mode)
    (should-not ecc-term-claude-auto-mode)
    (should-not (member 'ecc-term-claude-auto-send-accept ecc-term-claude-update-functions))))

(ert-deftest test-ecc-term-claude-auto-send-accept-vterm ()
  "Test the auto-send-accept function."
  (let* ((ecc-term-claude-auto-mode t)
         (auto-response-called nil)
         (orig-auto-send (symbol-function 'ecc-term-claude-auto-send)))
    
    ;; Mock the state detection and auto-send functions
    (cl-letf (((symbol-function 'ecc-term-claude-get-state)
               (lambda () :y/n))
              ((symbol-function 'ecc-term-claude-auto-send)
               (lambda (state) (setq auto-response-called state) t)))
      
      ;; Test the auto-response function
      (should (ecc-term-claude-auto-send-accept))
      (should (eq auto-response-called :y/n)))
    
    ;; Test with auto-mode disabled
    (setq ecc-term-claude-auto-mode nil)
    (setq auto-response-called nil)
    (should-not (ecc-term-claude-auto-send-accept))
    (should-not auto-response-called)
    
    ;; Restore the original function
    (fset 'ecc-term-claude-auto-send orig-auto-send)))

;;;; Backward Compatibility Tests

(ert-deftest test-ecc-term-claude-auto-send-compatibility ()
  "Test backward compatibility functions."
  (let* ((auto-response-called nil)
         (orig-auto-send (symbol-function 'ecc-term-claude-auto-send)))
    
    ;; Mock the auto-send function
    (cl-letf (((symbol-function 'ecc-term-claude-auto-send)
               (lambda (state) (setq auto-response-called state) t)))
      
      ;; Test the compatibility functions
      (ecc-term-claude-auto-send-y/n)
      (should (eq auto-response-called :y/n))
      
      (setq auto-response-called nil)
      (ecc-term-claude-auto-send-y/y/n)
      (should (eq auto-response-called :y/y/n))
      
      (setq auto-response-called nil)
      (ecc-term-claude-auto-send-continue)
      (should (eq auto-response-called :waiting))
      
      (setq auto-response-called nil)
      (ecc-term-claude-auto-send-initial-waiting)
      (should (eq auto-response-called :initial-waiting)))
    
    ;; Check that the alias is defined
    (should (fboundp 'ecc-term-claude-auto-mode-toggle))
    
    ;; Restore the original function
    (fset 'ecc-term-claude-auto-send orig-auto-send)))

(provide 'test-ecc-term-claude-auto)

;;; test-ecc-term-claude-auto.el ends here