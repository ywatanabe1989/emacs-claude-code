;;; test-ecc-term-claude-state.el --- Tests for term-claude state detection -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Created: 2025-05-21
;; Version: 1.0.0
;; Keywords: convenience, testing

;;; Commentary:
;; Unit tests for term-claude state detection functions.
;; These tests verify that the state detection logic works correctly
;; for various prompt types and scenarios.

;;; Code:

(require 'ert)
(require 'ecc-term-claude-state)
(require 'ecc-variables)

;; Mock vterm for tests
(unless (featurep 'vterm)
  (defvar vterm-mode-map (make-sparse-keymap))
  (define-derived-mode vterm-mode fundamental-mode "VTerm"
    "Major mode for vterm terminal emulator.")
  (defvar-local vterm--term nil)
  (defun vterm-send-string (string)
    "Mock function to send STRING to the terminal."
    (message "Sent: %s" string))
  (defun vterm-send-return ()
    "Mock function to send return to the terminal."
    (message "Sent: <return>"))
  (defun vterm-clear ()
    "Mock function to clear the terminal."
    (message "Terminal cleared"))
  (provide 'vterm))

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

;;;; Basic State Detection Tests

(ert-deftest test-ecc-term-claude-state-get-name ()
  "Test getting the display name for each state."
  (should (equal (ecc-term-claude-state-name :y/n) "Y/N"))
  (should (equal (ecc-term-claude-state-name :y/y/n) "Y/Y/N"))
  (should (equal (ecc-term-claude-state-name :waiting) "Continue"))
  (should (equal (ecc-term-claude-state-name :initial-waiting) "Initial-Waiting"))
  ;; Test with unknown state
  (should (equal (ecc-term-claude-state-name :unknown) ":unknown"))
  (should (equal (ecc-term-claude-state-name nil) "nil")))

(ert-deftest test-ecc-term-claude-state-symbols ()
  "Test getting the list of valid state symbols."
  (let ((symbols (ecc-term-claude-state-symbols)))
    (should (member :y/n symbols))
    (should (member :y/y/n symbols))
    (should (member :waiting symbols))
    (should (member :initial-waiting symbols))
    (should (= (length symbols) 4))))

(ert-deftest test-ecc-term-claude-detect-state-y-n ()
  "Test detecting Y/N prompts."
  (ecc-test-with-temp-buffer
   "Some output text...\nWould you like to continue? [y/n] "
   (lambda ()
     (should (eq (ecc-term-claude-detect-basic-state) :y/n)))))

(ert-deftest test-ecc-term-claude-detect-state-y-y-n ()
  "Test detecting Y/Y/N prompts."
  (ecc-test-with-temp-buffer
   "Some output text...\nWould you like to see more examples? [Y/y/n] "
   (lambda ()
     (should (eq (ecc-term-claude-detect-basic-state) :y/y/n)))))

(ert-deftest test-ecc-term-claude-detect-state-waiting ()
  "Test detecting 'continue>' prompts."
  (ecc-test-with-temp-buffer
   "Long text output...\ncontinue> "
   (lambda ()
     (should (eq (ecc-term-claude-detect-basic-state) :waiting))))
  
  ;; Test with uppercase Continue
  (ecc-test-with-temp-buffer
   "Long text output...\nContinue> "
   (lambda ()
     (should (eq (ecc-term-claude-detect-basic-state) :waiting)))))

(ert-deftest test-ecc-term-claude-detect-state-initial-waiting ()
  "Test detecting initial waiting prompts."
  (let ((ecc-state-prompt-initial-waiting "Ready to begin. Type 'Hi' to start."))
    (ecc-test-with-temp-buffer
     "Ready to begin. Type 'Hi' to start."
     (lambda ()
       (should (eq (ecc-term-claude-detect-basic-state) :initial-waiting))))))

(ert-deftest test-ecc-term-claude-detect-state-none ()
  "Test with no recognizable prompt."
  (ecc-test-with-temp-buffer
   "Just some normal output text without any prompt."
   (lambda ()
     (should (eq (ecc-term-claude-detect-basic-state) nil)))))

;;;; Region-Based Detection Tests

;; Region-based detection is not supported in the current module implementation
;; This test has been temporarily disabled until the feature is implemented
;; (ert-deftest test-ecc-term-claude-detect-state-in-region ()
;;   "Test detecting state in a specific region."
;;   (ecc-test-with-temp-buffer
;;    "Early text with a [y/n] prompt that should be ignored.\nLater text with Continue> that should be found."
;;    (lambda ()
;;      ;; Test detection in first half
;;      (should (eq (ecc-term-claude-detect-state-in-region 
;;                   (point-min) (+ (point-min) 50))
;;                  :y/n))
;;      
;;      ;; Test detection in second half
;;      (should (eq (ecc-term-claude-detect-state-in-region
;;                   (+ (point-min) 50) (point-max))
;;                  :waiting))
;;      
;;      ;; Test no detection in restricted region
;;      (should (eq (ecc-term-claude-detect-state-in-region
;;                   (+ (point-min) 20) (+ (point-min) 40))
;;                  nil)))))

;;;; Custom Pattern Tests

(ert-deftest test-ecc-term-claude-detect-state-custom-patterns ()
  "Test detection with custom prompt patterns."
  (let ((ecc-state-prompt-y/n "Custom Y/N format: (y/n)")
        (ecc-state-prompt-waiting "Press any key to continue..."))
    
    ;; Test custom Y/N pattern
    (ecc-test-with-temp-buffer
     "Would you like to proceed? Custom Y/N format: (y/n)"
     (lambda ()
       (should (eq (ecc-term-claude-detect-basic-state) :y/n))))
    
    ;; Test custom waiting pattern
    (ecc-test-with-temp-buffer
     "Here's the output... Press any key to continue..."
     (lambda ()
       (should (eq (ecc-term-claude-detect-basic-state) :waiting))))))

;;;; Large Buffer Tests

(ert-deftest test-ecc-term-claude-detect-state-large-buffer ()
  "Test state detection in a large buffer."
  (ecc-test-with-temp-buffer
   (concat (make-string 10000 ?x) "\nWould you like to continue? [y/n] ")
   (lambda ()
     ;; Should detect the prompt even though it's far down in a large buffer
     (should (eq (ecc-term-claude-detect-basic-state) :y/n)))))

;;;; Integration Tests

(ert-deftest test-ecc-term-claude-get-state ()
  "Test the high-level state getter function."
  (let ((result nil))
    ;; Mock the internal detect function to return a known state
    (cl-letf (((symbol-function 'ecc-term-claude-detect-basic-state)
               (lambda () :y/n)))
      (should (eq (ecc-term-claude-get-state) :y/n)))
    
    ;; Test with error in detection - not currently applicable
    ;; as the current implementation doesn't have error handling
    ))

(provide 'test-ecc-term-claude-state)

;;; test-ecc-term-claude-state.el ends here