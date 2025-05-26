;;; test-auto-response-debug.el --- Debug test for auto-response functionality

;;; Commentary:
;;; Simple test to verify auto-response is working with debug output

(require 'ecc-variables)
(require 'ecc-state-detection)
(require 'ecc-debug-utils)
(require 'ecc-auto-core)
(require 'ecc-vterm-utils)
(require 'ecc-auto-response)

;; Enable debug output
(setq ecc-auto-response-debug t)

;; Test 1: Basic state detection
(defun test-state-detection ()
  "Test if state detection is working."
  (message "\n=== TEST 1: State Detection ===")
  (with-temp-buffer
    (insert "Some text\n")
    (insert "❯ 1. Yes / 2. No\n")
    (let ((state (ecc-detect-state)))
      (message "Detected state: %s" state)
      (message "Expected: :y/n"))))

;; Test 2: Auto-response registration and processing
(defun test-auto-response-basic ()
  "Test basic auto-response functionality."
  (message "\n=== TEST 2: Auto-Response Basic ===")
  (let ((test-buffer (get-buffer-create "*test-auto-response*")))
    (with-current-buffer test-buffer
      ;; Add some prompt text
      (insert "Claude says something\n")
      (insert "❯ 1. Yes / 2. No\n")
      
      ;; Register buffer
      (ecc-auto-response-register-buffer test-buffer)
      
      ;; Start auto-response
      (ecc-auto-response-start)
      
      ;; Manually trigger processing to see debug output
      (message "Manually triggering buffer processing...")
      (ecc-auto-response--process-all-buffers)
      
      ;; Clean up
      (ecc-auto-response-stop))
    (kill-buffer test-buffer)))

;; Test 3: Pattern matching
(defun test-pattern-matching ()
  "Test if patterns are matching correctly."
  (message "\n=== TEST 3: Pattern Matching ===")
  (message "Y/N pattern: %S" ecc-state-prompt-y/n)
  (message "Y/Y/N pattern: %S" ecc-state-prompt-y/y/n)
  (message "Waiting pattern: %S" ecc-state-prompt-waiting)
  (message "Initial waiting pattern: %S" ecc-state-prompt-initial-waiting)
  
  (let ((test-texts '("❯ 1. Yes / 2. No"
                      " 2. Yes, and continue"
                      "│ >                            "
                      "│ > Try ")))
    (dolist (text test-texts)
      (with-temp-buffer
        (insert text)
        (let ((state (ecc-detect-state)))
          (message "Text: %S -> State: %s" text state))))))

;; Run all tests
(defun run-all-tests ()
  "Run all debug tests."
  (interactive)
  (message "Starting auto-response debug tests...")
  (test-state-detection)
  (test-pattern-matching)
  (test-auto-response-basic)
  (message "\nTests completed. Check messages for debug output."))

;; Run tests
(run-all-tests)

;;; test-auto-response-debug.el ends here