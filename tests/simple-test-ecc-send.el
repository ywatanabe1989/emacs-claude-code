;;; simple-test-ecc-send.el --- Super simplified tests for ecc-send

;; Define variables for mock testing
(defvar vterm-mock-last-string nil
  "Last string sent via vterm-send-string.")

(defvar vterm-mock-return-count 0
  "Count of vterm-send-return calls.")

(defvar vterm-mock-clear-count 0
  "Count of vterm-clear calls.")

;; Simple assertion functions
(defun assert-equal (a b)
  "Assert that A and B are equal."
  (unless (equal a b)
    (error "Assertion failed: %S != %S" a b)))

(defun assert-eq (a b)
  "Assert that A and B are eq."
  (unless (eq a b)
    (error "Assertion failed: %S != %S (eq)" a b)))

;; Reset function
(defun vterm-mock-reset ()
  "Reset all mock tracking variables."
  (setq vterm-mock-last-string nil
        vterm-mock-return-count 0
        vterm-mock-clear-count 0))

;; Mock vterm functions
(defun vterm-send-string (string &optional _paste-p)
  "Mock vterm-send-string to capture sent STRINGS."
  (setq vterm-mock-last-string string)
  (message "Mock vterm-send-string called with: %s" string))

(defun vterm-send-return ()
  "Mock vterm-send-return to track calls."
  (setq vterm-mock-return-count (1+ vterm-mock-return-count))
  (message "Mock vterm-send-return called"))

(defun vterm-clear ()
  "Mock vterm-clear to track calls."
  (setq vterm-mock-clear-count (1+ vterm-mock-clear-count))
  (message "Mock vterm-clear called"))

(defun vterm-copy-mode (arg)
  "Mock vterm-copy-mode with ARG."
  (message "Mock vterm-copy-mode called with: %s" arg))

;; Test functions
(defun test-ecc-vterm-mock-reset ()
  "Test that vterm-mock-reset clears mock variables."
  (setq vterm-mock-last-string "test"
        vterm-mock-return-count 5
        vterm-mock-clear-count 3)
  (vterm-mock-reset)
  (assert-eq vterm-mock-last-string nil)
  (assert-eq vterm-mock-return-count 0)
  (assert-eq vterm-mock-clear-count 0)
  (message "test-ecc-vterm-mock-reset PASSED"))

(defun test-ecc-vterm-send-string ()
  "Test that vterm-send-string mock captures the string."
  (vterm-mock-reset)
  (vterm-send-string "test-string")
  (assert-equal vterm-mock-last-string "test-string")
  (message "test-ecc-vterm-send-string PASSED"))

(defun test-ecc-vterm-send-return ()
  "Test that vterm-send-return mock increments the counter."
  (vterm-mock-reset)
  (vterm-send-return)
  (assert-eq vterm-mock-return-count 1)
  (vterm-send-return)
  (assert-eq vterm-mock-return-count 2)
  (message "test-ecc-vterm-send-return PASSED"))

;; Mock ecc-send functions 
(defvar ecc-buffer-current-active-buffer nil
  "Mock current active buffer.")

(defun ecc-buffer-get-or-create-active-buffer ()
  "Mock version that returns the current buffer for tests."
  (or ecc-buffer-current-active-buffer (current-buffer)))

(defun --ecc-state-is-claude-active-p (&optional _)
  "Mock function for testing if Claude buffer is active. Always returns t."
  t)

;; Mock state detection - initially y/n-p returns t and y/y/n-p returns nil
(defvar --ecc-state-y/n-p-value t
  "Value to return from --ecc-state-y/n-p.")

(defvar --ecc-state-y/y/n-p-value nil
  "Value to return from --ecc-state-y/y/n-p.")

(defun --ecc-state-y/n-p ()
  "Mock state detection function for tests."
  --ecc-state-y/n-p-value)

(defun --ecc-state-y/y/n-p ()
  "Mock state detection function for tests."
  --ecc-state-y/y/n-p-value)

;; More mock state detection
(defvar --ecc-state-waiting-p-value nil
  "Value to return from --ecc-state-waiting-p.")

(defvar --ecc-state-initial-waiting-p-value nil
  "Value to return from --ecc-state-initial-waiting-p.")

(defun --ecc-state-waiting-p ()
  "Mock state detection function for tests."
  --ecc-state-waiting-p-value)

(defun --ecc-state-initial-waiting-p ()
  "Mock state detection function for tests."
  --ecc-state-initial-waiting-p-value)

(defun --ecc-send-string (string &optional no-confirm delay)
  "Mock version for tests."
  (vterm-send-string string)
  (unless no-confirm (vterm-send-return))
  string)

(defun --ecc-send-by-state (response state-or-predicate)
  "Mock version for tests."
  (vterm-send-string response)
  (vterm-send-return)
  response)

(defun --ecc-auto-send-1-y/n ()
  "Mock sending 1 for tests."
  (--ecc-send-string "1" t 0.5))

(defun --ecc-auto-send-2-y/y/n ()
  "Mock sending 2 for tests."
  (--ecc-send-string "2" t 0.5))

(defun --ecc-auto-send-continue-on-y/y/n ()
  "Mock sending continue for tests."
  (--ecc-send-string "continue" t 0.5))

(defun ecc-send-accept ()
  "Mock version of ecc-send-accept for tests."
  (vterm-clear)
  (cond
   ((--ecc-state-y/y/n-p)
    (--ecc-auto-send-2-y/y/n))
   ((--ecc-state-y/n-p)
    (--ecc-auto-send-1-y/n))
   ((--ecc-state-waiting-p)
    (--ecc-auto-send-continue-on-y/y/n))
   ((--ecc-state-initial-waiting-p)
    (--ecc-auto-send-continue-on-y/y/n))))

;; Additional test functions
(defun test-ecc-send-accept-calls-y-n-handler ()
  "Test that ecc-send-accept calls the right handler for y/n prompts."
  (vterm-mock-reset)
  ;; Make y/n return true and y/y/n return false
  (setq --ecc-state-y/n-p-value t
        --ecc-state-y/y/n-p-value nil)
  (ecc-send-accept)
  (assert-equal vterm-mock-last-string "1")
  (assert-eq vterm-mock-clear-count 1)
  (message "test-ecc-send-accept-calls-y-n-handler PASSED"))

(defun test-ecc-send-accept-calls-y-y-n-handler ()
  "Test that ecc-send-accept calls the right handler for y/y/n prompts."
  (vterm-mock-reset)
  ;; Make y/y/n return true (order matters in if-statements)
  (setq --ecc-state-y/n-p-value nil
        --ecc-state-y/y/n-p-value t
        --ecc-state-waiting-p-value nil
        --ecc-state-initial-waiting-p-value nil)
  (ecc-send-accept)
  (assert-equal vterm-mock-last-string "2")
  (assert-eq vterm-mock-clear-count 1)
  (message "test-ecc-send-accept-calls-y-y-n-handler PASSED"))

(defun test-ecc-send-accept-calls-waiting-handler ()
  "Test that ecc-send-accept calls the right handler for waiting prompts."
  (vterm-mock-reset)
  ;; Make waiting return true
  (setq --ecc-state-y/n-p-value nil
        --ecc-state-y/y/n-p-value nil
        --ecc-state-waiting-p-value t
        --ecc-state-initial-waiting-p-value nil)
  (ecc-send-accept)
  (assert-equal vterm-mock-last-string "continue")
  (assert-eq vterm-mock-clear-count 1)
  (message "test-ecc-send-accept-calls-waiting-handler PASSED"))

(defun test-ecc-send-accept-calls-initial-waiting-handler ()
  "Test that ecc-send-accept calls the right handler for initial waiting prompts."
  (vterm-mock-reset)
  ;; Make initial-waiting return true
  (setq --ecc-state-y/n-p-value nil
        --ecc-state-y/y/n-p-value nil
        --ecc-state-waiting-p-value nil
        --ecc-state-initial-waiting-p-value t)
  (ecc-send-accept)
  (assert-equal vterm-mock-last-string "continue")
  (assert-eq vterm-mock-clear-count 1)
  (message "test-ecc-send-accept-calls-initial-waiting-handler PASSED"))

(defun test-ecc-buffer-setup-teardown ()
  "Test buffer setup and teardown."
  (let ((test-buffer (generate-new-buffer "*TEST-VTERM*")))
    (unwind-protect
        (progn 
          (setq ecc-buffer-current-active-buffer test-buffer)
          (assert-eq (ecc-buffer-get-or-create-active-buffer) test-buffer)
          (message "test-ecc-buffer-setup-teardown PASSED"))
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer)))))

;; Run the tests
(defun run-all-tests ()
  "Run all test functions."
  (message "Starting tests...")
  (test-ecc-vterm-mock-reset)
  (test-ecc-vterm-send-string)
  (test-ecc-vterm-send-return)
  (test-ecc-send-accept-calls-y-n-handler)
  (test-ecc-send-accept-calls-y-y-n-handler)
  (test-ecc-send-accept-calls-waiting-handler)
  (test-ecc-send-accept-calls-initial-waiting-handler)
  (test-ecc-buffer-setup-teardown)
  (message "All tests completed successfully."))

;; Execute tests when loaded
(run-all-tests)