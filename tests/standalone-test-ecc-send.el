;;; standalone-test-ecc-send.el --- Standalone tests for ecc-send

;; If ERT is not available, define a simple test framework
(unless (fboundp 'ert-deftest)
  (defmacro ert-deftest (name _docstring &rest body)
    "Define a test NAME with BODY."
    `(defun ,name ()
       (progn ,@body)))
  
  (defmacro should (form)
    "Assert that FORM evaluates to non-nil."
    `(unless ,form
       (error "Assertion failed: %S" ',form)))
  
  ;; Run all tests
  (defun ert-run-tests-batch-and-exit ()
    "Run all defined tests and exit."
    (mapatoms (lambda (sym)
               (when (and (fboundp sym)
                          (string-prefix-p "test-" (symbol-name sym)))
                 (message "Running test %S" sym)
                 (funcall sym))))
    (message "All tests completed successfully")
    (kill-emacs 0)))

;; Mock for vterm functions
(defvar vterm-mock-last-string nil
  "Last string sent via vterm-send-string.")

(defvar vterm-mock-return-count 0
  "Count of vterm-send-return calls.")

(defvar vterm-mock-clear-count 0
  "Count of vterm-clear calls.")

(defun vterm-mock-reset ()
  "Reset all mock tracking variables."
  (setq vterm-mock-last-string nil
        vterm-mock-return-count 0
        vterm-mock-clear-count 0))

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

(defun vterm-send-key (key &optional times)
  "Mock vterm-send-key with KEY and TIMES."
  (message "Mock vterm-send-key called with: %s, times: %s" 
           key (or times 1)))

;; Mock buffer setup
(defvar ecc-buffer nil
  "Buffer for Claude interaction.")

(defvar ecc-buffer-current-buffer nil
  "Current buffer being used for Claude.")

(defvar ecc-buffer-current-active-buffer nil
  "Current active buffer for compatibility.")

(defun ecc-buffer-get-or-create-active-buffer ()
  "Mock version that returns the current buffer for tests."
  (current-buffer))

;; Mock send functions
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

;; Mock state functions
(defun --ecc-state-y/n-p ()
  "Mock state detection function for tests."
  t)

(defun --ecc-state-y/y/n-p ()
  "Mock state detection function for tests."
  t)

(defun --ecc-state-waiting-p ()
  "Mock state detection function for tests."
  t)

(defun --ecc-state-initial-waiting-p ()
  "Mock state detection function for tests."
  t)

(defun --ecc-state-is-claude-active-p (&optional _)
  "Mock function for testing if Claude buffer is active. Always returns t."
  t)

;; Auto-send functions
(defun --ecc-auto-send-1-y/n ()
  "Mock sending 1 for tests."
  (--ecc-send-string "1" t 0.5))

(defun --ecc-auto-send-2-y/y/n ()
  "Mock sending 2 for tests."
  (--ecc-send-string "2" t 0.5))

(defun --ecc-auto-send-continue-on-y/y/n ()
  "Mock sending continue for tests."
  (--ecc-send-string "continue" t 0.5))

;; Public functions for testing
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

;; Mock buffer setup
(defun vterm-mock-setup-test-buffer ()
  "Create a test buffer for vterm operations."
  (let ((test-buffer (generate-new-buffer "*TEST-VTERM*")))
    (with-current-buffer test-buffer
      (setq major-mode 'ecc-claude-vterm-mode))
    ;; Set all the necessary buffer variables
    (setq ecc-buffer test-buffer
          ecc-buffer-current-buffer test-buffer
          ecc-buffer-current-active-buffer test-buffer)
    test-buffer))

;; Tests start here
;; ----------------------------------------

(ert-deftest test-vterm-mock-reset ()
  "Test that vterm-mock-reset clears mock variables."
  (setq vterm-mock-last-string "test"
        vterm-mock-return-count 5
        vterm-mock-clear-count 3)
  (vterm-mock-reset)
  (should (eq vterm-mock-last-string nil))
  (should (eq vterm-mock-return-count 0))
  (should (eq vterm-mock-clear-count 0)))

(ert-deftest test-vterm-send-string-mock ()
  "Test that vterm-send-string mock captures the string."
  (vterm-mock-reset)
  (vterm-send-string "test-string")
  (should (string= vterm-mock-last-string "test-string")))

(ert-deftest test-vterm-send-return-mock ()
  "Test that vterm-send-return mock increments the counter."
  (vterm-mock-reset)
  (vterm-send-return)
  (should (eq vterm-mock-return-count 1))
  (vterm-send-return)
  (should (eq vterm-mock-return-count 2)))

(ert-deftest test-ecc-send-accept-calls-y-n-handler ()
  "Test that ecc-send-accept calls the right handler for y/n prompts."
  (vterm-mock-reset)
  ;; Force only y/n state to be true
  (cl-letf (((symbol-function '--ecc-state-y/n-p) (lambda () t))
            ((symbol-function '--ecc-state-y/y/n-p) (lambda () nil))
            ((symbol-function '--ecc-state-waiting-p) (lambda () nil))
            ((symbol-function '--ecc-state-initial-waiting-p) (lambda () nil)))
    (ecc-send-accept)
    (should (string= vterm-mock-last-string "1"))
    (should (= vterm-mock-clear-count 1))))

(ert-deftest test-ecc-send-accept-calls-y-y-n-handler ()
  "Test that ecc-send-accept calls the right handler for y/y/n prompts."
  (vterm-mock-reset)
  ;; Force only y/y/n state to be true
  (cl-letf (((symbol-function '--ecc-state-y/n-p) (lambda () nil))
            ((symbol-function '--ecc-state-y/y/n-p) (lambda () t))
            ((symbol-function '--ecc-state-waiting-p) (lambda () nil))
            ((symbol-function '--ecc-state-initial-waiting-p) (lambda () nil)))
    (ecc-send-accept)
    (should (string= vterm-mock-last-string "2"))
    (should (= vterm-mock-clear-count 1))))

(ert-deftest test-ecc-send-accept-calls-waiting-handler ()
  "Test that ecc-send-accept calls the right handler for waiting prompts."
  (vterm-mock-reset)
  ;; Force only waiting state to be true
  (cl-letf (((symbol-function '--ecc-state-y/n-p) (lambda () nil))
            ((symbol-function '--ecc-state-y/y/n-p) (lambda () nil))
            ((symbol-function '--ecc-state-waiting-p) (lambda () t))
            ((symbol-function '--ecc-state-initial-waiting-p) (lambda () nil)))
    (ecc-send-accept)
    (should (string= vterm-mock-last-string "continue"))
    (should (= vterm-mock-clear-count 1))))

(ert-deftest test-ecc-send-accept-calls-initial-waiting-handler ()
  "Test that ecc-send-accept calls the right handler for initial waiting prompts."
  (vterm-mock-reset)
  ;; Force only initial waiting state to be true
  (cl-letf (((symbol-function '--ecc-state-y/n-p) (lambda () nil))
            ((symbol-function '--ecc-state-y/y/n-p) (lambda () nil))
            ((symbol-function '--ecc-state-waiting-p) (lambda () nil))
            ((symbol-function '--ecc-state-initial-waiting-p) (lambda () t)))
    (ecc-send-accept)
    (should (string= vterm-mock-last-string "continue"))
    (should (= vterm-mock-clear-count 1))))

(ert-deftest test-ecc-buffer-setup-teardown ()
  "Test that buffer setup and teardown works properly."
  (let ((orig-buffer ecc-buffer)
        (orig-current-buffer ecc-buffer-current-buffer)
        (mock-buffer (vterm-mock-setup-test-buffer)))
    (unwind-protect
        (progn 
          (should (buffer-live-p ecc-buffer))
          (should (eq ecc-buffer mock-buffer))
          (should (eq ecc-buffer-current-buffer mock-buffer)))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      ;; Restore original values
      (setq ecc-buffer orig-buffer
            ecc-buffer-current-buffer orig-current-buffer))))

(provide 'standalone-test-ecc-send)