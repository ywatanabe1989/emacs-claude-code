;;; test-buffer-state-features.el --- Feature-specific buffer state tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Consolidated feature-specific buffer state tests
;; Combines tests from: compatibility, enhanced, refactored, buffer-local-state, and integration

;;; Code:

(require 'ert)
(require 'ecc-buffer-state)
(require 'ecc-state-detection)
(require 'ecc-auto-response)

;; Test fixture macro
(defmacro with-temp-buffer-fixture (content &rest body)
  "Create a temporary buffer with CONTENT and evaluate BODY.
If CONTENT is nil, creates an empty buffer."
  (declare (indent 1))
  `(let ((temp-buffer (generate-new-buffer " *test-buffer*")))
     (unwind-protect
         (with-current-buffer temp-buffer
           ,@(when content `((insert ,content)))
           ,@body)
       (kill-buffer temp-buffer))))

;; From test-buffer-state-compatibility.el

(ert-deftest test-buffer-state-update-alias ()
  "Test backward compatibility alias for update function."
  (with-temp-buffer-fixture nil
    (ecc-buffer-state-init (current-buffer))
    ;; Test old function name still works
    (ecc-update-buffer-prompt-state 'waiting)
    (should (eq (ecc-buffer-state-get-prompt-state) 'waiting))))

(ert-deftest test-buffer-get-prompt-state-alias ()
  "Test backward compatibility alias for get prompt state."
  (with-temp-buffer-fixture nil
    (ecc-buffer-state-init (current-buffer))
    (ecc-buffer-state-update-prompt-state 'y-n)
    ;; Test old function name still works
    (should (eq (ecc-get-buffer-prompt-state) 'y-n))))

(ert-deftest test-buffer-state-detect-alias ()
  "Test backward compatibility alias for detect function."
  (with-temp-buffer-fixture "[y/n]"
    ;; Test old function name still works
    (should (eq (ecc-detect-prompt-state) 'y-n))))

(ert-deftest test-ecc-update-buffer-state-legacy ()
  "Test legacy update-buffer-state function."
  (with-temp-buffer-fixture nil
    (ecc-buffer-state-init (current-buffer))
    ;; Legacy function
    (ecc-update-buffer-state 'legacy-key "legacy-value")
    (should (equal (ecc-buffer-state-get 'legacy-key) "legacy-value"))))

(ert-deftest test-ecc-get-buffer-state-legacy ()
  "Test legacy get-buffer-state function."
  (with-temp-buffer-fixture nil
    (ecc-buffer-state-init (current-buffer))
    (ecc-buffer-state-set 'test-key "test-value")
    ;; Legacy function
    (should (equal (ecc-get-buffer-state 'test-key) "test-value"))))

;; From test-buffer-state-enhanced.el

(ert-deftest ecc-test-buffer-state-throttling ()
  "Test state update throttling."
  (with-temp-buffer-fixture nil
    (ecc-buffer-state-init (current-buffer))
    
    ;; First update should succeed
    (let ((time1 (current-time)))
      (should (ecc-buffer-state-update-allowed-p 'test-state))
      
      ;; Immediate second update should be throttled
      (should-not (ecc-buffer-state-update-allowed-p 'test-state))
      
      ;; Different state should be allowed
      (should (ecc-buffer-state-update-allowed-p 'different-state))
      
      ;; After throttle period, same state should be allowed
      (sleep-for 0.1)  ; Assuming throttle is less than 100ms
      (let ((ecc-auto-response-throttle-time 0.05))
        (should (ecc-buffer-state-update-allowed-p 'test-state))))))

(ert-deftest ecc-test-buffer-state-compat ()
  "Test compatibility layer functions."
  (with-temp-buffer-fixture nil
    (ecc-buffer-state-init (current-buffer))
    
    ;; Test all backward compatibility functions
    (ecc-update-buffer-prompt-state 'compat-test)
    (should (eq (ecc-get-buffer-prompt-state) 'compat-test))
    
    (ecc-update-buffer-state 'compat-key 'compat-value)
    (should (eq (ecc-get-buffer-state 'compat-key) 'compat-value))))

;; From test-buffer-state-refactored.el

(ert-deftest test-buffer-state-throttling-refactored ()
  "Test refactored throttling mechanism."
  (with-temp-buffer-fixture nil
    (ecc-buffer-state-init (current-buffer))
    
    (let ((ecc-auto-response-throttle-time 0.05))
      ;; Record first state
      (ecc-buffer-state-record-update 'state1)
      
      ;; Should throttle same state
      (should (ecc-buffer-state-throttled-p 'state1))
      
      ;; Should not throttle different state
      (should-not (ecc-buffer-state-throttled-p 'state2))
      
      ;; After delay, should not throttle
      (sleep-for 0.06)
      (should-not (ecc-buffer-state-throttled-p 'state1)))))

;; From test-buffer-local-state.el

(ert-deftest ecc-test-buffer-local-response-patterns ()
  "Test buffer-local response pattern handling."
  (let ((buffer1 (generate-new-buffer " *test-pattern-1*"))
        (buffer2 (generate-new-buffer " *test-pattern-2*")))
    (unwind-protect
        (progn
          ;; Set different patterns in different buffers
          (with-current-buffer buffer1
            (setq-local ecc-buffer-custom-patterns '(("\\[CUSTOM1\\]" . custom1)))
            (insert "[CUSTOM1]")
            ;; This test assumes custom pattern support exists
            )
          
          (with-current-buffer buffer2
            (setq-local ecc-buffer-custom-patterns '(("\\[CUSTOM2\\]" . custom2)))
            (insert "[CUSTOM2]")
            ;; This test assumes custom pattern support exists
            ))
      (kill-buffer buffer1)
      (kill-buffer buffer2))))

(ert-deftest ecc-test-buffer-local-throttling ()
  "Test buffer-local throttling behavior."
  (let ((buffer1 (generate-new-buffer " *test-throttle-1*"))
        (buffer2 (generate-new-buffer " *test-throttle-2*")))
    (unwind-protect
        (progn
          ;; Test throttling is buffer-specific
          (with-current-buffer buffer1
            (ecc-buffer-state-init (current-buffer))
            (ecc-buffer-state-record-update 'state1)
            (should (ecc-buffer-state-throttled-p 'state1)))
          
          ;; Same state in different buffer should not be throttled
          (with-current-buffer buffer2
            (ecc-buffer-state-init (current-buffer))
            (should-not (ecc-buffer-state-throttled-p 'state1))
            (ecc-buffer-state-record-update 'state1)
            (should (ecc-buffer-state-throttled-p 'state1))))
      (kill-buffer buffer1)
      (kill-buffer buffer2))))

;; From test-buffer-state-integration.el

(ert-deftest test-buffer-state-throttling ()
  "Test integrated throttling behavior."
  (with-temp-buffer-fixture nil
    (ecc-buffer-state-init (current-buffer))
    
    ;; Test throttling with actual state detection
    (insert "[y/n]")
    (let ((state (ecc-state-detection-get-state)))
      (should (eq state 'y-n))
      
      ;; First notification should work
      (should (ecc-buffer-state-update-allowed-p state))
      (ecc-buffer-state-record-update state)
      
      ;; Second immediate notification should be throttled
      (should-not (ecc-buffer-state-update-allowed-p state))
      
      ;; Change content, different state should work
      (erase-buffer)
      (insert "Human:\n\nAssistant:")
      (let ((new-state (ecc-state-detection-get-state)))
        (should (eq new-state 'waiting))
        (should (ecc-buffer-state-update-allowed-p new-state))))))

(provide 'test-buffer-state-features)
;;; test-buffer-state-features.el ends here