;;; test-buffer-state-core.el --- Core buffer state tests for emacs-claude-code -*- lexical-binding: t; -*-

;;; Commentary:
;; Consolidated core buffer state functionality tests
;; Combines tests from: basic, enhanced, refactored, and buffer-local-state

;;; Code:

(require 'ert)
(require 'ecc-buffer-state)
(require 'ecc-state-detection)

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

;; From test-buffer-state-basic.el

(ert-deftest test-buffer-state-init-creates-buffer-local-variables ()
  "Buffer state initialization creates buffer-local variables."
  (with-temp-buffer-fixture nil
    (ecc-buffer-state-init (current-buffer))
    ;; Check variables are buffer-local
    (should (local-variable-p 'ecc-buffer-state--data))
    (should (local-variable-p 'ecc-buffer-state--prompt-state))
    (should (local-variable-p 'ecc-buffer-state--last-update))
    ;; Check initial values
    (should (hash-table-p ecc-buffer-state--data))
    (should (null ecc-buffer-state--prompt-state))
    (should (null ecc-buffer-state--last-update))))

(ert-deftest test-buffer-state-update-prompt-changes-stored-value ()
  "Updating prompt state changes the stored value."
  (with-temp-buffer-fixture nil
    (ecc-buffer-state-init (current-buffer))
    (should (null (ecc-buffer-state-get-prompt-state)))
    (ecc-buffer-state-update-prompt-state 'waiting)
    (should (eq (ecc-buffer-state-get-prompt-state) 'waiting))))

(ert-deftest test-buffer-state-set-and-get-stores-arbitrary-state ()
  "Set and get operations store arbitrary state data."
  (with-temp-buffer-fixture nil
    (ecc-buffer-state-init (current-buffer))
    (ecc-buffer-state-set 'test-key "test-value")
    (should (equal (ecc-buffer-state-get 'test-key) "test-value"))
    ;; Test overwriting
    (ecc-buffer-state-set 'test-key "new-value")
    (should (equal (ecc-buffer-state-get 'test-key) "new-value"))))

(ert-deftest test-buffer-state-remains-independent-between-buffers ()
  "Buffer state remains independent between different buffers."
  (let ((buffer1 (generate-new-buffer " *test-buffer-1*"))
        (buffer2 (generate-new-buffer " *test-buffer-2*")))
    (unwind-protect
        (progn
          ;; Initialize both buffers
          (with-current-buffer buffer1
            (ecc-buffer-state-init (current-buffer))
            (ecc-buffer-state-set 'key "value1"))
          (with-current-buffer buffer2
            (ecc-buffer-state-init (current-buffer))
            (ecc-buffer-state-set 'key "value2"))
          ;; Check independence
          (with-current-buffer buffer1
            (should (equal (ecc-buffer-state-get 'key) "value1")))
          (with-current-buffer buffer2
            (should (equal (ecc-buffer-state-get 'key) "value2"))))
      (kill-buffer buffer1)
      (kill-buffer buffer2))))

;; From test-buffer-state-enhanced.el

(ert-deftest ecc-test-buffer-state-container ()
  "Test buffer state container operations."
  (with-temp-buffer-fixture nil
    (ecc-buffer-state-init (current-buffer))
    ;; Test multiple key-value pairs
    (ecc-buffer-state-set 'key1 "value1")
    (ecc-buffer-state-set 'key2 'symbol-value)
    (ecc-buffer-state-set 'key3 123)
    
    (should (equal (ecc-buffer-state-get 'key1) "value1"))
    (should (eq (ecc-buffer-state-get 'key2) 'symbol-value))
    (should (= (ecc-buffer-state-get 'key3) 123))
    
    ;; Test nil value
    (ecc-buffer-state-set 'key4 nil)
    (should (null (ecc-buffer-state-get 'key4)))
    
    ;; Test non-existent key
    (should (null (ecc-buffer-state-get 'non-existent)))))

(ert-deftest ecc-test-buffer-state-prompt-tracking ()
  "Test prompt state tracking and updates."
  (with-temp-buffer-fixture nil
    (ecc-buffer-state-init (current-buffer))
    
    ;; Test state transitions
    (ecc-buffer-state-update-prompt-state 'initial-waiting)
    (should (eq (ecc-buffer-state-get-prompt-state) 'initial-waiting))
    
    (ecc-buffer-state-update-prompt-state 'waiting)
    (should (eq (ecc-buffer-state-get-prompt-state) 'waiting))
    
    (ecc-buffer-state-update-prompt-state nil)
    (should (null (ecc-buffer-state-get-prompt-state)))))

;; From test-buffer-state-refactored.el

(ert-deftest test-buffer-state-get-set-refactored ()
  "Test refactored get/set operations."
  (with-temp-buffer-fixture nil
    (ecc-buffer-state-init (current-buffer))
    
    ;; Test various data types
    (ecc-buffer-state-set 'string-key "string value")
    (ecc-buffer-state-set 'number-key 42)
    (ecc-buffer-state-set 'list-key '(1 2 3))
    (ecc-buffer-state-set 'plist-key '(:a 1 :b 2))
    
    (should (equal (ecc-buffer-state-get 'string-key) "string value"))
    (should (= (ecc-buffer-state-get 'number-key) 42))
    (should (equal (ecc-buffer-state-get 'list-key) '(1 2 3)))
    (should (equal (ecc-buffer-state-get 'plist-key) '(:a 1 :b 2)))))

;; Export/import tests commented out until functions are implemented
;; (ert-deftest test-buffer-state-update-export-refactored ()
;;   "Test state export and import functionality."
;;   ...)

;; Debug info test commented out until function is implemented  
;; (ert-deftest test-buffer-state-debug-info-refactored ()
;;   "Test debug info generation."
;;   ...)

;; From test-buffer-local-state.el

(ert-deftest ecc-test-buffer-local-state-detection ()
  "Test buffer-local state detection."
  (let ((buffer1 (generate-new-buffer " *test-claude-1*"))
        (buffer2 (generate-new-buffer " *test-claude-2*")))
    (unwind-protect
        (progn
          ;; Set different states in different buffers
          (with-current-buffer buffer1
            (ecc-buffer-state-init (current-buffer))
            (insert "Human: test\n\n[y/n]")
            (let ((state (ecc-state-detection-get-state)))
              (should (eq state 'y-n))))
          
          (with-current-buffer buffer2
            (ecc-buffer-state-init (current-buffer))
            (insert "Human:\n\nAssistant:")
            (let ((state (ecc-state-detection-get-state)))
              (should (eq state 'waiting))))
          
          ;; Verify states remain independent
          (with-current-buffer buffer1
            (should (eq (ecc-state-detection-get-state) 'y-n)))
          (with-current-buffer buffer2
            (should (eq (ecc-state-detection-get-state) 'waiting))))
      (kill-buffer buffer1)
      (kill-buffer buffer2))))

(provide 'test-buffer-state-core)
;;; test-buffer-state-core.el ends here