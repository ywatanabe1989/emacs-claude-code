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

(ert-deftest test-buffer-state-should-create-data-as-buffer-local ()
  "Test that buffer state data variable is created as buffer-local."
  (with-temp-buffer-fixture nil
    ;; Act
    (ecc-buffer-state-init (current-buffer))
    
    ;; Assert
    (should (local-variable-p 'ecc-buffer-state--data))))

(ert-deftest test-buffer-state-should-create-prompt-state-as-buffer-local ()
  "Test that prompt state variable is created as buffer-local."
  (with-temp-buffer-fixture nil
    ;; Act
    (ecc-buffer-state-init (current-buffer))
    
    ;; Assert
    (should (local-variable-p 'ecc-buffer-state--prompt-state))))

(ert-deftest test-buffer-state-should-create-last-update-as-buffer-local ()
  "Test that last update variable is created as buffer-local."
  (with-temp-buffer-fixture nil
    ;; Act
    (ecc-buffer-state-init (current-buffer))
    
    ;; Assert
    (should (local-variable-p 'ecc-buffer-state--last-update))))

(ert-deftest test-buffer-state-should-initialize-data-as-hash-table ()
  "Test that buffer state data is initialized as a hash table."
  (with-temp-buffer-fixture nil
    ;; Act
    (ecc-buffer-state-init (current-buffer))
    
    ;; Assert
    (should (hash-table-p ecc-buffer-state--data))))

(ert-deftest test-buffer-state-should-initialize-prompt-state-as-nil ()
  "Test that prompt state is initialized as nil."
  (with-temp-buffer-fixture nil
    ;; Act
    (ecc-buffer-state-init (current-buffer))
    
    ;; Assert
    (should (null ecc-buffer-state--prompt-state))))

(ert-deftest test-buffer-state-should-initialize-last-update-as-nil ()
  "Test that last update is initialized as nil."
  (with-temp-buffer-fixture nil
    ;; Act
    (ecc-buffer-state-init (current-buffer))
    
    ;; Assert
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

(ert-deftest test-buffer-state-should-store-and-retrieve-string-value ()
  "Test that buffer state can store and retrieve string values."
  (with-temp-buffer-fixture nil
    ;; Arrange
    (ecc-buffer-state-init (current-buffer))
    
    ;; Act
    (ecc-buffer-state-set 'key1 "value1")
    
    ;; Assert
    (should (equal (ecc-buffer-state-get 'key1) "value1"))))

(ert-deftest test-buffer-state-should-store-and-retrieve-symbol-value ()
  "Test that buffer state can store and retrieve symbol values."
  (with-temp-buffer-fixture nil
    ;; Arrange
    (ecc-buffer-state-init (current-buffer))
    
    ;; Act
    (ecc-buffer-state-set 'key2 'symbol-value)
    
    ;; Assert
    (should (eq (ecc-buffer-state-get 'key2) 'symbol-value))))

(ert-deftest test-buffer-state-should-store-and-retrieve-numeric-value ()
  "Test that buffer state can store and retrieve numeric values."
  (with-temp-buffer-fixture nil
    ;; Arrange
    (ecc-buffer-state-init (current-buffer))
    
    ;; Act
    (ecc-buffer-state-set 'key3 123)
    
    ;; Assert
    (should (= (ecc-buffer-state-get 'key3) 123))))

(ert-deftest test-buffer-state-should-store-and-retrieve-nil-value ()
  "Test that buffer state can store and retrieve nil values."
  (with-temp-buffer-fixture nil
    ;; Arrange
    (ecc-buffer-state-init (current-buffer))
    
    ;; Act
    (ecc-buffer-state-set 'key4 nil)
    
    ;; Assert
    (should (null (ecc-buffer-state-get 'key4)))))

(ert-deftest test-buffer-state-should-return-nil-for-non-existent-key ()
  "Test that buffer state returns nil for non-existent keys."
  (with-temp-buffer-fixture nil
    ;; Arrange
    (ecc-buffer-state-init (current-buffer))
    
    ;; Act & Assert
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