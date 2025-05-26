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
    (should (local-variable-p 'ecc-buffer-state-data))))

(ert-deftest test-buffer-state-should-create-prompt-state-as-buffer-local ()
  "Test that prompt state variable is created as buffer-local."
  (with-temp-buffer-fixture nil
    ;; Act
    (ecc-buffer-state-init (current-buffer))
    
    ;; Assert
    (should (local-variable-p 'ecc-buffer-state-prompt))))

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


(ert-deftest test-buffer-state-should-initialize-last-update-as-nil ()
  "Test that last update is initialized as nil."
  (with-temp-buffer-fixture nil
    ;; Act
    (ecc-buffer-state-init (current-buffer))
    
    ;; Assert
    (should (null ecc-buffer-state--last-update))))

(ert-deftest test-buffer-state-should-initialize-prompt-state-as-nil ()
  "Test that prompt state is initially nil after buffer state init."
  (with-temp-buffer-fixture nil
    ;; Arrange & Act
    (ecc-buffer-state-init (current-buffer))
    
    ;; Assert
    (should (null (ecc-buffer-state-get-prompt)))))

(ert-deftest test-buffer-state-should-update-prompt-state-when-changed ()
  "Test that prompt state updates to new value when changed."
  (with-temp-buffer-fixture nil
    ;; Arrange
    (ecc-buffer-state-init (current-buffer))
    
    ;; Act
    (ecc-buffer-state-update-prompt 'waiting)
    
    ;; Assert
    (should (eq (ecc-buffer-state-get-prompt) 'waiting))))

(ert-deftest test-buffer-state-should-store-value-when-key-is-set ()
  "Test that buffer state stores value when key is set."
  (with-temp-buffer-fixture nil
    ;; Arrange
    (ecc-buffer-state-init (current-buffer))
    
    ;; Act
    (ecc-buffer-state-set 'test-key "test-value")
    
    ;; Assert
    (should (equal (ecc-buffer-state-get 'test-key) "test-value"))))

(ert-deftest test-buffer-state-should-overwrite-value-when-key-is-reset ()
  "Test that buffer state overwrites existing value when key is set again."
  (with-temp-buffer-fixture nil
    ;; Arrange
    (ecc-buffer-state-init (current-buffer))
    (ecc-buffer-state-set 'test-key "test-value")
    
    ;; Act
    (ecc-buffer-state-set 'test-key "new-value")
    
    ;; Assert
    (should (equal (ecc-buffer-state-get 'test-key) "new-value"))))

(ert-deftest test-buffer-state-should-maintain-value1-when-buffer2-modified ()
  "Test that buffer1 state remains unchanged when buffer2 is modified."
  (let ((buffer1 (generate-new-buffer " *test-buffer-1*"))
        (buffer2 (generate-new-buffer " *test-buffer-2*")))
    (unwind-protect
        (progn
          ;; Arrange - Initialize both buffers
          (with-current-buffer buffer1
            (ecc-buffer-state-init (current-buffer))
            (ecc-buffer-state-set 'key "value1"))
          
          ;; Act - Modify buffer2
          (with-current-buffer buffer2
            (ecc-buffer-state-init (current-buffer))
            (ecc-buffer-state-set 'key "value2"))
          
          ;; Assert - Buffer1 unchanged
          (with-current-buffer buffer1
            (should (equal (ecc-buffer-state-get 'key) "value1"))))
      (kill-buffer buffer1)
      (kill-buffer buffer2))))

(ert-deftest test-buffer-state-should-maintain-value2-when-buffer1-exists ()
  "Test that buffer2 state is independent from buffer1 state."
  (let ((buffer1 (generate-new-buffer " *test-buffer-1*"))
        (buffer2 (generate-new-buffer " *test-buffer-2*")))
    (unwind-protect
        (progn
          ;; Arrange - Initialize buffer1 first
          (with-current-buffer buffer1
            (ecc-buffer-state-init (current-buffer))
            (ecc-buffer-state-set 'key "value1"))
          
          ;; Act - Initialize and set buffer2
          (with-current-buffer buffer2
            (ecc-buffer-state-init (current-buffer))
            (ecc-buffer-state-set 'key "value2"))
          
          ;; Assert - Buffer2 has its own value
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

(ert-deftest test-buffer-state-should-update-to-initial-waiting-when-set ()
  "Test that prompt state updates to initial-waiting when set."
  (with-temp-buffer-fixture nil
    ;; Arrange
    (ecc-buffer-state-init (current-buffer))
    
    ;; Act
    (ecc-buffer-state-update-prompt 'initial-waiting)
    
    ;; Assert
    (should (eq (ecc-buffer-state-get-prompt) 'initial-waiting))))

(ert-deftest test-buffer-state-should-transition-from-initial-to-waiting ()
  "Test that prompt state can transition from initial-waiting to waiting."
  (with-temp-buffer-fixture nil
    ;; Arrange
    (ecc-buffer-state-init (current-buffer))
    (ecc-buffer-state-update-prompt 'initial-waiting)
    
    ;; Act
    (ecc-buffer-state-update-prompt 'waiting)
    
    ;; Assert
    (should (eq (ecc-buffer-state-get-prompt) 'waiting))))

(ert-deftest test-buffer-state-should-clear-prompt-state-when-set-to-nil ()
  "Test that prompt state is cleared when set to nil."
  (with-temp-buffer-fixture nil
    ;; Arrange
    (ecc-buffer-state-init (current-buffer))
    (ecc-buffer-state-update-prompt 'waiting)
    
    ;; Act
    (ecc-buffer-state-update-prompt nil)
    
    ;; Assert
    (should (null (ecc-buffer-state-get-prompt)))))

;; From test-buffer-state-refactored.el

(ert-deftest test-buffer-state-should-store-and-retrieve-string-when-set ()
  "Test that buffer state correctly stores and retrieves string values."
  (with-temp-buffer-fixture nil
    ;; Arrange
    (ecc-buffer-state-init (current-buffer))
    
    ;; Act
    (ecc-buffer-state-set 'string-key "string value")
    
    ;; Assert
    (should (equal (ecc-buffer-state-get 'string-key) "string value"))))

(ert-deftest test-buffer-state-should-store-and-retrieve-number-when-set ()
  "Test that buffer state correctly stores and retrieves numeric values."
  (with-temp-buffer-fixture nil
    ;; Arrange
    (ecc-buffer-state-init (current-buffer))
    
    ;; Act
    (ecc-buffer-state-set 'number-key 42)
    
    ;; Assert
    (should (= (ecc-buffer-state-get 'number-key) 42))))

(ert-deftest test-buffer-state-should-store-and-retrieve-list-when-set ()
  "Test that buffer state correctly stores and retrieves list values."
  (with-temp-buffer-fixture nil
    ;; Arrange
    (ecc-buffer-state-init (current-buffer))
    
    ;; Act
    (ecc-buffer-state-set 'list-key '(1 2 3))
    
    ;; Assert
    (should (equal (ecc-buffer-state-get 'list-key) '(1 2 3)))))

(ert-deftest test-buffer-state-should-store-and-retrieve-plist-when-set ()
  "Test that buffer state correctly stores and retrieves property list values."
  (with-temp-buffer-fixture nil
    ;; Arrange
    (ecc-buffer-state-init (current-buffer))
    
    ;; Act
    (ecc-buffer-state-set 'plist-key '(:a 1 :b 2))
    
    ;; Assert
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

(ert-deftest test-buffer-state-should-detect-yn-prompt-in-buffer1 ()
  "Test that buffer1 correctly detects y/n prompt state."
  (let ((buffer1 (generate-new-buffer " *test-claude-1*")))
    (unwind-protect
        (with-current-buffer buffer1
          ;; Arrange
          (ecc-buffer-state-init (current-buffer))
          (insert "Human: test\n\n[y/n]")
          
          ;; Act
          (let ((state (ecc-state-detection-get-state)))
            
            ;; Assert
            (should (eq state 'y-n))))
      (kill-buffer buffer1))))

(ert-deftest test-buffer-state-should-detect-waiting-state-in-buffer2 ()
  "Test that buffer2 correctly detects waiting state."
  (let ((buffer2 (generate-new-buffer " *test-claude-2*")))
    (unwind-protect
        (with-current-buffer buffer2
          ;; Arrange
          (ecc-buffer-state-init (current-buffer))
          (insert "Human:\n\nAssistant:")
          
          ;; Act
          (let ((state (ecc-state-detection-get-state)))
            
            ;; Assert
            (should (eq state 'waiting))))
      (kill-buffer buffer2))))

(ert-deftest test-buffer-state-should-maintain-yn-state-when-buffer2-created ()
  "Test that buffer1 maintains y/n state after buffer2 is created."
  (let ((buffer1 (generate-new-buffer " *test-claude-1*"))
        (buffer2 (generate-new-buffer " *test-claude-2*")))
    (unwind-protect
        (progn
          ;; Arrange - Set up buffer1 with y/n state
          (with-current-buffer buffer1
            (ecc-buffer-state-init (current-buffer))
            (insert "Human: test\n\n[y/n]"))
          
          ;; Act - Create buffer2 with different state
          (with-current-buffer buffer2
            (ecc-buffer-state-init (current-buffer))
            (insert "Human:\n\nAssistant:"))
          
          ;; Assert - Buffer1 still has y/n state
          (with-current-buffer buffer1
            (should (eq (ecc-state-detection-get-state) 'y-n))))
      (kill-buffer buffer1)
      (kill-buffer buffer2))))

(ert-deftest test-buffer-state-should-maintain-waiting-state-independently ()
  "Test that buffer2 maintains waiting state independent of buffer1."
  (let ((buffer1 (generate-new-buffer " *test-claude-1*"))
        (buffer2 (generate-new-buffer " *test-claude-2*")))
    (unwind-protect
        (progn
          ;; Arrange - Set up both buffers
          (with-current-buffer buffer1
            (ecc-buffer-state-init (current-buffer))
            (insert "Human: test\n\n[y/n]"))
          
          (with-current-buffer buffer2
            (ecc-buffer-state-init (current-buffer))
            (insert "Human:\n\nAssistant:"))
          
          ;; Act & Assert - Buffer2 has its own state
          (with-current-buffer buffer2
            (should (eq (ecc-state-detection-get-state) 'waiting))))
      (kill-buffer buffer1)
      (kill-buffer buffer2))))

(provide 'test-buffer-state-core)
;;; test-buffer-state-core.el ends here