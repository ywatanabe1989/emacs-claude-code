;;; test-buffer-state-detection.el --- Detection tests for buffer state -*- lexical-binding: t; -*-

;;; Commentary:
;; Consolidated buffer state detection tests
;; Combines tests from: detection, detection-integration, enhanced, refactored, and snapshots

;;; Code:

(require 'ert)
(require 'ecc-buffer-state)
(require 'ecc-state-detection)
(require 'ecc-vterm-utils)
(require 'ecc-notification)
(require 'ecc-buffer-api)

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

;; Keep original tests from test-buffer-state-detection.el

(ert-deftest test-buffer-state-detection-basic ()
  "Test basic buffer state detection."
  (with-temp-buffer-fixture "[y/n]"
    (should (eq (ecc-state-detection-get-state) 'y-n)))
  
  (with-temp-buffer-fixture "[Y/N]"
    (should (eq (ecc-state-detection-get-state) 'y-n)))
  
  (with-temp-buffer-fixture "Human:\n\nAssistant:"
    (should (eq (ecc-state-detection-get-state) 'waiting)))
  
  (with-temp-buffer-fixture "Human:\n\nAssistant: (typing"
    (should (eq (ecc-state-detection-get-state) 'initial-waiting))))

(ert-deftest test-buffer-state-detection-name-formatting ()
  "Test buffer name formatting in state detection."
  (let ((buffer-name "*CLAUDE-PROJECT-A*"))
    (with-temp-buffer-fixture "[y/n]"
      (rename-buffer buffer-name)
      (let ((result (ecc-state-detection-describe-state 'y-n)))
        (should (string-match-p buffer-name result))))))

(ert-deftest test-buffer-state-detection-complex-content ()
  "Test detection with complex buffer content."
  (with-temp-buffer-fixture "Some content\n[Continue?]"
    (should (eq (ecc-state-detection-get-state) 'continue)))
  
  (with-temp-buffer-fixture "Multiple lines\nof content\n[y/y/n]"
    (should (eq (ecc-state-detection-get-state) 'y-y-n))))

(ert-deftest test-buffer-state-detection-custom-patterns ()
  "Test detection with custom patterns."
  (let ((ecc-state-prompt-initial-waiting-alternatives
         '("\\[CUSTOM\\]" "\\[PATTERN\\]")))
    (with-temp-buffer-fixture "[CUSTOM]"
      (should (eq (ecc-state-detection-get-state) 'initial-waiting)))
    (with-temp-buffer-fixture "[PATTERN]"
      (should (eq (ecc-state-detection-get-state) 'initial-waiting)))))

(ert-deftest test-buffer-state-detection-notification ()
  "Test notification integration with state detection."
  (let ((notifications '()))
    (cl-flet ((ecc-notification-notify (msg)
                (push msg notifications)))
      (with-temp-buffer-fixture "[y/n]"
        (rename-buffer "*CLAUDE-TEST*")
        (ecc-state-detection-notify-state 'y-n)
        (should (cl-some (lambda (msg) (string-match-p "\\*CLAUDE-TEST\\*" msg))
                         notifications))))))

;; From test-buffer-state-detection-integration.el

(ert-deftest test-buffer-state-detection-integration ()
  "Test integration between buffer state and detection."
  (with-temp-buffer-fixture "[y/n] prompt"
    (ecc-buffer-state-init (current-buffer))
    (let ((detected-state (ecc-state-detection-get-state)))
      (should (eq detected-state 'y-n))
      (ecc-buffer-state-update-prompt-state detected-state)
      (should (eq (ecc-buffer-state-get-prompt-state) 'y-n)))))

(ert-deftest test-background-detection-cursor-independence ()
  "Test that background detection doesn't depend on cursor position."
  (with-temp-buffer-fixture "Line 1\nLine 2\n[y/n]"
    (goto-char (point-min))
    (should (eq (ecc-state-detection-get-state) 'y-n))
    (goto-char (point-max))
    (should (eq (ecc-state-detection-get-state) 'y-n))))

(ert-deftest test-buffer-state-independence ()
  "Test state independence across multiple buffers."
  (let ((buffer1 (generate-new-buffer " *test-1*"))
        (buffer2 (generate-new-buffer " *test-2*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer1
            (insert "[y/n]")
            (ecc-buffer-state-init (current-buffer))
            (should (eq (ecc-state-detection-get-state) 'y-n)))
          (with-current-buffer buffer2
            (insert "Human:\n\nAssistant:")
            (ecc-buffer-state-init (current-buffer))
            (should (eq (ecc-state-detection-get-state) 'waiting))))
      (kill-buffer buffer1)
      (kill-buffer buffer2))))

(ert-deftest test-vterm-utils-send-string-buffer-locality ()
  "Test vterm utils maintain buffer locality."
  (let ((test-buffers '())
        (original-send-string (symbol-function 'vterm-send-string)))
    (unwind-protect
        (progn
          ;; Mock vterm-send-string to track calls
          (cl-flet ((vterm-send-string (string)
                     (push (cons (current-buffer) string) test-buffers)))
            ;; Test in multiple buffers
            (let ((buffer1 (generate-new-buffer " *vterm-1*"))
                  (buffer2 (generate-new-buffer " *vterm-2*")))
              (with-current-buffer buffer1
                (ecc-vterm-send-string-to-term "test1"))
              (with-current-buffer buffer2
                (ecc-vterm-send-string-to-term "test2"))
              ;; Verify buffer locality
              (should (equal (cdr (assq buffer1 test-buffers)) "test1"))
              (should (equal (cdr (assq buffer2 test-buffers)) "test2"))
              (kill-buffer buffer1)
              (kill-buffer buffer2)))))))

;; From test-buffer-state-enhanced.el

(ert-deftest ecc-test-buffer-state-predicates ()
  "Test state predicate functions."
  (with-temp-buffer-fixture nil
    (ecc-buffer-state-init (current-buffer))
    
    ;; Test waiting states
    (ecc-buffer-state-update-prompt-state 'waiting)
    (should (ecc-buffer-state-waiting-p))
    (should-not (ecc-buffer-state-initial-waiting-p))
    
    (ecc-buffer-state-update-prompt-state 'initial-waiting)
    (should-not (ecc-buffer-state-waiting-p))
    (should (ecc-buffer-state-initial-waiting-p))
    
    ;; Test prompt states
    (ecc-buffer-state-update-prompt-state 'y-n)
    (should (ecc-buffer-state-has-prompt-p))
    
    (ecc-buffer-state-update-prompt-state nil)
    (should-not (ecc-buffer-state-has-prompt-p))))

;; From test-buffer-state-refactored.el

(ert-deftest test-buffer-state-predicates-refactored ()
  "Test refactored predicate functions."
  (with-temp-buffer-fixture nil
    (ecc-buffer-state-init (current-buffer))
    
    ;; Test combined waiting check
    (ecc-buffer-state-update-prompt-state 'waiting)
    (should (ecc-buffer-state-any-waiting-p))
    
    (ecc-buffer-state-update-prompt-state 'initial-waiting)
    (should (ecc-buffer-state-any-waiting-p))
    
    (ecc-buffer-state-update-prompt-state 'y-n)
    (should-not (ecc-buffer-state-any-waiting-p))))

;; From test-buffer-state-snapshots.el

(ert-deftest ecc-test-snapshot-y-n ()
  "Test snapshot with y/n prompt."
  (with-temp-buffer-fixture 
      "Human: Should I proceed with this?\n\nAssistant: [y/n]"
    (should (eq (ecc-state-detection-get-state) 'y-n))))

(ert-deftest ecc-test-snapshot-y-y-n ()
  "Test snapshot with y/y/n prompt."
  (with-temp-buffer-fixture
      "Human: Confirm?\n\nAssistant: [y/y/n]"
    (should (eq (ecc-state-detection-get-state) 'y-y-n))))

(ert-deftest ecc-test-snapshot-waiting ()
  "Test snapshot with waiting state."
  (with-temp-buffer-fixture
      "Human: What is the weather?\n\nAssistant:"
    (should (eq (ecc-state-detection-get-state) 'waiting))))

(ert-deftest ecc-test-snapshot-initial-waiting ()
  "Test snapshot with initial waiting state."
  (with-temp-buffer-fixture
      "Human: Hello\n\nAssistant: (typing"
    (should (eq (ecc-state-detection-get-state) 'initial-waiting))))

(ert-deftest ecc-test-snapshot-regular-content ()
  "Test snapshot with regular content (no special state)."
  (with-temp-buffer-fixture
      "Just some regular text without any prompts"
    (should (null (ecc-state-detection-get-state)))))

(ert-deftest ecc-test-snapshots-with-buffer-specific-patterns ()
  "Test snapshots with buffer-specific patterns."
  (let ((buffer-patterns '(("\\[SPECIAL\\]" . special-state))))
    (with-temp-buffer-fixture "[SPECIAL]"
      ;; This would need buffer-specific pattern implementation
      ;; For now, test that it doesn't match standard patterns
      (should-not (memq (ecc-state-detection-get-state) 
                        '(y-n y-y-n waiting initial-waiting))))))

(ert-deftest ecc-test-snapshot-with-alternative-patterns ()
  "Test snapshots with alternative initial-waiting patterns."
  (let ((ecc-state-prompt-initial-waiting-alternatives
         '("/user:auto" "/user:continue")))
    (with-temp-buffer-fixture "Human: test\n\n/user:auto"
      (should (eq (ecc-state-detection-get-state) 'initial-waiting)))
    (with-temp-buffer-fixture "Human: test\n\n/user:continue"
      (should (eq (ecc-state-detection-get-state) 'initial-waiting)))))

(provide 'test-buffer-state-detection)
;;; test-buffer-state-detection.el ends here