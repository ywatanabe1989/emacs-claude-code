;;; test-ecc-debug-utils.el --- Consolidated debug utilities tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Consolidated debug utilities tests combining original and refactored versions
;; Following "one assertion per test" principle from refactored version

;;; Code:

(require 'ert)
(require 'ecc-debug-utils)

;;;; Test helpers

(defun test-debug-capture-messages ()
  "Capture debug messages during test execution."
  (let ((messages '()))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      messages)))

(defmacro with-debug-capture (&rest body)
  "Execute BODY and return captured debug messages."
  `(let ((messages (test-debug-capture-messages)))
     ,@body
     (nreverse messages)))

;;;; Basic debug message tests

(ert-deftest test-debug-message-enabled ()
  "Debug messages appear when debug is enabled."
  (let ((ecc-debug-enabled t))
    (let ((messages (with-debug-capture
                      (ecc-debug-message "Test message"))))
      (should (cl-some (lambda (msg) (string-match-p "Test message" msg))
                       messages)))))

(ert-deftest test-debug-message-disabled ()
  "Debug messages don't appear when debug is disabled."
  (let ((ecc-debug-enabled nil))
    (let ((messages (with-debug-capture
                      (ecc-debug-message "Test message"))))
      (should-not messages))))

(ert-deftest test-debug-message-timestamp ()
  "Debug messages include timestamp when enabled."
  (let ((ecc-debug-enabled t)
        (ecc-debug-show-timestamp t))
    (let ((messages (with-debug-capture
                      (ecc-debug-message "Test"))))
      (should (cl-some (lambda (msg) 
                         (string-match-p "\\[ECC [0-9:-]+\\]" msg))
                       messages)))))

;;;; Category-based debug tests

(ert-deftest test-debug-category-message ()
  "Debug messages work with categories."
  (let ((ecc-debug-enabled t)
        (ecc-debug-categories '(test-category)))
    (let ((messages (with-debug-capture
                      (ecc-debug-category-message 'test-category "Category test"))))
      (should (cl-some (lambda (msg) (string-match-p "Category test" msg))
                       messages)))))

;; Category filtering tests from refactored

(ert-deftest test-debug-category-filtering-shows-enabled-category ()
  "Messages from enabled categories are shown."
  (let ((ecc-debug-enabled t)
        (ecc-debug-categories '(enabled-cat)))
    (let ((messages (with-debug-capture
                      (ecc-debug-category-message 'enabled-cat "Should show"))))
      (should (cl-some (lambda (msg) (string-match-p "Should show" msg))
                       messages)))))

(ert-deftest test-debug-category-filtering-hides-disabled-category ()
  "Messages from disabled categories are not shown."
  (let ((ecc-debug-enabled t)
        (ecc-debug-categories '(enabled-cat)))
    (let ((messages (with-debug-capture
                      (ecc-debug-category-message 'disabled-cat "Should not show"))))
      (should-not (cl-some (lambda (msg) (string-match-p "Should not show" msg))
                           messages)))))

;; Toggle category tests from refactored

(ert-deftest test-debug-toggle-category-all-enabled-initially ()
  "All categories are enabled initially."
  (let ((ecc-debug-categories nil))
    (should (ecc-debug-category-enabled-p 'any-category))))

(ert-deftest test-debug-toggle-category-disables-specific-category ()
  "Toggling disables a specific category."
  (let ((ecc-debug-categories nil))
    (ecc-debug-toggle-category 'test-cat)
    (should-not (ecc-debug-category-enabled-p 'test-cat))))

(ert-deftest test-debug-toggle-category-preserves-other-categories ()
  "Toggling one category doesn't affect others."
  (let ((ecc-debug-categories nil))
    (ecc-debug-toggle-category 'cat1)
    (should (ecc-debug-category-enabled-p 'cat2))))

(ert-deftest test-debug-toggle-category-reenables-disabled-category ()
  "Toggling a disabled category re-enables it."
  (let ((ecc-debug-categories '(test-cat)))  ; disabled
    (ecc-debug-toggle-category 'test-cat)
    (should (ecc-debug-category-enabled-p 'test-cat))))

(ert-deftest test-debug-enable-all-categories-resets-to-all-enabled ()
  "Enable all categories function resets to all enabled."
  (let ((ecc-debug-categories '(cat1 cat2 cat3)))
    (ecc-debug-enable-all-categories)
    (should-not ecc-debug-categories)))

;;;; Buffer-local debug tests

(ert-deftest test-debug-buffer-message ()
  "Buffer-local debug messages work correctly."
  (with-temp-buffer
    (let ((ecc-debug-enabled t)
          (ecc-debug-buffer-enabled t))
      (let ((messages (with-debug-capture
                        (ecc-debug-buffer-message "Buffer test"))))
        (should (cl-some (lambda (msg) (string-match-p "Buffer test" msg))
                         messages))))))

(ert-deftest test-debug-buffer-disabled ()
  "Buffer-local debug respects disabled state."
  (with-temp-buffer
    (let ((ecc-debug-enabled t)
          (ecc-debug-buffer-enabled nil))
      (let ((messages (with-debug-capture
                        (ecc-debug-buffer-message "Should not show"))))
        (should-not messages)))))

;; Toggle buffer tests from refactored

(ert-deftest test-debug-toggle-buffer-enables-when-disabled ()
  "Toggling buffer debug enables it when disabled."
  (with-temp-buffer
    (setq-local ecc-debug-buffer-enabled nil)
    (ecc-debug-toggle-buffer)
    (should ecc-debug-buffer-enabled)))

(ert-deftest test-debug-toggle-buffer-disables-when-enabled ()
  "Toggling buffer debug disables it when enabled."
  (with-temp-buffer
    (setq-local ecc-debug-buffer-enabled t)
    (ecc-debug-toggle-buffer)
    (should-not ecc-debug-buffer-enabled)))

;;;; Factory function tests from refactored

(ert-deftest test-debug-make-debug-fn-creates-global-debug-function ()
  "Make-debug-fn creates working global debug function."
  (let ((ecc-debug-enabled t))
    (let ((debug-fn (ecc-debug-make-debug-fn "TEST")))
      (let ((messages (with-debug-capture
                        (funcall debug-fn "Global test"))))
        (should (cl-some (lambda (msg) 
                           (and (string-match-p "TEST" msg)
                                (string-match-p "Global test" msg)))
                         messages))))))

(ert-deftest test-debug-make-debug-fn-creates-category-debug-function ()
  "Make-debug-fn creates working category debug function."
  (let ((ecc-debug-enabled t)
        (ecc-debug-categories '(test-cat)))
    (let ((debug-fn (ecc-debug-make-debug-fn "TEST" 'test-cat)))
      (let ((messages (with-debug-capture
                        (funcall debug-fn "Category test"))))
        (should (cl-some (lambda (msg) 
                           (and (string-match-p "TEST:test-cat" msg)
                                (string-match-p "Category test" msg)))
                         messages))))))

(ert-deftest test-debug-make-debug-fn-creates-buffer-debug-function ()
  "Make-debug-fn creates working buffer debug function."
  (with-temp-buffer
    (let ((ecc-debug-enabled t)
          (ecc-debug-buffer-enabled t))
      (let ((debug-fn (ecc-debug-make-debug-fn "TEST" nil t)))
        (let ((messages (with-debug-capture
                          (funcall debug-fn "Buffer test"))))
          (should (cl-some (lambda (msg) 
                             (and (string-match-p "TEST:buffer" msg)
                                  (string-match-p "Buffer test" msg)))
                           messages)))))))

(ert-deftest test-debug-make-debug-fn-creates-buffer-category-debug-function ()
  "Make-debug-fn creates working buffer+category debug function."
  (with-temp-buffer
    (let ((ecc-debug-enabled t)
          (ecc-debug-buffer-enabled t)
          (ecc-debug-categories '(test-cat)))
      (let ((debug-fn (ecc-debug-make-debug-fn "TEST" 'test-cat t)))
        (let ((messages (with-debug-capture
                          (funcall debug-fn "Combined test"))))
          (should (cl-some (lambda (msg) 
                             (and (string-match-p "TEST:test-cat:buffer" msg)
                                  (string-match-p "Combined test" msg)))
                           messages)))))))

;;;; Module-specific debug functions

(ert-deftest test-module-specific-debug ()
  "Module-specific debug functions work correctly."
  (let ((ecc-debug-enabled t))
    ;; Test ecc-auto-debug
    (let ((messages (with-debug-capture
                      (ecc-auto-debug "Auto module test"))))
      (should (cl-some (lambda (msg) 
                         (and (string-match-p "AUTO" msg)
                              (string-match-p "Auto module test" msg)))
                       messages)))
    
    ;; Test ecc-state-debug
    (let ((messages (with-debug-capture
                      (ecc-state-debug "State module test"))))
      (should (cl-some (lambda (msg) 
                         (and (string-match-p "STATE" msg)
                              (string-match-p "State module test" msg)))
                       messages)))
    
    ;; Test ecc-notify-debug
    (let ((messages (with-debug-capture
                      (ecc-notify-debug "Notify module test"))))
      (should (cl-some (lambda (msg) 
                         (and (string-match-p "NOTIFY" msg)
                              (string-match-p "Notify module test" msg)))
                       messages)))
    
    ;; Test ecc-vterm-debug
    (let ((messages (with-debug-capture
                      (ecc-vterm-debug "Vterm module test"))))
      (should (cl-some (lambda (msg) 
                         (and (string-match-p "VTERM" msg)
                              (string-match-p "Vterm module test" msg)))
                       messages)))
    
    ;; Test ecc-buffer-debug
    (let ((messages (with-debug-capture
                      (ecc-buffer-debug "Buffer module test"))))
      (should (cl-some (lambda (msg) 
                         (and (string-match-p "BUFFER" msg)
                              (string-match-p "Buffer module test" msg)))
                       messages)))))

;;;; Debug log buffer tests

(ert-deftest test-debug-log-buffer ()
  "Debug messages are logged to debug buffer."
  (let ((ecc-debug-enabled t)
        (ecc-debug-log-to-buffer t))
    (ecc-debug-message "Log test message")
    (with-current-buffer (get-buffer-create ecc-debug-buffer-name)
      (should (string-match-p "Log test message" (buffer-string)))
      (kill-buffer))))

(ert-deftest test-debug-log-buffer-trimming ()
  "Debug log buffer is trimmed when it exceeds max lines."
  (let ((ecc-debug-enabled t)
        (ecc-debug-log-to-buffer t)
        (ecc-debug-buffer-max-lines 5))
    ;; Add more than max lines
    (dotimes (i 10)
      (ecc-debug-message "Line %d" i))
    (with-current-buffer (get-buffer-create ecc-debug-buffer-name)
      (let ((line-count (count-lines (point-min) (point-max))))
        (should (<= line-count ecc-debug-buffer-max-lines))
        (kill-buffer)))))

;;;; Backward compatibility tests

(ert-deftest test-debug-backward-compatibility ()
  "Backward compatibility aliases work correctly."
  (let ((ecc-debug-enabled t))
    ;; Test old function name
    (let ((messages (with-debug-capture
                      (ecc-debug-utils-message "Compat test"))))
      (should (cl-some (lambda (msg) (string-match-p "Compat test" msg))
                       messages)))))

(provide 'test-ecc-debug-utils)
;;; test-ecc-debug-utils.el ends here