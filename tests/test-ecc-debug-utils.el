;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 17:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-debug-utils.el

;;; Commentary:
;;; Tests for debug utilities module functionality.
;;; These tests verify the functionality of the debug message system,
;;; debug categories, buffer-local debugging, and debug factory functions.

;;; Code:
(require 'ert)
(require 'ecc-variables-consolidated)
(require 'ecc-debug-utils-consolidated)

;;;; Test helpers

(defvar ecc-debug-test-messages nil
  "List of debug messages captured during testing.")

(defun ecc-debug-test-capture-messages (orig-fun &rest args)
  "Capture messages for testing.
ORIG-FUN is the original message function.
ARGS are the arguments to pass to it."
  (let ((msg (apply #'format args)))
    (push msg ecc-debug-test-messages)
    (apply orig-fun args)))

(defmacro with-debug-message-capture (&rest body)
  "Execute BODY while capturing debug messages."
  `(let ((ecc-debug-test-messages nil))
     (advice-add 'message :around #'ecc-debug-test-capture-messages)
     (unwind-protect
         (progn ,@body)
       (advice-remove 'message #'ecc-debug-test-capture-messages))))

(defun ecc-debug-test-message-contains-p (substring)
  "Return non-nil if any captured message contains SUBSTRING."
  (cl-some (lambda (msg) (string-match-p (regexp-quote substring) msg))
           ecc-debug-test-messages))

;;;; Basic debug message tests

(ert-deftest test-debug-message-enabled ()
  "Test that debug messages appear when debugging is enabled."
  (let ((ecc-debug-enabled t)
        (ecc-debug-prefix "[Test]")
        (ecc-debug-timestamp nil))
    (with-debug-message-capture
     (ecc-debug-message "Test message %d" 42)
     (should (ecc-debug-test-message-contains-p "[Test] Test message 42")))))

(ert-deftest test-debug-message-disabled ()
  "Test that debug messages don't appear when debugging is disabled."
  (let ((ecc-debug-enabled nil))
    (with-debug-message-capture
     (ecc-debug-message "Test message")
     (should-not ecc-debug-test-messages))))

(ert-deftest test-debug-message-timestamp ()
  "Test that timestamps appear in debug messages when enabled."
  (let ((ecc-debug-enabled t)
        (ecc-debug-timestamp t)
        (ecc-debug-prefix ""))
    (with-debug-message-capture
     (ecc-debug-message "Test with timestamp")
     ;; Check for timestamp format [HH:MM:SS.NNN]
     (should (string-match-p "\\[\\([0-9]\\{2\\}:\\)\\{2\\}[0-9]\\{2\\}\\.[0-9]\\{3\\}\\]"
                          (car ecc-debug-test-messages))))))

;;;; Category-based debug tests

(ert-deftest test-debug-category-message ()
  "Test debug messages with categories."
  (let ((ecc-debug-enabled t)
        (ecc-debug-prefix "")
        (ecc-debug-timestamp nil)
        (ecc-debug-enabled-categories nil))  ; All categories enabled
    (with-debug-message-capture
     (ecc-debug-message-category 'test "Category test")
     (should (ecc-debug-test-message-contains-p "[test] Category test")))))

(ert-deftest test-debug-category-filtering ()
  "Test that category filtering works correctly."
  (let ((ecc-debug-enabled t)
        (ecc-debug-prefix "")
        (ecc-debug-timestamp nil)
        (ecc-debug-enabled-categories '(enabled-category)))
    (with-debug-message-capture
     ;; Should appear (category is enabled)
     (ecc-debug-message-category 'enabled-category "Should appear")
     ;; Should not appear (category is not enabled)
     (ecc-debug-message-category 'disabled-category "Should not appear")
     
     (should (ecc-debug-test-message-contains-p "Should appear"))
     (should-not (ecc-debug-test-message-contains-p "Should not appear")))))

(ert-deftest test-debug-toggle-category ()
  "Test toggling debug categories."
  (let ((ecc-debug-enabled t)
        (ecc-debug-categories '(cat1 cat2 cat3))
        (ecc-debug-enabled-categories nil))  ; All enabled initially
    
    ;; Test initial state (all enabled)
    (should (ecc-debug--category-enabled-p 'cat1))
    (should (ecc-debug--category-enabled-p 'cat2))
    
    ;; Disable cat1
    (ecc-debug-toggle-category 'cat1)
    (should-not (ecc-debug--category-enabled-p 'cat1))
    (should (ecc-debug--category-enabled-p 'cat2))
    
    ;; Re-enable cat1
    (ecc-debug-toggle-category 'cat1)
    (should (ecc-debug--category-enabled-p 'cat1))
    
    ;; Disable cat2
    (ecc-debug-toggle-category 'cat2)
    (should (ecc-debug--category-enabled-p 'cat1))
    (should-not (ecc-debug--category-enabled-p 'cat2))
    
    ;; Enable all categories
    (ecc-debug-enable-all-categories)
    (should (ecc-debug--category-enabled-p 'cat1))
    (should (ecc-debug--category-enabled-p 'cat2))))

;;;; Buffer-local debug tests

(ert-deftest test-debug-buffer-message ()
  "Test buffer-local debug messages."
  (let ((ecc-debug-enabled t)
        (ecc-debug-prefix "")
        (ecc-debug-timestamp nil)
        (test-buffer (generate-new-buffer "*debug-test*")))
    (unwind-protect
        (with-current-buffer test-buffer
          (setq-local ecc-debug-buffer-enabled t)
          
          (with-debug-message-capture
           (ecc-debug-buffer-message test-buffer "Buffer test")
           (should (ecc-debug-test-message-contains-p 
                    (format "[%s] Buffer test" (buffer-name test-buffer))))))
      (kill-buffer test-buffer))))

(ert-deftest test-debug-buffer-disabled ()
  "Test that buffer-local debug is respected."
  (let ((ecc-debug-enabled t)
        (test-buffer (generate-new-buffer "*debug-test*")))
    (unwind-protect
        (with-current-buffer test-buffer
          (setq-local ecc-debug-buffer-enabled nil)
          
          (with-debug-message-capture
           (ecc-debug-buffer-message test-buffer "Should not appear")
           (should-not ecc-debug-test-messages)))
      (kill-buffer test-buffer))))

(ert-deftest test-debug-toggle-buffer ()
  "Test toggling buffer-local debug."
  (let ((test-buffer (generate-new-buffer "*debug-test*")))
    (unwind-protect
        (with-current-buffer test-buffer
          (setq-local ecc-debug-buffer-enabled nil)
          
          ;; Enable debug for this buffer
          (ecc-debug-toggle-buffer)
          (should ecc-debug-buffer-enabled)
          
          ;; Disable debug for this buffer
          (ecc-debug-toggle-buffer)
          (should-not ecc-debug-buffer-enabled))
      (kill-buffer test-buffer))))

;;;; Factory function tests

(ert-deftest test-debug-make-debug-fn ()
  "Test creating specialized debug functions."
  (let ((ecc-debug-enabled t)
        (ecc-debug-prefix "")
        (ecc-debug-timestamp nil)
        (ecc-debug-enabled-categories nil)
        (test-buffer (generate-new-buffer "*debug-test*")))
    
    (unwind-protect
        (with-current-buffer test-buffer
          (setq-local ecc-debug-buffer-enabled t)
          
          ;; Test global debug function
          (let ((debug-fn (ecc-debug-make-debug-fn)))
            (with-debug-message-capture
             (funcall debug-fn "Global debug")
             (should (ecc-debug-test-message-contains-p "Global debug"))))
          
          ;; Test category debug function
          (let ((debug-fn (ecc-debug-make-debug-fn nil 'test-category)))
            (with-debug-message-capture
             (funcall debug-fn "Category debug")
             (should (ecc-debug-test-message-contains-p 
                      "[test-category] Category debug"))))
          
          ;; Test buffer debug function
          (let ((debug-fn (ecc-debug-make-debug-fn test-buffer)))
            (with-debug-message-capture
             (funcall debug-fn "Buffer debug")
             (should (ecc-debug-test-message-contains-p 
                      (format "[%s] Buffer debug" (buffer-name test-buffer))))))
          
          ;; Test combined buffer+category debug function
          (let ((debug-fn (ecc-debug-make-debug-fn test-buffer 'test-category)))
            (with-debug-message-capture
             (funcall debug-fn "Combined debug")
             (should (ecc-debug-test-message-contains-p 
                      (format "[test-category] [%s] Combined debug" 
                              (buffer-name test-buffer)))))))
      (kill-buffer test-buffer))))

;;;; Module-specific debug functions

(ert-deftest test-module-specific-debug ()
  "Test module-specific debug functions."
  (let ((ecc-debug-enabled t)
        (ecc-debug-prefix "")
        (ecc-debug-timestamp nil)
        (ecc-debug-enabled-categories nil))
    
    ;; Test auto-response debug
    (with-debug-message-capture
     (ecc-debug-auto-response "Auto-response test")
     (should (ecc-debug-test-message-contains-p 
              "[auto-response] Auto-response test")))
    
    ;; Test state debug
    (with-debug-message-capture
     (ecc-debug-state "State test")
     (should (ecc-debug-test-message-contains-p 
              "[state] State test")))
    
    ;; Test core debug
    (with-debug-message-capture
     (ecc-debug-core "Core test")
     (should (ecc-debug-test-message-contains-p 
              "[core] Core test")))
    
    ;; Test buffer debug
    (with-debug-message-capture
     (ecc-debug-buffer "Buffer test")
     (should (ecc-debug-test-message-contains-p 
              "[buffer] Buffer test")))
    
    ;; Test vterm debug
    (with-debug-message-capture
     (ecc-debug-vterm "VTerm test")
     (should (ecc-debug-test-message-contains-p 
              "[vterm] VTerm test")))))

;;;; Debug log buffer tests

(ert-deftest test-debug-log-buffer ()
  "Test logging to debug buffer."
  (let ((ecc-debug-enabled t)
        (ecc-debug-prefix "")
        (ecc-debug-timestamp nil)
        (ecc-debug-log-buffer-name "*test-debug-log*"))
    
    ;; Clear any existing log buffer
    (when (get-buffer ecc-debug-log-buffer-name)
      (kill-buffer ecc-debug-log-buffer-name))
    
    ;; Log a message
    (ecc-debug-message "Log buffer test")
    
    ;; Check if log buffer was created and contains the message
    (let ((log-buffer (get-buffer ecc-debug-log-buffer-name)))
      (should log-buffer)
      (with-current-buffer log-buffer
        (should (string-match-p "Log buffer test" 
                             (buffer-substring-no-properties (point-min) (point-max))))))
    
    ;; Clear the log
    (ecc-debug-clear-log)
    
    ;; Check if log was cleared
    (with-current-buffer (get-buffer ecc-debug-log-buffer-name)
      (should (string-match-p "Log cleared at" 
                           (buffer-substring-no-properties (point-min) (point-max)))))
    
    ;; Clean up
    (kill-buffer ecc-debug-log-buffer-name)))

(ert-deftest test-debug-log-buffer-trimming ()
  "Test that log buffer is trimmed when it gets too long."
  (let ((ecc-debug-enabled t)
        (ecc-debug-prefix "")
        (ecc-debug-timestamp nil)
        (ecc-debug-log-buffer-name "*test-debug-log*")
        (ecc-debug-log-max-lines 10))
    
    ;; Clear any existing log buffer
    (when (get-buffer ecc-debug-log-buffer-name)
      (kill-buffer ecc-debug-log-buffer-name))
    
    ;; Log more messages than the max line count
    (dotimes (i 20)
      (ecc-debug-message "Log line %d" i))
    
    ;; Check that the buffer was trimmed
    (let ((log-buffer (get-buffer ecc-debug-log-buffer-name)))
      (should log-buffer)
      (with-current-buffer log-buffer
        ;; Should contain trimming message
        (should (string-match-p "Log trimmed at" 
                             (buffer-substring-no-properties (point-min) (point-max))))
        ;; Should not contain earliest messages
        (should-not (string-match-p "Log line 0" 
                                  (buffer-substring-no-properties (point-min) (point-max))))
        ;; Should contain later messages
        (should (string-match-p "Log line 19" 
                             (buffer-substring-no-properties (point-min) (point-max))))))
    
    ;; Clean up
    (kill-buffer ecc-debug-log-buffer-name)))

;;;; Backward compatibility tests

(ert-deftest test-debug-backward-compatibility ()
  "Test backward compatibility aliases."
  (let ((ecc-debug-enabled t)
        (ecc-debug-prefix "")
        (ecc-debug-timestamp nil))
    
    ;; Test ecc-debug-utils-message
    (with-debug-message-capture
     (ecc-debug-utils-message "Compatibility test")
     (should (ecc-debug-test-message-contains-p "Compatibility test")))
    
    ;; Test ecc-toggle-debug
    (ecc-toggle-debug)
    (should-not ecc-debug-enabled)
    (ecc-toggle-debug)
    (should ecc-debug-enabled)))

(provide 'test-ecc-debug-utils)

;;; test-ecc-debug-utils.el ends here