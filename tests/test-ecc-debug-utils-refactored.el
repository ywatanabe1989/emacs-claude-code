;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-25 02:45:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-debug-utils-refactored.el

;;; Commentary:
;;; Refactored example showing proper test modularization.
;;; Each test has exactly one assertion, following TDD principles.

;;; Code:
(require 'ert)
(require 'ecc-variables)
(require 'ecc-debug-utils)

;;;; Test helpers (unchanged)

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

;;;; Refactored test-debug-toggle-category tests

(ert-deftest test-debug-toggle-category-all-enabled-initially ()
  "Test that all debug categories are enabled when enabled-categories is nil."
  (let ((ecc-debug-enabled t)
        (ecc-debug-categories '(cat1 cat2 cat3))
        (ecc-debug-enabled-categories nil))  ; All enabled initially
    (should (ecc-debug--category-enabled-p 'cat1))))

(ert-deftest test-debug-toggle-category-disables-specific-category ()
  "Test that toggling a category disables it when all are initially enabled."
  (let ((ecc-debug-enabled t)
        (ecc-debug-categories '(cat1 cat2 cat3))
        (ecc-debug-enabled-categories nil))
    (ecc-debug-toggle-category 'cat1)
    (should-not (ecc-debug--category-enabled-p 'cat1))))

(ert-deftest test-debug-toggle-category-preserves-other-categories ()
  "Test that toggling one category doesn't affect others."
  (let ((ecc-debug-enabled t)
        (ecc-debug-categories '(cat1 cat2 cat3))
        (ecc-debug-enabled-categories nil))
    (ecc-debug-toggle-category 'cat1)
    (should (ecc-debug--category-enabled-p 'cat2))))

(ert-deftest test-debug-toggle-category-reenables-disabled-category ()
  "Test that toggling a disabled category re-enables it."
  (let ((ecc-debug-enabled t)
        (ecc-debug-categories '(cat1 cat2 cat3))
        (ecc-debug-enabled-categories nil))
    ;; First disable it
    (ecc-debug-toggle-category 'cat1)
    ;; Then re-enable it
    (ecc-debug-toggle-category 'cat1)
    (should (ecc-debug--category-enabled-p 'cat1))))

(ert-deftest test-debug-enable-all-categories-resets-to-all-enabled ()
  "Test that enable-all-categories clears the enabled list."
  (let ((ecc-debug-enabled t)
        (ecc-debug-categories '(cat1 cat2 cat3))
        (ecc-debug-enabled-categories '(cat1)))  ; Only cat1 enabled
    (ecc-debug-enable-all-categories)
    (should (null ecc-debug-enabled-categories))))

;;;; Refactored test-debug-toggle-buffer tests

(ert-deftest test-debug-toggle-buffer-enables-when-disabled ()
  "Test that toggle enables debug when initially disabled."
  (let ((test-buffer (generate-new-buffer "*debug-test*")))
    (unwind-protect
        (with-current-buffer test-buffer
          (setq-local ecc-debug-buffer-enabled nil)
          (ecc-debug-toggle-buffer)
          (should ecc-debug-buffer-enabled))
      (kill-buffer test-buffer))))

(ert-deftest test-debug-toggle-buffer-disables-when-enabled ()
  "Test that toggle disables debug when initially enabled."
  (let ((test-buffer (generate-new-buffer "*debug-test*")))
    (unwind-protect
        (with-current-buffer test-buffer
          (setq-local ecc-debug-buffer-enabled t)
          (ecc-debug-toggle-buffer)
          (should-not ecc-debug-buffer-enabled))
      (kill-buffer test-buffer))))

;;;; Refactored test-debug-make-debug-fn tests

(ert-deftest test-debug-make-debug-fn-creates-global-debug-function ()
  "Test that make-debug-fn without args creates global debug function."
  (let ((ecc-debug-enabled t)
        (ecc-debug-prefix "")
        (ecc-debug-timestamp nil))
    (let ((debug-fn (ecc-debug-make-debug-fn)))
      (with-debug-message-capture
       (funcall debug-fn "Global debug")
       (should (ecc-debug-test-message-contains-p "Global debug"))))))

(ert-deftest test-debug-make-debug-fn-creates-category-debug-function ()
  "Test that make-debug-fn with category creates category-aware function."
  (let ((ecc-debug-enabled t)
        (ecc-debug-prefix "")
        (ecc-debug-timestamp nil)
        (ecc-debug-enabled-categories nil))
    (let ((debug-fn (ecc-debug-make-debug-fn nil 'test-category)))
      (with-debug-message-capture
       (funcall debug-fn "Category debug")
       (should (ecc-debug-test-message-contains-p "[test-category] Category debug"))))))

(ert-deftest test-debug-make-debug-fn-creates-buffer-debug-function ()
  "Test that make-debug-fn with buffer creates buffer-aware function."
  (let ((ecc-debug-enabled t)
        (ecc-debug-prefix "")
        (ecc-debug-timestamp nil)
        (test-buffer (generate-new-buffer "*debug-test*")))
    (unwind-protect
        (with-current-buffer test-buffer
          (setq-local ecc-debug-buffer-enabled t)
          (let ((debug-fn (ecc-debug-make-debug-fn test-buffer)))
            (with-debug-message-capture
             (funcall debug-fn "Buffer debug")
             (should (ecc-debug-test-message-contains-p 
                      (format "[%s] Buffer debug" (buffer-name test-buffer)))))))
      (kill-buffer test-buffer))))

;;;; Refactored test-debug-category-filtering tests

(ert-deftest test-debug-category-filtering-shows-enabled-category ()
  "Test that enabled category messages appear."
  (let ((ecc-debug-enabled t)
        (ecc-debug-prefix "")
        (ecc-debug-timestamp nil)
        (ecc-debug-enabled-categories '(enabled-category)))
    (with-debug-message-capture
     (ecc-debug-message-category 'enabled-category "Should appear")
     (should (ecc-debug-test-message-contains-p "Should appear")))))

(ert-deftest test-debug-category-filtering-hides-disabled-category ()
  "Test that disabled category messages don't appear."
  (let ((ecc-debug-enabled t)
        (ecc-debug-prefix "")
        (ecc-debug-timestamp nil)
        (ecc-debug-enabled-categories '(enabled-category)))
    (with-debug-message-capture
     (ecc-debug-message-category 'disabled-category "Should not appear")
     (should-not (ecc-debug-test-message-contains-p "Should not appear")))))

(provide 'test-ecc-debug-utils-refactored)

;;; test-ecc-debug-utils-refactored.el ends here