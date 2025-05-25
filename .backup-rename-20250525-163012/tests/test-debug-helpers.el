;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-25 02:55:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-debug-helpers.el

;;; Commentary:
;;; Debug utilities for test files following the debug guidelines.
;;; This module provides consistent debug messaging for test execution.

;;; Code:

;;;; Test debug support

(defvar-local ecc-test-debug-enabled nil
  "Whether debug messages are enabled for test execution.")

(defun ecc-test-debug (format-string &rest args)
  "Send debug message for test execution.
FORMAT-STRING and ARGS are passed to `format'.
Messages go to *Messages* buffer without minibuffer echo."
  (when ecc-test-debug-enabled
    (let ((inhibit-message t))  ; Only to *Messages*, not minibuffer
      (message "[ECC-TEST DEBUG %s] %s" 
               (or (buffer-name) "no-buffer")
               (apply #'format format-string args)))))

(defmacro with-test-debug (&rest body)
  "Execute BODY with test debugging enabled."
  `(let ((ecc-test-debug-enabled t))
     (ecc-test-debug "Starting test execution")
     (unwind-protect
         (progn ,@body)
       (ecc-test-debug "Test execution completed"))))

(defmacro ert-deftest-with-debug (name args &rest body)
  "Define an ERT test with debug support.
NAME is the test name, ARGS are the test arguments, and BODY is the test code."
  (declare (indent 2))
  `(ert-deftest ,name ,args
     (with-test-debug
      (ecc-test-debug "Running test: %s" ',name)
      ,@body)))

(provide 'test-debug-helpers)

;;; test-debug-helpers.el ends here