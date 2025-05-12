;;; test-ehw-core.el --- Tests for ehw-core -*- lexical-binding: t; -*-

;; Author: ywatanabe
;; Timestamp: <2025-05-12 22:07:30>

;;; Commentary:
;; Tests for the ehw-core module

;;; Code:

(require 'ert)
(require 'ehw-core)

(ert-deftest test-ehw-core-greet-is-function ()
  "Test if ehw-core-greet is a function."
  (should
   (functionp 'ehw-core-greet)))

(ert-deftest test-ehw-core-greet-is-interactive ()
  "Test if ehw-core-greet is interactive."
  (should
   (commandp 'ehw-core-greet)))

(ert-deftest test-ehw-core-insert-greeting-is-function ()
  "Test if ehw-core-insert-greeting is a function."
  (should
   (functionp 'ehw-core-insert-greeting)))

(ert-deftest test-ehw-core-goodbye-is-function ()
  "Test if ehw-core-goodbye is a function."
  (should
   (functionp 'ehw-core-goodbye)))

(provide 'test-ehw-core)
;;; test-ehw-core.el ends here