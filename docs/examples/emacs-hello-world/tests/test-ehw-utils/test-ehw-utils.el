;;; test-ehw-utils.el --- Tests for ehw-utils -*- lexical-binding: t; -*-

;; Author: ywatanabe  
;; Timestamp: <2025-05-12 22:05:45>

;;; Commentary:
;; Tests for the ehw-utils module

;;; Code:

(require 'ert)
(require 'ehw-utils)

(ert-deftest test-ehw-utils-format ()
  "Test if ehw-utils-format correctly formats text."
  (should 
   (equal (ehw-utils-format "hello") ">> HELLO <<")))

(ert-deftest test-ehw-utils-count-words ()
  "Test if ehw-utils-count-words counts words correctly."
  (should
   (= (ehw-utils-count-words "hello world") 2)))

(ert-deftest test-ehw-utils-count-words-empty ()
  "Test if ehw-utils-count-words handles empty string."
  (should
   (= (ehw-utils-count-words "") 0)))

(ert-deftest test-ehw-utils-trim ()
  "Test if ehw-utils-trim trims whitespace."
  (should
   (equal (ehw-utils-trim "  hello  ") "hello")))

(provide 'test-ehw-utils)
;;; test-ehw-utils.el ends here