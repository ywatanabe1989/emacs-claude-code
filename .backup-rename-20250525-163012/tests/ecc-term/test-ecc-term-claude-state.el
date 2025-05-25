;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 23:59:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-term/test-ecc-term-claude-state.el

;;; Commentary:
;;; Tests for the ecc-term-claude-state module.

(require 'ert)
(require 'ecc-term-claude-state)

;;; Code:

(defun test-ecc-term-claude-with-buffer (content test-fn)
  "Helper function to test Claude state detection with CONTENT.
Creates a temporary buffer with CONTENT and calls TEST-FN with the buffer."
  (let ((temp-buffer (generate-new-buffer "*claude-test*")))
    (unwind-protect
        (progn
          (with-current-buffer temp-buffer
            (insert content))
          (funcall test-fn temp-buffer))
      (when (buffer-live-p temp-buffer)
        (kill-buffer temp-buffer)))))

(ert-deftest test-ecc-term-claude-detect-basic-state-y-n ()
  "Test basic detection of Y/N state."
  (test-ecc-term-claude-with-buffer
   "Some content with [y/n] prompt"
   (lambda (buffer)
     (with-current-buffer buffer
       (should (eq (ecc-term-claude-detect-basic-state) :y/n))))))

(ert-deftest test-ecc-term-claude-detect-basic-state-y-y-n ()
  "Test basic detection of Y/Y/N state."
  (test-ecc-term-claude-with-buffer
   "Some content with [Y/y/n] prompt"
   (lambda (buffer)
     (with-current-buffer buffer
       (should (eq (ecc-term-claude-detect-basic-state) :y/y/n))))))

(ert-deftest test-ecc-term-claude-detect-basic-state-waiting ()
  "Test basic detection of waiting state."
  (test-ecc-term-claude-with-buffer
   "Some content with continue> prompt"
   (lambda (buffer)
     (with-current-buffer buffer
       (should (eq (ecc-term-claude-detect-basic-state) :waiting))))))

(ert-deftest test-ecc-term-claude-detect-basic-state-none ()
  "Test basic detection with no recognizable state."
  (test-ecc-term-claude-with-buffer
   "Some content with no recognizable prompt"
   (lambda (buffer)
     (with-current-buffer buffer
       (should (eq (ecc-term-claude-detect-basic-state) nil))))))

(ert-deftest test-ecc-term-claude-get-state ()
  "Test the unified state detection interface."
  (test-ecc-term-claude-with-buffer
   "Some content with [y/n] prompt"
   (lambda (buffer)
     (with-current-buffer buffer
       (should (eq (ecc-term-claude-get-state) :y/n))))))

(ert-deftest test-ecc-term-claude-state-name ()
  "Test conversion of state symbols to human-readable names."
  (should (string= (ecc-term-claude-state-name :y/n) "Y/N"))
  (should (string= (ecc-term-claude-state-name :y/y/n) "Y/Y/N"))
  (should (string= (ecc-term-claude-state-name :waiting) "Continue"))
  (should (string= (ecc-term-claude-state-name :initial-waiting) "Initial-Waiting"))
  (should (string= (ecc-term-claude-state-name nil) "nil")))

(ert-deftest test-ecc-term-claude-state-symbols ()
  "Test retrieval of all known state symbols."
  (let ((symbols (ecc-term-claude-state-symbols)))
    (should (memq :y/n symbols))
    (should (memq :y/y/n symbols))
    (should (memq :waiting symbols))
    (should (memq :initial-waiting symbols))
    (should (= (length symbols) 4))))

(ert-deftest test-ecc-detect-simple-state-alias ()
  "Test backward compatibility alias."
  (test-ecc-term-claude-with-buffer
   "Some content with [y/n] prompt"
   (lambda (buffer)
     (with-current-buffer buffer
       (should (eq (ecc-detect-simple-state) :y/n))))))

(provide 'test-ecc-term-claude-state)

;;; test-ecc-term-claude-state.el ends here