;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 00:00:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-auto/test-ecc-auto-detect.el

;;; Commentary:
;;; Tests for the unified prompt detection module (ecc-auto-detect.el).

(require 'ert)
(require 'ecc-variables)
(require 'ecc-auto-detect)

;;; Code:

(defun test-ecc-auto-detect--mock-buffer (content)
  "Create a temporary buffer with CONTENT for testing."
  (let ((buffer (generate-new-buffer "*ecc-test*")))
    (with-current-buffer buffer
      (insert content))
    buffer))

(defun test-ecc-auto-detect--cleanup-buffer (buffer)
  "Clean up test BUFFER."
  (when (buffer-live-p buffer)
    (kill-buffer buffer)))

(ert-deftest test-ecc-auto-detect-y/n ()
  "Test Y/N prompt detection."
  (let* ((content (concat "Some text before the prompt\n"
                         "❯ 1. Yes\n"
                         "  2. No\n"))
         (buffer (test-ecc-auto-detect--mock-buffer content)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (should (eq (ecc-auto-detect-prompt) :y/n))
            (should (ecc-auto-detect-y/n-p))
            (should-not (ecc-auto-detect-y/y/n-p))
            (should-not (ecc-auto-detect-waiting-p))
            (should-not (ecc-auto-detect-initial-waiting-p))))
      (test-ecc-auto-detect--cleanup-buffer buffer))))

(ert-deftest test-ecc-auto-detect-y/y/n ()
  "Test Y/Y/N prompt detection."
  (let* ((content (concat "Some text before the prompt\n"
                         "❯ 1. Yes\n"
                         "  2. Yes, and revise the file further\n"
                         "  3. No\n"))
         (buffer (test-ecc-auto-detect--mock-buffer content)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (should (eq (ecc-auto-detect-prompt) :y/y/n))
            (should-not (ecc-auto-detect-y/n-p))
            (should (ecc-auto-detect-y/y/n-p))
            (should-not (ecc-auto-detect-waiting-p))
            (should-not (ecc-auto-detect-initial-waiting-p))))
      (test-ecc-auto-detect--cleanup-buffer buffer))))

(ert-deftest test-ecc-auto-detect-waiting ()
  "Test waiting prompt detection."
  (let* ((content (concat "Some text before the prompt\n"
                         "│ >                            \n"))
         (buffer (test-ecc-auto-detect--mock-buffer content)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (should (eq (ecc-auto-detect-prompt) :waiting))
            (should-not (ecc-auto-detect-y/n-p))
            (should-not (ecc-auto-detect-y/y/n-p))
            (should (ecc-auto-detect-waiting-p))
            (should-not (ecc-auto-detect-initial-waiting-p))))
      (test-ecc-auto-detect--cleanup-buffer buffer))))

(ert-deftest test-ecc-auto-detect-initial-waiting ()
  "Test initial waiting prompt detection."
  (let* ((content (concat "Claude is ready for your request\n"
                         "│ > Try \n"))
         (buffer (test-ecc-auto-detect--mock-buffer content)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (should (eq (ecc-auto-detect-prompt) :initial-waiting))
            (should-not (ecc-auto-detect-y/n-p))
            (should-not (ecc-auto-detect-y/y/n-p))
            (should-not (ecc-auto-detect-waiting-p))
            (should (ecc-auto-detect-initial-waiting-p))))
      (test-ecc-auto-detect--cleanup-buffer buffer))))

(ert-deftest test-ecc-auto-detect-name ()
  "Test getting human-readable state names."
  (should (string= (ecc-auto-detect-name :y/n) "Y/N"))
  (should (string= (ecc-auto-detect-name :y/y/n) "Y/Y/N"))
  (should (string= (ecc-auto-detect-name :waiting) "Continue"))
  (should (string= (ecc-auto-detect-name :initial-waiting) "Initial-Waiting"))
  (should (string= (ecc-auto-detect-name :something-else) ":SOMETHING-ELSE")))

(ert-deftest test-ecc-auto-detect-in-region ()
  "Test prompt detection in a specific region."
  (let* ((content (concat "Some text at the beginning\n"
                         "❯ 1. Yes\n"
                         "  2. No\n"
                         "More text at the end\n"))
         (buffer (test-ecc-auto-detect--mock-buffer content))
         start end)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (goto-char (point-min))
            (re-search-forward "❯ 1\\. Yes")
            (beginning-of-line)
            (setq start (point))
            (forward-line 2)
            (setq end (point))
            (should (eq (ecc-auto-detect-in-region start end) :y/n))))
      (test-ecc-auto-detect--cleanup-buffer buffer))))

(provide 'test-ecc-auto-detect)

;;; test-ecc-auto-detect.el ends here