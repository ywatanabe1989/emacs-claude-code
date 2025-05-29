;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-30 01:30:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-auto-periodical.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'ecc-auto-periodical)

;; Test that the module loads successfully
(ert-deftest test-ecc-auto-periodical-loadable ()
  "Test that ecc-auto-periodical loads without errors."
  (should (featurep 'ecc-auto-periodical)))

;; Test configuration variables
(ert-deftest test-ecc-auto-periodical-config-variables ()
  "Test that configuration variables exist with correct types."
  (should (boundp 'ecc-auto-periodical-enabled))
  (should (booleanp ecc-auto-periodical-enabled))
  (should (boundp 'ecc-auto-periodical-commands))
  (should (listp ecc-auto-periodical-commands))
  (should (boundp 'ecc-auto-periodical-prompt-before-execute))
  (should (booleanp ecc-auto-periodical-prompt-before-execute))
  (should (boundp 'ecc-auto-periodical-notify))
  (should (booleanp ecc-auto-periodical-notify)))

;; Test interaction counter
(ert-deftest test-ecc-auto-periodical-counter-increments ()
  "Test that interaction counter increments correctly."
  (with-temp-buffer
    (setq-local --ecc-auto-periodical-interaction-counter 0)
    (let ((ecc-auto-periodical-enabled t)
          (ecc-auto-periodical-commands nil)) ; No commands to avoid execution
      (ecc-auto-periodical-increment)
      (should (= --ecc-auto-periodical-interaction-counter 1))
      (ecc-auto-periodical-increment)
      (should (= --ecc-auto-periodical-interaction-counter 2)))))

;; Test counter doesn't increment when disabled
(ert-deftest test-ecc-auto-periodical-counter-disabled ()
  "Test that counter doesn't increment when disabled."
  (with-temp-buffer
    (setq-local --ecc-auto-periodical-interaction-counter 0)
    (let ((ecc-auto-periodical-enabled nil))
      (ecc-auto-periodical-increment)
      (should (= --ecc-auto-periodical-interaction-counter 0)))))

;; Test should-execute-p logic
(ert-deftest test-ecc-auto-periodical-should-execute-interval-5 ()
  "Test execution check for interval of 5."
  (with-temp-buffer
    (setq-local --ecc-auto-periodical-interaction-counter 5)
    (setq-local --ecc-auto-periodical-last-executed (make-hash-table :test 'equal))
    (should (ecc-auto-periodical--should-execute-p 5 "/test"))))

(ert-deftest test-ecc-auto-periodical-should-not-execute-interval-5 ()
  "Test non-execution for non-matching interval."
  (with-temp-buffer
    (setq-local --ecc-auto-periodical-interaction-counter 3)
    (setq-local --ecc-auto-periodical-last-executed (make-hash-table :test 'equal))
    (should-not (ecc-auto-periodical--should-execute-p 5 "/test"))))

;; Test duplicate prevention
(ert-deftest test-ecc-auto-periodical-prevents-duplicates ()
  "Test that same command isn't executed twice at same count."
  (with-temp-buffer
    (setq-local --ecc-auto-periodical-interaction-counter 10)
    (setq-local --ecc-auto-periodical-last-executed (make-hash-table :test 'equal))
    ;; First check should pass
    (should (ecc-auto-periodical--should-execute-p 10 "/test"))
    ;; Simulate execution
    (puthash "/test" 10 --ecc-auto-periodical-last-executed)
    ;; Second check at same count should fail
    (should-not (ecc-auto-periodical--should-execute-p 10 "/test"))))

;; Test reset counter
(ert-deftest test-ecc-auto-periodical-reset-counter ()
  "Test that reset counter works correctly."
  (with-temp-buffer
    (setq-local --ecc-auto-periodical-interaction-counter 42)
    (setq-local --ecc-auto-periodical-last-executed (make-hash-table :test 'equal))
    (puthash "/test" 10 --ecc-auto-periodical-last-executed)
    
    (ecc-auto-periodical-reset-counter)
    
    (should (= --ecc-auto-periodical-interaction-counter 0))
    (should (= (hash-table-count --ecc-auto-periodical-last-executed) 0))))

;; Test toggle functions
(ert-deftest test-ecc-auto-periodical-toggle-exists ()
  "Test that toggle function exists."
  (should (fboundp 'ecc-auto-periodical-toggle)))

(ert-deftest test-ecc-auto-periodical-toggle-interactive ()
  "Test that toggle function is interactive."
  (should (commandp 'ecc-auto-periodical-toggle)))

(ert-deftest test-ecc-auto-periodical-toggle-buffer-exists ()
  "Test that buffer toggle function exists."
  (should (fboundp 'ecc-auto-periodical-toggle-buffer)))

(ert-deftest test-ecc-auto-periodical-status-exists ()
  "Test that status function exists."
  (should (fboundp 'ecc-auto-periodical-status)))

(ert-deftest test-ecc-auto-periodical-status-interactive ()
  "Test that status function is interactive."
  (should (commandp 'ecc-auto-periodical-status)))

(provide 'test-ecc-auto-periodical)

(when (not load-file-name)
  (message "test-ecc-auto-periodical.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

;;; test-ecc-auto-periodical.el ends here