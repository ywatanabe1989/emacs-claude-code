;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-28 07:13:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-auto-response.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'ecc-auto-response)

;; Test setup and teardown functions
(defun test-ecc-auto-response-setup ()
  "Setup function to ensure clean test environment."
  ;; Clear the registry before each test
  (clrhash --ecc-auto-response--registered-buffers)
  ;; Ensure timer is stopped
  (when --ecc-auto-response--timer
    (cancel-timer --ecc-auto-response--timer)
    (setq --ecc-auto-response--timer nil)))

(defun test-ecc-auto-response-teardown ()
  "Teardown function to clean up after tests."
  ;; Clear the registry
  (clrhash --ecc-auto-response--registered-buffers)
  ;; Stop any running timers
  (when --ecc-auto-response--timer
    (cancel-timer --ecc-auto-response--timer)
    (setq --ecc-auto-response--timer nil)))

(ert-deftest test-ecc-auto-response-loadable ()
  "Test that ecc-auto-response loads correctly."
  (should (featurep 'ecc-auto-response)))

(ert-deftest test-ecc-auto-toggle-exists ()
  "Test that ecc-auto-toggle alias exists."
  (should (functionp 'ecc-auto-toggle)))

(ert-deftest test-ecc-auto-toggle-interactive ()
  "Test that ecc-auto-toggle is interactive."
  (should (commandp 'ecc-auto-toggle)))

(ert-deftest test-ecc-auto-response-interval-is-number ()
  "Test that auto-response interval is a number."
  (should (numberp --ecc-auto-response-interval)))

(ert-deftest test-ecc-auto-response-interval-positive ()
  "Test that auto-response interval is positive."
  (should (> --ecc-auto-response-interval 0)))

(ert-deftest test-ecc-auto-response-safe-interval-is-number ()
  "Test that safe interval is a number."
  (should (numberp --ecc-auto-response-safe-interval)))

(ert-deftest test-ecc-auto-response-safe-interval-positive ()
  "Test that safe interval is positive."
  (should (> --ecc-auto-response-safe-interval 0)))

(ert-deftest test-ecc-auto-response-throttle-duration-is-number ()
  "Test that throttle duration is a number."
  (should (numberp --ecc-auto-response-throttle-duration)))

(ert-deftest test-ecc-auto-response-throttle-duration-positive ()
  "Test that throttle duration is positive."
  (should (> --ecc-auto-response-throttle-duration 0)))

(ert-deftest test-ecc-auto-response-responses-is-list ()
  "Test that response alist is a list."
  (should (listp --ecc-auto-response-responses)))

(ert-deftest test-ecc-auto-response-responses-has-yn ()
  "Test that response alist contains :y/n key."
  (should (assq :y/n --ecc-auto-response-responses)))

(ert-deftest test-ecc-auto-response-responses-has-waiting ()
  "Test that response alist contains :waiting key."
  (should (assq :waiting --ecc-auto-response-responses)))

(ert-deftest test-ecc-auto-response-registry-exists ()
  "Test that buffer registry exists."
  (should (hash-table-p --ecc-auto-response--registered-buffers)))

(ert-deftest test-ecc-auto-response-modeline-buffer-local ()
  "Test that auto-response enabled state is buffer-local."
  (test-ecc-auto-response-setup)
  (let ((buf1 (generate-new-buffer "*test-auto-1*"))
        (buf2 (generate-new-buffer "*test-auto-2*")))
    (unwind-protect
        (progn
          ;; Enable auto-response in buf1
          (with-current-buffer buf1
            (--ecc-auto-response-enable-buffer buf1)
            (should --ecc-auto-response--enabled))
          
          ;; Check that buf2 is unaffected
          (with-current-buffer buf2
            (should-not --ecc-auto-response--enabled))
          
          ;; Enable auto-response in buf2
          (with-current-buffer buf2
            (--ecc-auto-response-enable-buffer buf2)
            (should --ecc-auto-response--enabled))
          
          ;; Disable auto-response in buf1
          (with-current-buffer buf1
            (--ecc-auto-response-disable-buffer buf1)
            (should-not --ecc-auto-response--enabled))
          
          ;; Check that buf2 still has auto-response enabled
          (with-current-buffer buf2
            (should --ecc-auto-response--enabled)))
      
      ;; Cleanup
      (when (buffer-live-p buf1) (kill-buffer buf1))
      (when (buffer-live-p buf2) (kill-buffer buf2))
      (test-ecc-auto-response-teardown))))

(ert-deftest test-ecc-auto-response-periodic-timer-starts ()
  "Test that periodic timer starts when auto-response is enabled."
  (let ((test-buffer (generate-new-buffer "*test-periodic*")))
    (unwind-protect
        (progn
          (with-current-buffer test-buffer
            ;; Enable auto-response
            (--ecc-auto-response-enable-buffer test-buffer)
            ;; Check that periodic timer is set
            (should --ecc-auto-response--periodic-timer)
            (should (timerp --ecc-auto-response--periodic-timer))))
      ;; Cleanup
      (when (buffer-live-p test-buffer)
        (--ecc-auto-response-disable-buffer test-buffer)
        (kill-buffer test-buffer)))))

(ert-deftest test-ecc-auto-response-periodic-timer-stops ()
  "Test that periodic timer stops when auto-response is disabled."
  (let ((test-buffer (generate-new-buffer "*test-periodic-stop*")))
    (unwind-protect
        (progn
          (with-current-buffer test-buffer
            ;; Enable and then disable auto-response
            (--ecc-auto-response-enable-buffer test-buffer)
            (--ecc-auto-response-disable-buffer test-buffer)
            ;; Check that periodic timer is stopped
            (should-not --ecc-auto-response--periodic-timer)))
      ;; Cleanup
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer)))))

(ert-deftest test-ecc-auto-response-periodic-config-variables ()
  "Test that periodic configuration variables exist with correct defaults."
  (should (boundp '--ecc-auto-response-periodic-interval))
  (should (boundp '--ecc-auto-response-periodic-enabled))
  (should (numberp --ecc-auto-response-periodic-interval))
  (should (= --ecc-auto-response-periodic-interval 300.0))
  (should (booleanp --ecc-auto-response-periodic-enabled)))

(provide 'test-ecc-auto-response)

(when (not load-file-name)
  (message "test-ecc-auto-response.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))