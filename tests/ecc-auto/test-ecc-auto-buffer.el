;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 00:10:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-auto/test-ecc-auto-buffer.el

;;; Commentary:
;;; Tests for the buffer-local auto-response module (ecc-auto-buffer.el).

(require 'ert)
(require 'ecc-variables)
(require 'ecc-auto-buffer)

;;; Code:

(defun test-ecc-auto-buffer--mock-buffer ()
  "Create a temporary buffer for testing."
  (let ((buffer (generate-new-buffer "*ecc-test*")))
    buffer))

(defun test-ecc-auto-buffer--cleanup-buffer (buffer)
  "Clean up test BUFFER."
  (when (buffer-live-p buffer)
    (kill-buffer buffer)))

(ert-deftest test-ecc-auto-buffer-init ()
  "Test buffer-local initialization."
  (let ((buffer (test-ecc-auto-buffer--mock-buffer)))
    (unwind-protect
        (progn
          (should (eq (ecc-auto-buffer-init buffer) buffer))
          (with-current-buffer buffer
            (should (boundp 'ecc-auto-buffer-enabled))
            (should (boundp 'ecc-auto-buffer-y/n))
            (should (boundp 'ecc-auto-buffer-y/y/n))
            (should (boundp 'ecc-auto-buffer-waiting))
            (should (boundp 'ecc-auto-buffer-initial-waiting))
            (should (boundp 'ecc-auto-buffer-state))
            (should (boundp 'ecc-auto-buffer-active-state))
            (should (boundp 'ecc-auto-buffer-last-detection-time))
            (should (boundp 'ecc-auto-buffer-last-response-times))))
      (test-ecc-auto-buffer--cleanup-buffer buffer))))

(ert-deftest test-ecc-auto-buffer-detect-and-update ()
  "Test detection and state updates in buffer."
  (let ((buffer (test-ecc-auto-buffer--mock-buffer)))
    (unwind-protect
        (progn
          (ecc-auto-buffer-init buffer)
          (with-current-buffer buffer
            ;; Mock a Y/N prompt
            (insert "Some text\n❯ 1. Yes\n  2. No\n")
            (should (eq (ecc-auto-buffer-detect-and-update) :y/n))
            (should (eq ecc-auto-buffer-state :y/n))
            (should (> ecc-auto-buffer-last-detection-time 0.0))))
      (test-ecc-auto-buffer--cleanup-buffer buffer))))

(ert-deftest test-ecc-auto-buffer-throttled-p ()
  "Test throttling logic for buffer-local responses."
  (let ((buffer (test-ecc-auto-buffer--mock-buffer)))
    (unwind-protect
        (progn
          (ecc-auto-buffer-init buffer)
          
          ;; Should not be throttled initially
          (with-current-buffer buffer
            (should-not (ecc-auto-buffer-throttled-p :y/n)))
          
          ;; Update the time to now, then should be throttled
          (with-current-buffer buffer
            (ecc-auto-buffer-update-response-time :y/n)
            (should (ecc-auto-buffer-throttled-p :y/n))
            
            ;; Other state types should not be throttled
            (should-not (ecc-auto-buffer-throttled-p :waiting))))
      (test-ecc-auto-buffer--cleanup-buffer buffer))))

(ert-deftest test-ecc-auto-buffer-active-state ()
  "Test setting and clearing the active state."
  (let ((buffer (test-ecc-auto-buffer--mock-buffer)))
    (unwind-protect
        (progn
          (ecc-auto-buffer-init buffer)
          
          ;; Check initial state
          (with-current-buffer buffer
            (should-not ecc-auto-buffer-active-state))
          
          ;; Set active state
          (ecc-auto-buffer-set-active :y/n buffer)
          (with-current-buffer buffer
            (should (eq ecc-auto-buffer-active-state :y/n)))
          
          ;; Clear active state
          (ecc-auto-buffer-clear-active buffer)
          (with-current-buffer buffer
            (should-not ecc-auto-buffer-active-state)))
      (test-ecc-auto-buffer--cleanup-buffer buffer))))

(ert-deftest test-ecc-auto-buffer-state-predicates ()
  "Test state predicate functions."
  (let ((buffer (test-ecc-auto-buffer--mock-buffer)))
    (unwind-protect
        (progn
          (ecc-auto-buffer-init buffer)
          
          ;; Initially no state
          (with-current-buffer buffer
            (should-not (ecc-auto-buffer-has-prompt-p))
            (should-not (ecc-auto-buffer-has-state-p :y/n)))
          
          ;; Set state and check predicates
          (with-current-buffer buffer
            (setq-local ecc-auto-buffer-state :y/n)
            (should (ecc-auto-buffer-has-prompt-p))
            (should (ecc-auto-buffer-has-state-p :y/n))
            (should-not (ecc-auto-buffer-has-state-p :waiting))))
      (test-ecc-auto-buffer--cleanup-buffer buffer))))

(ert-deftest test-ecc-auto-buffer-enable-disable ()
  "Test enabling and disabling auto-response in buffer."
  (let ((buffer (test-ecc-auto-buffer--mock-buffer)))
    (unwind-protect
        (progn
          (ecc-auto-buffer-init buffer)
          
          ;; Default should be disabled
          (with-current-buffer buffer
            (should-not ecc-auto-buffer-enabled))
          
          ;; Enable
          (ecc-auto-buffer-enable buffer)
          (with-current-buffer buffer
            (should ecc-auto-buffer-enabled))
          
          ;; Disable
          (ecc-auto-buffer-disable buffer)
          (with-current-buffer buffer
            (should-not ecc-auto-buffer-enabled))
          
          ;; Toggle
          (ecc-auto-buffer-toggle buffer)
          (with-current-buffer buffer
            (should ecc-auto-buffer-enabled)))
      (test-ecc-auto-buffer--cleanup-buffer buffer))))

(provide 'test-ecc-auto-buffer)

;;; test-ecc-auto-buffer.el ends here