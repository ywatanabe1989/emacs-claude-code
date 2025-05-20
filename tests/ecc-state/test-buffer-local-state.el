;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 18:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-state/test-buffer-local-state.el

;;; Commentary:
;;; Tests for buffer-local state tracking in Claude state detection.

(require 'ert)
(require 'ecc-variables)
(require 'ecc-state-detection)

;; Test fixtures
(defvar ecc-test-buffer-1 nil "First test buffer.")
(defvar ecc-test-buffer-2 nil "Second test buffer.")

(defun ecc-test-setup-buffers ()
  "Set up test buffers for buffer-local state tests."
  ;; Create test buffers
  (setq ecc-test-buffer-1 (generate-new-buffer "*ecc-test-1*"))
  (setq ecc-test-buffer-2 (generate-new-buffer "*ecc-test-2*"))
  
  ;; Register them as Claude buffers
  (with-current-buffer ecc-test-buffer-1
    (ecc-buffer-register ecc-test-buffer-1))
  (with-current-buffer ecc-test-buffer-2
    (ecc-buffer-register ecc-test-buffer-2))
  
  ;; Fill with different content
  (with-current-buffer ecc-test-buffer-1
    (erase-buffer)
    (insert "Some content here\n")
    (insert "│ > Try \n"))  ;; initial-waiting pattern
  
  (with-current-buffer ecc-test-buffer-2
    (erase-buffer)
    (insert "Other content\n")
    (insert "[Y/n]\n")))  ;; y/n pattern

(defun ecc-test-teardown-buffers ()
  "Clean up test buffers."
  (when (buffer-live-p ecc-test-buffer-1)
    (kill-buffer ecc-test-buffer-1))
  (when (buffer-live-p ecc-test-buffer-2)
    (kill-buffer ecc-test-buffer-2))
  (setq ecc-test-buffer-1 nil)
  (setq ecc-test-buffer-2 nil))

(ert-deftest ecc-test-buffer-local-state-detection ()
  "Test that state detection is buffer-specific."
  (ecc-test-setup-buffers)
  (unwind-protect
      (progn
        ;; Detect states in each buffer
        (with-current-buffer ecc-test-buffer-1
          (should (eq (ecc-detect-state) :initial-waiting))
          ;; Set buffer-local state variables
          (setq-local ecc-buffer-state :initial-waiting)
          (setq-local ecc-buffer-last-state-time (float-time)))
        
        (with-current-buffer ecc-test-buffer-2
          (should (eq (ecc-detect-state) :y/n))
          ;; Set buffer-local state variables
          (setq-local ecc-buffer-state :y/n)
          (setq-local ecc-buffer-last-state-time (float-time)))
        
        ;; Verify that states are maintained per-buffer
        (with-current-buffer ecc-test-buffer-1
          (should (eq ecc-buffer-state :initial-waiting)))
        
        (with-current-buffer ecc-test-buffer-2
          (should (eq ecc-buffer-state :y/n))))
    (ecc-test-teardown-buffers)))

(ert-deftest ecc-test-buffer-local-response-patterns ()
  "Test that response patterns can be buffer-specific."
  (ecc-test-setup-buffers)
  (unwind-protect
      (progn
        ;; Set different response patterns for each buffer
        (with-current-buffer ecc-test-buffer-1
          (setq-local ecc-buffer-auto-response-y/n "1")
          (setq-local ecc-buffer-auto-response-initial-waiting "/start"))
        
        (with-current-buffer ecc-test-buffer-2
          (setq-local ecc-buffer-auto-response-y/n "y")
          (setq-local ecc-buffer-auto-response-initial-waiting "/different"))
        
        ;; Test that each buffer has its own settings
        (with-current-buffer ecc-test-buffer-1
          (should (string= ecc-buffer-auto-response-y/n "1"))
          (should (string= ecc-buffer-auto-response-initial-waiting "/start")))
        
        (with-current-buffer ecc-test-buffer-2
          (should (string= ecc-buffer-auto-response-y/n "y"))
          (should (string= ecc-buffer-auto-response-initial-waiting "/different"))))
    (ecc-test-teardown-buffers)))

(ert-deftest ecc-test-buffer-local-throttling ()
  "Test that throttling is buffer-specific."
  (ecc-test-setup-buffers)
  (unwind-protect
      (progn
        ;; Set up throttle times in each buffer
        (with-current-buffer ecc-test-buffer-1
          (setq-local ecc-buffer-last-time-alist
                     '((:y/n . 0.0)
                       (:y/y/n . 0.0)
                       (:waiting . 0.0)
                       (:initial-waiting . 0.0)))
          ;; Update initial-waiting time
          (setf (alist-get :initial-waiting ecc-buffer-last-time-alist) 
                (float-time)))
        
        (with-current-buffer ecc-test-buffer-2
          (setq-local ecc-buffer-last-time-alist
                     '((:y/n . 0.0)
                       (:y/y/n . 0.0)
                       (:waiting . 0.0)
                       (:initial-waiting . 0.0))))
        
        ;; Test that throttling state is buffer-specific
        (with-current-buffer ecc-test-buffer-1
          (should (> (alist-get :initial-waiting ecc-buffer-last-time-alist) 0.0)))
        
        (with-current-buffer ecc-test-buffer-2
          (should (= (alist-get :initial-waiting ecc-buffer-last-time-alist) 0.0))))
    (ecc-test-teardown-buffers)))

(provide 'test-buffer-local-state)

;;; test-buffer-local-state.el ends here