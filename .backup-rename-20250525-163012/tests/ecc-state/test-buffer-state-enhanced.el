;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 21:45:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-state/test-buffer-state-enhanced.el

;;; Commentary:
;;; Tests for enhanced buffer-local state tracking.

(require 'ert)
(require 'ecc-variables)
(require 'ecc-state-detection)
(require 'ecc-buffer-state)

;; Test fixtures
(defvar ecc-test-buffer-a nil "First test buffer.")
(defvar ecc-test-buffer-b nil "Second test buffer.")

(defun ecc-test-setup-enhanced-buffers ()
  "Set up test buffers for enhanced buffer-local state tests."
  ;; Create test buffers
  (setq ecc-test-buffer-a (generate-new-buffer "*ecc-test-a*"))
  (setq ecc-test-buffer-b (generate-new-buffer "*ecc-test-b*"))
  
  ;; Initialize buffer state
  (with-current-buffer ecc-test-buffer-a
    (ecc-buffer-state-init))
  (with-current-buffer ecc-test-buffer-b
    (ecc-buffer-state-init))
  
  ;; Fill with different content
  (with-current-buffer ecc-test-buffer-a
    (erase-buffer)
    (insert "Some content here\n")
    (insert "│ > Try \n"))  ;; initial-waiting pattern
  
  (with-current-buffer ecc-test-buffer-b
    (erase-buffer)
    (insert "Other content\n")
    (insert "[Y/n]\n")))  ;; y/n pattern

(defun ecc-test-teardown-enhanced-buffers ()
  "Clean up test buffers."
  (when (buffer-live-p ecc-test-buffer-a)
    (kill-buffer ecc-test-buffer-a))
  (when (buffer-live-p ecc-test-buffer-b)
    (kill-buffer ecc-test-buffer-b))
  (setq ecc-test-buffer-a nil)
  (setq ecc-test-buffer-b nil))

(ert-deftest ecc-test-buffer-state-container ()
  "Test buffer-local state container operations."
  (ecc-test-setup-enhanced-buffers)
  (unwind-protect
      (progn
        ;; Test setting and getting values in first buffer
        (with-current-buffer ecc-test-buffer-a
          (ecc-buffer-state-set 'test-key "test-value")
          (should (string= (ecc-buffer-state-get 'test-key) "test-value"))
          (should (ecc-buffer-state-has-key-p 'test-key))
          (should-not (ecc-buffer-state-has-key-p 'nonexistent-key)))
        
        ;; Test values are buffer-local
        (with-current-buffer ecc-test-buffer-b
          (should-not (ecc-buffer-state-has-key-p 'test-key))
          (ecc-buffer-state-set 'test-key "different-value")
          (should (string= (ecc-buffer-state-get 'test-key) "different-value")))
        
        ;; Test first buffer still has its own value
        (with-current-buffer ecc-test-buffer-a
          (should (string= (ecc-buffer-state-get 'test-key) "test-value")))
        
        ;; Test removing keys
        (with-current-buffer ecc-test-buffer-a
          (ecc-buffer-state-remove 'test-key)
          (should-not (ecc-buffer-state-has-key-p 'test-key)))
        
        ;; Test clearing all state
        (with-current-buffer ecc-test-buffer-b
          (ecc-buffer-state-clear)
          (should-not (ecc-buffer-state-has-key-p 'test-key))))
    (ecc-test-teardown-enhanced-buffers)))

(ert-deftest ecc-test-buffer-state-prompt-tracking ()
  "Test buffer-local prompt state tracking."
  (ecc-test-setup-enhanced-buffers)
  (unwind-protect
      (progn
        ;; Test detecting and updating state in first buffer
        (with-current-buffer ecc-test-buffer-a
          (should (eq (ecc-detect-state) :initial-waiting))
          (ecc-buffer-state-update-prompt :initial-waiting)
          (should (eq (ecc-buffer-state-get-prompt) :initial-waiting))
          (should (> (ecc-buffer-state-get 'last-detection-time 0.0) 0.0))
          (should (> (ecc-buffer-state-get 'last-:initial-waiting-time 0.0) 0.0)))
        
        ;; Test different state in second buffer
        (with-current-buffer ecc-test-buffer-b
          (should (eq (ecc-detect-state) :y/n))
          (ecc-buffer-state-update-prompt :y/n)
          (should (eq (ecc-buffer-state-get-prompt) :y/n))
          (should (> (ecc-buffer-state-get 'last-:y/n-time 0.0) 0.0)))
        
        ;; Test states remain independent
        (with-current-buffer ecc-test-buffer-a
          (should (eq (ecc-buffer-state-get-prompt) :initial-waiting))
          (should (= (ecc-buffer-state-get 'last-:y/n-time 0.0) 0.0)))
        
        ;; Test detect-and-update function
        (with-current-buffer ecc-test-buffer-a
          (should (eq (ecc-buffer-state-detect-and-update) :initial-waiting))
          (should (eq (ecc-buffer-state-get-prompt) :initial-waiting)))
        
        (with-current-buffer ecc-test-buffer-b
          (should (eq (ecc-buffer-state-detect-and-update) :y/n))
          (should (eq (ecc-buffer-state-get-prompt) :y/n))))
    (ecc-test-teardown-enhanced-buffers)))

(ert-deftest ecc-test-buffer-state-throttling ()
  "Test buffer-local throttling mechanism."
  (ecc-test-setup-enhanced-buffers)
  (unwind-protect
      (progn
        ;; Set up states and timing
        (with-current-buffer ecc-test-buffer-a
          (ecc-buffer-state-update-prompt :initial-waiting)
          ;; Throttling should be active now
          (should (ecc-buffer-state-throttled-p :initial-waiting))
          ;; Different state shouldn't be throttled
          (should-not (ecc-buffer-state-throttled-p :y/n)))
        
        ;; Different buffer should have independent throttling
        (with-current-buffer ecc-test-buffer-b
          ;; No throttling in other buffer for same state
          (should-not (ecc-buffer-state-throttled-p :initial-waiting))
          ;; Set a different state
          (ecc-buffer-state-update-prompt :y/n)
          ;; Should be throttled for its own state
          (should (ecc-buffer-state-throttled-p :y/n))))
    (ecc-test-teardown-enhanced-buffers)))

(ert-deftest ecc-test-buffer-state-predicates ()
  "Test buffer-local state predicates."
  (ecc-test-setup-enhanced-buffers)
  (unwind-protect
      (progn
        ;; Set up states
        (with-current-buffer ecc-test-buffer-a
          (ecc-buffer-state-update-prompt :initial-waiting)
          ;; Test predicates
          (should (ecc-buffer-state-initial-waiting-p))
          (should-not (ecc-buffer-state-y/n-p))
          (should-not (ecc-buffer-state-y/y/n-p))
          (should-not (ecc-buffer-state-waiting-p)))
        
        ;; Different state in second buffer
        (with-current-buffer ecc-test-buffer-b
          (ecc-buffer-state-update-prompt :y/n)
          ;; Test predicates
          (should (ecc-buffer-state-y/n-p))
          (should-not (ecc-buffer-state-initial-waiting-p))
          (should-not (ecc-buffer-state-y/y/n-p))
          (should-not (ecc-buffer-state-waiting-p))))
    (ecc-test-teardown-enhanced-buffers)))

(ert-deftest ecc-test-buffer-state-compat ()
  "Test compatibility with standard buffer-local variables."
  (ecc-test-setup-enhanced-buffers)
  (unwind-protect
      (progn
        ;; Set up with traditional vars
        (with-current-buffer ecc-test-buffer-a
          (setq-local ecc-buffer-state :initial-waiting)
          (setq-local ecc-buffer-last-state-time 12345.0)
          (setq-local ecc-buffer-active-state :initial-waiting)
          
          ;; Import to new system
          (ecc-buffer-state-import-standard)
          
          ;; Test values imported correctly
          (should (eq (ecc-buffer-state-get 'prompt-state) :initial-waiting))
          (should (= (ecc-buffer-state-get 'last-detection-time) 12345.0))
          (should (eq (ecc-buffer-state-get 'active-state) :initial-waiting))
          
          ;; Modify with new system
          (ecc-buffer-state-set 'prompt-state :waiting)
          (ecc-buffer-state-set 'last-detection-time 54321.0)
          
          ;; Export back to traditional vars
          (ecc-buffer-state-export-standard)
          
          ;; Test values exported correctly
          (should (eq ecc-buffer-state :waiting))
          (should (= ecc-buffer-last-state-time 54321.0))))
    (ecc-test-teardown-enhanced-buffers)))

(provide 'test-buffer-state-enhanced)

;;; test-buffer-state-enhanced.el ends here