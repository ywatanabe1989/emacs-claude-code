;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 22:15:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-system/test-buffer-local-background-system.el

;;; Commentary:
;;; System integration tests for buffer-local states and background detection.

(require 'ert)
(require 'ecc-variables)
(require 'ecc-buffer-local)
(require 'ecc-buffer-state)
(require 'ecc-background-detection)
(require 'ecc-auto-response)
(require 'ecc-buffer-local)

;; Test fixtures
(defvar ecc-system-test-buffer-x nil "First system test buffer.")
(defvar ecc-system-test-buffer-y nil "Second system test buffer.")
(defvar ecc-system-test-responses-sent nil "List of responses sent during test.")

(defun ecc-system-test-setup-bg ()
  "Set up system test environment for background detection tests."
  ;; Reset response tracking
  (setq ecc-system-test-responses-sent nil)
  
  ;; Create test buffers
  (setq ecc-system-test-buffer-x (generate-new-buffer "*ecc-system-x*"))
  (setq ecc-system-test-buffer-y (generate-new-buffer "*ecc-system-y*"))
  
  ;; Initialize vterm mode simulation for buffers
  (with-current-buffer ecc-system-test-buffer-x
    (set (make-local-variable 'major-mode) 'vterm-mode))
  
  (with-current-buffer ecc-system-test-buffer-y
    (set (make-local-variable 'major-mode) 'vterm-mode))
  
  ;; Initialize buffer-local state
  (with-current-buffer ecc-system-test-buffer-x
    (ecc-buffer-state-init)
    (ecc-buffer-local-init))
  
  (with-current-buffer ecc-system-test-buffer-y
    (ecc-buffer-state-init)
    (ecc-buffer-local-init))
  
  ;; Fill with different content
  (with-current-buffer ecc-system-test-buffer-x
    (erase-buffer)
    (insert "Some content here\n")
    (insert "│ > Try \n"))  ;; initial-waiting pattern
  
  (with-current-buffer ecc-system-test-buffer-y
    (erase-buffer)
    (insert "Other content\n")
    (insert "[Y/n]\n"))  ;; y/n pattern
  
  ;; Set up distinct auto-response configurations
  (with-current-buffer ecc-system-test-buffer-x
    (setq-local ecc-buffer-auto-response-enabled t)
    (setq-local ecc-buffer-auto-response-initial-waiting "/x-custom")
    (setq-local ecc-buffer-auto-response-y/n "x-yes")
    (setq-local ecc-buffer-auto-notify-completions nil))
  
  (with-current-buffer ecc-system-test-buffer-y
    (setq-local ecc-buffer-auto-response-enabled t)
    (setq-local ecc-buffer-auto-response-initial-waiting "/y-custom")
    (setq-local ecc-buffer-auto-response-y/n "y-yes")
    (setq-local ecc-buffer-auto-notify-completions nil)))

(defun ecc-system-test-teardown-bg ()
  "Clean up system test environment for background detection."
  ;; Stop background detection if running
  (when (and (boundp 'ecc-background-detection-active)
             ecc-background-detection-active)
    (ecc-background-detection-stop))
  
  ;; Kill test buffers
  (when (buffer-live-p ecc-system-test-buffer-x)
    (kill-buffer ecc-system-test-buffer-x))
  (when (buffer-live-p ecc-system-test-buffer-y)
    (kill-buffer ecc-system-test-buffer-y))
  
  ;; Reset variables
  (setq ecc-system-test-buffer-x nil)
  (setq ecc-system-test-buffer-y nil)
  (setq ecc-system-test-responses-sent nil))

(defun ecc-system-test-mock-vterm-response ()
  "Mock vterm response functions for testing."
  ;; Mock vterm functions to track responses
  (cl-letf (((symbol-function 'vterm-send-string)
             (lambda (str)
               (push str ecc-system-test-responses-sent)
               str))
            ((symbol-function 'vterm-send-return)
             (lambda ()
               (push "<RETURN>" ecc-system-test-responses-sent)
               t)))
    ;; Return the mock setup for cleaning
    #'ecc-system-test-restore-vterm-response))

(defun ecc-system-test-restore-vterm-response ()
  "Restore original vterm response functions."
  (fmakunbound 'vterm-send-string)
  (fmakunbound 'vterm-send-return))

(ert-deftest ecc-test-system-bg-buffer-independent-detection ()
  "Test background detection in multiple buffers independently."
  (ecc-system-test-setup-bg)
  (unwind-protect
      (progn
        ;; Register buffers for background detection
        (ecc-background-detection-register-buffer ecc-system-test-buffer-x)
        (ecc-background-detection-register-buffer ecc-system-test-buffer-y)
        
        ;; Verify detection works independently
        (let ((state-x (ecc-background-detect-state-in-buffer ecc-system-test-buffer-x))
              (state-y (ecc-background-detect-state-in-buffer ecc-system-test-buffer-y)))
          
          ;; Check detection results
          (should (eq state-x :initial-waiting))
          (should (eq state-y :y/n))
          
          ;; Check buffer-local state was updated
          (with-current-buffer ecc-system-test-buffer-x
            (should (eq (ecc-buffer-state-get-prompt) :initial-waiting)))
          
          (with-current-buffer ecc-system-test-buffer-y
            (should (eq (ecc-buffer-state-get-prompt) :y/n)))))
    (ecc-system-test-teardown-bg)))

(ert-deftest ecc-test-system-bg-response-integration ()
  "Test background detection integrated with auto-response."
  (ecc-system-test-setup-bg)
  (let ((restore-func (ecc-system-test-mock-vterm-response)))
    (unwind-protect
        (progn
          ;; Enable auto-response system
          (setq ecc-auto-response-enabled t)
          
          ;; Register buffers for auto-response
          (ecc-auto-response-register-buffer ecc-system-test-buffer-x)
          (ecc-auto-response-register-buffer ecc-system-test-buffer-y)
          
          ;; Register buffers for background detection
          (ecc-background-detection-add-buffer ecc-system-test-buffer-x)
          (ecc-background-detection-add-buffer ecc-system-test-buffer-y)
          
          ;; Test auto-response with buffer-local check
          (ecc-auto-response--process-buffer-global ecc-system-test-buffer-x)
          (ecc-auto-response--process-buffer-global ecc-system-test-buffer-y)
          
          ;; Verify responses were sent with buffer-local config
          (let ((responses (reverse ecc-system-test-responses-sent)))
            ;; Should have 4 entries (2 responses + 2 returns)
            (should (= (length responses) 4))
            ;; Check specific responses
            (should (member "/x-custom" responses))
            (should (member "y-yes" responses))))
      
      ;; Clean up mocks
      (funcall restore-func)
      (ecc-system-test-teardown-bg))))

(ert-deftest ecc-test-system-bg-buffer-content-change ()
  "Test detection after buffer content changes."
  (ecc-system-test-setup-bg)
  (unwind-protect
      (progn
        ;; Register buffers
        (ecc-background-detection-register-buffer ecc-system-test-buffer-x)
        
        ;; Initial detection
        (let ((initial-state (ecc-background-detect-state-in-buffer 
                             ecc-system-test-buffer-x)))
          (should (eq initial-state :initial-waiting)))
        
        ;; Change buffer content with cursor away from end
        (with-current-buffer ecc-system-test-buffer-x
          (goto-char (point-min))  ;; Move cursor to beginning
          (erase-buffer)
          (insert "Changed content\n")
          (insert "[Y/y/n]\n"))  ;; y/y/n pattern
        
        ;; Verify detection still works
        (let ((new-state (ecc-background-detect-state-in-buffer 
                         ecc-system-test-buffer-x)))
          (should (eq new-state :y/y/n))
          
          ;; Check buffer-local state was updated
          (with-current-buffer ecc-system-test-buffer-x
            (should (eq (ecc-buffer-state-get-prompt) :y/y/n)))))
    (ecc-system-test-teardown-bg)))

(ert-deftest ecc-test-system-bg-full-cycle ()
  "Test full background detection cycle with multiple buffers."
  (ecc-system-test-setup-bg)
  (let ((restore-func (ecc-system-test-mock-vterm-response)))
    (unwind-protect
        (progn
          ;; Enable auto-response system
          (setq ecc-auto-response-enabled t)
          
          ;; Register buffers for auto-response
          (ecc-auto-response-register-buffer ecc-system-test-buffer-x)
          (ecc-auto-response-register-buffer ecc-system-test-buffer-y)
          
          ;; Start background detection with auto-response callback
          (ecc-background-detection-start #'ecc-auto-response--process-buffer-global)
          
          ;; Register buffers for background detection
          (ecc-background-detection-add-buffer ecc-system-test-buffer-x)
          (ecc-background-detection-add-buffer ecc-system-test-buffer-y)
          
          ;; Manually trigger processing
          (ecc-background-detection-process-buffers)
          
          ;; Verify responses were sent for both buffers
          (let ((responses (reverse ecc-system-test-responses-sent)))
            ;; Should have 4 entries (2 responses + 2 returns) or more
            (should (>= (length responses) 4))
            ;; Check specific responses were sent
            (should (member "/x-custom" responses))
            (should (member "y-yes" responses)))
          
          ;; Check states were updated
          (with-current-buffer ecc-system-test-buffer-x
            (should (eq (ecc-buffer-state-get-prompt) :initial-waiting))
            (should (eq (ecc-buffer-state-get-active) :initial-waiting)))
          
          (with-current-buffer ecc-system-test-buffer-y
            (should (eq (ecc-buffer-state-get-prompt) :y/n))
            (should (eq (ecc-buffer-state-get-active) :y/n))))
      
      ;; Clean up mocks
      (funcall restore-func)
      (ecc-system-test-teardown-bg))))

(ert-deftest ecc-test-system-bg-buffer-local-api ()
  "Test buffer-local auto-response API with background detection."
  (ecc-system-test-setup-bg)
  (unwind-protect
      (progn
        ;; Initialize buffers
        (with-current-buffer ecc-system-test-buffer-x
          (ecc-buffer-local-init))
        
        (with-current-buffer ecc-system-test-buffer-y
          (ecc-buffer-local-init))
        
        ;; Test toggling in first buffer
        (with-current-buffer ecc-system-test-buffer-x
          (setq-local ecc-buffer-auto-response-enabled nil)
          (setq ecc-buffer-auto-response-enabled t)
          (should ecc-buffer-auto-response-enabled)
          (setq ecc-buffer-auto-response-enabled nil)
          (should-not ecc-buffer-auto-response-enabled))
        
        ;; Test buffer-local state doesn't affect other buffer
        (with-current-buffer ecc-system-test-buffer-y
          (setq-local ecc-buffer-auto-response-enabled t))
        
        ;; Verify states remain independent
        (should-not (buffer-local-value 'ecc-buffer-auto-response-enabled
                                      ecc-system-test-buffer-x))
        (should (buffer-local-value 'ecc-buffer-auto-response-enabled
                                   ecc-system-test-buffer-y)))
    (ecc-system-test-teardown-bg)))

(ert-deftest ecc-test-system-bg-integration-multiple-buffers ()
  "Test background system with multiple buffers changing independently."
  (ecc-system-test-setup-bg)
  (unwind-protect
      (progn
        ;; Initialize and register buffers
        (with-current-buffer ecc-system-test-buffer-x
          (ecc-buffer-local-init))
        
        (with-current-buffer ecc-system-test-buffer-y
          (ecc-buffer-local-init))
        
        (ecc-background-detection-add-buffer ecc-system-test-buffer-x)
        (ecc-background-detection-add-buffer ecc-system-test-buffer-y)
        
        ;; Process initial states
        (ecc-background-detection-process-buffers)
        
        ;; Change content in first buffer
        (with-current-buffer ecc-system-test-buffer-x
          (erase-buffer)
          (insert "Content changed in X\n")
          (insert "[Y/n]\n"))  ;; y/n pattern
        
        ;; Change content in second buffer
        (with-current-buffer ecc-system-test-buffer-y
          (erase-buffer)
          (insert "Content changed in Y\n")
          (insert "continue>\n"))  ;; waiting pattern
        
        ;; Process buffers again
        (ecc-background-detection-process-buffers)
        
        ;; Verify states updated correctly
        (with-current-buffer ecc-system-test-buffer-x
          (should (eq (ecc-buffer-state-get-prompt) :y/n)))
        
        (with-current-buffer ecc-system-test-buffer-y
          (should (eq (ecc-buffer-state-get-prompt) :waiting))))
    (ecc-system-test-teardown-bg)))

(provide 'test-buffer-local-background-system)

;;; test-buffer-local-background-system.el ends here