;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 09:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-vterm/test-ecc-term-claude-mode.el

;;; Commentary:
;;; Tests for the Claude term mode functionality

(require 'ert)

;; Mock vterm if not available
(unless (featurep 'vterm)
  (defvar vterm-timer-delay 0.01)
  (defvar vterm-max-scrollback 1000)
  (defvar display-line-numbers-mode nil)
  (defvar vterm-disable-bold-font nil)
  (defvar vterm-disable-underline nil)
  (defun vterm-send-string (_string) nil)
  (defun vterm-send-return () nil)
  (defun vterm-clear () nil)
  (defvar font-lock-mode nil)
  (defun font-lock-mode (_) nil)
  (defun display-line-numbers-mode (_) nil)
  (defvar vterm-mode-map (make-sparse-keymap))
  (define-derived-mode vterm-mode nil "VTerm")
  (provide 'vterm))

(require 'ecc-variables)

;; Load the files under test
(when (file-exists-p "/home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-term-claude-mode.el")
  (load-file "/home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-term-claude-mode.el"))

;; Test the mode definition
(ert-deftest test-ecc-term-claude-mode-existence ()
  "Test that the Claude term mode exists and is properly defined."
  ;; Skip if function doesn't exist
  (skip-unless (fboundp 'ecc-term-claude-mode))
  
  ;; Verify it derives from vterm-mode
  (should (get 'ecc-term-claude-mode 'derived-mode-parent))
  (should (eq (get 'ecc-term-claude-mode 'derived-mode-parent) 'vterm-mode))
  
  ;; Verify mode map
  (should (boundp 'ecc-term-claude-mode-map))
  (should (keymapp ecc-term-claude-mode-map)))

;; Test mode customization variables
(ert-deftest test-ecc-term-claude-mode-customization ()
  "Test that customization variables for Claude term mode are defined."
  ;; Skip if group doesn't exist
  (skip-unless (get 'ecc-term-claude 'custom-group))
  
  ;; Check customization variables
  (should (boundp 'ecc-term-claude-line-numbers))
  (should (boundp 'ecc-term-claude-scroll-conservatively))
  (should (boundp 'ecc-term-claude-truncate-lines))
  (should (boundp 'ecc-term-claude-state-update-interval)))

;; Test mode commands
(ert-deftest test-ecc-term-claude-mode-commands ()
  "Test that Claude term mode commands exist and are callable."
  ;; Skip if required functions don't exist
  (skip-unless (and (fboundp 'ecc-term-claude-yes)
                    (fboundp 'ecc-term-claude-no)
                    (fboundp 'ecc-term-claude-clear)))
  
  ;; Test that commands are callable (only testing they don't error)
  (should-not (condition-case err 
                  (progn (call-interactively 'ecc-term-claude-yes) nil) 
                (error err)))
  
  (should-not (condition-case err 
                  (progn (call-interactively 'ecc-term-claude-no) nil) 
                (error err)))
  
  (should-not (condition-case err 
                  (progn (call-interactively 'ecc-term-claude-clear) nil) 
                (error err))))

;; Test the auto-mode functionality
(ert-deftest test-ecc-term-claude-auto-mode ()
  "Test that the auto-mode toggle functions properly."
  ;; Skip if function doesn't exist
  (skip-unless (fboundp 'ecc-term-claude-auto-mode-toggle))
  
  ;; Test that auto-mode toggle works
  (let ((ecc-term-claude-auto-mode nil))
    ;; Toggle on
    (ecc-term-claude-auto-mode-toggle)
    (should ecc-term-claude-auto-mode)
    
    ;; Toggle off
    (ecc-term-claude-auto-mode-toggle)
    (should-not ecc-term-claude-auto-mode)))

;; Test buffer registration
(ert-deftest test-ecc-register-buffer ()
  "Test that buffer registration works correctly."
  ;; Skip if function doesn't exist
  (skip-unless (fboundp 'ecc-register-buffer))
  
  ;; Test with current buffer
  (with-temp-buffer
    (let ((ecc-buffer-registered-buffers-alist nil)
          (ecc-buffer-current-buffer nil))
      
      ;; Register the buffer
      (ecc-register-buffer (current-buffer))
      
      ;; Verify it was registered
      (should (member (current-buffer) (mapcar 'car ecc-buffer-registered-buffers-alist)))
      (should (eq ecc-buffer-current-buffer (current-buffer))))))

;; Test follow-bottom functionality
(ert-deftest test-ecc-term-claude-follow-bottom ()
  "Test that follow-bottom functionality works correctly."
  ;; Skip if function doesn't exist
  (skip-unless (fboundp 'ecc-term-claude-toggle-follow-bottom))
  
  ;; Test toggle function
  (let ((ecc-vterm-always-follow-bottom nil))
    ;; Toggle on
    (ecc-term-claude-toggle-follow-bottom)
    (should ecc-vterm-always-follow-bottom)
    
    ;; Toggle off
    (ecc-term-claude-toggle-follow-bottom)
    (should-not ecc-vterm-always-follow-bottom)))

;; Test buffer setup functionality
(ert-deftest test-ecc-term-claude-setup-buffer ()
  "Test buffer setup functionality."
  ;; Skip if function doesn't exist
  (skip-unless (fboundp 'ecc-term-claude-setup-existing-buffer))
  
  ;; Mock functions to avoid errors
  (cl-letf (((symbol-function 'ecc-term-claude-setup-mode-line) (lambda () t))
            ((symbol-function 'ecc-term-claude-check-state) (lambda () t)))
    
    ;; Create a mock vterm buffer
    (with-temp-buffer
      ;; Should fail if not in vterm-mode
      (should-error (ecc-term-claude-setup-existing-buffer))
      
      ;; Set major mode to vterm-mode
      (setq major-mode 'vterm-mode)
      
      ;; Now should work
      (should-not (condition-case err 
                      (progn (ecc-term-claude-setup-existing-buffer) nil) 
                    (error err))))))

;; Test state detection functionality
(ert-deftest test-ecc-detect-simple-state ()
  "Test state detection functionality."
  ;; Skip if function doesn't exist
  (skip-unless (fboundp 'ecc-detect-simple-state))
  
  ;; Test with different buffer contents
  (with-temp-buffer
    ;; No prompt
    (erase-buffer)
    (insert "This is a regular message without prompts")
    (should-not (ecc-detect-simple-state))
    
    ;; Y/N prompt
    (erase-buffer)
    (insert "Would you like to continue? [y/n]")
    (should (eq (ecc-detect-simple-state) :y/n))
    
    ;; Y/Y/N prompt
    (erase-buffer)
    (insert "Would you like to see more? [Y/y/n]")
    (should (eq (ecc-detect-simple-state) :y/y/n))
    
    ;; Continue prompt
    (erase-buffer)
    (insert "Type 'continue>' to continue")
    (should (eq (ecc-detect-simple-state) :waiting))))

;; Test main creation function
(ert-deftest test-ecc-term-claude-creation ()
  "Test the main function that creates or converts Claude buffers."
  ;; Skip if function doesn't exist
  (skip-unless (fboundp 'ecc-term-claude))
  
  ;; Mock functions to avoid actual buffer creation
  (cl-letf (((symbol-function 'ecc-term-claude-setup-existing-buffer) 
             (lambda () (current-buffer)))
            ((symbol-function 'ecc-term-claude-mode)
             (lambda () (setq major-mode 'ecc-term-claude-mode)))
            ((symbol-function 'switch-to-buffer)
             (lambda (buf) buf)))
    
    ;; Test with vterm buffer
    (with-temp-buffer
      (setq major-mode 'vterm-mode)
      ;; Should call setup-existing-buffer
      (should (eq (ecc-term-claude) (current-buffer))))
    
    ;; Test without vterm buffer
    (with-temp-buffer
      ;; Should try to create new buffer
      (let ((get-buffer-return (current-buffer)))
        (cl-letf (((symbol-function 'get-buffer-create) 
                   (lambda (_) get-buffer-return))
                  ((symbol-function 'get-buffer)
                   (lambda (_) nil)))
          (should (eq (ecc-term-claude) get-buffer-return)))))))

(provide 'test-ecc-term-claude-mode)

;;; test-ecc-term-claude-mode.el ends here