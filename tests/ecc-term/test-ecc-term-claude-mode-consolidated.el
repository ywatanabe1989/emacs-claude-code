;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 18:45:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-term/test-ecc-term-claude-mode-consolidated.el

;;; Commentary:
;;; Tests for the Claude term mode consolidated functionality.
;;; These tests cover the main features of the consolidated term-claude mode,
;;; including customization, commands, debug functions, auto-mode, buffer registration,
;;; and state detection.

(require 'ert)

;; Mock vterm if not available for testing
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

;; Mock other required libraries
(unless (featurep 'ecc-variables-consolidated)
  (defvar ecc-auto-response-yes "y")
  (defvar ecc-auto-response-yes-plus "y")
  (defvar ecc-auto-response-continue "/continue")
  (defvar ecc-auto-response-initial-waiting "/user:understand-guidelines")
  (defvar ecc-vterm-always-follow-bottom nil)
  (defvar ecc-vterm-follow-bottom-margin 2)
  (defvar ecc-buffer-registered-buffers-alist nil)
  (defvar ecc-buffer-current-buffer nil)
  (provide 'ecc-variables-consolidated))

(unless (featurep 'ecc-state-detection-consolidated)
  (defun ecc-detect-state ()
    (or (get 'ecc-test-state 'current-state) nil))
  (provide 'ecc-state-detection-consolidated))

(unless (featurep 'ecc-auto-response-consolidated)
  (defun ecc-auto-response-register-buffer (buffer)
    buffer)
  (provide 'ecc-auto-response-consolidated))

(unless (featurep 'ecc-debug-utils-consolidated)
  (defun ecc-debug-message (format-string &rest args)
    (apply #'format format-string args))
  (provide 'ecc-debug-utils-consolidated))

(unless (featurep 'ecc-vterm-yank-as-file)
  (defun ecc-vterm-yank-as-file () (interactive) nil)
  (defun ecc-vterm-yank-buffer-as-file () (interactive) nil)
  (defun ecc-vterm-quick-yank-region () (interactive) nil)
  (provide 'ecc-vterm-yank-as-file))

;; Load the module under test
(require 'ecc-term-claude-mode-consolidated)
(require 'ecc-api)

;;; Tests for mode definition and customization

(ert-deftest test-ecc-term-claude-mode-consolidated-definition ()
  "Test that the Claude term mode is properly defined."
  ;; Verify it derives from vterm-mode
  (should (get 'ecc-term-claude-mode 'derived-mode-parent))
  (should (eq (get 'ecc-term-claude-mode 'derived-mode-parent) 'vterm-mode))
  
  ;; Verify mode map
  (should (boundp 'ecc-term-claude-mode-map))
  (should (keymapp ecc-term-claude-mode-map)))

(ert-deftest test-ecc-term-claude-mode-consolidated-customization ()
  "Test that customization variables are properly defined."
  ;; Check customization group
  (should (get 'ecc-term-claude 'custom-group))
  
  ;; Check basic customization variables
  (should (boundp 'ecc-term-claude-line-numbers))
  (should (boundp 'ecc-term-claude-scroll-conservatively))
  (should (boundp 'ecc-term-claude-truncate-lines))
  (should (boundp 'ecc-term-claude-state-update-interval))
  (should (boundp 'ecc-term-claude-auto-mode))
  (should (boundp 'ecc-term-claude-buffer-name))
  (should (boundp 'ecc-term-claude-show-state-in-mode-line))
  
  ;; Check new customization variables in consolidated version
  (should (boundp 'ecc-term-claude-update-frame-title))
  (should (boundp 'ecc-term-claude-debug))
  (should (boundp 'ecc-term-claude-debug-to-buffer))
  (should (boundp 'ecc-term-claude-disable-bold))
  (should (boundp 'ecc-term-claude-disable-underline)))

;;; Tests for debug functionality

(ert-deftest test-ecc-term-claude-mode-consolidated-debug ()
  "Test debug functionality."
  ;; Test debug toggle
  (let ((ecc-term-claude-debug nil))
    ;; Toggle on
    (ecc-term-claude-debug-toggle)
    (should ecc-term-claude-debug)
    
    ;; Toggle off
    (ecc-term-claude-debug-toggle)
    (should-not ecc-term-claude-debug))
  
  ;; Test debug message function (no errors)
  (let ((ecc-term-claude-debug t))
    (should-not (condition-case err 
                    (progn (ecc-term-claude--debug "Test message %s" "arg") nil) 
                  (error err))))
  
  ;; Test debug buffer function (no errors)
  (should-not (condition-case err 
                  (progn (ecc-term-claude-debug-show-buffer) nil) 
                (error err))))

;;; Tests for mode commands

(ert-deftest test-ecc-term-claude-mode-consolidated-commands ()
  "Test that the basic commands are available and callable."
  ;; Test that commands are defined
  (should (fboundp 'ecc-term-claude-yes))
  (should (fboundp 'ecc-term-claude-no))
  (should (fboundp 'ecc-term-claude-clear))
  (should (fboundp 'ecc-term-claude-auto-mode-toggle))
  (should (fboundp 'ecc-term-claude-toggle-follow-bottom))
  
  ;; Test debug commands
  (should (fboundp 'ecc-term-claude-debug-toggle))
  (should (fboundp 'ecc-term-claude-debug-show-buffer))
  
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

;;; Tests for auto-mode functionality

(ert-deftest test-ecc-term-claude-consolidated-auto-mode ()
  "Test that auto-mode toggle works correctly."
  ;; Test initial state
  (should-not ecc-term-claude-auto-mode)
  
  ;; Test toggling on
  (ecc-term-claude-auto-mode-toggle)
  (should ecc-term-claude-auto-mode)
  
  ;; Test toggling off
  (ecc-term-claude-auto-mode-toggle)
  (should-not ecc-term-claude-auto-mode))

;;; Tests for buffer setup

(ert-deftest test-ecc-term-claude-consolidated-setup-buffer ()
  "Test that buffer setup functions work correctly."
  ;; Mock functions to prevent side effects
  (cl-letf (((symbol-function 'ecc-term-claude--setup-display-options) (lambda () t))
            ((symbol-function 'ecc-term-claude--setup-performance-options) (lambda () t))
            ((symbol-function 'ecc-term-claude--register-buffer) (lambda (buffer) buffer))
            ((symbol-function 'ecc-term-claude--setup-mode-line) (lambda () t))
            ((symbol-function 'ecc-term-claude--setup-state-timer) (lambda () t))
            ((symbol-function 'ecc-term-claude--setup-hooks) (lambda () t))
            ((symbol-function 'ecc-term-claude--setup-appearance) (lambda () t))
            ((symbol-function 'add-hook) (lambda (&rest _) t)))
    
    ;; Test that setup function can be called without errors
    (should-not (condition-case err 
                    (progn (ecc-term-claude--setup-buffer) nil) 
                  (error err)))))

(ert-deftest test-ecc-term-claude-consolidated-register-buffer ()
  "Test buffer registration functionality."
  ;; Mock auto-response function
  (cl-letf (((symbol-function 'ecc-auto-response-register-buffer) (lambda (buffer) buffer)))
    
    ;; Test with current buffer
    (with-temp-buffer
      (let ((buf (current-buffer)))
        ;; Register buffer
        (should (eq (ecc-term-claude--register-buffer buf) buf))))))

;;; Tests for state detection and mode line

(ert-deftest test-ecc-term-claude-consolidated-mode-line-state ()
  "Test state detection and mode line integration."
  ;; Test different states
  (cl-letf (((symbol-function 'ecc-detect-state) (lambda () :waiting))
            ((symbol-function 'ecc-term-claude--update-frame-title) (lambda (_) nil)))
    (should (string= (ecc-term-claude--mode-line-state-indicator) " [Waiting]")))
  
  (cl-letf (((symbol-function 'ecc-detect-state) (lambda () :y/n))
            ((symbol-function 'ecc-term-claude--update-frame-title) (lambda (_) nil)))
    (should (string= (ecc-term-claude--mode-line-state-indicator) " [Y/N]")))
  
  (cl-letf (((symbol-function 'ecc-detect-state) (lambda () :y/y/n))
            ((symbol-function 'ecc-term-claude--update-frame-title) (lambda (_) nil)))
    (should (string= (ecc-term-claude--mode-line-state-indicator) " [Y/Y/N]")))
  
  (cl-letf (((symbol-function 'ecc-detect-state) (lambda () :initial-waiting))
            ((symbol-function 'ecc-term-claude--update-frame-title) (lambda (_) nil)))
    (should (string= (ecc-term-claude--mode-line-state-indicator) " [Init]")))
  
  (cl-letf (((symbol-function 'ecc-detect-state) (lambda () nil))
            ((symbol-function 'ecc-term-claude--update-frame-title) (lambda (_) nil)))
    (should (string= (ecc-term-claude--mode-line-state-indicator) ""))))

(ert-deftest test-ecc-term-claude-consolidated-frame-title ()
  "Test frame title update functionality."
  ;; Mock frame-parameter and set-frame-parameter functions
  (cl-letf (((symbol-function 'frame-parameter) 
             (lambda (&rest _) "Original Title"))
            ((symbol-function 'set-frame-parameter)
             (lambda (frame parameter value)
               (should (eq frame nil))
               (should (eq parameter 'name))
               (should (stringp value))
               value))
            ((symbol-function 'string-match-p)
             (lambda (regexp string)
               (string-match regexp string))))
    
    ;; Test different states
    (let ((ecc-term-claude-update-frame-title t))
      (ecc-term-claude--update-frame-title :waiting)
      (ecc-term-claude--update-frame-title :y/n)
      (ecc-term-claude--update-frame-title :y/y/n)
      (ecc-term-claude--update-frame-title :initial-waiting)
      (ecc-term-claude--update-frame-title nil))
    
    ;; Test with feature disabled
    (let ((ecc-term-claude-update-frame-title nil))
      (should-not (ecc-term-claude--update-frame-title :waiting)))))

;;; Tests for auto-response functions

(ert-deftest test-ecc-term-claude-consolidated-auto-respond ()
  "Test auto-response functions."
  ;; Mock functions
  (cl-letf (((symbol-function 'vterm-send-string) (lambda (string) string))
            ((symbol-function 'vterm-send-return) (lambda () t))
            ((symbol-function 'message) (lambda (&rest _) t)))
    
    ;; Test auto-response behavior
    (let ((ecc-term-claude-auto-mode t))
      ;; Test with different states
      (put 'ecc-test-state 'current-state :y/n)
      (should (ecc-term-claude--auto-send-respond))
      
      (put 'ecc-test-state 'current-state :y/y/n)
      (should (ecc-term-claude--auto-send-respond))
      
      (put 'ecc-test-state 'current-state :waiting)
      (should (ecc-term-claude--auto-send-respond))
      
      (put 'ecc-test-state 'current-state :initial-waiting)
      (should (ecc-term-claude--auto-send-respond)))))

;;; Tests for follow-bottom functionality

(ert-deftest test-ecc-term-claude-consolidated-follow-bottom ()
  "Test follow-bottom functionality."
  ;; Test toggle function
  (let ((ecc-vterm-always-follow-bottom nil))
    ;; Toggle on
    (ecc-term-claude-toggle-follow-bottom)
    (should ecc-vterm-always-follow-bottom)
    
    ;; Toggle off
    (ecc-term-claude-toggle-follow-bottom)
    (should-not ecc-vterm-always-follow-bottom)))

;;; Tests for backward compatibility

(ert-deftest test-ecc-term-claude-consolidated-backward-compatibility ()
  "Test backward compatibility with previous functions and variables."
  ;; Test function aliases
  (should (fboundp 'ecc-register-buffer))
  (should (fboundp 'ecc-term-claude-auto-send-accept))
  (should (fboundp 'ecc-term-claude-auto-send-y/n))
  (should (fboundp 'ecc-term-claude-auto-send-y/y/n))
  (should (fboundp 'ecc-term-claude-auto-send-continue))
  (should (fboundp 'ecc-term-claude-auto-send-initial-waiting))
  (should (fboundp 'ecc-term-claude-setup-existing-buffer))
  
  ;; Test variable aliases
  (should (boundp 'ecc-term-claude-update-functions))
  (should (boundp 'ecc-term-claude-state-timer))
  (should (boundp 'ecc-term-claude-menu))
  
  ;; Test new consolidated aliases
  (should (fboundp 'ecc-term-claude-consolidated-debug))
  (should (fboundp 'ecc-term-claude-consolidated-register-buffer))
  (should (fboundp 'ecc-term-claude-consolidated-mode-line-indicator)))

;;; Tests for main user commands

(ert-deftest test-ecc-term-claude-consolidated-user-commands ()
  "Test the main user-facing commands."
  ;; Mock functions to prevent side effects
  (cl-letf (((symbol-function 'ecc-term-claude--setup-existing-buffer) 
             (lambda () (current-buffer)))
            ((symbol-function 'ecc-term-claude-mode)
             (lambda () (setq major-mode 'ecc-term-claude-mode)))
            ((symbol-function 'switch-to-buffer)
             (lambda (buf) buf)))
    
    ;; Test ecc-term-claude-enable in vterm-mode
    (with-temp-buffer
      (setq major-mode 'vterm-mode)
      (should-not (condition-case err 
                      (progn (ecc-term-claude-enable) nil) 
                    (error err))))
    
    ;; Test ecc-term-claude function with vterm buffer
    (with-temp-buffer
      (setq major-mode 'vterm-mode)
      (should (eq (ecc-term-claude) (current-buffer))))
    
    ;; Test ecc-term-claude function without vterm buffer
    (with-temp-buffer
      (let ((get-buffer-return (current-buffer)))
        (cl-letf (((symbol-function 'get-buffer-create) 
                   (lambda (_) get-buffer-return))
                  ((symbol-function 'get-buffer)
                   (lambda (_) nil)))
          (should (eq (ecc-term-claude) get-buffer-return)))))))

;;; Tests for error handling

(ert-deftest test-ecc-term-claude-consolidated-error-handling ()
  "Test error handling in the consolidated module."
  ;; Test enabling Claude features in non-vterm buffer
  (with-temp-buffer
    (should (condition-case nil 
                (progn (ecc-term-claude-enable) nil) 
              (error t))))
  
  ;; Test setting up existing buffer when not in vterm-mode
  (with-temp-buffer
    (should (condition-case nil 
                (progn (ecc-term-claude--setup-existing-buffer) nil) 
              (error t))))
  
  ;; Test registering a non-existent buffer
  (should (condition-case nil 
              (progn (ecc-term-claude--register-buffer (generate-new-buffer-name "non-existent")) nil) 
            (error t))))

(provide 'test-ecc-term-claude-mode-consolidated)

;;; test-ecc-term-claude-mode-consolidated.el ends here