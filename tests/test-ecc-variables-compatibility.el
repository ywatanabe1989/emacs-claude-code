;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 17:00:10>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-variables-compatibility.el

;;; Commentary:
;;; Tests for the ecc-variables compatibility layer to ensure it provides
;;; seamless backward compatibility with code that depends on the old module.

;;; Code:

(require 'ert)
(require 'ecc-variables)

;; Basic module loading and feature providing
(ert-deftest test-ecc-variables-compat-loadable ()
  "Test that the compatibility layer loads properly."
  (should (featurep 'ecc-variables))
  ;; The compatibility layer should also load ecc-variables-refactored
  (should (featurep 'ecc-variables-refactored)))

;; Test that original variables are accessible
(ert-deftest test-ecc-variables-compat-buffer-vars ()
  "Test that buffer variables from original module are accessible."
  (should (boundp 'ecc-buffer-registered-buffers-alist))
  (should (boundp 'ecc-buffer-current-buffer))
  (should (boundp 'ecc-buffer-auto-response-enabled))
  (should (boundp 'ecc-buffer-counter))
  (should (boundp 'ecc-buffer-prefix))
  (should (boundp 'ecc-buffer-suffix))
  (should (boundp 'ecc-claude-buffers)))

;; Test that auto-response variables are accessible
(ert-deftest test-ecc-variables-compat-auto-response-vars ()
  "Test that auto-response variables from original module are accessible."
  (should (boundp 'ecc-auto-response-initial-waiting))
  (should (boundp 'ecc-auto-response-y/n))
  (should (boundp 'ecc-auto-response-y/y/n))
  (should (boundp 'ecc-auto-response-waiting))
  (should (boundp 'ecc-auto-response-timer)))

;; Test that state detection variables are accessible
(ert-deftest test-ecc-variables-compat-state-vars ()
  "Test that state detection variables from original module are accessible."
  (should (boundp 'ecc-state-prompt-initial-waiting))
  (should (boundp 'ecc-state-prompt-waiting))
  (should (boundp 'ecc-state-prompt-y/n))
  (should (boundp 'ecc-state-prompt-y/y/n))
  (should (boundp 'ecc-state-prompt-initial-waiting-alternatives)))

;; Test that VTerm variables are accessible
(ert-deftest test-ecc-variables-compat-vterm-vars ()
  "Test that VTerm variables from original module are accessible."
  (should (boundp 'ecc-vterm-always-follow-bottom))
  (should (boundp 'ecc-vterm-follow-bottom-margin)))

;; Test that notification variables are accessible
(ert-deftest test-ecc-variables-compat-notify-vars ()
  "Test that notification variables from original module are accessible."
  (should (boundp 'ecc-auto-notify-on-claude-prompt))
  (should (boundp 'ecc-auto-notify-completions)))

;; Test that interaction tracking variables are accessible
(ert-deftest test-ecc-variables-compat-interaction-vars ()
  "Test that interaction variables from original module are accessible."
  (should (boundp 'ecc-interaction-counter))
  (should (boundp 'ecc-interaction-timestamps)))

;; Test that debugging variables and functions are accessible
(ert-deftest test-ecc-variables-compat-debug-vars ()
  "Test that debugging variables and functions from original module are accessible."
  (should (boundp 'ecc-debug-enabled))
  (should (fboundp 'ecc-debug-message))
  (should (fboundp 'ecc-toggle-debug)))

;; Test that values remain consistent through the compatibility layer
(ert-deftest test-ecc-variables-compat-values ()
  "Test that variable values remain consistent through the compatibility layer."
  ;; Get values through variables module
  (let ((var-prompt-y-n ecc-state-prompt-y/n)
        (var-prompt-y-y-n ecc-state-prompt-y/y/n)
        (var-prompt-waiting ecc-state-prompt-waiting)
        (var-prompt-initial ecc-state-prompt-initial-waiting)
        (var-debug-enabled ecc-debug-enabled))
    
    ;; Temporarily unload both features
    (unload-feature 'ecc-variables t)
    (unload-feature 'ecc-variables-refactored t)
    
    ;; Now load the refactored module directly
    (require 'ecc-variables-refactored)
    
    ;; Check values are the same as when loaded through compatibility layer
    (should (equal var-prompt-y-n ecc-state-prompt-y/n))
    (should (equal var-prompt-y-y-n ecc-state-prompt-y/y/n))
    (should (equal var-prompt-waiting ecc-state-prompt-waiting))
    (should (equal var-prompt-initial ecc-state-prompt-initial-waiting))
    (should (equal var-debug-enabled ecc-debug-enabled))
    
    ;; Reload through compat layer
    (unload-feature 'ecc-variables-refactored t)
    (require 'ecc-variables)))

(provide 'test-ecc-variables-compatibility)

;;; test-ecc-variables-compatibility.el ends here