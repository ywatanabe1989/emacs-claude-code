;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 20:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-variables.el

;;; Commentary:
;;; Tests for variables module functionality.
;;; These tests verify the proper definition and organization of variables
;;; used throughout the emacs-claude-code package.

;;; Code:
(require 'ert)
(require 'ecc-variables)
(require 'ecc-api)

;;; Feature availability tests

(ert-deftest test-ecc-variables-features-available ()
  "Test that variables features are available."
  (should (featurep 'ecc-variables-consolidated)))

;;; Customization group tests

(ert-deftest test-ecc-variables-customization-groups ()
  "Test that customization groups are properly defined."
  (should (get 'emacs-claude-code 'custom-group))
  (should (get 'ecc-buffers 'custom-group))
  (should (get 'ecc-auto-response 'custom-group))
  (should (get 'ecc-state-detection 'custom-group))
  (should (get 'ecc-vterm 'custom-group))
  (should (get 'ecc-notification 'custom-group)))

;;; Buffer management variables tests

(ert-deftest test-ecc-variables-buffer-management ()
  "Test that buffer management variables are properly defined."
  (should (boundp 'ecc-buffer-prefix))
  (should (stringp ecc-buffer-prefix))
  (should (boundp 'ecc-buffer-suffix))
  (should (stringp ecc-buffer-suffix))
  (should (boundp 'ecc-buffer-counter))
  (should (numberp ecc-buffer-counter))
  (should (boundp 'ecc-buffer-registered-buffers-alist))
  (should (boundp 'ecc-buffer-current-buffer))
  (should (boundp 'ecc-claude-buffers)))

;;; Auto-response variables tests

(ert-deftest test-ecc-variables-auto-response ()
  "Test that auto-response variables are properly defined."
  (should (boundp 'ecc-auto-response-throttle-time))
  (should (numberp ecc-auto-response-throttle-time))
  (should (boundp 'ecc-auto-response-timer-interval))
  (should (numberp ecc-auto-response-timer-interval))
  (should (boundp 'ecc-auto-response-check-on-output))
  (should (booleanp ecc-auto-response-check-on-output))
  (should (boundp 'ecc-auto-response-initial-waiting))
  (should (stringp ecc-auto-response-initial-waiting))
  (should (boundp 'ecc-auto-response-y/n))
  (should (stringp ecc-auto-response-y/n))
  (should (boundp 'ecc-auto-response-y/y/n))
  (should (stringp ecc-auto-response-y/y/n))
  (should (boundp 'ecc-auto-response-waiting))
  (should (stringp ecc-auto-response-waiting))
  (should (boundp 'ecc-auto-response-timer))
  (should (boundp 'ecc-auto-response-last-time-alist))
  (should (boundp 'ecc-auto-response-active-state))
  (should (boundp 'ecc-auto-response-hooks)))

;;; State detection variables tests

(ert-deftest test-ecc-variables-state-detection ()
  "Test that state detection variables are properly defined."
  (should (boundp 'ecc-state-detection-buffer-size))
  (should (numberp ecc-state-detection-buffer-size))
  (should (boundp 'ecc-state-detection-line-count))
  (should (numberp ecc-state-detection-line-count))
  (should (boundp 'ecc-state-prompt-initial-waiting))
  (should (stringp ecc-state-prompt-initial-waiting))
  (should (boundp 'ecc-state-prompt-waiting))
  (should (stringp ecc-state-prompt-waiting))
  (should (boundp 'ecc-state-prompt-y/n))
  (should (stringp ecc-state-prompt-y/n))
  (should (boundp 'ecc-state-prompt-y/y/n))
  (should (stringp ecc-state-prompt-y/y/n))
  (should (boundp 'ecc-state-prompt-initial-waiting-alternatives))
  (should (listp ecc-state-prompt-initial-waiting-alternatives)))

;;; VTerm variables tests

(ert-deftest test-ecc-variables-vterm ()
  "Test that vterm variables are properly defined."
  (should (boundp 'ecc-vterm-always-follow-bottom))
  (should (booleanp ecc-vterm-always-follow-bottom))
  (should (boundp 'ecc-vterm-follow-bottom-margin))
  (should (numberp ecc-vterm-follow-bottom-margin)))

;;; Notification variables tests

(ert-deftest test-ecc-variables-notification ()
  "Test that notification variables are properly defined."
  (should (boundp 'ecc-auto-notify-on-claude-prompt))
  (should (booleanp ecc-auto-notify-on-claude-prompt))
  (should (boundp 'ecc-auto-notify-completions))
  (should (booleanp ecc-auto-notify-completions)))

;;; Interaction tracking variables tests

(ert-deftest test-ecc-variables-interaction-tracking ()
  "Test that interaction tracking variables are properly defined."
  (should (boundp 'ecc-interaction-counter))
  (should (numberp ecc-interaction-counter))
  (should (boundp 'ecc-interaction-timestamps))
  (should (listp ecc-interaction-timestamps)))

;;; Version information tests

(ert-deftest test-ecc-variables-version ()
  "Test that version information is properly defined."
  (should (boundp 'ecc-version))
  (should (stringp ecc-version)))

;;; Backward compatibility tests

(ert-deftest test-ecc-variables-backward-compatibility ()
  "Test that backward compatibility variables are properly defined."
  ;; Prompt patterns
  (should (boundp 'ecc-prompt-pattern-initial-waiting))
  (should (string= ecc-prompt-pattern-initial-waiting ecc-state-prompt-initial-waiting))
  (should (boundp 'ecc-prompt-pattern-waiting))
  (should (string= ecc-prompt-pattern-waiting ecc-state-prompt-waiting))
  (should (boundp 'ecc-prompt-pattern-y/n))
  (should (string= ecc-prompt-pattern-y/n ecc-state-prompt-y/n))
  (should (boundp 'ecc-prompt-pattern-y/y/n))
  (should (string= ecc-prompt-pattern-y/y/n ecc-state-prompt-y/y/n))
  
  ;; Auto response
  (should (boundp 'ecc-auto-interval-sec))
  (should (= ecc-auto-interval-sec ecc-auto-response-timer-interval))
  (should (boundp 'ecc-auto-timer))
  (should (eq ecc-auto-timer ecc-auto-response-timer))
  
  ;; Buffers
  (should (boundp 'ecc-buffer-name))
  (should (string= ecc-buffer-name ecc-buffer-prefix))
  (should (boundp 'ecc-buffer))
  (should (eq ecc-buffer ecc-buffer-current-buffer)))

(provide 'test-ecc-variables)

;;; test-ecc-variables.el ends here