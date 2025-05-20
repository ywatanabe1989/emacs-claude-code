;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 16:55:20>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-variables-refactored.el

;;; Commentary:
;;; Tests for the refactored variables module.

;;; Code:

(require 'ert)
(require 'ecc-variables-refactored)

;; Basic module loading
(ert-deftest test-ecc-variables-refactored-loadable ()
  "Test that the module loads properly."
  (should (featurep 'ecc-variables-refactored)))

;; Customization groups
(ert-deftest test-ecc-variables-customization-groups ()
  "Test that all required customization groups exist."
  (should (get 'emacs-claude-code 'custom-group))
  (should (get 'ecc-buffers 'custom-group))
  (should (get 'ecc-auto-response 'custom-group))
  (should (get 'ecc-state-detection 'custom-group))
  (should (get 'ecc-vterm 'custom-group))
  (should (get 'ecc-notification 'custom-group)))

;; Buffer management variables
(ert-deftest test-ecc-buffer-variables-exist ()
  "Test that essential buffer management variables exist."
  (should (boundp 'ecc-buffer-prefix))
  (should (stringp ecc-buffer-prefix))
  (should (boundp 'ecc-buffer-suffix))
  (should (stringp ecc-buffer-suffix))
  (should (boundp 'ecc-buffer-counter))
  (should (numberp ecc-buffer-counter))
  (should (boundp 'ecc-buffer-registered-buffers-alist))
  (should (boundp 'ecc-buffer-current-buffer))
  (should (boundp 'ecc-buffer-auto-response-enabled))
  (should (boundp 'ecc-claude-buffers)))

;; Auto-response variables
(ert-deftest test-ecc-auto-response-variables-exist ()
  "Test that essential auto-response variables exist."
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
  (should (listp ecc-auto-response-last-time-alist))
  (should (boundp 'ecc-auto-response-active-state))
  (should (boundp 'ecc-auto-response-hooks)))

;; State detection variables
(ert-deftest test-ecc-state-detection-variables-exist ()
  "Test that essential state detection variables exist."
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

;; VTerm variables
(ert-deftest test-ecc-vterm-variables-exist ()
  "Test that essential VTerm variables exist."
  (should (boundp 'ecc-vterm-always-follow-bottom))
  (should (booleanp ecc-vterm-always-follow-bottom))
  (should (boundp 'ecc-vterm-follow-bottom-margin))
  (should (numberp ecc-vterm-follow-bottom-margin)))

;; Notification variables
(ert-deftest test-ecc-notification-variables-exist ()
  "Test that essential notification variables exist."
  (should (boundp 'ecc-auto-notify-on-claude-prompt))
  (should (booleanp ecc-auto-notify-on-claude-prompt))
  (should (boundp 'ecc-auto-notify-completions))
  (should (booleanp ecc-auto-notify-completions)))

;; Interaction tracking variables
(ert-deftest test-ecc-interaction-variables-exist ()
  "Test that essential interaction tracking variables exist."
  (should (boundp 'ecc-interaction-counter))
  (should (numberp ecc-interaction-counter))
  (should (boundp 'ecc-interaction-timestamps))
  (should (listp ecc-interaction-timestamps)))

;; Debug variables and functions
(ert-deftest test-ecc-debug-variables-functions-exist ()
  "Test that debug variables and functions exist."
  (should (boundp 'ecc-debug-enabled))
  (should (booleanp ecc-debug-enabled))
  (should (fboundp 'ecc-debug-message))
  (should (fboundp 'ecc-toggle-debug)))

;; Check that values are reasonable
(ert-deftest test-ecc-variables-values-reasonable ()
  "Test that variable values fall within expected ranges."
  (should (>= ecc-auto-response-throttle-time 0.1))
  (should (<= ecc-auto-response-throttle-time 30.0))
  (should (>= ecc-auto-response-timer-interval 0.1))
  (should (<= ecc-auto-response-timer-interval 5.0))
  (should (>= ecc-state-detection-buffer-size 100))
  (should (<= ecc-state-detection-buffer-size 10000))
  (should (>= ecc-state-detection-line-count 10))
  (should (<= ecc-state-detection-line-count 1000))
  (should (>= ecc-vterm-follow-bottom-margin 0))
  (should (<= ecc-vterm-follow-bottom-margin 100)))

(provide 'test-ecc-variables-refactored)

;;; test-ecc-variables-refactored.el ends here