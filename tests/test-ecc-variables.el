;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 20:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-variables.el

;;; Commentary:
;;; Tests for variables module functionality.
;;; These tests verify the proper definition and organization of variables
;;; used throughout the emacs-claude-code package.
;;; REFACTORED: Each test now follows one-assertion-per-test principle.

;;; Code:
(require 'ert)
(require 'ecc-variables)
(require 'ecc-api)

;;; Feature availability tests

(ert-deftest test-ecc-variables-features-are-available ()
  "Test that ecc-variables-consolidated feature is available."
  (should (featurep 'ecc-variables-consolidated)))

;;; Customization group tests

(ert-deftest test-emacs-claude-code-custom-group-exists ()
  "Test that emacs-claude-code customization group is defined."
  (should (get 'emacs-claude-code 'custom-group)))

(ert-deftest test-ecc-buffers-custom-group-exists ()
  "Test that ecc-buffers customization group is defined."
  (should (get 'ecc-buffers 'custom-group)))

(ert-deftest test-ecc-auto-response-custom-group-exists ()
  "Test that ecc-auto-response customization group is defined."
  (should (get 'ecc-auto-response 'custom-group)))

(ert-deftest test-ecc-state-detection-custom-group-exists ()
  "Test that ecc-state-detection customization group is defined."
  (should (get 'ecc-state-detection 'custom-group)))

(ert-deftest test-ecc-vterm-custom-group-exists ()
  "Test that ecc-vterm customization group is defined."
  (should (get 'ecc-vterm 'custom-group)))

(ert-deftest test-ecc-notification-custom-group-exists ()
  "Test that ecc-notification customization group is defined."
  (should (get 'ecc-notification 'custom-group)))

;;; Buffer management variables tests

(ert-deftest test-ecc-buffer-prefix-is-bound ()
  "Test that ecc-buffer-prefix variable is bound."
  (should (boundp 'ecc-buffer-prefix)))

(ert-deftest test-ecc-buffer-prefix-is-string ()
  "Test that ecc-buffer-prefix is a string type."
  (should (stringp ecc-buffer-prefix)))

(ert-deftest test-ecc-buffer-suffix-is-bound ()
  "Test that ecc-buffer-suffix variable is bound."
  (should (boundp 'ecc-buffer-suffix)))

(ert-deftest test-ecc-buffer-suffix-is-string ()
  "Test that ecc-buffer-suffix is a string type."
  (should (stringp ecc-buffer-suffix)))

(ert-deftest test-ecc-buffer-counter-is-bound ()
  "Test that ecc-buffer-counter variable is bound."
  (should (boundp 'ecc-buffer-counter)))

(ert-deftest test-ecc-buffer-counter-is-number ()
  "Test that ecc-buffer-counter is a number type."
  (should (numberp ecc-buffer-counter)))

(ert-deftest test-ecc-buffer-registered-buffers-alist-is-bound ()
  "Test that ecc-buffer-registered-buffers-alist variable is bound."
  (should (boundp 'ecc-buffer-registered-buffers-alist)))

(ert-deftest test-ecc-buffer-current-buffer-is-bound ()
  "Test that ecc-buffer-current-buffer variable is bound."
  (should (boundp 'ecc-buffer-current-buffer)))

(ert-deftest test-ecc-claude-buffers-is-bound ()
  "Test that ecc-claude-buffers variable is bound."
  (should (boundp 'ecc-claude-buffers)))

;;; Auto-response variables tests

(ert-deftest test-ecc-auto-response-throttle-time-is-bound ()
  "Test that ecc-auto-response-throttle-time variable is bound."
  (should (boundp 'ecc-auto-response-throttle-time)))

(ert-deftest test-ecc-auto-response-throttle-time-is-number ()
  "Test that ecc-auto-response-throttle-time is a number type."
  (should (numberp ecc-auto-response-throttle-time)))

(ert-deftest test-ecc-auto-response-timer-interval-is-bound ()
  "Test that ecc-auto-response-timer-interval variable is bound."
  (should (boundp 'ecc-auto-response-timer-interval)))

(ert-deftest test-ecc-auto-response-timer-interval-is-number ()
  "Test that ecc-auto-response-timer-interval is a number type."
  (should (numberp ecc-auto-response-timer-interval)))

(ert-deftest test-ecc-auto-response-check-on-output-is-bound ()
  "Test that ecc-auto-response-check-on-output variable is bound."
  (should (boundp 'ecc-auto-response-check-on-output)))

(ert-deftest test-ecc-auto-response-check-on-output-is-boolean ()
  "Test that ecc-auto-response-check-on-output is a boolean type."
  (should (booleanp ecc-auto-response-check-on-output)))

(ert-deftest test-ecc-auto-response-initial-waiting-is-bound ()
  "Test that ecc-auto-response-initial-waiting variable is bound."
  (should (boundp 'ecc-auto-response-initial-waiting)))

(ert-deftest test-ecc-auto-response-initial-waiting-is-string ()
  "Test that ecc-auto-response-initial-waiting is a string type."
  (should (stringp ecc-auto-response-initial-waiting)))

(ert-deftest test-ecc-auto-response-y-n-is-bound ()
  "Test that ecc-auto-response-y/n variable is bound."
  (should (boundp 'ecc-auto-response-y/n)))

(ert-deftest test-ecc-auto-response-y-n-is-string ()
  "Test that ecc-auto-response-y/n is a string type."
  (should (stringp ecc-auto-response-y/n)))

(ert-deftest test-ecc-auto-response-y-y-n-is-bound ()
  "Test that ecc-auto-response-y/y/n variable is bound."
  (should (boundp 'ecc-auto-response-y/y/n)))

(ert-deftest test-ecc-auto-response-y-y-n-is-string ()
  "Test that ecc-auto-response-y/y/n is a string type."
  (should (stringp ecc-auto-response-y/y/n)))

(ert-deftest test-ecc-auto-response-waiting-is-bound ()
  "Test that ecc-auto-response-waiting variable is bound."
  (should (boundp 'ecc-auto-response-waiting)))

(ert-deftest test-ecc-auto-response-waiting-is-string ()
  "Test that ecc-auto-response-waiting is a string type."
  (should (stringp ecc-auto-response-waiting)))

(ert-deftest test-ecc-auto-response-timer-is-bound ()
  "Test that ecc-auto-response-timer variable is bound."
  (should (boundp 'ecc-auto-response-timer)))

(ert-deftest test-ecc-auto-response-last-time-alist-is-bound ()
  "Test that ecc-auto-response-last-time-alist variable is bound."
  (should (boundp 'ecc-auto-response-last-time-alist)))

(ert-deftest test-ecc-auto-response-active-state-is-bound ()
  "Test that ecc-auto-response-active-state variable is bound."
  (should (boundp 'ecc-auto-response-active-state)))

(ert-deftest test-ecc-auto-response-hooks-is-bound ()
  "Test that ecc-auto-response-hooks variable is bound."
  (should (boundp 'ecc-auto-response-hooks)))

;;; State detection variables tests

(ert-deftest test-ecc-state-detection-buffer-size-is-bound ()
  "Test that ecc-state-detection-buffer-size variable is bound."
  (should (boundp 'ecc-state-detection-buffer-size)))

(ert-deftest test-ecc-state-detection-buffer-size-is-number ()
  "Test that ecc-state-detection-buffer-size is a number type."
  (should (numberp ecc-state-detection-buffer-size)))

(ert-deftest test-ecc-state-detection-line-count-is-bound ()
  "Test that ecc-state-detection-line-count variable is bound."
  (should (boundp 'ecc-state-detection-line-count)))

(ert-deftest test-ecc-state-detection-line-count-is-number ()
  "Test that ecc-state-detection-line-count is a number type."
  (should (numberp ecc-state-detection-line-count)))

(ert-deftest test-ecc-state-prompt-initial-waiting-is-bound ()
  "Test that ecc-state-prompt-initial-waiting variable is bound."
  (should (boundp 'ecc-state-prompt-initial-waiting)))

(ert-deftest test-ecc-state-prompt-initial-waiting-is-string ()
  "Test that ecc-state-prompt-initial-waiting is a string type."
  (should (stringp ecc-state-prompt-initial-waiting)))

(ert-deftest test-ecc-state-prompt-waiting-is-bound ()
  "Test that ecc-state-prompt-waiting variable is bound."
  (should (boundp 'ecc-state-prompt-waiting)))

(ert-deftest test-ecc-state-prompt-waiting-is-string ()
  "Test that ecc-state-prompt-waiting is a string type."
  (should (stringp ecc-state-prompt-waiting)))

(ert-deftest test-ecc-state-prompt-y-n-is-bound ()
  "Test that ecc-state-prompt-y/n variable is bound."
  (should (boundp 'ecc-state-prompt-y/n)))

(ert-deftest test-ecc-state-prompt-y-n-is-string ()
  "Test that ecc-state-prompt-y/n is a string type."
  (should (stringp ecc-state-prompt-y/n)))

(ert-deftest test-ecc-state-prompt-y-y-n-is-bound ()
  "Test that ecc-state-prompt-y/y/n variable is bound."
  (should (boundp 'ecc-state-prompt-y/y/n)))

(ert-deftest test-ecc-state-prompt-y-y-n-is-string ()
  "Test that ecc-state-prompt-y/y/n is a string type."
  (should (stringp ecc-state-prompt-y/y/n)))

(ert-deftest test-ecc-state-prompt-initial-waiting-alternatives-is-bound ()
  "Test that ecc-state-prompt-initial-waiting-alternatives variable is bound."
  (should (boundp 'ecc-state-prompt-initial-waiting-alternatives)))

(ert-deftest test-ecc-state-prompt-initial-waiting-alternatives-is-list ()
  "Test that ecc-state-prompt-initial-waiting-alternatives is a list type."
  (should (listp ecc-state-prompt-initial-waiting-alternatives)))

;;; VTerm variables tests

(ert-deftest test-ecc-vterm-always-follow-bottom-is-bound ()
  "Test that ecc-vterm-always-follow-bottom variable is bound."
  (should (boundp 'ecc-vterm-always-follow-bottom)))

(ert-deftest test-ecc-vterm-always-follow-bottom-is-boolean ()
  "Test that ecc-vterm-always-follow-bottom is a boolean type."
  (should (booleanp ecc-vterm-always-follow-bottom)))

(ert-deftest test-ecc-vterm-follow-bottom-margin-is-bound ()
  "Test that ecc-vterm-follow-bottom-margin variable is bound."
  (should (boundp 'ecc-vterm-follow-bottom-margin)))

(ert-deftest test-ecc-vterm-follow-bottom-margin-is-number ()
  "Test that ecc-vterm-follow-bottom-margin is a number type."
  (should (numberp ecc-vterm-follow-bottom-margin)))

;;; Notification variables tests

(ert-deftest test-ecc-auto-notify-on-claude-prompt-is-bound ()
  "Test that ecc-auto-notify-on-claude-prompt variable is bound."
  (should (boundp 'ecc-auto-notify-on-claude-prompt)))

(ert-deftest test-ecc-auto-notify-on-claude-prompt-is-boolean ()
  "Test that ecc-auto-notify-on-claude-prompt is a boolean type."
  (should (booleanp ecc-auto-notify-on-claude-prompt)))

(ert-deftest test-ecc-auto-notify-completions-is-bound ()
  "Test that ecc-auto-notify-completions variable is bound."
  (should (boundp 'ecc-auto-notify-completions)))

(ert-deftest test-ecc-auto-notify-completions-is-boolean ()
  "Test that ecc-auto-notify-completions is a boolean type."
  (should (booleanp ecc-auto-notify-completions)))

;;; Interaction tracking variables tests

(ert-deftest test-ecc-interaction-counter-is-bound ()
  "Test that ecc-interaction-counter variable is bound."
  (should (boundp 'ecc-interaction-counter)))

(ert-deftest test-ecc-interaction-counter-is-number ()
  "Test that ecc-interaction-counter is a number type."
  (should (numberp ecc-interaction-counter)))

(ert-deftest test-ecc-interaction-timestamps-is-bound ()
  "Test that ecc-interaction-timestamps variable is bound."
  (should (boundp 'ecc-interaction-timestamps)))

(ert-deftest test-ecc-interaction-timestamps-is-list ()
  "Test that ecc-interaction-timestamps is a list type."
  (should (listp ecc-interaction-timestamps)))

;;; Version information tests

(ert-deftest test-ecc-version-is-bound ()
  "Test that ecc-version variable is bound."
  (should (boundp 'ecc-version)))

(ert-deftest test-ecc-version-is-string ()
  "Test that ecc-version is a string type."
  (should (stringp ecc-version)))

;;; Backward compatibility tests

(ert-deftest test-ecc-prompt-pattern-initial-waiting-is-bound ()
  "Test that ecc-prompt-pattern-initial-waiting backward compatibility variable is bound."
  (should (boundp 'ecc-prompt-pattern-initial-waiting)))

(ert-deftest test-ecc-prompt-pattern-initial-waiting-equals-state-prompt ()
  "Test that ecc-prompt-pattern-initial-waiting equals ecc-state-prompt-initial-waiting."
  (should (string= ecc-prompt-pattern-initial-waiting ecc-state-prompt-initial-waiting)))

(ert-deftest test-ecc-prompt-pattern-waiting-is-bound ()
  "Test that ecc-prompt-pattern-waiting backward compatibility variable is bound."
  (should (boundp 'ecc-prompt-pattern-waiting)))

(ert-deftest test-ecc-prompt-pattern-waiting-equals-state-prompt ()
  "Test that ecc-prompt-pattern-waiting equals ecc-state-prompt-waiting."
  (should (string= ecc-prompt-pattern-waiting ecc-state-prompt-waiting)))

(ert-deftest test-ecc-prompt-pattern-y-n-is-bound ()
  "Test that ecc-prompt-pattern-y/n backward compatibility variable is bound."
  (should (boundp 'ecc-prompt-pattern-y/n)))

(ert-deftest test-ecc-prompt-pattern-y-n-equals-state-prompt ()
  "Test that ecc-prompt-pattern-y/n equals ecc-state-prompt-y/n."
  (should (string= ecc-prompt-pattern-y/n ecc-state-prompt-y/n)))

(ert-deftest test-ecc-prompt-pattern-y-y-n-is-bound ()
  "Test that ecc-prompt-pattern-y/y/n backward compatibility variable is bound."
  (should (boundp 'ecc-prompt-pattern-y/y/n)))

(ert-deftest test-ecc-prompt-pattern-y-y-n-equals-state-prompt ()
  "Test that ecc-prompt-pattern-y/y/n equals ecc-state-prompt-y/y/n."
  (should (string= ecc-prompt-pattern-y/y/n ecc-state-prompt-y/y/n)))

(ert-deftest test-ecc-auto-interval-sec-is-bound ()
  "Test that ecc-auto-interval-sec backward compatibility variable is bound."
  (should (boundp 'ecc-auto-interval-sec)))

(ert-deftest test-ecc-auto-interval-sec-equals-timer-interval ()
  "Test that ecc-auto-interval-sec equals ecc-auto-response-timer-interval."
  (should (= ecc-auto-interval-sec ecc-auto-response-timer-interval)))

(ert-deftest test-ecc-auto-timer-is-bound ()
  "Test that ecc-auto-timer backward compatibility variable is bound."
  (should (boundp 'ecc-auto-timer)))

(ert-deftest test-ecc-auto-timer-equals-auto-response-timer ()
  "Test that ecc-auto-timer equals ecc-auto-response-timer."
  (should (eq ecc-auto-timer ecc-auto-response-timer)))

(ert-deftest test-ecc-buffer-name-is-bound ()
  "Test that ecc-buffer-name backward compatibility variable is bound."
  (should (boundp 'ecc-buffer-name)))

(ert-deftest test-ecc-buffer-name-equals-buffer-prefix ()
  "Test that ecc-buffer-name equals ecc-buffer-prefix."
  (should (string= ecc-buffer-name ecc-buffer-prefix)))

(ert-deftest test-ecc-buffer-is-bound ()
  "Test that ecc-buffer backward compatibility variable is bound."
  (should (boundp 'ecc-buffer)))

(ert-deftest test-ecc-buffer-equals-current-buffer ()
  "Test that ecc-buffer equals ecc-buffer-current-buffer."
  (should (eq ecc-buffer ecc-buffer-current-buffer)))

(provide 'test-ecc-variables)

;;; test-ecc-variables.el ends here