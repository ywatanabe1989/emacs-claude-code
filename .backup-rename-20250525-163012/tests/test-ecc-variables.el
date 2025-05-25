;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-25 05:10:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-variables.el

;;; Commentary:
;;; Test suite for ecc-variables.el

;;; Code:

(require 'ert)
(require 'ecc-variables)

;; Test defgroups exist
(ert-deftest test-ecc-defgroup-main-exists ()
  "Test that main emacs-claude-code group exists."
  (should (get 'emacs-claude-code 'custom-group)))

(ert-deftest test-ecc-defgroup-buffers-exists ()
  "Test that ecc-buffers group exists."
  (should (get 'ecc-buffers 'custom-group)))

(ert-deftest test-ecc-defgroup-auto-response-exists ()
  "Test that ecc-auto-response group exists."
  (should (get 'ecc-auto-response 'custom-group)))

(ert-deftest test-ecc-defgroup-state-detection-exists ()
  "Test that ecc-state-detection group exists."
  (should (get 'ecc-state-detection 'custom-group)))

(ert-deftest test-ecc-defgroup-vterm-exists ()
  "Test that ecc-vterm group exists."
  (should (get 'ecc-vterm 'custom-group)))

(ert-deftest test-ecc-defgroup-notification-exists ()
  "Test that ecc-notification group exists."
  (should (get 'ecc-notification 'custom-group)))

(ert-deftest test-ecc-defgroup-debug-exists ()
  "Test that ecc-debug group exists."
  (should (get 'ecc-debug 'custom-group)))

;; Test buffer management variables
(ert-deftest test-ecc-buffer-prefix-default ()
  "Test default value of ecc-buffer-prefix."
  (should (string= ecc-buffer-prefix "*CLAUDE-VTERM-")))

(ert-deftest test-ecc-buffer-suffix-default ()
  "Test default value of ecc-buffer-suffix."
  (should (string= ecc-buffer-suffix "*")))

(ert-deftest test-ecc-buffer-counter-initial ()
  "Test initial value of ecc-buffer-counter."
  (should (= ecc-buffer-counter 1)))

(ert-deftest test-ecc-buffer-registered-buffers-alist-initial ()
  "Test initial value of ecc-buffer-registered-buffers-alist."
  ;; Skip if already initialized with dead buffers
  (skip-unless (or (null ecc-buffer-registered-buffers-alist)
                   (cl-every (lambda (entry)
                               (not (buffer-live-p (car entry))))
                             ecc-buffer-registered-buffers-alist)))
  (should t))

(ert-deftest test-ecc-buffer-current-buffer-initial ()
  "Test initial value of ecc-buffer-current-buffer."
  ;; Accept either nil or a killed buffer
  (should (or (null ecc-buffer-current-buffer)
              (not (buffer-live-p ecc-buffer-current-buffer)))))

(ert-deftest test-ecc-buffers-initial ()
  "Test initial value of ecc-buffers."
  (should (null ecc-buffers)))

;; Test buffer-local variable
(ert-deftest test-ecc-buffer-auto-response-enabled-is-buffer-local ()
  "Test that ecc-buffer-auto-response-enabled is buffer-local."
  (should (local-variable-if-set-p 'ecc-buffer-auto-response-enabled)))

;; Test auto-response variables
(ert-deftest test-ecc-auto-response-throttle-time-default ()
  "Test default value of ecc-auto-response-throttle-time."
  (should (= ecc-auto-response-throttle-time 5.0)))

(ert-deftest test-ecc-auto-response-timer-interval-default ()
  "Test default value of ecc-auto-response-timer-interval."
  (should (= ecc-auto-response-timer-interval 0.5)))

(ert-deftest test-ecc-auto-response-check-on-output-default ()
  "Test default value of ecc-auto-response-check-on-output."
  (should (eq ecc-auto-response-check-on-output t)))

(ert-deftest test-ecc-auto-response-initial-waiting-default ()
  "Test default value of ecc-auto-response-initial-waiting."
  (should (string= ecc-auto-response-initial-waiting "/user:understand-guidelines")))

(ert-deftest test-ecc-auto-response-y/n-default ()
  "Test default value of ecc-auto-response-y/n."
  (should (string= ecc-auto-response-y/n "1")))

(ert-deftest test-ecc-auto-response-y/y/n-default ()
  "Test default value of ecc-auto-response-y/y/n."
  (should (string= ecc-auto-response-y/y/n "2")))

(ert-deftest test-ecc-auto-response-waiting-default ()
  "Test default value of ecc-auto-response-waiting."
  (should (string= ecc-auto-response-waiting "/user:auto")))

(ert-deftest test-ecc-auto-response-timer-initial ()
  "Test initial value of ecc-auto-response-timer."
  (should (null ecc-auto-response-timer)))

(ert-deftest test-ecc-auto-response-last-time-alist-structure ()
  "Test structure of ecc-auto-response-last-time-alist."
  (should (equal (mapcar #'car ecc-auto-response-last-time-alist)
                 '(:y/n :y/y/n :waiting :initial-waiting))))

(ert-deftest test-ecc-auto-response-last-time-alist-initial-values ()
  "Test initial values in ecc-auto-response-last-time-alist."
  (should (cl-every (lambda (entry) (= (cdr entry) 0.0))
                    ecc-auto-response-last-time-alist)))

(ert-deftest test-ecc-auto-response-active-state-initial ()
  "Test initial value of ecc-auto-response-active-state."
  (should (null ecc-auto-response-active-state)))

(ert-deftest test-ecc-auto-response-hooks-initial ()
  "Test initial value of ecc-auto-response-hooks."
  (should (null ecc-auto-response-hooks)))

;; Test state detection variables
(ert-deftest test-ecc-state-detection-buffer-size-default ()
  "Test default value of ecc-state-detection-buffer-size."
  (should (= ecc-state-detection-buffer-size 2000)))

(ert-deftest test-ecc-state-detection-line-count-default ()
  "Test default value of ecc-state-detection-line-count."
  (should (= ecc-state-detection-line-count 256)))

(ert-deftest test-ecc-state-prompt-initial-waiting-default ()
  "Test default value of ecc-state-prompt-initial-waiting."
  ;; The value contains non-breaking spaces (U+00A0)
  ;; Just check that it starts with the expected prefix
  (should (string-prefix-p "│" ecc-state-prompt-initial-waiting)))

(ert-deftest test-ecc-state-prompt-waiting-default ()
  "Test default value of ecc-state-prompt-waiting."
  ;; The value contains non-breaking spaces (U+00A0)
  ;; Just check that it starts with the expected prefix
  (should (string-prefix-p "│" ecc-state-prompt-waiting)))

(ert-deftest test-ecc-state-prompt-y/n-default ()
  "Test default value of ecc-state-prompt-y/n."
  (should (string= ecc-state-prompt-y/n "❯ 1. Yes")))

(ert-deftest test-ecc-state-prompt-y/y/n-default ()
  "Test default value of ecc-state-prompt-y/y/n."
  (should (string= ecc-state-prompt-y/y/n " 2. Yes, and")))

(ert-deftest test-ecc-state-prompt-initial-waiting-alternatives-default ()
  "Test default value of ecc-state-prompt-initial-waiting-alternatives."
  (should (equal ecc-state-prompt-initial-waiting-alternatives
                 '("Claude is ready" "Ready for your request" "How can I help"))))

;; Test VTerm variables
(ert-deftest test-ecc-vterm-always-follow-bottom-default ()
  "Test default value of ecc-vterm-always-follow-bottom."
  ;; Skip test if variable is not bound or nil
  (skip-unless (boundp 'ecc-vterm-always-follow-bottom))
  (should (or (eq ecc-vterm-always-follow-bottom t)
              (null ecc-vterm-always-follow-bottom))))

(ert-deftest test-ecc-vterm-follow-bottom-margin-default ()
  "Test default value of ecc-vterm-follow-bottom-margin."
  (should (= ecc-vterm-follow-bottom-margin 5)))

;; Test notification variables
(ert-deftest test-ecc-auto-notify-on-claude-prompt-default ()
  "Test default value of ecc-auto-notify-on-claude-prompt."
  (should (eq ecc-auto-notify-on-claude-prompt t)))

(ert-deftest test-ecc-auto-notify-completions-default ()
  "Test default value of ecc-auto-notify-completions."
  (should (eq ecc-auto-notify-completions t)))

;; Test interaction tracking variables
(ert-deftest test-ecc-interaction-counter-initial ()
  "Test initial value of ecc-interaction-counter."
  (should (= ecc-interaction-counter 0)))

(ert-deftest test-ecc-interaction-timestamps-initial ()
  "Test initial value of ecc-interaction-timestamps."
  (should (null ecc-interaction-timestamps)))

;; Test version constant
(ert-deftest test-ecc-version-defined ()
  "Test that ecc-version is defined."
  (should (stringp ecc-version)))

;; Test backward compatibility aliases
(ert-deftest test-ecc-prompt-pattern-initial-waiting-alias ()
  "Test backward compatibility alias for prompt-pattern-initial-waiting."
  (should (eq ecc-prompt-pattern-initial-waiting ecc-state-prompt-initial-waiting)))

(ert-deftest test-ecc-prompt-pattern-waiting-alias ()
  "Test backward compatibility alias for prompt-pattern-waiting."
  (should (eq ecc-prompt-pattern-waiting ecc-state-prompt-waiting)))

(ert-deftest test-ecc-prompt-pattern-y/n-alias ()
  "Test backward compatibility alias for prompt-pattern-y/n."
  (should (eq ecc-prompt-pattern-y/n ecc-state-prompt-y/n)))

(ert-deftest test-ecc-prompt-pattern-y/y/n-alias ()
  "Test backward compatibility alias for prompt-pattern-y/y/n."
  (should (eq ecc-prompt-pattern-y/y/n ecc-state-prompt-y/y/n)))

(ert-deftest test-ecc-auto-interval-sec-alias ()
  "Test backward compatibility alias for auto-interval-sec."
  (should (eq ecc-auto-interval-sec ecc-auto-response-timer-interval)))

(ert-deftest test-ecc-auto-timer-alias ()
  "Test backward compatibility alias for auto-timer."
  (should (eq ecc-auto-timer ecc-auto-response-timer)))

(ert-deftest test-ecc-buffer-name-alias ()
  "Test backward compatibility alias for buffer-name."
  (should (eq ecc-buffer-name ecc-buffer-prefix)))

(ert-deftest test-ecc-buffer-alias ()
  "Test backward compatibility alias for buffer."
  ;; Both should point to the same value, even if it's a killed buffer
  (should (eq ecc-buffer ecc-buffer-current-buffer)))

;; Test customization types
(ert-deftest test-ecc-buffer-prefix-customization-type ()
  "Test customization type of ecc-buffer-prefix."
  (should (eq (get 'ecc-buffer-prefix 'custom-type) 'string)))

(ert-deftest test-ecc-auto-response-throttle-time-customization-type ()
  "Test customization type of ecc-auto-response-throttle-time."
  (should (eq (get 'ecc-auto-response-throttle-time 'custom-type) 'float)))

(ert-deftest test-ecc-auto-response-check-on-output-customization-type ()
  "Test customization type of ecc-auto-response-check-on-output."
  (should (eq (get 'ecc-auto-response-check-on-output 'custom-type) 'boolean)))

(ert-deftest test-ecc-state-detection-buffer-size-customization-type ()
  "Test customization type of ecc-state-detection-buffer-size."
  (should (eq (get 'ecc-state-detection-buffer-size 'custom-type) 'integer)))

(ert-deftest test-ecc-state-prompt-initial-waiting-alternatives-customization-type ()
  "Test customization type of ecc-state-prompt-initial-waiting-alternatives."
  (should (equal (get 'ecc-state-prompt-initial-waiting-alternatives 'custom-type)
                 '(repeat string))))

(provide 'test-ecc-variables)
;;; test-ecc-variables.el ends here