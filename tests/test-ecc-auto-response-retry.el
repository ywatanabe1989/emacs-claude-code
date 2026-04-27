;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-03-21 12:00:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-auto-response-retry.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)

(require 'ert)
(require 'ecc-auto-response-retry)

(ert-deftest test-ecc-auto-response-retry-loadable ()
  "Test that ecc-auto-response-retry loads correctly."
  (should (featurep 'ecc-auto-response-retry)))

(ert-deftest test-ecc-auto-response-retry-max-default ()
  "Send retry max should have a reasonable default."
  (should (> --ecc-auto-response-send-retry-max 0)))

(ert-deftest test-ecc-auto-response-retry-verify-delay-positive ()
  "Verify delay should be positive."
  (should (> --ecc-auto-response-send-verify-delay 0)))

(ert-deftest test-ecc-auto-response-retry-permission-max ()
  "Permission retry max should be defined and positive."
  (should (>= --ecc-auto-response-permission-retry-max 0)))

(ert-deftest test-ecc-auto-response-retry-accumulation-max ()
  "Accumulation max should be a positive integer."
  (should (> --ecc-auto-response-accumulation-max 0)))

(ert-deftest test-ecc-auto-response-retry-verify-send-function ()
  "Verify send function should exist."
  (should (functionp '--ecc-auto-response--verify-send)))

(ert-deftest test-ecc-auto-response-retry-accumulated-p-function ()
  "Accumulation detection function should exist."
  (should (functionp '--ecc-auto-response--response-accumulated-p)))

(ert-deftest test-ecc-auto-response-retry-send-to-buffer-function ()
  "Send to buffer function should exist."
  (should (functionp '--ecc-auto-response--send-to-buffer)))

(ert-deftest test-ecc-auto-response-retry-send-return-function ()
  "Send return function should exist."
  (should (functionp '--ecc-auto-response--send-return)))

(ert-deftest
    test-ecc-auto-response-retry-send-text-and-return-function ()
  "Send text and return function should exist."
  (should (functionp '--ecc-auto-response--send-text-and-return)))

(ert-deftest test-ecc-auto-response-retry-accumulated-p-nil-buffer ()
  "Accumulation check on dead buffer should return nil."
  (let ((dead-buf (generate-new-buffer " *test-dead*")))
    (kill-buffer dead-buf)
    (should-not (--ecc-auto-response--response-accumulated-p
                 dead-buf "test"))))

(ert-deftest test-ecc-auto-response-retry-accumulated-p-nil-response
    ()
  "Accumulation check with nil response should return nil."
  (with-temp-buffer
    (should-not (--ecc-auto-response--response-accumulated-p
                 (current-buffer) nil))))

(ert-deftest test-ecc-auto-response-retry-accumulated-p-not-found ()
  "Accumulation check should return nil when text not in buffer."
  (with-temp-buffer
    (insert "some unrelated content")
    (let ((--ecc-state-detection-buffer-size 1000)
          (--ecc-auto-response-accumulation-max 1))
      (should-not (--ecc-auto-response--response-accumulated-p
                   (current-buffer) "unique-text-not-present")))))

(ert-deftest test-ecc-auto-response-retry-accumulated-p-expanded-slash
    ()
  "Accumulation must match Claude Code's expanded /.claude:commands:NAME form."
  (with-temp-buffer
    (insert "❯ /.claude:commands:speak-and-call\n")
    (let ((--ecc-state-detection-buffer-size 1000)
          (--ecc-auto-response-accumulation-max 1))
      (should (--ecc-auto-response--response-accumulated-p
               (current-buffer) "/speak-and-call")))))

(ert-deftest
    test-ecc-auto-response-retry-accumulated-p-literal-still-works ()
  "Literal slash command still matches when buffer shows the unexpanded form."
  (with-temp-buffer
    (insert "❯ /speak-and-call\n")
    (let ((--ecc-state-detection-buffer-size 1000)
          (--ecc-auto-response-accumulation-max 1))
      (should (--ecc-auto-response--response-accumulated-p
               (current-buffer) "/speak-and-call")))))

(ert-deftest test-ecc-auto-response-retry-verify-send-dead-buffer ()
  "Verify send should handle dead buffers gracefully."
  (let ((dead-buf (generate-new-buffer " *test-dead*")))
    (kill-buffer dead-buf)
    (should-not (--ecc-auto-response--verify-send dead-buf :y/n))))

(provide 'test-ecc-auto-response-retry)

(when (not load-file-name)
  (message "test-ecc-auto-response-retry.el loaded."))
