;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-03-21 12:00:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-auto-response-ui.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)

(require 'ert)
(require 'ecc-auto-response-ui)

(ert-deftest test-ecc-auto-response-ui-loadable ()
  "Test that ecc-auto-response-ui loads correctly."
  (should (featurep 'ecc-auto-response-ui)))

(ert-deftest test-ecc-auto-response-ui-pulse-timer-functions ()
  "Pulse timer start/stop functions should exist."
  (should (functionp '--ecc-auto-response--start-pulse-timer))
  (should (functionp '--ecc-auto-response--stop-pulse-timer)))

(ert-deftest test-ecc-auto-response-ui-flash-yellow-function ()
  "Flash yellow function should exist."
  (should (functionp '--ecc-auto-response--flash-yellow)))

(ert-deftest test-ecc-auto-response-ui-show-encouragement-function ()
  "Show encouragement function should exist."
  (should (functionp '--ecc-auto-response--show-encouragement)))

(ert-deftest test-ecc-auto-response-ui-disable-visual-modes-function
    ()
  "Disable visual modes function should exist."
  (should (functionp '--ecc-auto-response--disable-visual-modes)))

(ert-deftest test-ecc-auto-response-ui-restore-visual-modes-function
    ()
  "Restore visual modes function should exist."
  (should (functionp '--ecc-auto-response--restore-visual-modes)))

(ert-deftest test-ecc-auto-response-ui-update-mode-line-function ()
  "Update mode-line function should exist."
  (should (functionp '--ecc-auto-response--update-mode-line)))

(ert-deftest
    test-ecc-auto-response-ui-refresh-all-mode-lines-interactive ()
  "Refresh all mode-lines should be an interactive command."
  (should (commandp '--ecc-auto-response-refresh-all-mode-lines)))

(ert-deftest test-ecc-auto-response-ui-face-defined ()
  "Auto indicator face should be defined."
  (should (facep 'ecc-auto-indicator-face)))

(ert-deftest test-ecc-auto-response-ui-flash-yellow-dead-buffer ()
  "Flash yellow should handle dead buffers gracefully."
  (let ((dead-buf (generate-new-buffer " *test-dead*")))
    (kill-buffer dead-buf)
    (--ecc-auto-response--flash-yellow dead-buf)))

(ert-deftest test-ecc-auto-response-ui-show-encouragement-dead-buffer
    ()
  "Show encouragement should handle dead buffers gracefully."
  (let ((dead-buf (generate-new-buffer " *test-dead*")))
    (kill-buffer dead-buf)
    (--ecc-auto-response--show-encouragement dead-buf "test")))

(ert-deftest test-ecc-auto-response-ui-mode-line-disabled ()
  "Mode-line update with auto disabled should not add indicator."
  (with-temp-buffer
    (let ((--ecc-auto-response--enabled nil))
      (--ecc-auto-response--update-mode-line)
      (should-not
       (local-variable-p '--ecc-auto-response--original-mode-line)))))

(provide 'test-ecc-auto-response-ui)

(when (not load-file-name)
  (message "test-ecc-auto-response-ui.el loaded."))
