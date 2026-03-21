;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-03-21 12:00:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-state-speaking-flash-feedback.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)

(require 'ert)
(require 'ecc-state-speaking-flash-feedback)

(ert-deftest test-ecc-speaking-flash-loadable ()
  "Test that ecc-state-speaking-flash-feedback loads correctly."
  (should (featurep 'ecc-state-speaking-flash-feedback)))

(ert-deftest test-ecc-speaking-flash-patterns-defined ()
  "Speaking flash patterns should be a non-empty list."
  (should (listp ecc-speaking-flash-patterns))
  (should ecc-speaking-flash-patterns))

(ert-deftest test-ecc-speaking-flash-duration-positive ()
  "Flash duration should be positive."
  (should (> ecc-speaking-flash-duration 0)))

(ert-deftest test-ecc-speaking-flash-scan-size-positive ()
  "Scan size should be positive."
  (should (> ecc-speaking-flash-scan-size 0)))

(ert-deftest test-ecc-speaking-scan-buffer-no-match ()
  "Scan should return nil when no speaking pattern present."
  (with-temp-buffer
    (insert "some unrelated text without any patterns")
    (should-not (ecc-speaking--scan-buffer))))

(ert-deftest test-ecc-speaking-scan-buffer-with-match ()
  "Scan should find position when speaking pattern is present."
  (with-temp-buffer
    (insert "output: scitex - audio_speak done")
    (let ((ecc-speaking--last-match-pos nil))
      (should (numberp (ecc-speaking--scan-buffer))))))

(ert-deftest test-ecc-speaking-scan-buffer-dedup ()
  "Scan should not re-trigger at same position."
  (with-temp-buffer
    (insert "output: scitex - audio_speak done")
    (let* ((pos (ecc-speaking--scan-buffer)))
      (setq-local ecc-speaking--last-match-pos pos)
      (should-not (ecc-speaking--scan-buffer)))))

(ert-deftest test-ecc-speaking-trigger-flash-dead-buffer ()
  "Trigger flash should handle dead buffers gracefully."
  (let ((dead-buf (generate-new-buffer " *test-dead*")))
    (kill-buffer dead-buf)
    (ecc-speaking--trigger-flash dead-buf 0)))

(ert-deftest test-ecc-speaking-trigger-flash-sets-active ()
  "Trigger flash should set flash-active to t."
  (with-temp-buffer
    (ecc-speaking--trigger-flash (current-buffer) 10)
    (should ecc-speaking--flash-active)
    ;; Clean up timer
    (when (and ecc-speaking--flash-timer
               (timerp ecc-speaking--flash-timer))
      (cancel-timer ecc-speaking--flash-timer))))

(ert-deftest test-ecc-speaking-mode-line-face-returns-plist ()
  "Mode-line face should return a plist with :background."
  (let ((face-on (ecc-speaking--mode-line-face t))
        (face-off (ecc-speaking--mode-line-face nil)))
    (should (plist-get face-on :background))
    (should (plist-get face-off :background))
    (should-not (equal face-on face-off))))

(provide 'test-ecc-state-speaking-flash-feedback)

(when (not load-file-name)
  (message "test-ecc-state-speaking-flash-feedback.el loaded."))
