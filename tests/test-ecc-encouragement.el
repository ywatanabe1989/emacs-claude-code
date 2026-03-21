;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-03-21 12:00:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-encouragement.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)

(require 'ert)
(require 'ecc-encouragement)

(ert-deftest test-ecc-encouragement-loadable ()
  "Test that ecc-encouragement loads correctly."
  (should (featurep 'ecc-encouragement)))

(ert-deftest test-ecc-encouragement-enabled-by-default ()
  "Encouragement should be enabled by default."
  (should ecc-encouragement-enabled))

(ert-deftest test-ecc-encouragement-phrases-non-empty ()
  "All phrase lists should be non-empty."
  (should ecc-encouragement-phrases-general)
  (should ecc-encouragement-phrases-critical)
  (should ecc-encouragement-phrases-confidence)
  (should ecc-encouragement-phrases-motivational)
  (should ecc-encouragement-phrases-simplicity)
  (should ecc-encouragement-phrases-craft)
  (should ecc-encouragement-phrases-debugging)
  (should ecc-encouragement-phrases-workflow))

(ert-deftest test-ecc-encouragement-get-random-phrase-returns-string
    ()
  "get-random-phrase should return a string."
  (let ((ecc-encouragement--speak-count 0)
        (ecc-encouragement--last-phrase-time 0)
        (ecc-encouragement-speak-max-count 100))
    (should (stringp (ecc-encouragement-get-random-phrase)))))

(ert-deftest test-ecc-encouragement-idle-loop-suppression ()
  "After max count, get-random-phrase should return nil."
  (let ((ecc-encouragement--speak-count 100)
        (ecc-encouragement-speak-max-count 100)
        (ecc-encouragement--last-phrase-time (float-time))
        (ecc-encouragement-min-work-duration 30.0))
    (should-not (ecc-encouragement-get-random-phrase))))

(ert-deftest test-ecc-encouragement-counter-resets-after-real-work ()
  "Counter should reset when enough time passes between calls."
  (let ((ecc-encouragement--speak-count 50)
        (ecc-encouragement-speak-max-count 100)
        (ecc-encouragement--last-phrase-time 0)
        (ecc-encouragement-min-work-duration 1.0))
    (ecc-encouragement-get-random-phrase)
    (should (= ecc-encouragement--speak-count 1))))

(ert-deftest test-ecc-encouragement-get-phrase-for-state ()
  "get-phrase-for-state should return a phrase when enabled."
  (let ((ecc-encouragement-enabled t)
        (ecc-encouragement--speak-count 0)
        (ecc-encouragement--last-phrase-time 0))
    (should
     (stringp (ecc-encouragement-get-phrase-for-state :waiting)))))

(ert-deftest test-ecc-encouragement-toggle-interactive ()
  "Toggle function should be interactive."
  (should (commandp 'ecc-encouragement-toggle)))

(ert-deftest test-ecc-encouragement-reset-function ()
  "Reset function should zero out counters."
  (setq ecc-encouragement--speak-count 42)
  (ecc-encouragement-reset-speak-count)
  (should (= ecc-encouragement--speak-count 0))
  (should (= ecc-encouragement--last-phrase-time 0)))

(ert-deftest test-ecc-encouragement-setup-function ()
  "Setup function should exist and be callable."
  (should (functionp 'ecc-encouragement-setup)))

(ert-deftest test-ecc-encouragement-update-responses-function ()
  "Update responses function should exist."
  (should (functionp 'ecc-encouragement-update-responses)))

(ert-deftest test-ecc-encouragement-speak-max-count-reasonable ()
  "Speak max count should be a reasonable positive number."
  (should (> ecc-encouragement-speak-max-count 0)))

(provide 'test-ecc-encouragement)

(when (not load-file-name)
  (message "test-ecc-encouragement.el loaded."))
