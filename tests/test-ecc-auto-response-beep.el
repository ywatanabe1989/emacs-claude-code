;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-03-21 12:00:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-auto-response-beep.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)

(require 'ert)
(require 'ecc-auto-response-beep)

(ert-deftest test-ecc-auto-response-beep-loadable ()
  "Test that ecc-auto-response-beep loads correctly."
  (should (featurep 'ecc-auto-response-beep)))

(ert-deftest test-ecc-auto-response-beep-disabled-by-default ()
  "Running beep should be disabled by default."
  (should-not ecc-auto-response-running-beep-enabled))

(ert-deftest test-ecc-auto-response-beep-interval-positive ()
  "Beep interval should be a positive number."
  (should (> ecc-auto-response-running-beep-interval 0)))

(ert-deftest test-ecc-auto-response-beep-cooldown-positive ()
  "Cooldown should be a positive number."
  (should (> ecc-auto-response-beep-cooldown 0)))

(ert-deftest test-ecc-auto-response-beep-audio-filenames-defined ()
  "Audio filename alist should have running and sent entries."
  (should (assoc "running" --ecc-auto-response--audio-filenames))
  (should (assoc "sent" --ecc-auto-response--audio-filenames)))

(ert-deftest test-ecc-auto-response-beep-force-beep-function ()
  "Force beep function should exist."
  (should (functionp '--ecc-auto-response--force-beep)))

(ert-deftest test-ecc-auto-response-beep-tone-beep-function ()
  "Tone beep function should exist."
  (should (functionp '--ecc-auto-response--tone-beep)))

(ert-deftest test-ecc-auto-response-beep-can-notify-p ()
  "can-notify-p should return t when enough time has passed."
  (let ((--ecc-auto-response--last-notify-time 0.0)
        (ecc-auto-response-beep-cooldown 2.0))
    (should (--ecc-auto-response--can-notify-p))))

(ert-deftest test-ecc-auto-response-beep-can-notify-p-within-cooldown
    ()
  "can-notify-p should return nil within cooldown period."
  (let ((--ecc-auto-response--last-notify-time (float-time))
        (ecc-auto-response-beep-cooldown 999.0))
    (should-not (--ecc-auto-response--can-notify-p))))

(ert-deftest test-ecc-auto-response-beep-toggle-interactive ()
  "Toggle function should be interactive."
  (should (commandp 'ecc-auto-response-running-beep-toggle)))

(ert-deftest test-ecc-auto-response-beep-tts-toggle-interactive ()
  "TTS toggle function should be interactive."
  (should (commandp 'ecc-auto-response-tts-toggle)))

(ert-deftest test-ecc-auto-response-beep-cleanup-timers-interactive ()
  "Cleanup timers function should be interactive."
  (should (commandp 'ecc-auto-response-cleanup-timers)))

(ert-deftest test-ecc-auto-response-beep-audio-dir-function ()
  "Audio dir function should exist and return a string."
  (should (functionp '--ecc-auto-response--audio-dir))
  (should (stringp (--ecc-auto-response--audio-dir))))

(ert-deftest test-ecc-auto-response-beep-audio-path-function ()
  "Audio path function should return path for known events."
  (should (stringp (--ecc-auto-response--audio-path "running")))
  (should (stringp (--ecc-auto-response--audio-path "sent"))))

(ert-deftest test-ecc-auto-response-beep-timer-vars-list ()
  "All timer vars list should contain expected timer variables."
  (should (memq '--ecc-auto-response--running-beep-timer
                --ecc-auto-response--all-timer-vars))
  (should (memq '--ecc-auto-response--timer
                --ecc-auto-response--all-timer-vars)))

(provide 'test-ecc-auto-response-beep)

(when (not load-file-name)
  (message "test-ecc-auto-response-beep.el loaded."))
