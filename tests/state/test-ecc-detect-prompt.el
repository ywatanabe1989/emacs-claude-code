;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 21:20:10>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-detect-prompt.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ert)
(require 'ecc-state)

(ert-deftest test-ecc-detect-prompt-loadable ()
  (should (featurep 'ecc-state)))

(ert-deftest test-ecc-detect-prompt-waiting-defined ()
  (should (fboundp '--ecc-state-waiting-p)))

(ert-deftest test-ecc-detect-prompt-initial-waiting-defined
    ()
  (should (fboundp '--ecc-state-initial-waiting-p)))

(ert-deftest test-ecc-detect-prompt-y/n-defined ()
  (should (fboundp '--ecc-state-y/n-p)))

(ert-deftest test-ecc-detect-prompt-y/y/n-defined ()
  (should (fboundp '--ecc-state-y/y/n-p)))

(ert-deftest test-ecc-detect-prompt-defined ()
  (should (fboundp '--ecc-state-detect-prompt)))

(ert-deftest test-ecc-detect-prompt-with-mock-buffer ()
  (let ((orig-buffer ecc-buffer-current-active-buffer)
        (mock-buffer (generate-new-buffer "*MOCK-CLAUDE*"))
        (test-prompt "❯ 1. Yes"))
    (unwind-protect
        (progn
          (with-current-buffer mock-buffer
            (insert "Some content before the prompt\n" test-prompt
                    "\nSome content after"))
          (setq ecc-buffer-current-active-buffer mock-buffer)
          (should (--ecc-state-detect-prompt test-prompt)))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (setq ecc-buffer-current-active-buffer orig-buffer))))

(ert-deftest test-ecc-detect-prompt-handles-nil-buffer ()
  (let ((orig-buffer ecc-buffer-current-active-buffer))
    (unwind-protect
        (progn
          (setq ecc-buffer-current-active-buffer nil)
          (should-not (--ecc-state-detect-prompt "any prompt")))
      (setq ecc-buffer-current-active-buffer orig-buffer))))

(ert-deftest
    test-ecc-detect-prompt-returns-nil-when-not-found ()
  (let ((orig-buffer ecc-buffer-current-active-buffer)
        (mock-buffer (generate-new-buffer "*MOCK-CLAUDE*")))
    (unwind-protect
        (progn
          (with-current-buffer mock-buffer
            (insert "Content without the target prompt"))
          (setq ecc-buffer-current-active-buffer mock-buffer)
          (should-not
           (--ecc-state-detect-prompt "Target prompt not present")))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (setq ecc-buffer-current-active-buffer orig-buffer))))

(ert-deftest test-ecc-y/n-prompt-detection ()
  (let ((orig-buffer ecc-buffer-current-active-buffer)
        (mock-buffer (generate-new-buffer "*MOCK-CLAUDE*")))
    (unwind-protect
        (progn
          (with-current-buffer mock-buffer
            (insert "Some content\n❯ 1. Yes\nMore content"))
          (setq ecc-buffer-current-active-buffer mock-buffer)
          (should (--ecc-state-y/n-p)))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (setq ecc-buffer-current-active-buffer orig-buffer))))


(provide 'test-ecc-detect-prompt)

(when (not load-file-name)
  (message "test-ecc-detect-prompt.el loaded: %s"
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))