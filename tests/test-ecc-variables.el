;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 21:20:12>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-variables.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ert)
(require 'ecc-variables)

(ert-deftest test-ecc-variables-loadable ()
  (should (featurep 'ecc-variables)))

(ert-deftest test-ecc-variables-customization-group ()
  (should (get 'emacs-claude 'custom-group)))

(ert-deftest test-ecc-buffer-name-defined ()
  (should (boundp 'ecc-buffer-name))
  (should (stringp ecc-buffer-name)))

(ert-deftest test-ecc-buffer-defined ()
  (should (boundp 'ecc-buffer)))

(ert-deftest test-ecc-auto-timer-defined ()
  (should (boundp 'ecc-auto-timer)))

(ert-deftest test-ecc-prompt-patterns-defined ()
  (should (boundp 'ecc-prompt-pattern-y/n))
  (should (stringp ecc-prompt-pattern-y/n))
  (should (boundp 'ecc-prompt-pattern-y/y/n))
  (should (stringp ecc-prompt-pattern-y/y/n))
  (should (boundp 'ecc-prompt-pattern-waiting))
  (should (stringp ecc-prompt-pattern-waiting))
  (should (boundp 'ecc-prompt-pattern-initial-waiting))
  (should (stringp ecc-prompt-pattern-initial-waiting)))

(ert-deftest test-ecc-auto-interval-sec-defined ()
  (should (boundp 'ecc-auto-interval-sec))
  (should (numberp (if (stringp ecc-auto-interval-sec)
                       (string-to-number
                        ecc-auto-interval-sec)
                     ecc-auto-interval-sec))))


(provide 'test-ecc-variables)

(when (not load-file-name)
  (message "test-ecc-variables.el loaded: %s"
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))