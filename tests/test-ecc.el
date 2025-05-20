;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-07 12:27:11>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ert)
(require 'emacs-claude-code)

(ert-deftest test-ecc-loadable ()
  (should (featurep 'emacs-claude-code)))

(ert-deftest test-ecc-variables-required ()
  (should (featurep 'ecc-variables)))

(ert-deftest test-ecc-detect-prompt-required ()
  (should (featurep 'ecc-state)))

(ert-deftest test-ecc-send-required ()
  (should (featurep 'ecc-send)))

(ert-deftest test-ecc-update-mode-line-required ()
  (should (featurep 'ecc-ui-mode-line)))

(ert-deftest test-ecc-auto-enable-stop-required ()
  (should (featurep 'ecc-auto)))

(ert-deftest test-ecc-run-required ()
  (should (featurep 'ecc-run)))

(ert-deftest test-ecc-repository-copy-contents-required ()
  (should (featurep 'ecc-repository)))


(provide 'test-ecc)

(when
    (not load-file-name)
  (message "test-emacs-claude-code.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))