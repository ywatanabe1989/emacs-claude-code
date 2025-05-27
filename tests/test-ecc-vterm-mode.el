;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-28 07:21:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-vterm-mode.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'ecc-vterm-mode)

(ert-deftest test-ecc-vterm-mode-loadable ()
  "Test that ecc-vterm-mode loads correctly."
  (should (featurep 'ecc-vterm-mode)))

(ert-deftest test-ecc-vterm-mode-map-exists ()
  "Test that ecc-vterm-mode map exists."
  (should (keymapp --ecc-vterm-mode-map)))

(ert-deftest test-ecc-vterm-mode-function-exists ()
  "Test that ecc-vterm-mode function exists."
  (should (functionp '--ecc-vterm-mode)))

(ert-deftest test-ecc-vterm-mode-hook-exists ()
  "Test that ecc-vterm-mode-hook exists."
  (should (boundp '--ecc-vterm-mode-hook)))

(provide 'test-ecc-vterm-mode)

(when (not load-file-name)
  (message "test-ecc-vterm-mode.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))