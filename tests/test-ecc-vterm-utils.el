;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-28 07:22:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-vterm-utils.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'ecc-vterm-utils)

(ert-deftest test-ecc-vterm-utils-loadable ()
  "Test that ecc-vterm-utils loads correctly."
  (should (featurep 'ecc-vterm-utils)))

(ert-deftest test-ecc-vterm-visual-effect-exists ()
  "Test that visual effect function exists."
  (should (functionp '--ecc-vterm-visual-effect)))

(ert-deftest test-ecc-vterm-safe-execute-exists ()
  "Test that safe execute function exists."
  (should (functionp '--ecc-vterm-safe-execute)))

(ert-deftest test-ecc-vterm-check-exists ()
  "Test that vterm check function exists."
  (should (functionp '--ecc-vterm-check)))

(provide 'test-ecc-vterm-utils)

(when (not load-file-name)
  (message "test-ecc-vterm-utils.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))