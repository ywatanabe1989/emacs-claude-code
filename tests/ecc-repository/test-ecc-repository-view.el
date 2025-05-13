;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 21:00:10>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-repository/test-ecc-repository-view.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'ecc-repository-view)

(ert-deftest test-ecc-repository-view-loadable ()
  "Test that ecc-repository-view.el can be loaded."
  (should (featurep 'ecc-repository-view)))

(ert-deftest test-ecc-repo-view-path ()
  "Test that ecc-repo-view-path returns a directory."
  (let ((path (ecc-repo-view-path)))
    (should path)
    (should (file-directory-p path))))

(ert-deftest test-ecc-repo-view-script-exists ()
  "Test that the view_repo.sh script exists."
  (let ((script-path ecc-repo-view-script))
    (should script-path)
    (should (file-exists-p script-path))))

;; More thorough tests would require mocking the shell commands
;; and testing the integration with Claude, which would be complex.
;; For now, we'll just test the basic functionality and structure.

(provide 'test-ecc-repository-view)