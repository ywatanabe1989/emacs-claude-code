;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-06-04 07:06:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-host-switch.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'ecc-host-switch)

(ert-deftest test-ecc-host-switch-loadable ()
  "Test if ecc-host-switch is loadable."
  (should (featurep 'ecc-host-switch)))

(ert-deftest test-ecc-ssh-hosts-cache-exists ()
  "Test that SSH hosts cache variable exists."
  (should (boundp 'ecc-ssh-hosts-cache)))

(ert-deftest test-ecc-switch-host-interactive ()
  "Test that ecc-switch-host is an interactive command."
  (should (commandp 'ecc-switch-host)))

(ert-deftest test-ecc-switch-to-local-interactive ()
  "Test that ecc-switch-to-local is an interactive command."
  (should (commandp 'ecc-switch-to-local)))

(ert-deftest test-ecc-refresh-ssh-hosts-interactive ()
  "Test that ecc-refresh-ssh-hosts is an interactive command."
  (should (commandp 'ecc-refresh-ssh-hosts)))

(provide 'test-ecc-host-switch)

(when (not load-file-name)
  (message "test-ecc-host-switch.el loaded."
           (file_name-nondirectory
            (or load-file-name buffer-file-name))))