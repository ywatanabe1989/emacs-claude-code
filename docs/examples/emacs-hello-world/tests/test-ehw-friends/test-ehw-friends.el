;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 00:20:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-hello-world/tests/test-ehw-friends.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)

;; Test umbrella module loadability
(ert-deftest test-ehw-friends-loadable ()
  "Test if ehw-friends umbrella module is loadable."
  (should
   (progn
     (require 'ehw-friends)
     (featurep 'ehw-friends))))

;; Load individual component tests
(require 'test-ehw-friends-data)
(require 'test-ehw-friends-manage)
(require 'test-ehw-friends-greet)

(provide 'test-ehw-friends)