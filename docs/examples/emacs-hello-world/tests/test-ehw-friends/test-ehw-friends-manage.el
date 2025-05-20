;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 00:10:30>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-hello-world/tests/test-ehw-friends/test-ehw-friends-manage.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:
;; Tests for the ehw-friends-manage module

;;; Code:

(require 'ert)
(require 'ehw-friends-data)
(require 'ehw-friends-manage)

;;; Module loadability tests
(ert-deftest test-ehw-friends-manage-loadable ()
  "Test if ehw-friends-manage module is loadable."
  (should (featurep 'ehw-friends-manage)))

;;; Function existence tests
(ert-deftest test-ehw-friends-manage-add-function-exists ()
  "Test if ehw-friends-add function exists."
  (should (fboundp 'ehw-friends-add)))

(ert-deftest test-ehw-friends-manage-remove-function-exists ()
  "Test if ehw-friends-remove function exists."
  (should (fboundp 'ehw-friends-remove)))

(ert-deftest test-ehw-friends-manage-list-function-exists ()
  "Test if ehw-friends-list function exists."
  (should (fboundp 'ehw-friends-list)))

;;; Function behavior tests
(ert-deftest test-ehw-friends-manage-add-functionality ()
  "Test adding a friend works correctly."
  (let ((ehw-friends-list nil))
    (ehw-friends-add "TestFriend" "Hi, %s!")
    (should (equal '(("TestFriend" . "Hi, %s!")) ehw-friends-list))))

(ert-deftest test-ehw-friends-manage-remove-functionality ()
  "Test removing a friend works correctly."
  (let ((ehw-friends-list '(("Friend1" . nil) ("Friend2" . "Hello, %s!"))))
    (ehw-friends-remove "Friend1")
    (should (equal '(("Friend2" . "Hello, %s!")) ehw-friends-list))))

;;; List function tests
(ert-deftest test-ehw-friends-manage-list-returns-string ()
  "Test that ehw-friends-list returns a string."
  (let ((ehw-friends-list '(("Friend1" . nil) ("Friend2" . "Hello, %s!"))))
    (should (stringp (ehw-friends-list)))))

(ert-deftest test-ehw-friends-manage-list-includes-friends ()
  "Test that ehw-friends-list includes all friends."
  (let ((ehw-friends-list '(("Friend1" . nil) ("Friend2" . "Hello, %s!"))))
    (let ((list-output (ehw-friends-list)))
      (should (string-match-p "Friend1" list-output))
      (should (string-match-p "Friend2" list-output)))))

(ert-deftest test-ehw-friends-manage-list-empty ()
  "Test listing when no friends exist."
  (let ((ehw-friends-list nil))
    (should (string-match-p "No friends" (ehw-friends-list)))))

(provide 'test-ehw-friends-manage)
;;; test-ehw-friends-manage.el ends here