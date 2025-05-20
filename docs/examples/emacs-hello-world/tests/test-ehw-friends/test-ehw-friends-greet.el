;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 00:05:30>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-hello-world/tests/test-ehw-friends/test-ehw-friends-greet.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:
;; Tests for the ehw-friends-greet module

;;; Code:

(require 'ert)
(require 'ehw-friends-data)
(require 'ehw-friends-greet)
(require 'ehw-core)
(require 'cl-lib)

;;; Module loadability tests
(ert-deftest test-ehw-friends-greet-loadable ()
  "Test if ehw-friends-greet module is loadable."
  (should (featurep 'ehw-friends-greet)))

;;; Function existence tests
(ert-deftest test-ehw-friends-greet-function-exists ()
  "Test if ehw-friends-greet function exists."
  (should (fboundp 'ehw-friends-greet)))

(ert-deftest test-ehw-friends-greet-all-function-exists ()
  "Test if ehw-friends-greet-all function exists."
  (should (fboundp 'ehw-friends-greet-all)))

;;; Interactive command tests
(ert-deftest test-ehw-friends-greet-is-interactive ()
  "Test if ehw-friends-greet is interactive."
  (should (commandp 'ehw-friends-greet)))

(ert-deftest test-ehw-friends-greet-all-is-interactive ()
  "Test if ehw-friends-greet-all is interactive."
  (should (commandp 'ehw-friends-greet-all)))

;;; Greeting functionality tests
(ert-deftest test-ehw-friends-greet-existing-friend ()
  "Test greeting an existing friend."
  (let ((ehw-friends-list '(("TestFriend" . "Hey %s!"))))
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (should (equal format-string "Hey %s!"))
                 (should (equal (car args) "TestFriend")))))
      (ehw-friends-greet "TestFriend"))))

(ert-deftest test-ehw-friends-greet-nonexistent-friend ()
  "Test greeting a nonexistent friend."
  (let ((ehw-friends-list nil))
    (should-error (ehw-friends-greet "NonexistentFriend"))))

(ert-deftest test-ehw-friends-greet-all-with-friends ()
  "Test greeting all friends."
  (let ((ehw-friends-list '(("Friend1" . "Hi %s!")
                            ("Friend2" . nil)))
        (ehw-friends-default-greeting "Hello, %s!")
        (message-log '()))
    ;; Mock the message function
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format (cons format-string args)) message-log))))
      (ehw-friends-greet-all)
      ;; Should contain 2 messages
      (should (= (length message-log) 2))
      ;; Should contain both greetings
      (should (member "Hi Friend1!" message-log))
      (should (member "Hello, Friend2!" message-log)))))

(ert-deftest test-ehw-friends-greet-all-no-friends ()
  "Test greeting all friends when none exist."
  (let ((ehw-friends-list nil))
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (should (string-match-p "No friends" format-string)))))
      (ehw-friends-greet-all))))

(provide 'test-ehw-friends-greet)
;;; test-ehw-friends-greet.el ends here