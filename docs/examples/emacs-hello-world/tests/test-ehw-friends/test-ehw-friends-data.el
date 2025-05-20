;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 00:00:15>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-hello-world/tests/test-ehw-friends/test-ehw-friends-data.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:
;; Tests for the ehw-friends-data module

;;; Code:

(require 'ert)
(require 'ehw-friends-data)

;;; Module loadability tests
(ert-deftest test-ehw-friends-data-loadable ()
  "Test if ehw-friends-data module is loadable."
  (should (featurep 'ehw-friends-data)))

;;; Customization and variable tests
(ert-deftest test-ehw-friends-data-custom-vars-exist ()
  "Test if customization variables exist."
  (should (boundp 'ehw-friends-storage-directory))
  (should (boundp 'ehw-friends-default-greeting)))

(ert-deftest test-ehw-friends-data-list-exists ()
  "Test if friends list variable exists."
  (should (boundp 'ehw-friends-list)))

;;; Data structure manipulation tests
(ert-deftest test-ehw-friends-data-get-nonexistent ()
  "Test getting a nonexistent friend returns nil."
  (let ((ehw-friends-list nil))
    (should (eq nil (ehw-friends-data-get "NonexistentFriend")))))

(ert-deftest test-ehw-friends-data-set-new-friend ()
  "Test setting a new friend."
  (let ((ehw-friends-list nil))
    (ehw-friends-data-set "TestFriend" nil)
    (should (equal '(("TestFriend" . nil)) ehw-friends-list))))

(ert-deftest test-ehw-friends-data-set-existing-friend ()
  "Test updating an existing friend."
  (let ((ehw-friends-list '(("TestFriend" . nil))))
    (ehw-friends-data-set "TestFriend" "Custom greeting, %s!")
    (should (equal '(("TestFriend" . "Custom greeting, %s!")) ehw-friends-list))))

(ert-deftest test-ehw-friends-data-remove-friend ()
  "Test removing a friend."
  (let ((ehw-friends-list '(("TestFriend" . nil) ("OtherFriend" . "Hi, %s!"))))
    (ehw-friends-data-remove "TestFriend")
    (should (equal '(("OtherFriend" . "Hi, %s!")) ehw-friends-list))))

;;; Greeting tests
(ert-deftest test-ehw-friends-data-get-greeting-custom ()
  "Test getting a custom greeting."
  (let ((ehw-friends-list '(("TestFriend" . "Hey %s, what's up?")))
        (ehw-friends-default-greeting "Hello, %s!"))
    (should (equal "Hey %s, what's up?" (ehw-friends-data-get-greeting "TestFriend")))))

(ert-deftest test-ehw-friends-data-get-greeting-default ()
  "Test getting the default greeting when no custom greeting is set."
  (let ((ehw-friends-list '(("TestFriend" . nil)))
        (ehw-friends-default-greeting "Hello, %s!"))
    (should (equal "Hello, %s!" (ehw-friends-data-get-greeting "TestFriend")))))

(ert-deftest test-ehw-friends-data-get-greeting-nonexistent ()
  "Test getting greeting for nonexistent friend returns default."
  (let ((ehw-friends-list nil)
        (ehw-friends-default-greeting "Hello, %s!"))
    (should (equal "Hello, %s!" (ehw-friends-data-get-greeting "NonexistentFriend")))))

;;; Persistence tests
(ert-deftest test-ehw-friends-data-save-function-exists ()
  "Test if ehw-friends-data-save function exists."
  (should (fboundp 'ehw-friends-data-save)))

(ert-deftest test-ehw-friends-data-load-function-exists ()
  "Test if ehw-friends-data-load function exists."
  (should (fboundp 'ehw-friends-data-load)))

(ert-deftest test-ehw-friends-data-save-and-load ()
  "Test saving and loading friends data."
  (let* ((temp-dir (make-temp-file "ehw-friends-test-" t))
         (ehw-friends-storage-directory temp-dir)
         (original-list '(("TestFriend" . "Hi, %s!")
                          ("OtherFriend" . nil)))
         (ehw-friends-list original-list))
    (unwind-protect
        (progn
          ;; Save the data
          (ehw-friends-data-save)
          
          ;; Clear the list
          (setq ehw-friends-list nil)
          (should (null ehw-friends-list))
          
          ;; Load the data
          (ehw-friends-data-load)
          
          ;; Verify the list is restored
          (should (equal ehw-friends-list original-list)))
      
      ;; Cleanup temporary directory
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest test-ehw-friends-data-load-nonexistent ()
  "Test loading from a non-existent file."
  (let* ((temp-dir (make-temp-file "ehw-friends-test-" t))
         (ehw-friends-storage-directory temp-dir)
         (ehw-friends-list '(("InitialFriend" . nil))))
    (unwind-protect
        (progn
          ;; Delete any existing file
          (let ((file-path (expand-file-name "friends.el" temp-dir)))
            (when (file-exists-p file-path)
              (delete-file file-path)))
          
          ;; Try to load (should not change the list or error)
          (ehw-friends-data-load)
          
          ;; List should remain unchanged
          (should (equal ehw-friends-list '(("InitialFriend" . nil)))))
      
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest test-ehw-friends-data-save-creates-directory ()
  "Test that saving creates the directory if it doesn't exist."
  (let* ((temp-dir (expand-file-name "ehw-friends-test" temporary-file-directory))
         (ehw-friends-storage-directory temp-dir)
         (ehw-friends-list '(("TestFriend" . nil))))
    
    ;; Ensure directory doesn't exist
    (when (file-exists-p temp-dir)
      (delete-directory temp-dir t))
    
    (unwind-protect
        (progn
          ;; Directory should not exist yet
          (should-not (file-exists-p temp-dir))
          
          ;; Save should create directory
          (ehw-friends-data-save)
          
          ;; Directory should now exist
          (should (file-directory-p temp-dir))
          
          ;; File should exist
          (should (file-exists-p (expand-file-name "friends.el" temp-dir))))
      
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(provide 'test-ehw-friends-data)
;;; test-ehw-friends-data.el ends here