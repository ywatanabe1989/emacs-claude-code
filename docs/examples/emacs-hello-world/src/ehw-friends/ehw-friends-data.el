;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-12 23:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-hello-world/ehw-friends/data/ehw-friends-data.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ehw-utils)

;; 1. Main entry point
;; ----------------------------------------

(defcustom ehw-friends-storage-directory
  (expand-file-name ".ehw-friends" user-emacs-directory)
  "Directory to store friends data."
  :type 'directory
  :group 'ehw-friends)

(defcustom ehw-friends-default-greeting
  "Hello, %s! Nice to see you!"
  "Default greeting format for friends without custom greetings."
  :type 'string
  :group 'ehw-friends)

(defvar ehw-friends-list nil
  "List of friends with optional custom greeting templates.
Each element is a cons cell (NAME . GREETING) where NAME is a string
and GREETING is either a string format or nil (for default greeting).")

;; 2. Core functions
;; ----------------------------------------

(defun ehw-friends-data-get (name)
  "Get friend data for NAME.
Returns nil if friend not found."
  (assoc name ehw-friends-list))

(defun ehw-friends-data-set (name greeting)
  "Set friend NAME with GREETING.
If GREETING is nil, the default greeting will be used."
  (let ((existing (assoc name ehw-friends-list)))
    (if existing
        (setcdr existing greeting)
      (push (cons name greeting) ehw-friends-list))))

(defun ehw-friends-data-remove (name)
  "Remove friend with NAME from friends list."
  (setq ehw-friends-list
        (cl-remove name ehw-friends-list :key #'car :test #'string=)))

(defun ehw-friends-data-get-greeting (name)
  "Get greeting format for friend NAME.
If no custom greeting is set, returns the default greeting."
  (let ((friend-data (ehw-friends-data-get name)))
    (if friend-data
        (or (cdr friend-data) ehw-friends-default-greeting)
      ehw-friends-default-greeting)))

;; 3. Persistence functions
;; ----------------------------------------

(defun ehw-friends-data-save ()
  "Save friends data to file."
  (unless (file-exists-p ehw-friends-storage-directory)
    (make-directory ehw-friends-storage-directory t))
  (let ((save-path (expand-file-name "friends.el" ehw-friends-storage-directory)))
    (with-temp-file save-path
      (insert ";; ehw-friends data - automatically generated\n")
      (insert "(setq ehw-friends-list '")
      (prin1 ehw-friends-list (current-buffer))
      (insert ")\n"))))

(defun ehw-friends-data-load ()
  "Load friends data from file."
  (let ((file-path (expand-file-name "friends.el" ehw-friends-storage-directory)))
    (when (file-exists-p file-path)
      (load-file file-path))))

;; Initialize storage directory
(unless (file-exists-p ehw-friends-storage-directory)
  (make-directory ehw-friends-storage-directory t))

;; Try to load data
(ehw-friends-data-load)

(provide 'ehw-friends-data)

(when (not load-file-name)
  (message "ehw-friends-data.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))