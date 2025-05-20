;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-12 23:34:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-hello-world/ehw-friends/greet/ehw-friends-greet.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ehw-utils)
(require 'ehw-core)
(require 'ehw-friends-data)

;; 1. Interactive commands
;; ----------------------------------------

;;;###autoload
(defun ehw-friends-greet (name)
  "Send a greeting to a friend with NAME.
Uses the friend's custom greeting if set, or the default greeting."
  (interactive
   (list
    (completing-read "Greet friend: " 
                     (mapcar #'car ehw-friends-list)
                     nil t)))
  (if (assoc name ehw-friends-list)
      (let ((greeting-format (ehw-friends-data-get-greeting name)))
        (message greeting-format name))
    (error "Friend %s not found in your list" name)))

;;;###autoload
(defun ehw-friends-greet-all ()
  "Send greetings to all friends in the list."
  (interactive)
  (if (null ehw-friends-list)
      (message "No friends in your list to greet.")
    (dolist (friend ehw-friends-list)
      (let ((name (car friend))
            (greeting-format (ehw-friends-data-get-greeting (car friend))))
        (message greeting-format name)))))

(provide 'ehw-friends-greet)

(when (not load-file-name)
  (message "ehw-friends-greet.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))