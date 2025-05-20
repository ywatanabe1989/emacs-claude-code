;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-12 23:32:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-hello-world/ehw-friends/manage/ehw-friends-manage.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ehw-utils)
(require 'ehw-friends-data)

;; 1. Interactive commands
;; ----------------------------------------

;;;###autoload
(defun ehw-friends-add (name &optional greeting)
  "Add a friend with NAME and optional custom GREETING.
If GREETING is nil, the default greeting will be used."
  (interactive "sFriend name: \nsCustom greeting (leave empty for default): ")
  (when (equal greeting "")
    (setq greeting nil))
  (ehw-friends-data-set name greeting)
  (ehw-friends-data-save)
  (message "Friend %s added." name))

;;;###autoload
(defun ehw-friends-remove (name)
  "Remove a friend with NAME from the friends list."
  (interactive
   (list
    (completing-read "Remove friend: " 
                     (mapcar #'car ehw-friends-list)
                     nil t)))
  (ehw-friends-data-remove name)
  (ehw-friends-data-save)
  (message "Friend %s removed." name))

;;;###autoload
(defun ehw-friends-list ()
  "List all friends with their custom greetings."
  (interactive)
  (if (null ehw-friends-list)
      "No friends in your list."
    (let ((output-buffer (get-buffer-create "*Friends List*")))
      (with-current-buffer output-buffer
        (erase-buffer)
        (insert "Friends List:\n\n")
        (dolist (friend ehw-friends-list)
          (let ((name (car friend))
                (greeting (cdr friend)))
            (insert (format "- %s" name))
            (when greeting
              (insert (format " (Custom greeting: \"%s\")" greeting)))
            (insert "\n")))
        (goto-char (point-min))
        (when (called-interactively-p 'any)
          (switch-to-buffer output-buffer))
        (buffer-string)))))

(provide 'ehw-friends-manage)

(when (not load-file-name)
  (message "ehw-friends-manage.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))