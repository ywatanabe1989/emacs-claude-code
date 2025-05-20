;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-12 23:50:27>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-hello-world/src/ehw-core/ehw-core-hello-world.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;;###autoload
(defun ehw-core-hello-world (name)
  "Say hello to NAME with a friendly message."
  (interactive "sEnter your name: ")
  (ehw-core-greet name))

;;;###autoload
(defun ehw-core-format (text)
  "Format TEXT with hello world styling."
  (interactive "sEnter text to format: ")
  (insert (ehw-utils-format text)))

;;;###autoload
(defun ehw-core-manage-friends ()
  "Manage your friends list."
  (interactive)
  (let ((action (completing-read "Choose action: " 
                                '("Add friend" 
                                  "Remove friend" 
                                  "List friends" 
                                  "Greet friend" 
                                  "Greet all friends"))))
    (cond
     ((string= action "Add friend") 
      (call-interactively 'ehw-friends-add))
     ((string= action "Remove friend") 
      (call-interactively 'ehw-friends-remove))
     ((string= action "List friends") 
      (call-interactively 'ehw-friends-list))
     ((string= action "Greet friend") 
      (call-interactively 'ehw-friends-greet))
     ((string= action "Greet all friends") 
      (call-interactively 'ehw-friends-greet-all)))))


(provide 'ehw-core-hello-world)

(when
    (not load-file-name)
  (message "ehw-core-hello-world.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
