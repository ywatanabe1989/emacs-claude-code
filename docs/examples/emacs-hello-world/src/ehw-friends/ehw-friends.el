;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-12 23:53:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-hello-world/ehw-friends/ehw-friends.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:
;;; This is the umbrella module for the ehw-friends functionality.
;;; It manages friends and allows sending personalized greetings.

;;; Code:

(require 'ehw-core)
(require 'ehw-friends-data)
(require 'ehw-friends-manage)
(require 'ehw-friends-greet)

(defgroup ehw-friends nil
  "Friends management for emacs-hello-world."
  :group 'applications
  :prefix "ehw-friends-")

;;;###autoload
(defun ehw-friends-list-all ()
  "List all friends with their custom greetings."
  (interactive)
  (ehw-friends-list))

;;;###autoload
(defun ehw-friends-greet-everyone ()
  "Send greetings to all friends."
  (interactive)
  (ehw-friends-greet-all))

(provide 'ehw-friends)

(when (not load-file-name)
  (message "ehw-friends.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

;;; ehw-friends.el ends here