;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 18:10:14>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-ui/ecc-ui-notification.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;; Notification support functions

(defun ecc-auto-notification-on ()
  "Send notification that auto mode has been enabled."
  (condition-case nil
      (when (file-exists-p "~/.bin/utils/notify.sh")
        (call-process "~/.bin/utils/notify.sh" nil 0 nil
                      "Claude Auto Mode" "Auto-accept mode enabled"))
    (error nil)))

(defun ecc-auto-notification-off ()
  "Send notification that auto mode has been disabled."
  (condition-case nil
      (when (file-exists-p "~/.bin/utils/notify.sh")
        (call-process "~/.bin/utils/notify.sh" nil 0 nil
                      "Claude Auto Mode" "Auto-accept mode disabled"))
    (error nil)))

(defun ecc-auto-notify-completion (prompt-type)
  "Send notification that Claude has responded to a prompt.
PROMPT-TYPE is a string describing the type of prompt that was handled."
  (condition-case nil
      (when (and (file-exists-p "~/.bin/utils/notify.sh")
                 (boundp 'ecc-auto-mode)
                 ecc-auto-mode)
        (call-process "~/.bin/utils/notify.sh" nil 0 nil
                      "Claude Auto Response"
                      (format "Auto-responded to %s prompt"
                              prompt-type)))
    (error nil)))


(provide 'ecc-ui-notification)

(when
    (not load-file-name)
  (message "ecc-ui-notification.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))