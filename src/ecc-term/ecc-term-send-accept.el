;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 09:50:31>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-term/ecc-term-send-accept.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(defvar ecc-auto-send-waiting-command
  "/git"
  "Commands to send on waiting status")

(defun ecc-term-send-accept ()
  "Automatically respond to Claude prompts in vterm."
  (interactive)
  (when
      (and ecc-buffer-name (stringp ecc-buffer-name)
           (get-buffer ecc-buffer-name))
    (with-current-buffer ecc-buffer-name
      (cond
       ((--ecc-detect-prompt-y/y/n)
        (--ecc-auto-send-yy))
       ((--ecc-detect-prompt-y/n)
        (--ecc-auto-send-y))
       ((--ecc-detect-prompt-waiting)
        (--ecc-auto-send-waiting))
       ((--ecc-detect-prompt-initial-waiting)
        (--ecc-auto-send-waiting))))))

;; Provide backward compatibility alias

(defalias 'ecc-auto-send-accept 'ecc-term-send-accept)

;; Sub-functions
;; ------------------------------

(defun --ecc-auto-send-y ()
  "Automatically respond with '1' to Claude prompts in vterm."
  (interactive)
  (with-current-buffer ecc-buffer-name
    (when (--ecc-detect-prompt-y/n)
      (sit-for 3.0)
      (vterm-send-string "1")
      (sit-for 3.0)
      (vterm-send-return)
      (vterm-copy-mode -1)
      (message "[ecc-auto-send-accept] Automatic Response: 1"))))

(defun --ecc-auto-send-yy ()
  "Automatically respond with '2' to Claude prompts in vterm."
  (interactive)
  (with-current-buffer ecc-buffer-name
    (when (--ecc-detect-prompt-y/y/n)
      (sit-for 3.0)
      (vterm-send-string "2")
      (sit-for 3.0)
      (vterm-send-return)
      (vterm-copy-mode -1)
      (sit-for 1.0)
      (message "[ecc-auto-send-accept] Automatic Response: 2"))))

(defun --ecc-auto-send-waiting ()
  "Automatically respond with 'ecc-auto-send-waiting-command' to Claude waiting prompts in vterm."
  (interactive)
  (with-current-buffer ecc-buffer-name
    (when (or (--ecc-detect-prompt-waiting)
              (--ecc-detect-prompt-initial-waiting))
      (sit-for 3.0)
      (vterm-send-string ecc-auto-send-waiting-command)
      (sit-for 3.0)
      (vterm-send-return)
      (vterm-copy-mode -1)
      (message
       "[ecc-auto-send-accept] Automatic Response: %s"
       ecc-auto-send-waiting-command))))


(provide 'ecc-term-send-accept)

(when
    (not load-file-name)
  (message "ecc-term-send-accept.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))