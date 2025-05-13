;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 17:59:30>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-state/ecc-state-send.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(defun ecc-auto-accept-send ()
  "Automatically respond to Claude prompts in vterm."
  (interactive)
  (when (get-buffer ecc-buffer-name)
    (with-current-buffer ecc-buffer-name
      (cond
       ((--ecc-detect-prompt-y/y/n)
        (--ecc-auto-send-yy))
       ((--ecc-detect-prompt-y/n)
        (--ecc-auto-send-y))
       ((--ecc-detect-prompt-waiting)
        (--ecc-auto-send-continue))
       ((--ecc-detect-prompt-initial-waiting)
        (--ecc-auto-send-continue))))))

;; Sub-functions
;; ------------------------------

(defun --ecc-auto-send-y ()
  "Automatically respond with '1' to Claude prompts in vterm."
  (interactive)
  (with-current-buffer ecc-buffer-name
    (when (--ecc-detect-prompt-y/n)
      (sit-for 0.3)
      (vterm-send-string "1\n")
      (vterm-send-return)
      (sit-for 0.3)
      (vterm-copy-mode -1)
      (message "[ecc-auto-accept-send] Automatic Response: 1"))))

(defun --ecc-auto-send-yy ()
  "Automatically respond with '2' to Claude prompts in vterm."
  (interactive)
  (with-current-buffer ecc-buffer-name
    (when (--ecc-detect-prompt-y/y/n)
      (sit-for 0.3)
      (vterm-send-string "2\n")
      (vterm-send-return)
      (vterm-copy-mode -1)
      (sit-for 0.3)
      (message "[ecc-auto-accept-send] Automatic Response: 2"))))

(defun --ecc-auto-send-continue ()
  "Automatically respond with 'continue' to Claude waiting prompts in vterm."
  (interactive)
  (with-current-buffer ecc-buffer-name
    (when (or (--ecc-detect-prompt-waiting)
              (--ecc-detect-prompt-initial-waiting))
      (sit-for 0.3)
      (vterm-send-string "continue\n")
      (vterm-send-return)
      (vterm-copy-mode -1)
      (sit-for 0.3)
      (message
       "[ecc-auto-accept-send] Automatic Response: continue"))))


(provide 'ecc-state-send)

(when
    (not load-file-name)
  (message "ecc-state-send.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))