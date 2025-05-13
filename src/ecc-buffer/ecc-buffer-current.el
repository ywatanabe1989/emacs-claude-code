;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 19:20:30>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-buffer/ecc-buffer-current.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ecc-buffer-variables)
(require 'ecc-buffer-registry)

(defun ecc-buffer-set-current-buffer (buf)
  "Set BUF as the current Claude buffer.
Return the set buffer or nil if buffer is invalid."
  (when buf
    (let ((buffer (if (bufferp buf) buf (get-buffer buf))))
      (when (and buffer (buffer-live-p buffer))
        (ecc-buffer-register-buffer buffer)
        (setq ecc-buffer-current-buffer buffer)
        buffer))))

(defun ecc-buffer-get-current-buffer ()
  "Get the current Claude buffer.
Return nil if no valid buffer is set."
  (when (and ecc-buffer-current-buffer
             (buffer-live-p ecc-buffer-current-buffer))
    ecc-buffer-current-buffer))


(provide 'ecc-buffer-current)

(when
    (not load-file-name)
  (message "ecc-buffer-current.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))