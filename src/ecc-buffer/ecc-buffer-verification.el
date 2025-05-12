;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 00:15:14>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/ecc-buffer/ecc-buffer-verification.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ecc-variables)

(defun ecc-buffer-verify-buffer (buf)
  "Verify that BUF is a valid vterm buffer.
Return t if buffer is valid, in vterm-mode, and registered, nil otherwise."
  (when buf
    (let* ((buffer (if (bufferp buf) buf (get-buffer buf)))
           (is-alive (and buffer (buffer-live-p buffer)))
           (is-vterm (when is-alive
                       (with-current-buffer buffer
                         (derived-mode-p 'vterm-mode))))
           (is-registered (when buffer
                            (assoc buffer
                                   ecc-buffer-registered-buffers-alist))))
      (and is-alive
           is-vterm
           is-registered))))


;; Register this feature with standard naming
(provide 'ecc-buffer-verification)


(when
    (not load-file-name)
  (message "ecc-buffer-verification.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))