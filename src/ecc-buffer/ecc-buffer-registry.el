;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 11:00:36>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/ecc-buffer/ecc-buffer-registry.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ecc-variables)
(require 'ecc-buffer/ecc-buffer-timestamp)

;; Registry
;; ------------------------------

(defun ecc-buffer-create-registered-buffer (&optional display)
  "Create a new Claude buffer and register it.
Return the new buffer."
  (interactive)
  (let* ((buffer-name (format "*CLAUDE-CODE-%02d*"
                              (+
                               (length
                                ecc-buffer-registered-buffers-alist)
                               1)))
         (new-buffer (get-buffer-create buffer-name)))
    (with-current-buffer new-buffer
      (vterm-mode)
      (set (make-local-variable 'ecc-original-name) buffer-name))
    (ecc-buffer-register-buffer new-buffer)
    (if display
        (display-buffer new-buffer))
    new-buffer))

(defun ecc-buffer-list-registered-buffers ()
  "List all registered Claude buffers."
  (interactive)
  ecc-buffer-registered-buffers-alist)

(defun ecc-buffer-get-registered-buffers ()
  "List all registered Claude buffers."
  (interactive)
  (mapcar #'car ecc-buffer-registered-buffers-alist))

(defun ecc-buffer-register-buffer (buf)
  "Register BUF in `ecc-buffer-registered-buffers-alist'.
Also records a timestamp for the buffer.
Return the registered buffer."
  (when buf
    (let ((buffer (if (bufferp buf) buf (get-buffer buf))))
      (when (and buffer (buffer-live-p buffer))
        ;; Register the buffer if not already registered
        (unless (assoc buffer ecc-buffer-registered-buffers-alist)
          (push (cons buffer nil) ecc-buffer-registered-buffers-alist)
          ;; Record a timestamp for the buffer
          (ecc-buffer-record-timestamp buffer))
        buffer))))

(defun ecc-buffer-unregister-buffer (buf)
  "Unregister BUF from `ecc-buffer-registered-buffers-alist'.
Return t if buffer was removed, nil otherwise."
  (when buf
    (let ((buffer (if (bufferp buf) buf (get-buffer buf))))
      (when (assoc buffer ecc-buffer-registered-buffers-alist)
        (setq ecc-buffer-registered-buffers-alist
              (assoc-delete-all buffer
                                ecc-buffer-registered-buffers-alist))
        t))))


(provide 'ecc-buffer/ecc-buffer-registry)

(when
    (not load-file-name)
  (message "ecc-buffer-registry.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))