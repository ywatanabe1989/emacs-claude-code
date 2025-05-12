;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 15:00:12>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/ecc-buffer/ecc-buffer-timestamp.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'cl-lib)

;; Buffer timestamps
(defvar ecc-buffer-timestamps (make-hash-table :test 'equal)
  "Hash table mapping Claude buffers to their creation timestamps.")

(defun ecc-buffer-record-timestamp (buffer)
  "Record the current time as timestamp for BUFFER.
Returns the timestamp that was recorded."
  (when (buffer-live-p buffer)
    (let ((timestamp (current-time)))
      (puthash buffer timestamp ecc-buffer-timestamps)
      timestamp)))

(defun ecc-buffer-get-timestamp (buffer)
  "Get the timestamp for BUFFER.
Returns nil if no timestamp is recorded for this buffer."
  (when (buffer-live-p buffer)
    (gethash buffer ecc-buffer-timestamps)))

;; Register this feature with standard naming
(provide 'ecc-buffer-timestamp)

;; Also provide with prefix to match test expectations
(provide 'ecc-buffer/ecc-buffer-timestamp)


(when (not load-file-name)
  (message "%s loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))