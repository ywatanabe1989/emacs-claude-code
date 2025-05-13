;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 11:16:31>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-state/ecc-state-current-buffer.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ecc-buffer-current)

(defun ecc-buffer-get-current-buffer-state ()
  "Get the state of the current Claude buffer.
Return nil if no valid buffer is set or buffer has no state."
  (when-let ((current-buffer (ecc-buffer-get-current-buffer)))
    (ecc-buffer-get-buffer-state current-buffer)))

(defun ecc-buffer-set-current-buffer-state (state)
  "Set the state of the current Claude buffer to STATE.
Return t if state was set successfully, nil otherwise."
  (when-let ((current-buffer (ecc-buffer-get-current-buffer)))
    (ecc-buffer-set-buffer-state current-buffer state)))


(provide 'ecc-state-current-buffer)

(when
    (not load-file-name)
  (message "ecc-state-current-buffer.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))