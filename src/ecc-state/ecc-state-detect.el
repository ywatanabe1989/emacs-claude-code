;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 03:30:15>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-state/ecc-state-detect.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;; This is a compatibility module for ecc-state.el

(require 'ecc-state)

;; Define compatibility functions with proper argument handling
(defun ecc-state-detect-prompt (prompt-text &optional n-lines &rest _)
  "Compatibility function for state detection.
Calls internal state detection with PROMPT-TEXT and optional N-LINES.
Ignores any additional arguments for backward compatibility.
Supports both vterm and standard buffers."
  (--ecc-state-detect-prompt prompt-text n-lines))

;; Provide public wrapper for internal detection function
(defun ecc-state-get (&rest _)
  "Get the current Claude prompt state.
Calls internal state detection function.
Ignores any arguments for backward compatibility.
Returns one of: :y/y/n, :y/n, :waiting, :initial-waiting, :running, or nil."
  (if (and (bound-and-true-p ecc-active-buffer)
           (buffer-live-p ecc-active-buffer))
      (with-current-buffer ecc-active-buffer
        (cond
         ((--ecc-state-y/y/n-p)
          :y/y/n)
         ((--ecc-state-y/n-p)
          :y/n)
         ((--ecc-state-waiting-p)
          :waiting)
         ((--ecc-state-initial-waiting-p)
          :initial-waiting)
         ((--ecc-state-running-p)
          :running)
         (t nil)))
    nil))

;; Provide both features to maintain backward compatibility
(provide 'ecc-state-detect)
(provide 'ecc-state/ecc-state-detect)

(when (not load-file-name)
  (message "ecc-state-detect.el loaded: %s"
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))