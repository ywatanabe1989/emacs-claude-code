;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-12 21:37:34>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-hello-world/ehw-prep/ehw-prep-add-path.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; ehw-prep-add-path.el --- Add directories to load-path for emacs-hello-world

;;; Commentary:
;; This file provides functions to add directories to the load-path
;; for the emacs-hello-world package.

;;; Code:

(require 'ehw-prep-variables)

(defun --ehw-prep-add-subdirs-to-loadpath (parent-dir)
  "Add all visible subdirectories of PARENT-DIR to `load-path'.
Recursively adds all non-hidden subdirectories to the load path.
Hidden directories (starting with '.') are ignored."
  (let ((default-directory parent-dir))
    ;; Add parent directory itself
    (add-to-list 'load-path parent-dir)

    ;; Get all non-hidden directories
    (dolist (dir (directory-files parent-dir t))
      (when (and (file-directory-p dir)
                 (not (string-match-p "/\\.\\.?$" dir))  ; Skip . and ..
                 (not (string-match-p "/\\." dir)))
                                    ; Skip hidden dirs
        ;; Add this directory to load path
        (add-to-list 'load-path dir)))))

;; Determine current directory and add to load-path
(let ((current-dir
       (file-name-directory (or load-file-name buffer-file-name))))
  (--ehw-prep-add-subdirs-to-loadpath current-dir))

(provide 'ehw-prep-add-path)

(when (not load-file-name)
  (message "ehw-prep-add-path.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

;;; ehw-prep-add-path.el ends here