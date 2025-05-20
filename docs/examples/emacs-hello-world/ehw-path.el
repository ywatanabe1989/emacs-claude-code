;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-12 23:17:19>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-hello-world/emacs-hello-world.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;; Add subdirectories to load path

(defun --ehw-add-subdirs-to-loadpath (parent-dir)
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
(let
    ((current-dir
      (file-name-directory (or load-file-name buffer-file-name))))
  (--ehw-add-subdirs-to-loadpath current-dir))

;; Require the module heads
(require 'ehw-utils)
(require 'ehw-core)
(require 'ehw-friends)


(provide 'emacs-hello-world)

(when
    (not load-file-name)
  (message "emacs-hello-world.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))