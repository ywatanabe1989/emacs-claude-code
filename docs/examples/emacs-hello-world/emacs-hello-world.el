;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-12 23:19:10>
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
    ;; Add src directory and its subdirectories
    (let ((src-dir (expand-file-name "src" parent-dir)))
      (when (file-directory-p src-dir)
        (add-to-list 'load-path src-dir)
        (dolist (dir (directory-files src-dir t))
          (when (and (file-directory-p dir)
                     (not (string-match-p "/\\.\\.?$" dir))  ; Skip . and ..
                     (not (string-match-p "/\\." dir)))
                                        ; Skip hidden dirs
            ;; Add this directory to load path
            (add-to-list 'load-path dir)))))

    ;; Add tests directory and its subdirectories
    (let ((tests-dir (expand-file-name "tests" parent-dir)))
      (when (file-directory-p tests-dir)
        (add-to-list 'load-path tests-dir)
        (dolist (dir (directory-files tests-dir t))
          (when (and (file-directory-p dir)
                     (not (string-match-p "/\\.\\.?$" dir))  ; Skip . and ..
                     (not (string-match-p "/\\." dir)))
                                        ; Skip hidden dirs
            ;; Add this directory to load path
            (add-to-list 'load-path dir)))))))

;; Determine current directory and add to load-path
(let
    ((current-dir
      (file-name-directory (or load-file-name buffer-file-name))))
  (--ehw-add-subdirs-to-loadpath current-dir))

;; Require the module heads
(require 'ehw-prep)
(require 'ehw-utils)
(require 'ehw-core)
(require 'ehw-friends)


(provide 'emacs-hello-world)

(when
    (not load-file-name)
  (message "emacs-hello-world.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))