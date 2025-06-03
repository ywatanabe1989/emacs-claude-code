;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-28 05:43:59>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/emacs-claude-code.el
;;; Version: 3.0.0

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'cl-lib)

(defun --ecc-add-all-to-loadpath ()
  "Add src and tests directories recursively to `load-path`."
  (let* ((base-dir (file-name-directory
                    (or load-file-name buffer-file-name)))
         (src-dir (expand-file-name "src" base-dir))
         (tests-dir (expand-file-name "tests" base-dir)))
    (cl-labels ((add-dir-recursively
                  (dir)
                  (when (file-directory-p dir)
                    (add-to-list 'load-path dir)
                    (dolist (subdir (directory-files dir t))
                      (when (and (file-directory-p subdir)
                                 (not
                                  (string-match-p "/\\.$\\|/\\.\\.$"
                                                  subdir))
                                 (not
                                  (string-match-p
                                   "contrib\\|.old\\|old"
                                   subdir)))
                        (add-dir-recursively subdir))))))
      (when (file-directory-p src-dir)
        (add-dir-recursively src-dir))
      (when (file-directory-p tests-dir)
        (add-dir-recursively tests-dir)))))

(--ecc-add-all-to-loadpath)

(require 'ecc)


(provide 'emacs-claude-code)

(when
    (not load-file-name)
  (message "emacs-claude-code.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))