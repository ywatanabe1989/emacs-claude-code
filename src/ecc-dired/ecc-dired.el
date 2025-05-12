;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-07 12:27:25>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/ecc-dired.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(defun ecc-get-repository-files (dir)
  "Get list of files in DIR, filtered by blacklist and size."
  (let ((files '()))
    (ecc--dired-collect-files dir files)
    (nreverse files)))

(defun ecc--dired-collect-files (dir files-list)
  "Recursively collect files from DIR into FILES-LIST."
  (dolist (file (directory-files dir t))
    (let ((file-name (file-name-nondirectory file)))
      (unless (or (string= file-name ".")
                  (string= file-name ".."))
        (if (file-directory-p file)
            (unless (ecc-repository-blacklisted-p file)
              (ecc--dired-collect-files file files-list))
          (when (and (not (ecc-repository-blacklisted-p file))
                     (<= (file-attribute-size (file-attributes file))
                         ecc-repository-max-file-size))
            (push file files-list))))))
  files-list)


(provide 'ecc-dired)

(when
    (not load-file-name)
  (message "ecc-dired.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))