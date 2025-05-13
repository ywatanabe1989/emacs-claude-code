;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 04:44:35>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-ui/ecc-ui-dired.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ecc-repository)

(defun ecc-ui-get-repository-files (dir)
  "Get list of files in DIR, filtered by blacklist and size."
  (let ((files '()))
    (ecc-ui--dired-collect-files dir files)
    (nreverse files)))

(defun ecc-ui--dired-collect-files (dir files-list)
  "Recursively collect files from DIR into FILES-LIST."
  (dolist (file (directory-files dir t))
    (let ((file-name (file-name-nondirectory file)))
      (unless (or (string= file-name ".")
                  (string= file-name ".."))
        (if (file-directory-p file)
            (unless (ecc-repository-blacklisted-p file)
              (ecc-ui--dired-collect-files file files-list))
          (when (and (not (ecc-repository-blacklisted-p file))
                     (<= (file-attribute-size (file-attributes file))
                         ecc-repository-max-file-size))
            (push file files-list))))))
  files-list)

;; Backward compatibility for old function names
(defalias 'ecc-get-repository-files 'ecc-ui-get-repository-files)
(defalias 'ecc--dired-collect-files 'ecc-ui--dired-collect-files)

;; Provide both the new and old feature names for backward compatibility
(provide 'ecc-ui-dired)
(provide 'ecc-dired)

(when
    (not load-file-name)
  (message "ecc-ui-dired.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))