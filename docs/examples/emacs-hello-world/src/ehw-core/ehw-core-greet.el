;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-12 23:15:54>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-hello-world/src/ehw-core/ehw-core-greet.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;; 1. Main entry point
;; ----------------------------------------

(defun ehw-core-greet (name)
  "Greet NAME with a friendly message."
  (interactive "sEnter your name: ")
  (message "Hello, %s! Welcome to the Emacs world!"
           (ehw-utils-trim name)))

;; 2. Core functions
;; ----------------------------------------

(defun ehw-core-insert-greeting (name)
  "Insert greeting for NAME at point."
  (interactive "sEnter your name: ")
  (insert (format "Hello, %s! Welcome to the Emacs world!" name)))

(defun ehw-core-goodbye (name)
  "Say goodbye to NAME."
  (interactive "sEnter your name: ")
  (message "Goodbye, %s! Have a nice day!" name))

;; 3. Helper functions
;; ----------------------------------------

(defun --ehw-core-internal-helper (format-string &rest args)
  "Internal helper for formatting.
FORMAT-STRING and ARGS are passed to `format'."
  (apply #'format format-string args))


(provide 'ehw-core-greet)

(when
    (not load-file-name)
  (message "ehw-core-greet.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))