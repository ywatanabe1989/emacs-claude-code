;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-06-04 08:47:04>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-debug.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;; 1. Dependencies
;; ----------------------------------------
;; No dependencies for this module

;; 2. Configuration
;; ----------------------------------------

(defcustom --ecc-debug-enabled nil
  "Whether debugging is globally enabled."
  :type 'boolean
  :group 'ecc)

;; 3. Variables
;; ----------------------------------------

(defvar --ecc-debug-prefix "[ECC] "
  "Prefix for debug messages.")

;; 4. Main Entry Points
;; ----------------------------------------

(defun ecc-debug-toggle ()
  "Toggle global debug output."
  (interactive)
  (setq --ecc-debug-enabled (not --ecc-debug-enabled))
  (message "ECC debug %s"
           (if --ecc-debug-enabled "enabled" "disabled")))

;; 5. Core Functions
;; ----------------------------------------

(defun --ecc-debug-message (format-string &rest args)
  "Output a debug message if debugging is enabled."
  (when --ecc-debug-enabled
    (apply #'message (concat --ecc-debug-prefix format-string) args)))

;; 6. Helper/Utility Functions
;; ----------------------------------------
;; No helper functions in this file

(when
    (not load-file-name)
  (--ecc-debug-message "ecc-debug.el loaded."
                       (file-name-nondirectory
                        (or load-file-name buffer-file-name))))


(provide 'ecc-debug)

(when
    (not load-file-name)
  (message "ecc-debug.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))