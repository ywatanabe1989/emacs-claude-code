;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-28 06:12:34>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;; 1. Package configuration
;; ----------------------------------------

(defgroup ecc nil
  "Emacs Claude Code package."
  :prefix "--ecc-"
  :group 'tools)

(defconst --ecc-version "1.0.0"
  "Current version of the emacs-claude-code package.")


;; 2. Dependencies
;; ----------------------------------------

(require 'ecc-debug)
(require 'ecc-state-detection)
(require 'ecc-notification)
(require 'ecc-auto-response)
(require 'ecc-vterm-utils)
(require 'ecc-vterm-mode)
(require 'ecc-list)


;; 3. Main entry point
;; ----------------------------------------

;;;###autoload
(defun --ecc-create-vterm ()
  "Create a new Claude vterm buffer."
  (interactive)
  (--ecc-debug-message "Creating new Claude vterm buffer...")
  (let ((buffer (--ecc-vterm-mode-create)))
    (--ecc-debug-message "Created vterm buffer: %s" (buffer-name buffer))
    buffer))


(provide 'ecc)

(when
    (not load-file-name)
  (message "ecc.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))