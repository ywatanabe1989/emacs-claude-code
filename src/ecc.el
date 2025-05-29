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

(defconst --ecc-version "2.0.0"
  "Current version of the emacs-claude-code package.")


;; 2. Dependencies
;; ----------------------------------------

(require 'ecc-debug)
(require 'ecc-state-detection)
(require 'ecc-notification)
(require 'ecc-auto-response)
(require 'ecc-vterm-utils)
(require 'ecc-list)
(require 'ecc-auto-periodical)


;; 3. Main entry point
;; ----------------------------------------

;;;###autoload
(defun --ecc-create-vterm ()
  "Create a new vterm buffer with Claude auto-response enabled."
  (interactive)
  (--ecc-debug-message "Creating new Claude vterm buffer...")
  (require 'vterm nil t)
  (if (not (fboundp 'vterm))
      (user-error "vterm is not installed")
    (let ((buffer (vterm "*Claude-vterm*")))
      (with-current-buffer buffer
        (--ecc-auto-response-enable-buffer))
      (--ecc-debug-message "Created vterm buffer: %s" (buffer-name buffer))
      buffer)))


(provide 'ecc)

(when
    (not load-file-name)
  (message "ecc.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))