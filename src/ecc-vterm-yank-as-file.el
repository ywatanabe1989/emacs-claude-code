;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-06-04 06:41:35>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-vterm-yank-as-file.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'vterm nil t)

;; 1. Main entry point
;; ----------------------------------------
(defun ecc-vterm-yank-as-file ()
  "Create a temporary file with the latest kill-ring content and send path to vterm.

Example:
  ;; Copy some text to kill-ring
  ;; Run M-x ecc-vterm-yank-as-file
  ;; Sends \"Read /path/to/temp/file.tmp\" to vterm"
  (interactive)
  (if (not (derived-mode-p 'vterm-mode))
      (message "This command only works in vterm-mode")
    (let ((temp-file (--ecc-vterm-create-temp-file))
          (content (--ecc-vterm-get-kill-ring-content)))
      (if content
          (progn
            (--ecc-vterm-write-content-to-file content temp-file)
            (--ecc-vterm-send-read-command temp-file))
        (message "Kill ring is empty")))))

;; 2. Core functions
;; ----------------------------------------
(defun --ecc-vterm-create-temp-file ()
  "Create a unique temporary file in the current directory.

Returns the absolute path of the created file."
  (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (filename (format "kill-ring-%s.tmp" timestamp))
         (filepath (expand-file-name filename default-directory)))
    filepath))

(defun --ecc-vterm-get-kill-ring-content ()
  "Get the latest content from the kill-ring.

Returns the content as a string, or nil if kill-ring is empty."
  (when kill-ring
    (car kill-ring)))

(defun --ecc-vterm-write-content-to-file (content filepath)
  "Write CONTENT to FILEPATH."
  (with-temp-file filepath
    (insert content))
  (message "Created temporary file: %s" filepath))

(defun --ecc-vterm-send-read-command (filepath)
  "Send a Read command with FILEPATH to the vterm buffer."
  (let ((command (format "Read %s" filepath)))
    (if (fboundp 'vterm-send-string)
        (progn
          (vterm-send-string command)
          (vterm-send-return))
      (message "vterm not available, command would be: %s" command))))

;; 3. Keybinding setup
;; ----------------------------------------
(defun ecc-vterm-yank-as-file-setup-keybinding ()
  "Set up keybinding for ecc-vterm-yank-as-file in vterm-mode."
  (when (fboundp 'vterm-mode-map)
    (define-key vterm-mode-map (kbd "C-c C-y") 'ecc-vterm-yank-as-file)))

;; Set up keybinding when vterm is loaded
(with-eval-after-load 'vterm
  (ecc-vterm-yank-as-file-setup-keybinding))

(provide 'ecc-vterm-yank-as-file)

(when (not load-file-name)
  (message "ecc-vterm-yank-as-file.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))