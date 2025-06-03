;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-06-04 08:47:08>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-vterm-yank-as-file.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'vterm nil t)

;; 1. Main entry point
;; ----------------------------------------

(defcustom ecc-vterm-yank-default-directory "~/"
  "Default directory for creating yank files.
Useful for SSH sessions where current directory might be remote."
  :type 'string
  :group 'ecc)

(defun ecc-vterm-yank-as-file (&optional use-default-dir)
  "Create a temporary file with the latest kill-ring content and send path to vterm.

With prefix arg USE-DEFAULT-DIR, creates file in `ecc-vterm-yank-default-directory'.
This is useful for SSH sessions where files should be created locally.

Example:
  ;; Copy some text to kill-ring
  ;; Run M-x ecc-vterm-yank-as-file
  ;; Or C-u M-x ecc-vterm-yank-as-file (to use default dir)
  ;; Sends \"Read /path/to/temp/file.tmp\" to vterm"
  (interactive "P")
  (if (not (derived-mode-p 'vterm-mode))
      (message "This command only works in vterm-mode")
    (let ((temp-file (--ecc-vterm-create-temp-file use-default-dir))
          (content (--ecc-vterm-get-kill-ring-content)))
      (if content
          (progn
            (--ecc-vterm-write-content-to-file content temp-file)
            (--ecc-vterm-send-read-command temp-file))
        (message "Kill ring is empty")))))

;; 2. Core functions
;; ----------------------------------------

(defun --ecc-vterm-create-temp-file (&optional use-default-dir)
  "Create a unique temporary file in the current or default directory.

If USE-DEFAULT-DIR is non-nil, creates file in `ecc-vterm-yank-default-directory'.
Returns the absolute path of the created file."
  (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (filename (format "kill-ring-%s.tmp" timestamp))
         (directory (if use-default-dir
                        (expand-file-name
                         ecc-vterm-yank-default-directory)
                      default-directory))
         (filepath (expand-file-name filename directory)))
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

(when
    (not load-file-name)
  (message "ecc-vterm-yank-as-file.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))