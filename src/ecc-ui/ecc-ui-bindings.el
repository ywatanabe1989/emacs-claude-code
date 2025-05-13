;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 18:10:13>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-ui/ecc-ui-bindings.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ecc-buffer-navigation)
(require 'ecc-auto)
(require 'ecc-run)
(require 'ecc-send)
(require 'ecc-repository)
(require 'ecc-template)

;; 1. Configuration and customization
;; ----------------------------------------

(defgroup ecc-keymaps nil
  "Key bindings for emacs-claude-code."
  :group 'emacs-claude)

(defcustom ecc-keymap-prefix "C-c c"
  "Prefix for ecc-mode key bindings."
  :type 'string
  :group 'ecc-keymaps)

;; 2. Core keymap definition
;; ----------------------------------------

(defvar ecc-keymap
  (let ((map (make-sparse-keymap)))
    ;; Buffer navigation
    (define-key map (kbd "c") 'ecc-buffer-create-registered-buffer)
    (define-key map (kbd "n") 'ecc-buffer-next)
    (define-key map (kbd "p") 'ecc-buffer-prev)
    (define-key map (kbd "l") 'ecc-buffer-list-buffers)
    ;; Auto-accept functions
    (define-key map (kbd "a") 'ecc-auto-toggle)
    (define-key map (kbd "y") 'ecc-send-accept)
    ;; Run functions
    (define-key map (kbd "r") 'ecc-run-on-region)
    (define-key map (kbd "b") 'ecc-run-on-buffer)
    (define-key map (kbd "q") 'ecc-run-quick)
    (define-key map (kbd "L r") 'ecc-buffer-optim-send-region)
    (define-key map (kbd "L b") 'ecc-buffer-optim-send-buffer)
    (define-key map (kbd "L f") 'ecc-buffer-optim-send-file)
    ;; Send functions
    (define-key map (kbd "i") 'ecc-state-initialize)
    (define-key map (kbd "ESC") 'ecc-send-interrupt)
    (define-key map (kbd "t") 'ecc-send-template)
    ;; History/Memory management functions
    (define-key map (kbd "m") 'ecc-send-edit-memory)
    (define-key map (kbd "h c") 'ecc-send-compact-history)
    (define-key map (kbd "h l") 'ecc-send-clear-history)
    (define-key map (kbd "h b") 'ecc-history-browse)
    ;; Repository functions
    (define-key map (kbd "R c") 'ecc-repository-copy-contents)
    (define-key map (kbd "R v") 'ecc-repo-view-and-send)
    (define-key map (kbd "R s") 'ecc-repo-view-section-and-send)
    (define-key map (kbd "R g") 'ecc-repo-view-generate)
    ;; Template functions
    (define-key map (kbd "T c") 'ecc-template-create)
    (define-key map (kbd "T e") 'ecc-template-edit)
    map)
  "Keymap for ECC commands.")

;; 3. Helper functions
;; ----------------------------------------

(defun ecc-setup-keymap ()
  "Set up the keymap for ECC commands."
  (let ((prefix-key (kbd ecc-keymap-prefix)))
    ;; Global map
    (global-set-key prefix-key ecc-keymap)
    ;; Apply same keymap to dired mode
    (with-eval-after-load 'dired
      (define-key dired-mode-map prefix-key ecc-keymap))))

(defun ecc-reload-keymap ()
  "Reload ECC keymap with updated bindings."
  (interactive)
  (setq ecc-keymap
        (let ((map (make-sparse-keymap)))
          ;; Buffer navigation
          (define-key map (kbd "c")
                      'ecc-buffer-create-registered-buffer)
          (define-key map (kbd "n") 'ecc-buffer-next)
          (define-key map (kbd "p") 'ecc-buffer-prev)
          (define-key map (kbd "l") 'ecc-buffer-list-buffers)
          ;; Auto-accept functions
          (define-key map (kbd "a") 'ecc-auto-toggle)
          (define-key map (kbd "y") 'ecc-send-accept)
          ;; Run functions
          (define-key map (kbd "r") 'ecc-run-on-region)
          (define-key map (kbd "b") 'ecc-run-on-buffer)
          (define-key map (kbd "q") 'ecc-run-quick)
          (define-key map (kbd "L r") 'ecc-buffer-optim-send-region)
          (define-key map (kbd "L b") 'ecc-buffer-optim-send-buffer)
          (define-key map (kbd "L f") 'ecc-buffer-optim-send-file)
          ;; Send functions
          (define-key map (kbd "i") 'ecc-state-initialize)
          (define-key map (kbd "ESC") 'ecc-send-interrupt)
          (define-key map (kbd "t") 'ecc-send-template)
          ;; History/Memory management functions
          (define-key map (kbd "m") 'ecc-send-edit-memory)
          (define-key map (kbd "h c") 'ecc-send-compact-history)
          (define-key map (kbd "h l") 'ecc-send-clear-history)
          (define-key map (kbd "h b") 'ecc-history-browse)
          ;; Repository functions
          (define-key map (kbd "R c") 'ecc-repository-copy-contents)
          (define-key map (kbd "R v") 'ecc-repo-view-and-send)
          (define-key map (kbd "R s") 'ecc-repo-view-section-and-send)
          (define-key map (kbd "R g") 'ecc-repo-view-generate)
          ;; Template functions
          (define-key map (kbd "T c") 'ecc-template-create)
          (define-key map (kbd "T e") 'ecc-template-edit)
          map))
  (ecc-setup-keymap)
  (message "ECC keymap reloaded"))

(ecc-reload-keymap)


(provide 'ecc-ui-bindings)

(when
    (not load-file-name)
  (message "ecc-ui-bindings.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))