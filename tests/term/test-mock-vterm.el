;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-07 14:45:55>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-mock-vterm.el

;; This file provides mock implementations of vterm functions for testing

;; Define vterm-mode
(unless (boundp 'vterm-update-functions)
  (defvar vterm-update-functions nil))

;; Be sure vterm-update-functions is a list
(setq vterm-update-functions (if (listp vterm-update-functions) 
                               vterm-update-functions 
                               nil))

;; Define ecc buffer variables for testing
(defvar ecc-buffers nil
  "Mock list of Claude buffers.")

(defvar ecc-active-buffer nil
  "Mock currently active Claude buffer.")

(defvar ecc-buffer-current-active-buffer nil
  "Mock currently active Claude buffer.")

(defvar ecc-buffer-timestamps (make-hash-table :test 'equal)
  "Mock timestamps for Claude buffers.")

;; Mock buffer function
(unless (fboundp 'ecc-buffer-get-or-create-active-buffer)
  (defun ecc-buffer-get-or-create-active-buffer ()
    "Mock implementation to get or create a Claude buffer."
    (or ecc-active-buffer
        (setq ecc-active-buffer (generate-new-buffer "*MOCK-CLAUDE*")))))

(unless (fboundp 'vterm-mode)
  (defun vterm-mode ()
    "Mock implementation of vterm-mode for testing."
    (setq major-mode 'vterm-mode)))

;; Mock vterm functions used in tests
(unless (fboundp 'vterm-send-string)
  (defun vterm-send-string (string)
    "Mock implementation of vterm-send-string for testing."
    (message "Sending string: %s" string)))

(unless (fboundp 'vterm-send-return)
  (defun vterm-send-return ()
    "Mock implementation of vterm-send-return for testing."
    (message "Sending return")))

(unless (fboundp 'vterm-copy-mode)
  (defun vterm-copy-mode (&optional arg)
    "Mock implementation of vterm-copy-mode for testing."
    (message "Setting copy mode to %s" arg)))

(unless (fboundp 'vterm-clear)
  (defun vterm-clear ()
    "Mock implementation of vterm-clear for testing."
    (message "Clearing vterm buffer")))

(unless (fboundp 'vterm-send-key)
  (defun vterm-send-key (key &optional mod1 mod2)
    "Mock implementation of vterm-send-key for testing."
    (message "Sending key: %s with modifiers %s %s" key mod1 mod2)))

(provide 'test-mock-vterm)