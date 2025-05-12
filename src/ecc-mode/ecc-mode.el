;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 03:40:26>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/ecc-mode.el

;;; Commentary:
;;; Mode definition for Emacs Claude Code
;;; This file defines a minor mode for controlling Claude interactions from Emacs.

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ecc-state)
(require 'ecc-auto)
(require 'ecc-send)

;; Define keymaps
(defvar ecc-minor-mode-map (make-sparse-keymap)
  "Keymap for Claude Code minor mode.")

(defvar ecc-response-mode-map (make-sparse-keymap)
  "Keymap for Claude response mode.")

;; Notification support functions
(defun ecc-auto-notification-on ()
  "Send notification that auto mode has been enabled."
  (condition-case nil
      (when (file-exists-p "~/.bin/utils/notify.sh")
        (call-process "~/.bin/utils/notify.sh" nil 0 nil 
                     "Claude Auto Mode" "Auto-accept mode enabled"))
    (error nil)))

(defun ecc-auto-notification-off ()
  "Send notification that auto mode has been disabled."
  (condition-case nil
      (when (file-exists-p "~/.bin/utils/notify.sh")
        (call-process "~/.bin/utils/notify.sh" nil 0 nil 
                     "Claude Auto Mode" "Auto-accept mode disabled"))
    (error nil)))

(defun ecc-auto-notify-completion (prompt-type)
  "Send notification that Claude has responded to a prompt.
PROMPT-TYPE is a string describing the type of prompt that was handled."
  (condition-case nil
      (when (and (file-exists-p "~/.bin/utils/notify.sh") 
                (boundp 'ecc-auto-mode)
                ecc-auto-mode)
        (call-process "~/.bin/utils/notify.sh" nil 0 nil 
                     "Claude Auto Response" 
                     (format "Auto-responded to %s prompt" prompt-type)))
    (error nil)))

;;;###autoload
(define-minor-mode ecc-mode
  "Minor mode for Claude Code interactions.
When enabled, provides key bindings and functions for
communicating with Claude AI from Emacs."
  :init-value nil
  :lighter " Claude"
  :keymap ecc-minor-mode-map
  :group 'ecc
  (if ecc-mode
      (progn
        (message "Claude Code mode enabled.")
        (unless ecc-auto-mode
          (ecc-auto-mode 1)))
    (progn
      (message "Claude Code mode disabled.")
      (when ecc-auto-mode
        (ecc-auto-mode -1)))))

;;;###autoload
(define-minor-mode ecc-auto-mode
  "Minor mode to automatically accept Claude responses.
When enabled, responses from Claude will be automatically accepted
without requiring manual confirmation. This streamlines the workflow
when working with Claude prompts that require interaction."
  :init-value nil
  :lighter " Auto"
  :group 'ecc
  (if ecc-auto-mode
      (progn
        (ecc-auto-enable)
        (ecc-auto-notification-on)
        (setq ecc-auto-accept t)
        (message "Auto-accept mode enabled for Claude responses."))
    (progn
      (ecc-auto-disable)
      (ecc-auto-notification-off)
      (setq ecc-auto-accept nil)
      (message "Auto-accept mode disabled for Claude responses."))))

;;;###autoload
(define-minor-mode ecc-response-mode
  "Minor mode for Claude response buffers.
Provides convenient key bindings for working with Claude's responses
and sending follow-up messages."
  :init-value nil
  :lighter " Response"
  :keymap ecc-response-mode-map
  :group 'ecc
  (if ecc-response-mode
      (message "Claude response mode enabled.")
    (message "Claude response mode disabled.")))

(provide 'ecc-mode)

(when
    (not load-file-name)
  (message "ecc-mode.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))