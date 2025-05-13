;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 09:30:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-auto/ecc-auto-mode.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;; This file provides backward compatibility for ecc-auto-mode functions
;; that were removed or renamed in the refactoring.

(require 'ecc-auto-monitor)

(defvar ecc-auto-mode nil
  "When non-nil, automatically respond to Claude prompts.")

(defvar ecc-auto-accept nil
  "When non-nil, automatic acceptance of prompts is enabled.")

;;;###autoload
(defun ecc-auto-mode (arg)
  "Toggle automatic response to Claude prompts.
When called with a positive ARG, enable the mode.
When called with a zero or negative ARG, disable the mode."
  (interactive "p")
  (setq ecc-auto-mode (> arg 0))
  
  ;; Update state based on mode status
  (if ecc-auto-mode
      (progn
        (ecc-auto-enable)
        (when (fboundp 'ecc-auto-notification-on)
          (ecc-auto-notification-on))
        (setq ecc-auto-accept t)
        (message "Claude auto-accept enabled"))
    (progn
      (ecc-auto-disable)
      (when (fboundp 'ecc-auto-notification-off)
        (ecc-auto-notification-off))
      (setq ecc-auto-accept nil)
      (message "Claude auto-accept disabled"))))

;;;###autoload
(defun ecc-auto-mode-enable ()
  "Enable automatic response to Claude prompts."
  (interactive)
  (ecc-auto-mode 1))

;;;###autoload
(defun ecc-auto-mode-disable ()
  "Disable automatic response to Claude prompts."
  (interactive)
  (ecc-auto-mode -1))

;; Define the notification functions if not already defined
(unless (fboundp 'ecc-auto-notification-on)
  (defun ecc-auto-notification-on ()
    "Send notification that auto mode has been enabled."
    (message "Auto-accept mode enabled")
    (when (fboundp 'ecc-ui-notification)
      (ecc-ui-notification "Claude auto-accept enabled"))))

(unless (fboundp 'ecc-auto-notification-off)
  (defun ecc-auto-notification-off ()
    "Send notification that auto mode has been disabled."
    (message "Auto-accept mode disabled")
    (when (fboundp 'ecc-ui-notification)
      (ecc-ui-notification "Claude auto-accept disabled"))))

(unless (fboundp 'ecc-auto-notify-completion)
  (defun ecc-auto-notify-completion (type)
    "Send notification about auto-response completion of TYPE."
    (when (and (fboundp 'ecc-ui-notification)
               (boundp 'ecc-auto-notify)
               ecc-auto-notify)
      (ecc-ui-notification (format "Claude auto-response: %s" type)))))

;; Define helper functions
(defun ecc-auto-enable ()
  "Enable automatic response to Claude prompts."
  (when (fboundp 'ecc-term-send-accept)
    (add-hook 'vterm-update-functions 'ecc-term-send-accept))
  (setq ecc-auto-accept t))

(defun ecc-auto-disable ()
  "Disable automatic response to Claude prompts."
  (when (fboundp 'ecc-term-send-accept)
    (remove-hook 'vterm-update-functions 'ecc-term-send-accept))
  (setq ecc-auto-accept nil))

(provide 'ecc-auto-mode)

(when (not load-file-name)
  (message "ecc-auto-mode.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))