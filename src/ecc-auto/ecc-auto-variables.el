;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 18:34:37>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-auto/ecc-auto-variables.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(defgroup emacs-claude-auto nil
  "Automatic response handling for Claude."
  :group 'emacs-claude
  :prefix "ecc-auto-")

;; Auto Mode Status
;; ------------------------------

(defvar ecc-auto--active nil
  "Flag to track if auto-accept is currently active.")

(defvar ecc-auto--accept nil
  "Flag indicating whether auto-accept is enabled.")

(defvar ecc-auto--mode nil
  "Flag indicating whether auto mode is enabled.")

;; Timer Variables
;; ------------------------------

(defvar ecc-auto--timer nil
  "Timer for auto-handling of Claude prompts.")

(defcustom ecc-auto-interval-sec 1.5
  "Time interval in seconds for auto-handling Claude prompts."
  :type 'number
  :group 'emacs-claude-auto)

;; Mode Line Indicators
;; ------------------------------

(defcustom ecc-auto-mode-line-indicator " [AUTO]"
  "Indicator for auto-accept mode in the mode line."
  :type 'string
  :group 'emacs-claude-auto)

;; Response Configuration
;; ------------------------------

(defcustom ecc-auto-response-y/n "1"
  "Response to send for Y/N prompts when auto mode is active."
  :type 'string
  :group 'emacs-claude-auto)

(defcustom ecc-auto-response-y/y/n "2"
  "Response to send for Y/Y/N prompts when auto mode is active."
  :type 'string
  :group 'emacs-claude-auto)

(defcustom ecc-auto-response-waiting "/auto"
  "Response to send for waiting prompts when auto mode is active."
  :type 'string
  :group 'emacs-claude-auto)

;; Notification Settings
;; ------------------------------

(defcustom ecc-auto-notify-completions t
  "Whether to display notifications on auto-completions."
  :type 'boolean
  :group 'emacs-claude-auto)

(defcustom ecc-auto-notification-timeout 2
  "Timeout in seconds for auto-completion notifications."
  :type 'number
  :group 'emacs-claude-auto)


(provide 'ecc-auto-variables)

(when
    (not load-file-name)
  (message "ecc-auto-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))