;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 17:59:30>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-state/ecc-state-variables.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(defgroup emacs-claude nil
  "Customization group for emacs-claude.auto-accept package."
  :group 'external)

(defcustom ecc-buffer-name "*CLAUDE-CODE*"
  "Buffer name where Claude prompts appear."
  :type 'string
  :group 'emacs-claude)

(defvar ecc-auto-accept-timer nil
  "Timer object for polling Claude prompts.")

(defcustom ecc-prompt-y/n "❯ 1. Yes"
  "Y/n prompt pattern to match when Claude asks for confirmation."
  :type 'string
  :group 'emacs-claude)

(defcustom ecc-prompt-y/y/n " 2. Yes, and"
  "y/Y/n prompt pattern to match when Claude asks for confirmation."
  :type 'string
  :group 'emacs-claude)

(defcustom ecc-prompt-waiting
  "
╭──────────────────────────────────────────────────────────────────────────────╮
│ >                                                                            │
╰──────────────────────────────────────────────────────────────────────────────╯"
  "Pattern that matches the waiting prompt shown in Claude interface."
  :type 'string
  :group 'emacs-claude)

;; (defcustom ecc-prompt-waiting
;;   "│ > [[:space:]]+|"
;;   "Pattern that matches the waiting prompt shown in Claude interface."
;;   :type 'string
;;   :group 'emacs-claude)

(defcustom ecc-prompt-initial-waiting
  "│ > Try "
  "Pattern that matches the waiting prompt shown in Claude interface."
  :type 'string
  :group 'emacs-claude)

(defcustom ecc-auto-accept-interval-sec 1
  "Interval in seconds between checks for Claude prompts."
  :type 'string
  :group 'emacs-claude)


(provide 'ecc-state-variables)

(when
    (not load-file-name)
  (message "ecc-state-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))