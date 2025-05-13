;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 07:43:01>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-state/-ecc-state-variables.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(defcustom emacs-claude-prompt-y/n "❯ 1. Yes"
  "Y/n prompt pattern to match when Claude asks for confirmation."
  :type 'string
  :group 'emacs-claude)

(defcustom emacs-claude-prompt-y/y/n " 2. Yes, and"
  "y/Y/n prompt pattern to match when Claude asks for confirmation."
  :type 'string
  :group 'emacs-claude)

(defcustom emacs-claude-prompt-waiting
  "
╭──────────────────────────────────────────────────────────────────────────────╮
│ >                                                                            │
╰──────────────────────────────────────────────────────────────────────────────╯"
  "Pattern that matches the waiting prompt shown in Claude interface."
  :type 'string
  :group 'emacs-claude)

;; (defcustom emacs-claude-prompt-waiting
;;   "│ > [[:space:]]+|"
;;   "Pattern that matches the waiting prompt shown in Claude interface."
;;   :type 'string
;;   :group 'emacs-claude)

;; (defcustom emacs-claude-prompt-waiting
;;   "│ > [[:space:]]+|"
;;   "Pattern that matches the waiting prompt shown in Claude interface."
;;   :type 'string
;;   :group 'emacs-claude)

(defcustom emacs-claude-prompt-initial-waiting
  "│ > Try "
  "Pattern that matches the waiting prompt shown in Claude interface."
  :type 'string
  :group 'emacs-claude)


(provide '-ecc-state-variables)

(when
    (not load-file-name)
  (message "-ecc-state-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))