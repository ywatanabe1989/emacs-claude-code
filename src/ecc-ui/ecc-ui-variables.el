;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 18:10:15>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-ui/ecc-variables.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(defgroup emacs-claude nil
  "Customization group for Emacs Claude Code."
  :group 'tools
  :prefix "ecc-")

;; Buffer Variables

(defcustom ecc-buffer-name "*CLAUDE-CODE*"
  "Name for the Claude interaction buffer."
  :type 'string
  :group 'emacs-claude)

(defvar ecc-buffer nil
  "Buffer for Claude interaction.")

(defvar ecc-active-buffer nil
  "Current active buffer for Claude interaction.")

(defvar ecc-buffers nil
  "List of registered Claude buffers.")

;; Timer Variables

(defvar ecc-timer nil
  "Timer for auto-detection of Claude state.")

(defcustom ecc-interval-sec 1.5
  "Time interval in seconds for checking Claude state."
  :type 'number
  :group 'emacs-claude)

(defvar ecc-auto-timer nil
  "Timer for auto-handling of Claude prompts.")

(defcustom ecc-auto-interval-sec 1.5
  "Time interval in seconds for auto-handling Claude prompts."
  :type 'number
  :group 'emacs-claude)

;; Prompt Detection Patterns

(defcustom ecc-prompt-waiting "/tests"
  "Text pattern for detecting waiting prompt."
  :type 'string
  :group 'emacs-claude)

(defcustom ecc-prompt-y/n "(y/n)"
  "Text pattern for detecting yes/no prompt."
  :type 'string
  :group 'emacs-claude)

(defcustom ecc-prompt-y/y/n "(Y/y/n)"
  "Text pattern for detecting Y/y/n prompt."
  :type 'string
  :group 'emacs-claude)

(defcustom ecc-prompt-initial-waiting
  "/auto"
  "Text pattern for detecting initial waiting prompt."
  :type 'string
  :group 'emacs-claude)

(defcustom ecc-prompt-to-send-on-waiting "/auto"
  "Text to send when Claude is in waiting state."
  :type 'string
  :group 'emacs-claude)

;; Regex patterns for prompt detection

(defcustom ecc-prompt-pattern-y/n "(y/n)"
  "Regex pattern for detecting yes/no prompt."
  :type 'string
  :group 'emacs-claude)

(defcustom ecc-prompt-pattern-y/y/n "(Y/y/n)"
  "Regex pattern for detecting Y/y/n prompt."
  :type 'string
  :group 'emacs-claude)

(defcustom ecc-prompt-pattern-waiting "Continue generati"
  "Regex pattern for detecting waiting prompt."
  :type 'string
  :group 'emacs-claude)

(defcustom ecc-prompt-pattern-initial-waiting
  "Would you like Claude to continue?"
  "Regex pattern for detecting initial waiting prompt."
  :type 'string
  :group 'emacs-claude)


(provide 'ecc-variables)

(when
    (not load-file-name)
  (message "ecc-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))