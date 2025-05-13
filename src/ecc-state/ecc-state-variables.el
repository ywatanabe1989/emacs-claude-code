;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 18:45:30>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-state/ecc-state-variables.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(defgroup emacs-claude-state nil
  "Customization group for Claude state detection and management."
  :group 'emacs-claude
  :prefix "ecc-state-")

;; State Detection Timer
;; ------------------------------

(defvar ecc-state--timer nil
  "Timer for auto-detection of Claude state.")

(defcustom ecc-state-interval-sec 1.5
  "Time interval in seconds for checking Claude state."
  :type 'number
  :group 'emacs-claude-state)

;; Auto-response Timer
;; ------------------------------

(defvar ecc-state--auto-timer nil
  "Timer for auto-handling of Claude prompts.")

(defcustom ecc-state-auto-interval-sec 1.5
  "Time interval in seconds for auto-handling Claude prompts."
  :type 'number
  :group 'emacs-claude-state)

;; State Categories
;; ------------------------------

(defvar ecc-state-available-states
  '(nil ready waiting y/n y/y/n active)
  "List of available buffer states for Claude buffers.
nil     - No specific state
ready   - Ready for input
waiting - Waiting for user to continue
y/n     - Yes/no prompt
y/y/n   - Yes/yes/no prompt
active  - Buffer is active (for backward compatibility)")

;; Prompt Detection Patterns
;; ------------------------------

(defcustom ecc-state-prompt-waiting
  "
╭──────────────────────────────────────────────────────────────────────────────╮
│ >                                                                            │
╰──────────────────────────────────────────────────────────────────────────────╯"
  "Pattern that matches the waiting prompt shown in Claude interface."
  :type 'string
  :group 'emacs-claude-state)

(defcustom ecc-state-prompt-y/n "❯ 1. Yes"
  "Y/n prompt pattern to match when Claude asks for confirmation."
  :type 'string
  :group 'emacs-claude-state)

(defcustom ecc-state-prompt-y/y/n " 2. Yes, and"
  "y/Y/n prompt pattern to match when Claude asks for confirmation."
  :type 'string
  :group 'emacs-claude-state)

(defcustom ecc-state-prompt-initial-waiting
  "│ > Try "
  "Pattern that matches the waiting prompt shown in Claude interface."
  :type 'string
  :group 'emacs-claude-state)

;; Regex Patterns
;; ------------------------------

(defcustom ecc-state-prompt-pattern-y/n "(y/n)"
  "Regex pattern for detecting yes/no prompt."
  :type 'string
  :group 'emacs-claude-state)

(defcustom ecc-state-prompt-pattern-y/y/n "(Y/y/n)"
  "Regex pattern for detecting Y/y/n prompt."
  :type 'string
  :group 'emacs-claude-state)

(defcustom ecc-state-prompt-pattern-waiting "Continue generati"
  "Regex pattern for detecting waiting prompt."
  :type 'string
  :group 'emacs-claude-state)

(defcustom ecc-state-prompt-pattern-initial-waiting
  "Would you like Claude to continue?"
  "Regex pattern for detecting initial waiting prompt."
  :type 'string
  :group 'emacs-claude-state)

;; Auto-response Text
;; ------------------------------

(defcustom ecc-state-prompt-to-send-on-waiting "continue"
  "Text to send when Claude is in waiting state."
  :type 'string
  :group 'emacs-claude-state)

;; Status Tracking
;; ------------------------------

(defvar ecc-state--current-state nil
  "Current state of the active Claude buffer.")

(defvar ecc-state--status-history nil
  "History of state changes for debugging and analysis.")


(provide 'ecc-state-variables)

(when
    (not load-file-name)
  (message "ecc-state-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))