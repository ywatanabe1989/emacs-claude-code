;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 18:55:30>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-term/ecc-term-variables.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(defgroup emacs-claude-term nil
  "Terminal and vterm integration for Claude."
  :group 'emacs-claude
  :prefix "ecc-term-")

;; VTerm Detection
;; ------------------------------

(defvar ecc-term--vterm-available
  (condition-case nil
      (progn (require 'vterm) t)
    (error nil))
  "Whether the vterm package is available.")

;; State Detection
;; ------------------------------

(defvar ecc-term--state-timer nil
  "Timer for updating the Claude state in terminal mode.")

(defcustom ecc-term-state-update-interval 1.0
  "Interval in seconds for updating Claude state in terminal mode."
  :type 'number
  :group 'emacs-claude-term)

;; VTerm Configuration
;; ------------------------------

(defcustom ecc-term-vterm-auto-detect-state t
  "Whether to automatically detect Claude state in vterm buffers."
  :type 'boolean
  :group 'emacs-claude-term)

(defcustom ecc-term-vterm-poll-interval 0.5
  "Interval in seconds to poll vterm buffer for state changes."
  :type 'number
  :group 'emacs-claude-term)

(defvar ecc-term--vterm-timers (make-hash-table :test 'eq)
  "Hash table of state detection timers for vterm buffers.")

(defvar ecc-term--vterm-command-history nil
  "History of commands sent to vterm.")

(defvar ecc-term--vterm-output-ring (make-ring 5)
  "Ring buffer of recent outputs from vterm.")

;; VTerm Display Configuration
;; ------------------------------

(defcustom ecc-term-vterm-line-numbers nil
  "Whether to display line numbers in Claude vterm buffers.
Disabling line numbers can improve performance for large outputs."
  :type 'boolean
  :group 'emacs-claude-term)

(defcustom ecc-term-vterm-scroll-conservatively 10000
  "Value for scroll-conservatively in Claude vterm buffers.
Higher values prevent recentering during fast output."
  :type 'integer
  :group 'emacs-claude-term)

(defcustom ecc-term-vterm-truncate-lines t
  "Whether to truncate lines in Claude vterm buffers.
Enabling truncation can improve performance for long lines."
  :type 'boolean
  :group 'emacs-claude-term)

(defcustom ecc-term-vterm-always-follow-bottom t
  "Whether to always follow the bottom of the buffer.
When non-nil, the view will automatically scroll to show new output."
  :type 'boolean
  :group 'emacs-claude-term)

(defcustom ecc-term-vterm-follow-bottom-margin 2
  "Number of lines to keep visible below the point when following bottom.
Higher values give more context when scrolling."
  :type 'integer
  :group 'emacs-claude-term)

;; Auto Mode Configuration
;; ------------------------------

(defcustom ecc-term-vterm-auto-mode nil
  "When non-nil, automatically respond to Claude prompts in vterm."
  :type 'boolean
  :group 'emacs-claude-term)

(defvar ecc-term--vterm-update-functions nil
  "Functions to run after vterm output is updated.")

;; Prompt Detection Patterns for VTerm
;; ------------------------------

(defcustom ecc-term-vterm-prompt-waiting "Continue generati"
  "Text pattern for detecting waiting prompt in vterm mode."
  :type 'string
  :group 'emacs-claude-term)

(defcustom ecc-term-vterm-prompt-y/n "y/n"
  "Text pattern for detecting yes/no prompt in vterm mode."
  :type 'string
  :group 'emacs-claude-term)

(defcustom ecc-term-vterm-prompt-y/y/n "Y/y/n"
  "Text pattern for detecting Y/y/n prompt in vterm mode."
  :type 'string
  :group 'emacs-claude-term)

(defcustom ecc-term-vterm-prompt-initial-waiting
  "Would you like Claude to continue?"
  "Text pattern for detecting initial waiting prompt in vterm mode."
  :type 'string
  :group 'emacs-claude-term)

(provide 'ecc-term-variables)

(when
    (not load-file-name)
  (message "ecc-term-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
