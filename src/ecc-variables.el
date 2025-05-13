;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 19:10:30>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-variables.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;; Main customization group for all Claude modules
(defgroup emacs-claude nil
  "Customization group for Emacs Claude Code."
  :group 'tools
  :prefix "ecc-")

;; Ensure the subgroups are defined after the main group
(require 'ecc-buffer-variables)
(require 'ecc-state-variables)
(require 'ecc-term-variables)
(require 'ecc-auto-variables)

;; Global application variables that don't belong to a specific module
;; ------------------------------

;; Version Information
(defconst ecc-version "1.0.0"
  "Version of emacs-claude-code.")

;; Debug Settings
(defcustom ecc-debug-mode nil
  "When non-nil, enable debug logging and features."
  :type 'boolean
  :group 'emacs-claude)

(defcustom ecc-debug-log-file nil
  "When non-nil, specify file to write debug logs to.
If nil, logs are written to the *Messages* buffer."
  :type '(choice (const :tag "No debug log file" nil)
                (file :tag "Debug log file"))
  :group 'emacs-claude)

;; Compatibility Settings
(defcustom ecc-use-legacy-mode nil
  "When non-nil, use legacy compatibility mode."
  :type 'boolean
  :group 'emacs-claude)

;; Default Claude command for initialization
(defcustom ecc-default-command "claude-code"
  "Default command to run Claude."
  :type 'string
  :group 'emacs-claude)

;; Global key bindings prefix
(defcustom ecc-keymap-prefix "C-c C-c"
  "Prefix for ecc-mode key bindings."
  :type 'string
  :group 'emacs-claude)

;; Integration with other packages
(defcustom ecc-integrate-with-project nil
  "When non-nil, integrate with project.el."
  :type 'boolean
  :group 'emacs-claude)

(defcustom ecc-integrate-with-vc nil
  "When non-nil, integrate with version control."
  :type 'boolean
  :group 'emacs-claude)


(provide 'ecc-variables)

(when
    (not load-file-name)
  (message "ecc-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))