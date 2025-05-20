;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 11:02:41>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-variables.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; Commentary:
;;; Global variables for emacs-claude-code.

;; Core variables

(defvar ecc-buffer-registered-buffers-alist nil
  "Alist of registered Claude buffers and their properties.")

(defvar ecc-buffer-current-buffer nil
  "Current active Claude buffer.")

(defvar ecc-buffer-auto-response-enabled nil
  "Whether auto-response is enabled.")

;; Auto-response variables

(defvar ecc-auto-response-initial-waiting
  "/user:understand-guidelines"
  "Response to send for initial waiting state.")

(defvar ecc-auto-response-y/n "1"
  "Response to send for Y/N prompts.")

(defvar ecc-auto-response-y/y/n "2"
  "Response to send for Y/Y/N prompts.")

(defvar ecc-auto-response-waiting "/auto"
  "Response to send for waiting state.")

(defvar ecc-auto-response-timer nil
  "Timer for checking and responding to Claude prompts.")

(defvar ecc-auto-notify-on-claude-prompt t
  "Whether to notify when claude ask user response.")

(defvar ecc-auto-notify-completions t
  "Whether to notify when auto-response completes.")

;; State detection variables

(defcustom ecc-state-prompt-initial-waiting
  "│ > Try "
  "Pattern that matches the waiting prompt shown in Claude interface.
` ` is the correct expression so do not change to space."
  :type 'string
  :group 'emacs-claude-state)

(defcustom ecc-state-prompt-waiting
  "│ >                            "
  "Pattern that matches the waiting prompt shown in Claude interface.
` ` is the correct expression so do not change to space."
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

;; VTerm variables

(defvar ecc-vterm-always-follow-bottom t
  "Whether to always follow bottom in vterm buffers.")

(defvar ecc-vterm-follow-bottom-margin 5
  "Number of lines to keep as margin when following bottom.")

;; Claude buffer naming variables

(defvar ecc-buffer-counter 1
  "Counter for creating new numbered Claude vterm buffers.")

(defvar ecc-buffer-prefix "*CLAUDE-VTERM-"
  "Prefix for Claude vterm buffer names.")

(defvar ecc-buffer-suffix "*"
  "Suffix for Claude vterm buffer names.")

(defvar ecc-claude-buffers nil
  "List of all Claude vterm buffers (both dedicated and converted).")

;; Interaction tracking variables

(defvar ecc-interaction-counter 0
  "Counter for tracking the number of interactions with Claude.")

(defvar ecc-interaction-timestamps nil
  "List of timestamps for Claude interactions.")

;; Additional initial waiting patterns for fallback detection
(defvar ecc-state-prompt-initial-waiting-alternatives
  '("Claude is ready" "Ready for your request" "How can I help")
  "Alternative patterns that might indicate Claude's initial waiting state.
These are used as fallbacks if the primary pattern doesn't match.")

;; Debugging variables

(defvar ecc-debug-enabled nil
  "Whether to enable debugging output.
When non-nil, debug messages will be printed to the *Messages* buffer.
Set this to t during development and nil in production.

To toggle debugging interactively, use the command `ecc-toggle-debug'.")

(defmacro ecc-debug-message (format-string &rest args)
  "Output a debug message if debugging is enabled.
Only prints the message when `ecc-debug-enabled' is non-nil.
Accepts the same arguments as `message': FORMAT-STRING and ARGS."
  `(when (and (boundp 'ecc-debug-enabled) ecc-debug-enabled)
     (message ,format-string ,@args)))

;;;###autoload
(defun ecc-toggle-debug ()
  "Toggle debug message output.
When enabled, debug messages will be shown in the *Messages* buffer.
This is useful for troubleshooting auto-response and other functionality."
  (interactive)
  (setq ecc-debug-enabled (not ecc-debug-enabled))
  (message "Claude debug messages %s" 
           (if ecc-debug-enabled "enabled" "disabled")))

;;; ecc-variables.el ends here


(provide 'ecc-variables)

(when
    (not load-file-name)
  (message "ecc-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
