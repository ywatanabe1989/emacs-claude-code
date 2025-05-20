;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 17:56:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-variables-refactored.el

;;; Commentary:
;;; Global variables for emacs-claude-code.
;;; This file provides a centralized and organized location for all
;;; configuration variables and customization options.

;;; Code:

;; Customization groups

(defgroup emacs-claude-code nil
  "Settings for the Emacs Claude Code package."
  :prefix "ecc-"
  :group 'applications)

(defgroup ecc-buffers nil
  "Settings for Claude buffer management and naming."
  :group 'emacs-claude-code
  :prefix "ecc-buffer-")

(defgroup ecc-auto-response nil
  "Settings for Claude auto-response system."
  :group 'emacs-claude-code
  :prefix "ecc-auto-response-")

(defgroup ecc-state-detection nil
  "Settings for Claude prompt detection."
  :group 'emacs-claude-code
  :prefix "ecc-state-detection-")

(defgroup ecc-vterm nil
  "Settings for Claude vterm integration."
  :group 'emacs-claude-code
  :prefix "ecc-vterm-")

(defgroup ecc-notification nil
  "Settings for Claude notification system."
  :group 'emacs-claude-code
  :prefix "ecc-notification-")

;; Buffer management variables

(defcustom ecc-buffer-prefix "*CLAUDE-VTERM-"
  "Prefix for Claude vterm buffer names."
  :type 'string
  :group 'ecc-buffers)

(defcustom ecc-buffer-suffix "*"
  "Suffix for Claude vterm buffer names."
  :type 'string 
  :group 'ecc-buffers)

(defvar ecc-buffer-counter 1
  "Counter for creating new numbered Claude vterm buffers.")

(defvar ecc-buffer-registered-buffers-alist nil
  "Alist of registered Claude buffers and their properties.")

(defvar ecc-buffer-current-buffer nil
  "Current active Claude buffer.")

(defvar ecc-buffer-auto-response-enabled nil
  "Whether auto-response is enabled.")

(defvar ecc-claude-buffers nil
  "List of all Claude vterm buffers (both dedicated and converted).")

;; Auto-response variables

(defcustom ecc-auto-response-throttle-time 5.0
  "Minimum seconds between auto-responses to the same state.
Prevents rapid consecutive auto-responses to waiting prompts."
  :type 'float
  :group 'ecc-auto-response)

(defcustom ecc-auto-response-timer-interval 0.5
  "Interval in seconds for auto-response timer checks."
  :type 'float
  :group 'ecc-auto-response)

(defcustom ecc-auto-response-check-on-output t
  "Whether to check for prompts whenever new output appears."
  :type 'boolean
  :group 'ecc-auto-response)

(defcustom ecc-auto-response-initial-waiting
  "/user:understand-guidelines"
  "Response to send for initial waiting state."
  :type 'string
  :group 'ecc-auto-response)

(defcustom ecc-auto-response-y/n "1"
  "Response to send for Y/N prompts."
  :type 'string
  :group 'ecc-auto-response)

(defcustom ecc-auto-response-y/y/n "2"
  "Response to send for Y/Y/N prompts."
  :type 'string
  :group 'ecc-auto-response)

(defcustom ecc-auto-response-waiting "/auto"
  "Response to send for waiting state."
  :type 'string
  :group 'ecc-auto-response)

(defvar ecc-auto-response-timer nil
  "Timer for checking and responding to Claude prompts.")

(defvar ecc-auto-response-last-time-alist
  '((:y/n . 0.0)
    (:y/y/n . 0.0)
    (:waiting . 0.0)
    (:initial-waiting . 0.0))
  "Alist tracking last time each type of response was sent.")

(defvar ecc-auto-response-active-state nil
  "The currently active Claude prompt state being processed.
Used to prevent duplicate responses to the same prompt.")

(defvar ecc-auto-response-hooks nil
  "Hooks to run after an auto-response is sent.")

;; State detection variables

(defcustom ecc-state-detection-buffer-size 2000
  "Number of characters to check from the end of buffer for basic prompt detection."
  :type 'integer
  :group 'ecc-state-detection)

(defcustom ecc-state-detection-line-count 256
  "Number of lines to check from the end of buffer for line-based prompt detection.
A larger number increases detection accuracy but may impact performance
with very large buffers. The default value of 256 is a balance between
thorough detection and performance."
  :type 'integer
  :group 'ecc-state-detection)

(defcustom ecc-state-prompt-initial-waiting
  "│ > Try "
  "Pattern that matches the waiting prompt shown in Claude interface.
` ` is the correct expression so do not change to space."
  :type 'string
  :group 'ecc-state-detection)

(defcustom ecc-state-prompt-waiting
  "│ >                            "
  "Pattern that matches the waiting prompt shown in Claude interface.
` ` is the correct expression so do not change to space."
  :type 'string
  :group 'ecc-state-detection)

(defcustom ecc-state-prompt-y/n "❯ 1. Yes"
  "Y/n prompt pattern to match when Claude asks for confirmation."
  :type 'string
  :group 'ecc-state-detection)

(defcustom ecc-state-prompt-y/y/n " 2. Yes, and"
  "y/Y/n prompt pattern to match when Claude asks for confirmation."
  :type 'string
  :group 'ecc-state-detection)

(defcustom ecc-state-prompt-initial-waiting-alternatives
  '("Claude is ready" "Ready for your request" "How can I help")
  "Alternative patterns that might indicate Claude's initial waiting state.
These are used as fallbacks if the primary pattern doesn't match."
  :type '(repeat string)
  :group 'ecc-state-detection)

;; VTerm variables

(defcustom ecc-vterm-always-follow-bottom t
  "Whether to always follow bottom in vterm buffers."
  :type 'boolean
  :group 'ecc-vterm)

(defcustom ecc-vterm-follow-bottom-margin 5
  "Number of lines to keep as margin when following bottom."
  :type 'integer
  :group 'ecc-vterm)

;; Notification variables

(defcustom ecc-auto-notify-on-claude-prompt t
  "Whether to notify when claude asks for user response."
  :type 'boolean
  :group 'ecc-notification)

(defcustom ecc-auto-notify-completions t
  "Whether to notify when auto-response completes."
  :type 'boolean
  :group 'ecc-notification)

;; Interaction tracking variables

(defvar ecc-interaction-counter 0
  "Counter for tracking the number of interactions with Claude.")

(defvar ecc-interaction-timestamps nil
  "List of timestamps for Claude interactions.")

;; Debugging variables

(defcustom ecc-debug-enabled nil
  "Whether to enable debugging output.
When non-nil, debug messages will be printed to the *Messages* buffer.
Set this to t during development and nil in production."
  :type 'boolean
  :group 'ecc-vterm)

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

(provide 'ecc-variables-refactored)

;;; ecc-variables-refactored.el ends here