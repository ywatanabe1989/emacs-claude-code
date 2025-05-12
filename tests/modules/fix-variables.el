;;; fix-variables.el --- Fix variable initialization for testing

;; Define the customization group first
(defgroup emacs-claude nil
  "Customization group for Emacs Claude Code."
  :group 'tools
  :prefix "ecc-")

;; Define all essential variables using defvar first to break circular dependencies

;; Buffer variables
(defvar ecc-buffer-name "*Claude*"
  "Name for the Claude interaction buffer.")

(defvar ecc-buffer nil
  "Buffer for Claude interaction.")

(defvar ecc-active-buffer nil
  "Current active buffer for Claude interaction.")

(defvar ecc-buffers nil
  "List of registered Claude buffers.")

;; Buffer current variables
(defvar ecc-buffer-current-buffer nil
  "Current buffer being used for Claude.")

(defvar ecc-buffer-current-active-buffer nil
  "Current active buffer for compatibility.")

;; Buffer registry variables
(defvar ecc-buffer-registered-buffers-alist nil
  "Registry of all buffers for compatibility.")

;; State detection flags and variables
(defvar ecc-state-running-p nil
  "Flag indicating if Claude is actively running/generating.")

;; Test helper variables
(defvar ecc-claude-vterm--vterm-available t
  "Flag indicating if vterm is available (always true for tests).")

;; Timer variables
(defvar ecc-timer nil
  "Timer for auto-detection of Claude state.")

(defvar ecc-auto-timer nil
  "Timer for auto-handling of Claude prompts.")

;; Interval variables - define first to prevent circular dependencies
(defvar ecc-interval-sec 1.5
  "Time interval in seconds for checking Claude state.")

(defvar ecc-auto-interval-sec 1.5
  "Time interval in seconds for auto-handling Claude prompts.")

;; Prompt detection patterns
(defvar ecc-prompt-waiting "Continue generati"
  "Text pattern for detecting waiting prompt.")

(defvar ecc-prompt-initial-waiting "Would you like Claude to continue?"
  "Text pattern for detecting initial waiting prompt.")

(defvar ecc-prompt-y/n "(y/n)"
  "Text pattern for detecting yes/no prompt.")

(defvar ecc-prompt-y/y/n "(Y/y/n)"
  "Text pattern for detecting Y/y/n prompt.")

(defvar ecc-prompt-thinking "Thinking…"
  "Text pattern for detecting thinking state.")

(defvar ecc-prompt-loading "Loading…"
  "Text pattern for detecting loading state.")

(defvar ecc-prompt-to-send-on-waiting "continue"
  "Text to send when Claude is in waiting state.")

;; Regex patterns for prompt detection
(defvar ecc-prompt-pattern-y/n "(y/n)"
  "Regex pattern for detecting yes/no prompt.")

(defvar ecc-prompt-pattern-y/y/n "(Y/y/n)"
  "Regex pattern for detecting Y/y/n prompt.")

(defvar ecc-prompt-pattern-waiting "Continue generati"
  "Regex pattern for detecting waiting prompt.")

(defvar ecc-prompt-pattern-initial-waiting "Would you like Claude to continue?"
  "Regex pattern for detecting initial waiting prompt.")

;; Set up variable aliases for backward compatibility
(defvaralias 'ecc-auto-timer 'ecc-timer)
(defvaralias 'ecc-active-buffer 'ecc-buffer-current-buffer)
(defvaralias 'ecc-buffer-current-active-buffer 'ecc-buffer-current-buffer)
(defvaralias 'ecc-buffers 'ecc-buffer-registered-buffers-alist)

;; Provide the feature
(provide 'fix-variables)