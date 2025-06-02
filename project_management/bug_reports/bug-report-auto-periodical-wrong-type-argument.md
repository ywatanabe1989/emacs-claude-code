<!-- ---
!-- Timestamp: 2025-06-02 15:22
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/project_management/bug_reports/bug-report-auto-periodical-wrong-type-argument.md
!-- --- -->

# Bug Report: Wrong Type Argument Error in Auto-Response Timer

## Issue Description
When auto-response is enabled, the following error occurs repeatedly:
```
Error running timer '--ecc-auto-response--process-all-buffers': (wrong-type-argument listp 10)
```

## Error Context
The error occurs when:
1. Auto-response is enabled in a buffer
2. The timer tries to process buffers
3. The auto-periodical integration attempts to iterate over commands

## Root Cause
In `src/ecc-auto-periodical.el`, the default value of `ecc-auto-periodical-commands` is incorrectly defined:

```elisp
(defcustom ecc-auto-periodical-commands
  '(10 . "/compact")  ; This is a single cons cell, not a list
  "Alist of (INTERVAL . COMMAND) pairs for periodic execution.
  INTERVAL is the number of interactions between executions.
  COMMAND is the string to send to the buffer."
  :type '(alist :key-type integer :value-type string)
  :group 'ecc)
```

The `dolist` in `ecc-auto-periodical-check` expects a list but receives a cons cell:
```elisp
(dolist (entry ecc-auto-periodical-commands)  ; Tries to iterate over '(10 . "/compact")
  (let ((interval (car entry))  ; This would be 10
        (command (cdr entry)))   ; This would be "/compact"
```

## Fix Required
Change the default value to be a proper alist (list of cons cells):
```elisp
(defcustom ecc-auto-periodical-commands
  '((10 . "/compact"))  ; Note the extra parentheses to make it a list
  "Alist of (INTERVAL . COMMAND) pairs for periodic execution.
  INTERVAL is the number of interactions between executions.
  COMMAND is the string to send to the buffer."
  :type '(alist :key-type integer :value-type string)
  :group 'ecc)
```

## Impact
- High: This error prevents auto-response from working properly
- The error is logged every time the auto-response timer runs
- Users see repeated error messages in the *Messages* buffer

## Reproduction Steps
1. Enable auto-response in any vterm buffer: `M-x ecc-auto-toggle`
2. Wait for the auto-response timer to trigger (default: 3 seconds)
3. Observe error in *Messages* buffer

<!-- EOF -->