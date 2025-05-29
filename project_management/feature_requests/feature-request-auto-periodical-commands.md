<!-- ---
!-- Timestamp: 2025-05-28 07:00:00
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/project_management/feature_requests/feature-request-auto-periodical-commands.md
!-- --- -->

# Feature Request: Auto Periodical Commands [IMPLEMENTED]

## Summary
Add `ecc-auto-periodical` functionality that automatically executes predefined commands at regular intervals based on interaction count (e.g., every 10 interactions, run `/git` command).

## Status: IMPLEMENTED (2025-05-30)
This feature has been successfully implemented in `src/ecc-auto-periodical.el`.

## Motivation
During extended Claude sessions, it's easy to forget to commit changes regularly. Automatic periodic execution of commands like `/git` would help maintain good version control practices and reduce the risk of losing work.

## Proposed Implementation

### 1. Core Functionality
```elisp
(defcustom ecc-auto-periodical-commands
  '((10 . "/git")
    (20 . "/user:auto"))
  "Alist of (INTERVAL . COMMAND) pairs for periodic execution."
  :type '(alist :key-type integer :value-type string)
  :group 'ecc)

(defvar-local --ecc-interaction-counter 0
  "Counter for interactions in current buffer.")

(defun ecc-auto-periodical-check ()
  "Check if any periodic command should be executed."
  (dolist (entry ecc-auto-periodical-commands)
    (let ((interval (car entry))
          (command (cdr entry)))
      (when (and (> --ecc-interaction-counter 0)
                 (= (mod --ecc-interaction-counter interval) 0))
        (ecc-auto-periodical-execute command)))))
```

### 2. Integration Points
- Hook into existing interaction tracking in `ecc-auto-response.el`
- Increment counter when auto-response is sent
- Check for periodic commands after each interaction

### 3. Configuration Options
- Enable/disable periodic commands globally
- Buffer-local overrides
- Customizable command list and intervals
- Option to prompt before executing

### 4. Example Use Cases
- `/git` every 10 interactions for version control
- `/user:auto` every 20 interactions for status checks
- Custom user commands at specified intervals

## Benefits
- Automated version control workflow
- Reduced cognitive load
- Better project management
- Configurable to user preferences

## Implementation Priority
Medium - This would enhance productivity but isn't critical for core functionality.

<!-- EOF -->