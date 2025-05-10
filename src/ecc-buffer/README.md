<!-- ---
!-- Timestamp: 2025-05-07 23:10:47
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/ecc-buffer/README.md
!-- --- -->

# ecc-buffer Module

The `ecc-buffer` module manages Claude terminal buffers within Emacs. It provides functionality for creating, tracking, and interacting with Claude AI terminal sessions.

## Core Functionality

- Buffer creation and registration
- State management for Claude buffers
- Detection of Claude prompt states
- Navigation between buffers
- Stale buffer handling and cleanup

## Components

- **ecc-buffer-definitions.el**: Core definitions and constants
- **ecc-buffer-registry.el**: Register and track buffer instances
- **ecc-buffer-current.el**: Current active buffer management
- **ecc-buffer-state.el**: Claude state detection (waiting, active, etc.)
- **ecc-buffer-stale.el**: Cleanup of invalid/stale buffers
- **ecc-buffer-navigation.el**: Navigation between registered buffers

## Key Functions

- `ecc-buffer-create-registered-buffer`: Create new Claude buffer
- `ecc-buffer-set-current-buffer`: Set active Claude buffer
- `ecc-buffer-get-current-buffer`: Get active Claude buffer
- `ecc-state-get`: Detect Claude's current state
- `ecc-buffer-next`/`ecc-buffer-prev`: Navigate between buffers

### Basic Usage Examples

```elisp
```

### Common Workflows

1. Setting up multiple Claude agents for different projects:
   ```elisp
   ;; Create buffers for different projects
   (setq project-buffer-1 (ecc-buffer-create-registered-buffer))
   (setq project-buffer-2 (ecc-buffer-create-registered-buffer))
   
   ;; Switch between them as needed
   (ecc-buffer-set-current-buffer project-buffer-1)
   ```

2. Detecting and responding to Claude's state:
   ```elisp
   (defun my/claude-check-and-respond ()
     "Example of checking Claude's state and responding appropriately."
     (let ((state (ecc-state-get)))
       (cond
        ((eq state :waiting) (--ecc-send-string "continue"))
        ((eq state :y/n) (--ecc-send-string "1"))
        ((eq state :y/y/n) (--ecc-send-string "2")))))

<!-- EOF -->