<!-- ---
!-- Timestamp: 2025-05-10 04:02:41
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/docs/architecture.md
!-- --- -->

# Emacs Claude Code Architecture

This document describes the modern architecture of Emacs Claude Code, which follows a clean, modular design with proper separation of concerns.

## Overview

The architecture consists of several key components:

1. **State Engine**: A proper state machine for Claude interactions
2. **Buffer Manager**: Robust buffer lifecycle management
3. **Command System**: Centralized command processing
4. **Integration Layer**: Backward compatibility with the legacy system

## State Engine (`ecc-state-engine.el`)

The state engine provides a clean implementation of the state machine pattern:

```
┌─────────┐      ┌──────────┐      ┌──────────┐
│   Idle  │─────▶│  Running │─────▶│  Waiting │
└─────────┘      └──────────┘      └──────────┘
     ▲                │                  │
     │                │                  │
     │                ▼                  │
     │           ┌─────────┐             │
     │           │  Yes/No │             │
     │           └─────────┘             │
     │                │                  │
     │                ▼                  │
     │         ┌───────────────┐         │
     │         │  Yes/Yes/No   │         │
     │         └───────────────┘         │
     │                │                  │
     └────────────────┴──────────────────┘
```

### Key Features

- **State Structures**: Using `cl-defstruct` for proper encapsulation
- **Pattern Matching**: Regular expressions to detect state from buffer content
- **Hook System**: Hooks for state transitions
- **Predefined States**: Common Claude interaction states

### Example Usage

```elisp
;; Detect state in buffer
(ecc-state-engine-detect-state buffer)

;; Set state manually
(ecc-state-engine-set-state 'waiting)

;; Add hook for state transition
(ecc-state-engine-add-hook 'running 'enter 
                          (lambda () 
                            (message "Claude is thinking...")))
```

## Buffer Manager (`ecc-buffer-manager.el`)

The buffer manager handles the lifecycle of Claude interaction buffers:

```
┌───────────────┐     ┌────────────────┐
│  Create Buffer│────▶│ Buffer Registry│
└───────────────┘     └────────────────┘
                             │
                             ▼
┌───────────────┐     ┌────────────────┐
│   Kill Buffer │◀────│Buffer Selection│
└───────────────┘     └────────────────┘
                             │
                             ▼
                      ┌────────────────┐
                      │Buffer Metadata │
                      └────────────────┘
```

### Key Features

- **Buffer Structures**: Using `cl-defstruct` for proper encapsulation
- **Buffer Registry**: Hash table of all Claude buffers
- **Metadata Storage**: Per-buffer metadata
- **Navigation**: Convenient navigation between multiple Claude buffers
- **Cleanup**: Automatic cleanup of killed buffers

### Example Usage

```elisp
;; Create a new Claude buffer
(ecc-buffer-manager-create "My Claude Session")

;; Set current buffer
(ecc-buffer-manager-set-current claude-buffer)

;; Navigate between buffers
(ecc-buffer-manager-next)
(ecc-buffer-manager-previous)

;; Store and retrieve metadata
(ecc-buffer-manager-set-metadata claude-buffer 'project "my-project")
(ecc-buffer-manager-get-metadata claude-buffer 'project)
```

## Command System (`ecc-command.el`)

The command system centralizes Claude interactions through a unified command interface:

```
┌─────────┐    ┌──────────────┐    ┌───────────────┐
│  Input  │───▶│ Command Parse│───▶│Command Registry│
└─────────┘    └──────────────┘    └───────────────┘
                                          │
┌─────────┐                               │
│ History │◀──────────────────────────────┘
└─────────┘                               │
                                          ▼
                                   ┌────────────────┐
                                   │Command Execution│
                                   └────────────────┘
```

### Key Features

- **Command Registry**: Centralized command definition and lookup
- **Command Aliases**: Multiple names for the same command
- **Command History**: Tracking of executed commands
- **Built-in Commands**: Core set of commands for buffer management and interactions

### Built-in Commands

- `/help` - Display help on available commands
- `/new` - Create a new Claude conversation
- `/switch` - Switch between Claude conversations
- `/list` - List all Claude conversations
- `/rename` - Rename the current Claude conversation
- `/kill` - Kill a Claude conversation
- `/clear` - Clear the current conversation
- `/compact` - Compact the conversation history
- `/template` - Use a template for a new prompt
- `/retry` - Retry the last prompt

### Example Usage

```elisp
;; Register a custom command
(ecc-command-register
 (ecc-command--create
  :id "summary"
  :name "Summarize"
  :handler (lambda (args) (message "Summarizing: %s" args))
  :description "Summarize text with Claude"
  :usage "/summary [text]"
  :aliases '("sum" "s")))

;; Execute a command
(ecc-command-execute "/summary This text needs summarizing")

;; Get command history
(ecc-command-get-history)
```

## Integration Layer (`ecc-integration.el`)

The integration layer provides backward compatibility between the legacy system and the new architecture:

```
┌────────────────┐    ┌────────────────┐
│ Legacy Buffer  │───▶│  Import/Export │
└────────────────┘    └────────────────┘
                             │
                             ▼
┌────────────────┐    ┌────────────────┐
│ Legacy State   │───▶│State Conversion│
└────────────────┘    └────────────────┘
                             │
                             ▼
┌────────────────┐    ┌────────────────┐
│Legacy Functions│◀───│ New Architecture│
└────────────────┘    └────────────────┘
```

### Key Features

- **Buffer Conversion**: Import legacy buffers into the new system
- **State Synchronization**: Keep state consistent between old and new systems
- **Input Handling**: Process input through the new command system

### Example Usage

```elisp
;; Import existing Claude buffers to the new system
(ecc-integration-import-legacy-buffers)

;; Process input with the new command system
(ecc-integration-handle-input "/help" buffer)

;; Detect state using both systems and update both
(ecc-integration-detect-and-update-state buffer)
```

## VTERM Integration (`ecc-vterm.el`)

The VTERM integration connects the VTERM terminal emulator with the new architecture:

```
┌──────────────┐    ┌────────────────┐
│ VTERM Buffer │───▶│ State Detection│
└──────────────┘    └────────────────┘
       │                   │
       ▼                   ▼
┌──────────────┐    ┌────────────────┐
│Input Commands│    │  State Engine  │
└──────────────┘    └────────────────┘
```

### Key Features

- **Terminal Integration**: Use Claude in a proper terminal interface
- **State Detection**: Monitor terminal output for Claude state changes
- **Command Helpers**: Utilities for sending common responses

### Example Usage

```elisp
;; Create a new VTERM Claude session
(ecc-vterm-create)

;; Send commands to Claude in VTERM
(ecc-vterm-send-command buffer "Write a hello world program in Python")

;; Send responses
(ecc-vterm-send-yes buffer)
(ecc-vterm-send-no buffer)
```

## Conclusion

The new architecture provides a solid foundation for future development, with proper separation of concerns, clean interfaces, and good encapsulation. The integration layer ensures backward compatibility while allowing incremental migration to the new architecture.

<!-- EOF -->