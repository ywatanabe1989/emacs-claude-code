<!-- ---
!-- Timestamp: 2025-07-01 05:52:04
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/README.md
!-- --- -->

# Emacs Claude Code

Emacs interface for Claude Code with intelligent auto-response and enhanced vterm integration.

## Key Features
- **Auto-Response**: Automatically responds to Claude prompts (INITIAL WAITING, Y/N, Y/Y/N, and WAITING states)
- **Periodic Auto-Response**: Sends periodic commands to keep Claude sessions active and healthy
- **Buffer Management**: Centralized dashboard to monitor all Claude sessions 
- **Yank-as-File**: Yank large kill ring contents as file with remote support

## Installation

```bash
git clone https://github.com/ywatanabe1989/emacs-claude-code.git ~/.emacs.d/lisp/emacs-claude-code
```

Add to your `init.el`:
```elisp
(add-to-list 'load-path "~/.emacs.d/lisp/emacs-claude-code")
(require 'emacs-claude-code)
```

## Quick Start

### Essential Commands
- `M-x ecc-list-buffers`        - Show all Claude buffers with status
- `M-x ecc-auto-toggle`         - Toggle auto-response for current vterm buffer
- `M-x ecc-auto-periodical-toggle`              - Toggle periodic auto-response commands
- `M-x ecc-vterm-yank-as-file`  - Yank clipboard content as file (supports remote hosts)

### Basic Configuration
    
In Claude Code, text input starting with `/` is regarded as a command. See Anthropic's Official Documentation for details: https://www.anthropic.com/engineering/claude-code-best-practices

```elisp
;; Enable auto-response patterns
(setq --ecc-auto-response-responses
  '((:y/n . "1")                                     ; Respond "1" to Y/N prompts
    (:y/y/n . "2")                                   ; Respond "2" to Y/Y/N prompts
    (:waiting . "/auto")                             ; Send /auto commands when waiting
    (:initial-waiting . "/understand-guidelines")))  ; Send /understand-guidelines as commands when startup

;; Enable yank-as-file for large content
(--ecc-vterm-utils-enable-yank-advice)

;; Enable periodic auto-response to keep sessions active
(ecc-auto-periodical-toggle)

;; Configure periodic commands (optional)
(setq ecc-auto-periodical-commands
  '((10 . "/compact")     ; Run /compact every 10 interactions
    (20 . "/git")))      ; Run /git every 20 interactions
```

## Buffer List Management

### Buffer List Dashboard

The buffer list dashboard provides real-time monitoring of all Claude sessions:

![Buffer List Dashboard](./docs/images/buffer-list-dashboard.gif)

### Key Features
- **Real-time Status**: Shows current state of each Claude buffer (WAITING, PROCESSING, etc.)
- **Auto-Response Indicators**: Visual indicators for buffers with auto-response enabled
- **Quick Navigation**: One-click access to any Claude session
- **Session Management**: Easy overview of all active Claude interactions

### Usage
```elisp
M-x ecc-list-buffers  ; Open the buffer list dashboard
```

The dashboard automatically refreshes and shows:
- Buffer names and their current Claude states
- Auto-response status indicators
- Last activity timestamps
- Quick action buttons for each session

## Custom Commands

The `/auto` and `/understand-guidelines` commands are custom commands defined as markdown files in your project. These commands help Claude understand your project context and guidelines:

- `/auto` - Sends project-specific automation commands
- `/understand-guidelines` - Sends project guidelines and documentation

These commands are automatically sent based on Claude's state to provide context-aware assistance.

## Project Context Directory

The `./docs/to_claude/` directory contains project-specific context files that are automatically synced and made read-only by the `cld` command:

- `guidelines/` - Project guidelines and coding standards
- `bin/` - Project-specific scripts and utilities  
- `examples/` - Code examples and templates

This directory is synchronized from `~/.claude/to_claude/` and protected with read-only permissions to ensure consistent project context across Claude sessions.

## Yank Target Directory

Yank-as-file saves content to `~/.emacs-claude-code/` by default. You can customize this directory:

```elisp
;; Set custom yank directory
(setq ecc-directory-for-yank-as-file "~/my-custom-yank-dir/")
```

## Optional Keybindings

To set up convenient keybindings:

```elisp
;; Optional keybindings
(define-key vterm-mode-map (kbd "C-c C-l") 'ecc-list-buffers)
(define-key vterm-mode-map (kbd "C-c C-a") 'ecc-auto-toggle)
(define-key vterm-mode-map (kbd "C-c C-y") 'ecc-vterm-yank-as-file)
```

## Documentation
See [`./docs/`](./docs/) for detailed guides and configuration options.

## Appendix: Author's custom shell functions and commands for Claude Code

### Bash Commands
Example bash functions for Claude Code workflow management (see `docs/example_bash_config/`):

- `cld_forget [n]` - Delete latest n JSONL files from Claude project history (default: 1)
- `cld_logout` - Clear Claude account credentials
- `cld` - Start Claude session with project-specific configurations and MCP support
- `cld_worktree_toggle` (alias: `ct`) - Toggle between original project and Claude worktree directories

### Claude Commands
Custom `/` commands for Claude Code workflow (see `docs/commands/`):

- `/auto`, `/understand-guidelines`, `/plan`, `/tests`, `/git`, `/refactor`, `/cleanup`
- `/bug-report`, `/feature-request`, `/progress`, `/timeline`, `/finalize`
- `/worktree`, `/rollback`, `/resolve-conflicts`, `/factor-out`, `/rename`

These are reference command templates that can be customized for your project workflow. Please see the Anthropic's doc to understand where to place such markdown files for custom commands.

## Contact
Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

<!-- EOF -->