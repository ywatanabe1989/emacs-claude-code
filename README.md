<!-- ---
!-- Timestamp: 2025-07-03 08:54:56
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/README.md
!-- --- -->

# Emacs Claude Code

Emacs interface for Claude Code with intelligent auto-response and enhanced vterm integration.

## 🚀 Key Features
- **🤖 Auto-Response**: Automatically responds to Claude prompts (INITIAL WAITING, Y/N, Y/Y/N, and WAITING states)
- **⏰ Periodic Auto-Response**: Sends periodic commands based on number of interactions
- **📊 Buffer Management**: Centralized dashboard to monitor all Claude sessions 
- **📋 Yank-as-File**: Yank large contents as file for clean terminal, with remote supported

---

## 📺 Examples

![Emacs Claude Code Example](./docs/emacs-claude-code-demo.gif)

*Real-time demonstration of auto-response functionality*

## 📋 Buffer List Management

``` plaintext
ECC Claude Buffer List
=====================

    Buffer Name                Auto-Response   Last Sent    State
─── ─────────────────────────  ──────────────  ──────────── ─────
    my-awesome-buffer-1        Enabled         10:22:54     Continue
    my-awesome-buffer-2        Enabled         09:18:34     None
    my-awesome-buffer-3        Disabled        Never        Y/N
    my-awesome-buffer-4        Disabled        Never        Running
    my-awesome-buffer-5        Disabled        Never        Continue
    my-awesome-buffer-6        Enabled         08:45:12     None

Commands:
  RET/SPC  - Jump to buffer
  o        - Display buffer in other window
  a        - Toggle auto-response
  d        - Kill buffer(s)
  m        - Mark buffer
  u        - Unmark buffer
  U        - Unmark all buffers
  t        - Toggle marks
  g        - Refresh list
  r        - Toggle auto-refresh
  q        - Quit
  n/p      - Next/previous line

Auto-refresh: ON (every 2.0s)
```

### Usage
```elisp
M-x ecc-list-buffers  ; Open the buffer list dashboard
```

---

## 📦 Installation

```bash
git clone https://github.com/ywatanabe1989/emacs-claude-code.git ~/.emacs.d/lisp/emacs-claude-code
```

Add to your `init.el`:
```elisp
(add-to-list 'load-path "~/.emacs.d/lisp/emacs-claude-code")
(require 'emacs-claude-code)
```

---

## 🎯 Quick Start

### Essential Commands
| Command | Description |
|---------|-------------|
| `M-x ecc-list-buffers` | Show all Claude buffers with status |
| `M-x ecc-auto-toggle` | Toggle auto-response for current vterm buffer |
| `M-x ecc-auto-periodical-toggle` | Toggle periodic auto-response commands |
| `M-x ecc-vterm-yank-as-file` | Yank clipboard content as file (supports remote hosts) |

### Basic Configuration

```elisp
;; Enable auto-response patterns
(setq --ecc-auto-response-responses
  '((:y/n . "1")                                     ; Respond "1" to Y/N prompts
    (:y/y/n . "2")                                   ; Respond "2" to Y/Y/N prompts
    (:waiting . "/auto")                             ; Send /auto custom command when waiting
    (:initial-waiting . "/understand-guidelines")))  ; Send /understand-guidelines custom command when startup

;;;; Enable yank-as-file for large content
;; (--ecc-vterm-utils-enable-yank-advice)

;;;; Enable periodic auto-response to keep sessions active
;; (ecc-auto-periodical-toggle)

;; Configure periodic commands (optional)
(setq ecc-auto-periodical-commands
  '((10 . "/compact")     ; Run (preset) /compact command every 10 interactions
    (20 . "/git")))      ; Run (custom) /git command every 20 interactions
```

---

## 💻 Custom Claude Commands

In Claude Code, custom slash commands can be created by adding .md files to `.claude/commands/` in your project or `~/.claude/commands/` for commands that work in any project. See [Anthropic's Official Documentation](https://www.anthropic.com/engineering/claude-code-best-practices) for details.

  The default settings of this `emacs-claude-code` assumes that `/auto` ([`./docs/commands/auto.md`](./docs/commands/auto.md)) and `/understand-guidelines` ([`./docs/commands/understand-guidelines.md`](./docs/commands/understand-guidelines.md)) commands are defined as `~/.claude/commands/{auto,understand-cuidelines}.md` (user level) or `./.claude/commands/{auto,understand-cuidelines}.md` (project level; most prioritized).

---

## 📁 Yank Target Directory

Yank-as-file saves content to `~/.emacs-claude-code/` by default. You can customize this directory:

```elisp
;; Set custom yank directory
(setq ecc-directory-for-yank-as-file "~/my-custom-yank-dir/")
```

---

## ⌨️ Optional Keybindings

To set up convenient keybindings:

```elisp
;; Optional keybindings
(define-key vterm-mode-map (kbd "C-c C-l") 'ecc-list-buffers)
(define-key vterm-mode-map (kbd "C-c C-a") 'ecc-auto-toggle)
(define-key vterm-mode-map (kbd "C-c C-y") 'ecc-vterm-yank-as-file)
```

---

## 📚 Documentation
See [`./docs/`](./docs/) for detailed guides and configuration options.

---

## 📎 Appendix: Author's Custom Workflow Reference

### Bash Commands
Example bash functions for Claude Code workflow management (see `docs/example_bash_config/`):

- `cld_forget [n]` - Delete latest n JSONL files from Claude project history (default: 1)
- `cld_logout` - Clear Claude account credentials
- `cld` - Start Claude session with project-specific configurations and MCP support
- `cld_worktree_toggle` (alias: `ct`) - Toggle between original project and Claude worktree directories

### Project Context Directory

The `./docs/to_claude/` directory contains project-specific context files that are automatically synced and made read-only by the `cld` command:

- `guidelines/` - Project guidelines and coding standards
- `bin/` - Project-specific scripts and utilities  
- `examples/` - Code examples and templates

This directory is synchronized from `~/.claude/to_claude/` and protected with read-only permissions to ensure consistent project context across Claude sessions.

### Claude Commands
Custom `/` commands for Claude Code workflow (see `./.claude/commands/`):

- `/auto`, `/understand-guidelines`, `/plan`, `/tests`, `/git`, `/refactor`, `/cleanup`
- `/bug-report`, `/feature-request`, `/progress`, `/timeline`, `/finalize`
- `/worktree`, `/rollback`, `/resolve-conflicts`, `/factor-out`, `/rename`

These are reference command templates that can be customized for your project workflow. Please see [Anthropic's documentation](https://www.anthropic.com/engineering/claude-code-best-practices) to understand where to place such markdown files for custom commands.

---

## 📧 Contact
Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

<!-- EOF -->