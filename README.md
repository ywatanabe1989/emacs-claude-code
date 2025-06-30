<!-- ---
!-- Timestamp: 2025-07-01 05:39:51
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/README.md
!-- --- -->

# Emacs Claude Code

Emacs interface for Claude Code with intelligent auto-response and enhanced vterm integration.

## Key Features
- **Auto-Response**: Automatically responds to Claude prompts (INITIAL WAITING, Y/N, Y/Y/N, and WAITING states)
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
- `M-x ecc-list-buffers (e.g., C-c C-l)`        - Show all Claude buffers with status
- `M-x ecc-auto-toggle (e.g., C-c C-a)`         - Toggle auto-response for current vterm buffer
- `M-x ecc-vterm-yank-as-file (e.g., C-c C-y)`  - Yank clipboard content as file (supports remote hosts)

### Basic Configuration
    
In Claude Code, text input starts from `/` is regarded as command. See Anthropic's Official Documentation for details: https://www.anthropic.com/engineering/claude-code-best-practices

```elisp
;; Enable auto-response patterns
(setq --ecc-auto-response-responses
  '((:y/n . "1")                                     ; Respond "1" to Y/N prompts
    (:y/y/n . "2")                                   ; Respond "2" to Y/Y/N prompts
    (:waiting . "/auto")                             ; Send /auto commands when waiting
    (:initial-waiting . "/understand-guidelines")))  ; Send /understand-guidelines as commands when startup

;; Enable yank-as-file for large content
(--ecc-vterm-utils-enable-yank-advice)
```

## Documentation
See [`./docs/`](./docs/) for detailed guides and configuration options.

## Contact
Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

<!-- EOF -->