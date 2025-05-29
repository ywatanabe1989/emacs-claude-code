<!-- ---
!-- Timestamp: 2025-05-26 10:54:45
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/README.md
!-- --- -->

# Emacs Claude Code
Simplified Emacs interface for Claude with auto-response capabilities and vterm integration.

![Demo](./docs/emacs-claude-code-demo.gif)

## Architecture

```mermaid
graph TD
    subgraph "Entry Points"
        A[emacs-claude-code.el]
        B[ecc.el]
    end
    
    subgraph "Core Modules"
        C[ecc-auto-response.el]
        D[ecc-state-detection.el]
        E[ecc-vterm-mode.el]
        F[ecc-list.el]
    end
    
    subgraph "Support Modules"
        G[ecc-notification.el]
        H[ecc-vterm-utils.el]
        I[ecc-debug.el]
        J[ecc-variables.el]
    end
    
    A --> B
    B --> C
    B --> D
    B --> E
    B --> F
    C --> D
    C --> G
    E --> H
    C --> I
    D --> I
    
    classDef entry fill:#e1f5fe
    classDef core fill:#fff3e0
    classDef support fill:#e8f5e8
    
    class A,B entry
    class C,D,E,F core
    class G,H,I,J support
```

## Installation
Installation guide is available at [`./docs/installation.md`](./docs/installation.md)

## Quick Start
```elisp
;; Add to your init.el
(add-to-list 'load-path "~/.emacs.d/lisp/emacs-claude-code")
(require 'emacs-claude-code)

;; Main commands
(ecc-auto-toggle)         ; Toggle auto-response for current buffer
(ecc-list-buffers)        ; Show all Claude buffers with status
(ecc-notification-toggle) ; Toggle desktop notifications
(ecc-debug-toggle)        ; Toggle debug mode

;; New v2.1.0 features
(ecc-auto-periodical-toggle)      ; Enable auto-periodical commands
(--ecc-vterm-utils-enable-yank-advice) ; Enable yank-as-file

;; Configuration examples (optional)
(setq --ecc-vterm-yank-as-file-threshold 100) ; Prompt threshold
(setq --ecc-auto-response-periodic-interval 300) ; 5 minutes
(setq ecc-auto-periodical-commands '((10 . "/git") (20 . "/user:auto")))

;; The package automatically enhances vterm-mode buffers
;; with Claude-specific features when enabled
```

## Key Features

### Core Features
- **Auto-Response System**: Automatically responds to Claude prompts (Y/N, Continue, etc.)
- **Buffer Management**: List and navigate between Claude terminal sessions
- **State Detection**: Intelligent detection of Claude's current state
- **Desktop Notifications**: Get notified when Claude needs attention
- **Debug Mode**: Built-in debugging for troubleshooting

### New in v2.1.0
- **Yank-as-File**: Automatically saves large yanked content to temporary files
  - Smart file type detection (Python, JS, HTML, CSS, Elisp, Shell)
  - Configurable thresholds (100+ chars prompts, 500+ chars auto-saves)
  - Sends file references to Claude instead of pasting large blocks
- **Auto-Periodical Commands**: Execute commands at regular intervals
  - Default: `/git` every 10 interactions
  - Fully configurable command list and intervals
- **Periodic Return Sending**: Improves auto-response reliability
  - Sends return key every 5 minutes to resolve timing issues
  - Buffer-local timers for multi-session support

## Details
- Auto-response system [`./docs/auto-response.md`](./docs/auto-response.md)
- Buffer management [`./docs/buffer-management.md`](./docs/buffer-management.md)
- Configuration options [`./docs/configuration.md`](./docs/configuration.md)
- Debug utilities [`./docs/debug-usage.md`](./docs/debug-usage.md)

## Contact
Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

<!-- EOF -->