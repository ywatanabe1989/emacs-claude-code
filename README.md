<!-- ---
!-- Timestamp: 2025-05-26 10:54:45
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/README.md
!-- --- -->

# Emacs Claude Code
This repository provides a comprehensive Claude Code interface for Emacs with intelligent auto-response capabilities, advanced vterm integration, and productivity enhancements.

![Demo](./docs/emacs-claude-code-demo.gif)

## Architecture

```mermaid
graph TD
    subgraph "Entry Point"
        A[emacs-claude-code.el]
    end
    
    subgraph "Core Components"
        B[ecc-vterm-mode.el]
        C[ecc-auto-response.el]
        D[ecc-state-detection.el]
        E[ecc-term-claude-mode.el]
    end
    
    subgraph "Advanced Features"
        F[ecc-vterm-yank-as-file.el]
        G[ecc-auto-notify.el]
        H[ecc-visual-aid.el]
        I[ecc-interaction-tracker.el]
    end
    
    subgraph "Utilities"
        J[ecc-variables.el]
        K[ecc-debug-utils.el]
        L[ecc-convenience-commands.el]
    end
    
    A --> B
    A --> C
    A --> D
    A --> E
    B --> F
    C --> G
    D --> H
    E --> I
    B --> J
    C --> K
    D --> L
    
    classDef entry fill:#e1f5fe
    classDef core fill:#fff3e0
    classDef advanced fill:#f3e5f5
    classDef utility fill:#e8f5e8
    
    class A entry
    class B,C,D,E core
    class F,G,H,I advanced
    class J,K,L utility
```

## Installation
Installation guide is available at [`./docs/installation.md`](./docs/installation.md)

## Quick Start
```elisp
;; Add to your init.el
(add-to-list 'load-path "~/.emacs.d/lisp/emacs-claude-code")
(require 'emacs-claude-code)

;; Start Claude terminal
(ecc-vterm)

;; Toggle auto-response mode globally
(ecc-auto-response-toggle)

;; Toggle auto-response for current buffer only
(ecc-auto-response-buffer-local-toggle)

;; Yank vterm content as file with auto-detection
(ecc-vterm-yank-as-file)
```

## Details
- Core features and vterm integration [`./docs/core-features.md`](./docs/core-features.md)
- Auto-response system (global and buffer-local) [`./docs/auto-response.md`](./docs/auto-response.md)
- Visual aids and color themes [`./docs/visual-enhancements.md`](./docs/visual-enhancements.md)
- State detection and interaction tracking [`./docs/state-detection.md`](./docs/state-detection.md)
- Configuration and customization [`./docs/configuration.md`](./docs/configuration.md)
- Testing and development workflow [`./docs/testing.md`](./docs/testing.md)
- Debug utilities and troubleshooting [`./docs/debug-usage.md`](./docs/debug-usage.md)

## Contact
Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

<!-- EOF -->