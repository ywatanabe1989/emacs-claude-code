# emacs-claude-code v2.1.0 Released! 🎉

I'm excited to announce the release of emacs-claude-code v2.1.0, bringing major enhancements to the Emacs-Claude integration experience.

## What's New

### 🔥 Yank-as-File
No more pasting huge code blocks! The new yank-as-file feature automatically:
- Detects when you're yanking large content (100+ chars)
- Saves it to a temporary file with smart file type detection
- Sends `See /tmp/file.ext` to Claude instead
- Supports Python, JavaScript, HTML, CSS, Elisp, and Shell

### ⏰ Auto-Periodical Commands
Stay synchronized with automatic command execution:
- Runs `/git` every 10 interactions by default
- Fully customizable command list and intervals
- Helps maintain context awareness during long sessions

### 🔄 Periodic Return Sending
Improved reliability with periodic return key sending:
- Resolves timing issues in auto-response
- Sends return every 5 minutes (configurable)
- Buffer-local timers for multi-session support

### 🐛 Bug Fixes
- Fixed modeline buffer awareness issues
- Resolved CI/CD pipeline problems
- Enhanced notification system consistency

## Quick Start

```elisp
;; Enable new features
(ecc-auto-periodical-toggle)           ; Auto-periodical commands
(--ecc-vterm-utils-enable-yank-advice) ; Yank-as-file

;; Optional configuration
(setq --ecc-vterm-yank-as-file-threshold 100)
(setq ecc-auto-periodical-commands '((10 . "/git") (20 . "/user:auto")))
```

## Installation

```elisp
(add-to-list 'load-path "~/.emacs.d/lisp/emacs-claude-code")
(require 'emacs-claude-code)
```

## Links
- [GitHub Repository](https://github.com/ywatanabe1989/emacs-claude-code)
- [Full Changelog](https://github.com/ywatanabe1989/emacs-claude-code/blob/main/CHANGELOG.md)
- [Documentation](https://github.com/ywatanabe1989/emacs-claude-code/tree/main/docs)

## Acknowledgments
Thanks to all users who provided feedback and bug reports. Special thanks to Claude for being an excellent coding companion! 🤖

## Feedback
Found a bug or have a feature request? Please [open an issue](https://github.com/ywatanabe1989/emacs-claude-code/issues)!

---
Happy coding with Claude! 🚀