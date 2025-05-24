<!-- ---
!-- Timestamp: 2025-05-22 14:39:26
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/README.md
!-- --- -->

# Emacs Claude Code

An Emacs package for seamless integration with Claude AI, providing optimized terminal interaction, automatic response handling, and specialized features for an enhanced Claude experience.

## Features

- **Claude VTerm Mode**: Optimized terminal mode for interacting with Claude
- **Auto-Response System**: Automatically respond to Claude prompts (Y/N, continue, etc.)
- **State Detection**: Intelligent detection of different Claude prompt states
- **Grayscale Mode**: Toggle between color and grayscale in vterm buffers
- **Yank as File**: Save Claude outputs to files with automatic file type detection
- **Eye-Friendly Features**: Smooth scrolling and visual comfort optimizations
- **Customizable Themes**: Switch between dark, light, and gray themes

## Installation

### Prerequisites

- Emacs 27.1 or later
- VTerm package

### Basic Installation

```elisp
;; Clone the repository
(let ((repo-dir (expand-file-name "~/.emacs.d/lisp/emacs-claude-code")))
  (unless (file-exists-p repo-dir)
    (make-directory repo-dir t)
    (call-process "git" nil nil nil "clone" 
                  "https://github.com/your-username/emacs-claude-code.git" 
                  repo-dir)))

;; Add to load path
(add-to-list 'load-path "~/.emacs.d/lisp/emacs-claude-code")

;; Load the package
(require 'emacs-claude-code)
```

## Usage

### Starting Claude VTerm

```elisp
M-x ecc-claude-vterm
```

### Key Bindings

- `C-c c v` - Start a new Claude vterm session
- `C-c c a` - Toggle auto-response mode
- `C-c c g` - Toggle grayscale mode
- `C-c c t` - Toggle visual aids
- `C-c c m` - Toggle between color themes
- `C-c c e` - Toggle eye-friendly mode

### Yank as File

- `C-c C-f` - Yank selected region to a file
- `C-c C-b` - Yank entire buffer to a file
- `C-c C-q` - Quick yank with auto-generated filename

## Customization

```elisp
;; Auto-response settings
(setq ecc-auto-response-yes "y")          ; Response for Y/N prompts
(setq ecc-auto-response-continue "/user:auto") ; Response for continue prompts

;; Grayscale mode settings
(setq ecc-vterm-grayscale-default t)      ; Enable grayscale by default
(setq ecc-vterm-grayscale-auto-enable t)  ; Auto-enable for Claude buffers

;; Theme settings
(setq ecc-colors-theme 'gray)            ; 'dark, 'light, or 'gray
```

## Architecture

The package is designed with clean code principles and organized into several modules:

- **Core modules**: Basic functionality and variables
- **State detection**: Logic for detecting Claude prompt states
- **Auto-response**: Automatic response to different prompt types
- **VTerm enhancements**: Optimized terminal for Claude interaction
- **Visual aids**: UI improvements for better user experience

## Contributing

Contributions are welcome! Please see the [CONTRIBUTING.md](CONTRIBUTING.md) file for guidelines.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

<!-- EOF -->