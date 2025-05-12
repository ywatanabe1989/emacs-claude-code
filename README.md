<!-- ---
!-- Timestamp: 2025-05-10 04:04:43
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/README.md
!-- --- -->


# Emacs Claude Code

[![Build Status](https://github.com/ywatanabe1989/emacs-claude-code/workflows/tests/badge.svg)](https://github.com/ywatanabe1989/emacs-claude-code/actions)

A streamlined Emacs interface for Claude AI coding assistance.

## Overview

Emacs Claude Code (ECC) enhances your workflow by seamlessly integrating Claude's AI capabilities directly into Emacs. The package provides:

- Comprehensive buffer management for Claude interactions
- Optimized VTERM mode for high-performance Claude interaction
- Automatic response handling with state detection
- Template system for common queries
- Web browsing integration for research and information gathering
- Region/buffer processing
- Repository analysis capabilities
- Robust test framework
- Modern architecture with proper state machine and command system

## Installation

### Requirements

- Emacs 28+
- vterm package
- Claude account (Anthropic)

### Setup

#### Method 1: Direct Installation

1. Clone this repository:
   ```
   git clone https://github.com/ywatanabe/emacs-claude-code.git ~/.emacs.d/lisp/emacs-claude-code
   ```

2. Add to your Emacs configuration:
   ```elisp
   (add-to-list 'load-path "~/.emacs.d/lisp/emacs-claude-code")
   (require 'emacs-claude-code)
   ```

#### Method 2: Using the Standalone Script

For a quick setup without modifying your Emacs configuration:

```bash
cd ~/.emacs.d/lisp/emacs-claude-code
./launch-claude-standalone.sh
```

This script launches Emacs with a temporary configuration that loads the Claude VTerm environment.


## Usage
All commands are available under the `C-c c` prefix:

### Buffer Management
- `C-c c c` - Create new Claude session
- `C-c c v` - Create new Claude VTERM session
- `C-c c V h` - Display help for Claude VTERM mode
- `C-c c n` - Navigate to next Claude buffer
- `C-c c p` - Navigate to previous Claude buffer
- `C-c c l` - List all Claude buffers

### Web Browsing Integration
- `C-c C-w` - Open EWW web browser
- `C-c C-s` - Search the web using EWW
- `C-c C-b` - Browse a URL and send content to Claude

For more details on web integration, see the Web Integration Documentation.

### Interaction
- `C-c c a` - Toggle auto-accept mode
- `C-c c y` - Manually accept Claude prompt
- `C-c c i` - Initialize/restart Claude session
- `C-c c ESC` - Interrupt Claude processing

### Content Processing
- `C-c c r` - Run Claude on region
- `C-c c b` - Run Claude on buffer
- `C-c c q` - Quick query to Claude
- `C-c c t` - Send a template

### Templates
- `C-c c T c` - Create new template
- `C-c c T e` - Edit existing template

### Memory Management
- `C-c c m` - Edit Claude memory
- `C-c c h c` - Compact history
- `C-c c h l` - Clear history

### Repository
- `C-c c R` - Copy repository contents

## VTERM Mode

ECC provides an optimized VTERM mode for high-performance Claude interaction, with specialized features for handling Claude's interactive capabilities.

### How to Use

1. **Start VTERM Mode**: Use `C-c c v` or `M-x ecc-run-vterm-claude` to start a new VTERM session

2. **Key Bindings**:
   - `C-c C-y` - Send 'yes' response
   - `C-c C-n` - Send 'no' response
   - `C-c C-r` - Send 'retry' response
   - `C-c C-c` - Interrupt Claude
   - `C-c C-a` - Toggle auto-mode
   - `C-c C-l` - Clear buffer
   - `C-c C-p/C-c C-b` - Previous buffer
   - `C-c C-f` - Next buffer
   - `C-c C-t` - New buffer

3. **Web Integration Key Bindings**:
   - `C-c C-w` - Open EWW web browser
   - `C-c C-s` - Search the web using EWW
   - `C-c C-b` - Browse a URL and send content to Claude

4. **Mode Line Indicators**:
   - `[Waiting]` - Claude is waiting for continuation
   - `[Y/N]` - Claude is waiting for yes/no
   - `[Y/Y/N]` - Claude is waiting for complex choice
   - `[Running]` - Claude is processing

For more details, see the [VTERM Mode documentation](docs/vterm-mode.md).

## Auto-Accept Mode

ECC provides an Auto-Accept mode that automatically handles Claude's interactive prompts without requiring manual intervention. This is particularly useful for long-running interactions where Claude frequently asks for confirmation or choices.

### How to Use

1. **Toggle Auto-Accept Mode**: 
   - In standard mode: Use `C-c c a` to toggle auto-accept mode on/off
   - In VTERM mode: Use `C-c C-a` to toggle auto-mode on/off

2. **Automatic Actions**:
   - When Claude displays a Y/N prompt, auto-accept automatically selects "Y"
   - For multi-option prompts (Y/Y/N), it selects the middle option
   - When Claude waits for confirmation to continue, it automatically sends the continue command

3. **Notifications**:
   - Desktop notifications are sent when auto-accept is enabled/disabled
   - Notifications are also sent each time Claude prompts are automatically handled

### Configuration

Auto-accept mode can be configured through these variables:

```elisp
;; Standard mode configuration
;; Set auto-accept interval (defaults to 1 second)
(setq ecc-auto-interval-sec 2)

;; Enable auto-accept when ECC starts
(setq ecc-auto-enable t)

;; VTERM mode configuration
;; Enable auto-mode in VTERM by default
(setq ecc-claude-vterm-auto-mode t)
```

When auto-accept mode is active, you'll see "Auto" in the mode line indicator.

## Customization

Configure through the `customize` interface:
```elisp
M-x customize-group RET emacs-claude-code RET
```

## Testing

The project includes a comprehensive test suite that can be run in multiple ways:

### Standard Tests (Quick)

Run all tests with standard output:

```bash
./run_tests.sh
```

Run a single test file:

```bash
./run_tests.sh tests/test-ecc-buffer.el
```

### Detailed Test Reports

The test script automatically generates detailed org-mode and PDF reports with test results.

### Testing VTERM Mode

To specifically test the VTERM mode functionality:

```bash
emacs -batch -l tests/test-vterm-mode.el
```

This script is designed to work properly even if the vterm package is not available.

## Architecture

Emacs Claude Code uses a modern, well-structured architecture with proper separation of concerns:

- **State Engine**: Proper state machine implementation for Claude interactions
- **Buffer Manager**: Robust buffer lifecycle management with metadata
- **Command System**: Unified command interface with history and extensibility
- **Integration Layer**: Backward compatibility with legacy systems

For more details, see the [Architecture Documentation](docs/architecture.md).


## Contact
Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

<!-- EOF -->