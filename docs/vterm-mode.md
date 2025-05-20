# Claude Code VTERM Mode

## Overview

Claude Code VTERM Mode provides an optimized terminal interface for interacting with Claude through Emacs. It uses the `vterm` package for high-performance terminal emulation, while adding Claude-specific features such as:

- State detection and visual indicators in the mode line
- Automatic response to common prompts (Y/N, continuation, etc.)
- Buffer management for multiple Claude sessions
- Specialized key bindings for common Claude operations

## Requirements

- Emacs 27.1 or later
- `vterm` package (optional, falls back to `special-mode` if not available)
- Claude Code package
- Claude CLI tool (optional, for real API access)


## Basic Usage

### Starting a VTERM session

```elisp
M-x ecc-claude-vterm
```

This creates a new buffer running the optimized VTERM mode for Claude interactions.

### Key Bindings

| Key Binding | Function | Description |
|-------------|----------|-------------|
| `C-c C-k` | `ecc-claude-vterm-interrupt` | Interrupt the current Claude process |
| `C-c C-c` | `ecc-claude-vterm-interrupt` | Alternative binding for interrupt |
| `C-c C-y` | `ecc-claude-vterm-yes` | Send 'y' response to Claude prompt |
| `C-c C-n` | `ecc-claude-vterm-no` | Send 'n' response to Claude prompt |
| `C-c C-r` | `ecc-claude-vterm-retry` | Send 'r' response to retry |
| `C-c C-l` | `ecc-claude-vterm-clear` | Clear the vterm buffer |
| `C-c C-a` | `ecc-claude-vterm-auto-mode-toggle` | Toggle automatic response mode |
| `C-c C-p` | `ecc-claude-vterm-prev-buffer` | Switch to previous Claude buffer |
| `C-c C-f` | `ecc-claude-vterm-next-buffer` | Switch to next Claude buffer |
| `C-c C-b` | `ecc-claude-vterm-prev-buffer` | Alternative for previous buffer |
| `C-c C-t` | `ecc-claude-vterm` | Create a new Claude buffer |

### Mode Line Indicators

The mode line displays the current state of Claude:

- `[Waiting]`: Claude is waiting for you to continue
- `[Y/N]`: Claude is prompting for a yes/no response 
- `[Y/Y/N]`: Claude is prompting for a complex choice
- `[Running]`: Claude is processing or generating a response
- `[Continue?]`: Claude is asking if you want to continue

## Auto-Response Mode

Auto-response mode automatically sends responses to common Claude prompts:

- For "continue generating" prompts, it sends "continue"
- For Y/N prompts, it sends "y"
- For Y/Y/N prompts, it sends "y"

Toggle auto-response mode with `C-c C-a` or through the Claude menu.

## Buffer Management

Claude VTERM mode integrates with the Claude buffer management system:

- Create multiple Claude buffers with `C-c C-t`
- Navigate between buffers with `C-c C-p` and `C-c C-f`
- Buffers are automatically registered and unregistered

## Menu Interface

The Claude menu provides access to all VTERM functions:

- **Buffer Navigation**: Create new buffers, navigate between them
- **Auto-mode**: Toggle automatic response 
- **Responses**: Send standard responses (Yes, No, Retry)
- **Control**: Interrupt processing, clear the buffer

## Advanced Features

### State Detection

Claude VTERM mode detects the state of Claude by scanning the buffer for known prompt patterns. This allows it to:

- Update the mode line indicator
- Apply appropriate automatic responses
- Provide visual cues about Claude's state

### Integration with Buffer Registry

VTERM mode integrates with the Claude Code buffer registry system, which allows:

- Tracking of multiple Claude sessions
- Efficient switching between sessions
- Proper cleanup when sessions are terminated

## Customization

Customize VTERM mode through these variables:

- `ecc-claude-vterm-line-numbers`: Whether to display line numbers (default: nil)
- `ecc-claude-vterm-scroll-conservatively`: Value for scroll-conservatively (default: 10000)  
- `ecc-claude-vterm-truncate-lines`: Whether to truncate lines (default: t)
- `ecc-claude-vterm-prompt-waiting`: Pattern for detecting waiting prompts
- `ecc-claude-vterm-prompt-y/n`: Pattern for detecting Y/N prompts
- `ecc-claude-vterm-prompt-y/y/n`: Pattern for detecting Y/Y/N prompts
- `ecc-claude-vterm-auto-mode`: Whether auto-mode is enabled (default: nil)