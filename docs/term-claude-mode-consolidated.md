# Term-Claude Mode Consolidation Documentation

This document describes the consolidated `ecc-term-claude-mode` module in the emacs-claude-code project.

## Overview

The `ecc-term-claude-mode-consolidated.el` module provides an optimized Emacs vterm mode for interacting with the Claude AI assistant. It combines functionality from multiple previous implementations into a single, well-organized module that follows clean code principles.

This consolidation brings together:
- `ecc-term-claude-mode.el` (original implementation)
- `ecc-term-claude-mode-improved.el` (enhanced organization and documentation)

The consolidated module maintains backward compatibility while providing a cleaner structure, better organization, and integration with other consolidated modules.

## Features

The consolidated term-claude mode provides:

- **Performance Optimizations**: Tailored for high-volume streaming output from Claude
- **State Detection**: Real-time detection of Claude prompt states (Y/N, waiting, etc.)
- **Auto-Response**: Automatic responses to common Claude prompts
- **Visual Indicators**: Mode line indicators for Claude's current state
- **Follow-Bottom**: Smart scrolling to keep the latest output visible
- **Yank-as-File**: Easy extraction of Claude's output to files
- **Buffer Management**: Register buffers for Claude interaction
- **Two Usage Modes**: 
  - Full major mode (ecc-term-claude-mode)
  - Enhancement functions for existing vterm buffers

## Module Structure

The module is organized into clear functional sections:

1. **Customization**: User-configurable options
2. **Internal Variables**: Private variables for internal use
3. **Mode Definition**: Major mode definition and keymaps
4. **Buffer Setup**: Functions for setting up and configuring buffers
5. **State Detection**: Functions for detecting and displaying Claude's state
6. **Interaction Commands**: Commands for user interaction with Claude
7. **Auto-Response**: Functionality for automatic responses to prompts
8. **Buffer Management**: Functions for managing Claude buffers
9. **Follow-Bottom**: Smart scrolling functionality
10. **Public APIs**: Main user-facing commands
11. **Backward Compatibility**: Aliases for functions and variables from older versions

## API Reference

### Main Commands

| Command | Description |
|---------|-------------|
| `ecc-term-claude-mode` | Major mode for Claude interaction in vterm |
| `ecc-term-claude` | Create a new Claude vterm buffer or enhance existing one |
| `ecc-term-claude-enable` | Add Claude features to existing vterm buffer |
| `ecc-term-claude-yes` | Send 'y' response to Claude |
| `ecc-term-claude-no` | Send 'n' response to Claude |
| `ecc-term-claude-clear` | Clear the vterm buffer |

### Auto-Response Commands

| Command | Description |
|---------|-------------|
| `ecc-term-claude-auto-mode-toggle` | Toggle automatic response to Claude prompts |

### Buffer Management Commands

| Command | Description |
|---------|-------------|
| `ecc-term-claude-toggle-follow-bottom` | Toggle follow-bottom scrolling |

### Internal Functions

These functions are prefixed with double-dash to indicate they are private:

| Function | Description |
|----------|-------------|
| `ecc-term-claude--setup-buffer` | Set up buffer for Claude interaction |
| `ecc-term-claude--register-buffer` | Register buffer for Claude interaction |
| `ecc-term-claude--auto-send-respond` | Automatically respond to prompts |
| `ecc-term-claude--mode-line-state-indicator` | Show Claude state in mode line |

### Backward Compatibility

Previous function names are aliased to new ones for backward compatibility:

| Old Function | New Function |
|--------------|--------------|
| `ecc-register-buffer` | `ecc-term-claude--register-buffer` |
| `ecc-term-claude-auto-send-accept` | `ecc-term-claude--auto-send-respond` |
| `ecc-term-claude-setup-existing-buffer` | `ecc-term-claude--setup-existing-buffer` |

## Customization Options

| Option | Description | Default |
|--------|-------------|---------|
| `ecc-term-claude-line-numbers` | Show line numbers | `nil` |
| `ecc-term-claude-scroll-conservatively` | Scroll smoothness | `10000` |
| `ecc-term-claude-truncate-lines` | Truncate long lines | `t` |
| `ecc-term-claude-state-update-interval` | How often to check state | `1.0` |
| `ecc-term-claude-auto-mode` | Auto-respond to prompts | `nil` |
| `ecc-term-claude-buffer-name` | Default buffer name | `"*CLAUDE-VTERM*"` |
| `ecc-term-claude-show-state-in-mode-line` | Show state in mode line | `t` |

## Integration with Other Modules

The consolidated term-claude mode integrates with other consolidated modules:

- `ecc-variables-consolidated.el`: Shared configuration variables
- `ecc-state-detection-consolidated.el`: Claude prompt state detection
- `ecc-auto-response-consolidated.el`: Auto-response functionality
- `ecc-vterm-yank-as-file.el`: Output extraction to files

## Usage Examples

### Basic Usage

```elisp
;; Create a new Claude vterm buffer
M-x ecc-term-claude

;; Or, in an existing vterm buffer:
M-x ecc-term-claude-enable
```

### Manual Responses to Prompts

```elisp
;; When Claude asks a Yes/No question:
M-x ecc-term-claude-yes
;; or
M-x ecc-term-claude-no

;; Or use the keybindings:
;; C-c C-y for yes
;; C-c C-n for no
```

### Auto-Response Mode

```elisp
;; Toggle auto-response mode
M-x ecc-term-claude-auto-mode-toggle

;; Or use the keybinding:
;; C-c C-a
```

### Yank Output to Files

```elisp
;; Save region to file with prompt for filename
M-x ecc-vterm-yank-as-file
;; or C-c C-f

;; Save entire buffer to file
M-x ecc-vterm-yank-buffer-as-file
;; or C-c C-b

;; Quick save with auto-generated filename
M-x ecc-vterm-quick-yank-region
;; or C-c C-q
```

### Follow-Bottom Behavior

```elisp
;; Toggle follow-bottom scrolling
M-x ecc-term-claude-toggle-follow-bottom
;; or C-c C-v
```

## Improvements Over Previous Versions

The consolidated module offers several improvements:

1. **Clean Code Principles**:
   - Clear function responsibilities
   - Private vs. public API separation
   - Consistent naming conventions
   - Better organization

2. **Enhanced Documentation**:
   - Comprehensive docstrings
   - Clear section organization
   - Better function descriptions

3. **Improved Integration**:
   - Works with consolidated state detection
   - Works with consolidated auto-response system
   - Better buffer management

4. **New Features**:
   - Enhanced customization options
   - Better error handling
   - More consistent behavior

## Migration Guide

### From Original Mode

If you were using the original `ecc-term-claude-mode`:

1. Replace `require` statements:
   ```elisp
   ;; Old
   (require 'ecc-term-claude-mode)
   
   ;; New
   (require 'ecc-term-claude-mode-consolidated)
   ```

2. Function calls remain compatible through aliases

### From Improved Mode

If you were using the improved `ecc-term-claude-mode-improved`:

1. Replace `require` statements:
   ```elisp
   ;; Old
   (require 'ecc-term-claude-mode-improved)
   
   ;; New
   (require 'ecc-term-claude-mode-consolidated)
   ```

2. Update any internal function calls from `ecc-term-claude--function` to `ecc-term-claude--function`
   (Note: Most function names remain the same, just with a consolidated module)

## Testing

The consolidated module comes with comprehensive tests in `tests/ecc-vterm/test-ecc-term-claude-mode-consolidated.el` covering:

- Mode definition and customization
- Basic commands
- Auto-mode functionality
- Buffer setup and registration
- State detection and mode line integration
- Auto-response functions
- Follow-bottom functionality
- Backward compatibility
- Main user commands

## Conclusion

The consolidated term-claude mode provides a more maintainable, better organized, and more robust implementation of Claude interaction functionality. It maintains backward compatibility with previous versions while offering improved integration with other consolidated modules and following clean code principles.

By combining the best features of both previous implementations, it offers a superior experience for users interacting with Claude in Emacs.