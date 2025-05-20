# Term Claude Mode Consolidated

## Overview

The `ecc-term-claude-mode-consolidated.el` module provides a consolidated implementation of the Claude terminal interaction mode, combining the best features from multiple implementations while maintaining backward compatibility.

## Features

### Core Functionality
- **Dedicated Major Mode**: Derived from `vterm-mode` for Claude interaction
- **Auto-Response System**: Automatic handling of Claude prompts (Y/N, continue, etc.)
- **State Detection**: Integration with Claude's state detection system
- **Buffer Registration**: Manages Claude buffers and their features
- **Performance Optimizations**: Optimized for high-volume streaming output

### Enhanced Features
- **Debug Support**: Comprehensive debugging capabilities using consolidated debug utils
- **Frame Title Updates**: Shows Claude state in the frame title
- **Appearance Customization**: Control over font styles and appearance
- **Robust Error Handling**: Better error messages and recovery
- **Menu Integration**: Context menus with Claude-specific actions

### Compatibility Features
- **Backward Compatibility**: Full compatibility with existing code
- **Module Integration**: Works with both original and consolidated modules
- **Function Aliases**: Maintains old function names for compatibility

## Usage

### Basic Usage

```elisp
;; Create a new Claude vterm buffer
(ecc-term-claude)

;; Enable Claude features in existing vterm buffer
(ecc-term-claude-enable)

;; Toggle auto-response mode
(ecc-term-claude-auto-mode-toggle)
```

### Customization

The module provides extensive customization options:

```elisp
;; Appearance
(setq ecc-term-claude-line-numbers t)              ; Show line numbers
(setq ecc-term-claude-disable-bold t)              ; Disable bold fonts
(setq ecc-term-claude-disable-underline t)         ; Disable underlines

;; Performance
(setq ecc-term-claude-scroll-conservatively 5000)  ; Scroll behavior
(setq ecc-term-claude-truncate-lines nil)          ; Line wrapping

;; State detection
(setq ecc-term-claude-state-update-interval 0.5)   ; Faster updates
(setq ecc-term-claude-show-state-in-mode-line t)   ; Mode line indicator
(setq ecc-term-claude-update-frame-title t)        ; Frame title updates

;; Debug features
(setq ecc-term-claude-debug t)                     ; Enable debug output
(setq ecc-term-claude-debug-to-buffer t)           ; Debug to buffer
```

### Key Bindings

In Claude vterm mode, the following key bindings are available:

| Key Binding | Function | Description |
|-------------|----------|-------------|
| `C-c C-y` | `ecc-term-claude-yes` | Send 'y' response |
| `C-c C-n` | `ecc-term-claude-no` | Send 'n' response |
| `C-c C-l` | `ecc-term-claude-clear` | Clear buffer |
| `C-c C-a` | `ecc-term-claude-auto-mode-toggle` | Toggle auto-mode |
| `C-c C-v` | `ecc-term-claude-toggle-follow-bottom` | Toggle follow-bottom |
| `C-c C-d` | `ecc-term-claude-debug-toggle` | Toggle debug mode |
| `C-c C-f` | `ecc-vterm-yank-as-file` | Yank region to file |
| `C-c C-b` | `ecc-vterm-yank-buffer-as-file` | Yank buffer to file |
| `C-c C-q` | `ecc-vterm-quick-yank-region` | Quick yank region |

### Debug Features

The consolidated module includes enhanced debugging capabilities:

```elisp
;; Toggle debug mode
(ecc-term-claude-debug-toggle)

;; Show debug buffer
(ecc-term-claude-debug-show-buffer)

;; Enable debug messages to buffer instead of *Messages*
(setq ecc-term-claude-debug-to-buffer t)
```

Debug messages include:
- Buffer setup and registration
- State changes and timer events
- Auto-response actions
- Hook execution
- Error conditions

## Integration

### With Other Modules

The consolidated module integrates with:

- **State Detection**: Uses consolidated state detection when available
- **Auto Response**: Works with consolidated auto-response system
- **Debug Utils**: Uses consolidated debug utilities for enhanced logging
- **Variables**: Reads configuration from consolidated variables module

### Fallback Behavior

The module gracefully falls back to original modules if consolidated versions are not available:

```elisp
;; Prefers consolidated modules but falls back gracefully
(if (featurep 'ecc-state-detection-consolidated)
    (require 'ecc-state-detection-consolidated)
  (require 'ecc-state-detection))
```

## API Reference

### Main Functions

#### `ecc-term-claude ()`
Create a new Claude vterm buffer or apply Claude settings to existing vterm buffer.

#### `ecc-term-claude-enable ()`
Enable Claude features in the current vterm buffer without changing the major mode.

#### `ecc-term-claude-mode`
Major mode derived from `vterm-mode` for Claude interaction.

### Configuration Functions

#### `ecc-term-claude-auto-mode-toggle ()`
Toggle automatic response to Claude prompts.

#### `ecc-term-claude-toggle-follow-bottom ()`
Toggle automatic scrolling to bottom of buffer.

#### `ecc-term-claude-debug-toggle ()`
Toggle debug output for Claude vterm mode.

### Internal Functions

All internal functions use the `ecc-term-claude--` prefix for clarity:

- `ecc-term-claude--setup-buffer ()`
- `ecc-term-claude--register-buffer (buffer)`
- `ecc-term-claude--debug (format-string &rest args)`
- `ecc-term-claude--mode-line-state-indicator ()`

## Backward Compatibility

The module maintains full backward compatibility through function and variable aliases:

```elisp
;; Old function names still work
(ecc-register-buffer)                    ; → ecc-term-claude--register-buffer
(ecc-term-claude-setup-existing-buffer)  ; → ecc-term-claude--setup-existing-buffer

;; Old variable names still work
ecc-term-claude-update-functions         ; → ecc-term-claude--update-functions
ecc-term-claude-state-timer             ; → ecc-term-claude--state-timer
```

## Migration Guide

### From Original Module

No changes needed - the consolidated module provides the same interface with enhanced features.

### New Features Available

1. **Debug Support**: Use `ecc-term-claude-debug-toggle` for troubleshooting
2. **Frame Title Updates**: Enable with `ecc-term-claude-update-frame-title`
3. **Enhanced Error Handling**: Better error messages and recovery
4. **Appearance Control**: More customization options for font styles

## Testing

The module includes comprehensive tests covering:

- Mode definition and customization
- Debug functionality
- Command availability and execution
- Auto-mode functionality
- Buffer setup and registration
- State detection and mode line integration
- Frame title updates
- Auto-response functions
- Follow-bottom functionality
- Backward compatibility
- Error handling

Run tests with:

```bash
emacs -batch -L . -L src -L tests/ecc-term \
  -l tests/ecc-term/test-ecc-term-claude-mode-consolidated.el \
  -f ert-run-tests-batch-and-exit
```

## Implementation Details

### Clean Code Principles

The consolidated module follows clean code principles:

- **Single Responsibility**: Each function has a clear, single purpose
- **Meaningful Names**: Function and variable names clearly indicate their purpose
- **DRY (Don't Repeat Yourself)**: Common functionality is extracted into reusable functions
- **Error Handling**: Robust error handling with informative messages

### Module Organization

```
ecc-term-claude-mode-consolidated.el
├── Customization (user options)
├── Internal Variables (private state)
├── Mode Menu (context menu)
├── Keybindings (key mappings)
├── Debugging Functions (debug support)
├── Mode Definition (major mode)
├── Buffer Setup Functions (initialization)
├── State Detection (Claude state monitoring)
├── Claude Interaction Commands (user commands)
├── Auto-Response Functions (automatic responses)
├── Buffer Management (lifecycle management)
├── Follow Bottom Functionality (scrolling)
├── Public APIs (main interface)
└── Backward Compatibility (aliases)
```

### Integration Architecture

The module uses a layered approach for integration:

1. **Preference Layer**: Prefers consolidated modules when available
2. **Fallback Layer**: Falls back to original modules gracefully
3. **Compatibility Layer**: Maintains old interfaces through aliases
4. **Enhancement Layer**: Adds new features without breaking existing code

This architecture ensures maximum compatibility while providing enhanced functionality when consolidated modules are available.