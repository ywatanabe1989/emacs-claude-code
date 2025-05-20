# Consolidated Modules Documentation

## Overview

This document describes the consolidated modules that form the core of the emacs-claude-code package. These modules have been refactored following clean code principles to ensure maintainability, clarity, and minimal duplication.

## Latest Updates

### Term Claude Mode Consolidated (May 21, 2025)

Added the consolidated implementation of the term-claude-mode module:

- **File**: `src/ecc-term-claude-mode-consolidated.el`
- **Tests**: `tests/ecc-term/test-ecc-term-claude-mode-consolidated.el`
- **Documentation**: `docs/term-claude-mode-consolidated.md`
- **Status**: ✅ Complete with comprehensive test coverage

## Module Dependency Structure

The dependency structure is designed to avoid circular dependencies and maintain a clear hierarchy:

```
ecc-variables
    |
    +--> ecc-debug-utils-consolidated
    |        |
    |        v
    +--> ecc-state-detection
    |        |
    |        v
    +--> ecc-auto-core
    |        |
    |        v
    +--> ecc-auto-response
    |        |
    |        v
    +--> ecc-term-claude-mode-consolidated
```

### Core Modules

These modules form the foundation of the system and have minimal dependencies:

1. **ecc-variables** (No dependencies)
   - Core configuration variables and customization options
   - Central location for all user settings
   - Used by all other modules

2. **ecc-debug-utils-consolidated** (Depends on: ecc-variables)
   - Comprehensive debugging utilities
   - Buffer-local and global debugging support
   - Debugging categories and logging functionality
   - Module-specific debug functions
   - Consistent interface with backward compatibility

### Functional Modules

These modules provide key functionality and depend on core modules:

3. **ecc-state-detection** (Depends on: ecc-variables)
   - Claude prompt state detection
   - Detects Y/N, Y/Y/N, waiting, and initial-waiting states
   - Used by auto-response and notification systems

4. **ecc-auto-core** (Depends on: ecc-variables, ecc-state-detection, ecc-debug-utils)
   - Core infrastructure for auto-response functionality
   - Timer management, buffer tracking, and state monitoring
   - Used by higher-level modules like auto-response and notification

### Higher-Level Modules

These modules build on the core and functional modules to provide user-facing features:

5. **ecc-auto-response** (Higher-level module)
   - Auto-response functionality for Claude prompts
   - User commands for managing auto-response
   - Buffer-specific auto-response configuration

6. **ecc-auto-notify** (Higher-level module)
   - Notification system for Claude prompts and state changes
   - Visual feedback when Claude needs attention
   - Integration with auto-response system

7. **ecc-term-claude-mode-consolidated** (High-level module)
   - Main user interface for Claude interaction through vterm
   - Optimized terminal mode for Claude AI interaction
   - Integration with state detection, auto-response, and debugging systems
   - Performance optimizations for high-volume streaming output
   - Visual indicators for Claude state in the mode line
   - Follow-bottom scrolling for real-time interaction
   - Support for yank-as-file and other utility functions

## Core Module APIs

### ecc-variables

This module defines variables and customization options used throughout the package.

```elisp
;; Customization groups
(defgroup emacs-claude-code nil ...)
(defgroup ecc-buffers nil ...)
(defgroup ecc-auto-response nil ...)
(defgroup ecc-state-detection nil ...)
(defgroup ecc-vterm nil ...)
(defgroup ecc-notification nil ...)
(defgroup ecc-debug nil ...)

;; Key user-configurable options
(defcustom ecc-auto-response-y/n "1" ...)
(defcustom ecc-auto-response-y/y/n "2" ...)
(defcustom ecc-auto-response-waiting "/auto" ...)
(defcustom ecc-auto-response-initial-waiting "/user:understand-guidelines" ...)

;; State detection patterns
(defcustom ecc-state-prompt-y/n "❯ 1. Yes" ...)
(defcustom ecc-state-prompt-y/y/n " 2. Yes, and" ...)
(defcustom ecc-state-prompt-waiting "│ >                            " ...)
(defcustom ecc-state-prompt-initial-waiting "│ > Try " ...)
```

### ecc-debug-utils-consolidated

This consolidated module provides comprehensive debugging utilities for the entire package, with consistent naming, formatting, and extensive documentation.

```elisp
;; Main debug functions
(defun ecc-debug-message (format-string &rest args) ...)
(defun ecc-debug-message-category (category format-string &rest args) ...)
(defun ecc-debug-buffer-message (buffer format-string &rest args) ...)
(defun ecc-debug-buffer-message-category (buffer category format-string &rest args) ...)

;; Debug function factory
(defun ecc-debug-make-debug-fn (&optional buffer category) ...)

;; Debug control
(defun ecc-debug-toggle-global () ...)
(defun ecc-debug-toggle-buffer (&optional buffer) ...)
(defun ecc-debug-toggle-category (category) ...)
(defun ecc-debug-enable-all-categories () ...)

;; Debug information
(defun ecc-debug-print-state-info (&optional buffer) ...)
(defun ecc-debug-view-log () ...)
(defun ecc-debug-clear-log () ...)

;; Module-specific debug utilities
(defun ecc-debug-auto-response (format-string &rest args) ...)
(defun ecc-debug-state (format-string &rest args) ...)
(defun ecc-debug-core (format-string &rest args) ...)
(defun ecc-debug-buffer (format-string &rest args) ...)
(defun ecc-debug-vterm (format-string &rest args) ...)

;; Backward compatibility functions (aliases)
(defalias 'ecc-debug-utils-make-debug-fn 'ecc-debug-make-debug-fn ...)
(defalias 'ecc-debug-utils-message 'ecc-debug-message ...)
(defalias 'ecc-debug-utils-print-state-info 'ecc-debug-print-state-info ...)
(defalias 'ecc-toggle-debug 'ecc-debug-toggle-global ...)
```

### ecc-state-detection

This module provides Claude prompt state detection functionality.

```elisp
;; Core state detection
(defun ecc-detect-state (&optional buffer) ...)
(defun ecc-detect-basic-state () ...)
(defun ecc-detect-prompt-in-last-lines (&optional n-lines) ...)
(defun ecc-detect-prompt-in-region (start end) ...)

;; Utility functions
(defun ecc-state-get-name (state) ...)
(defun ecc-state-symbols () ...)

;; Notification interface
(defun ecc-state-notify-if-prompt-detected (buffer) ...)
```

### ecc-auto-core

This module provides the infrastructure for the auto-response system.

```elisp
;; Timer management
(defun ecc-auto-core-timer-active-p () ...)
(defun ecc-auto-core-timer-start (callback) ...)
(defun ecc-auto-core-timer-stop () ...)

;; State tracking
(defun ecc-auto-core-update-state (state) ...)
(defun ecc-auto-core-throttled-p (state) ...)
(defun ecc-auto-core-reset-state () ...)

;; Buffer management
(defun ecc-auto-core-register-buffer (buffer) ...)
(defun ecc-auto-core-unregister-buffer (buffer) ...)
(defun ecc-auto-core-registered-buffers () ...)
(defun ecc-auto-core-cleanup-buffers () ...)

;; Processing
(defun ecc-auto-core-process-buffer (buffer callback) ...)
(defun ecc-auto-core-initial-check (buffer callback) ...)
(defun ecc-auto-core-process-all-buffers (callback) ...)

;; Lifecycle
(defun ecc-auto-core-initialize () ...)
(defun ecc-auto-core-shutdown () ...)
```

### ecc-term-claude-mode-consolidated

This module provides a comprehensive vterm mode optimized for Claude AI interaction.

```elisp
;; Major mode definition
(define-derived-mode ecc-term-claude-mode vterm-mode "Claude-VTerm" ...)

;; Main user commands
(defun ecc-term-claude () ...)
(defun ecc-term-claude-enable () ...)
(defun ecc-term-claude-yes () ...)
(defun ecc-term-claude-no () ...)
(defun ecc-term-claude-clear () ...)
(defun ecc-term-claude-auto-mode-toggle () ...)
(defun ecc-term-claude-toggle-follow-bottom () ...)

;; Core functionality
(defun ecc-term-claude--setup-buffer () ...)
(defun ecc-term-claude--setup-display-options () ...)
(defun ecc-term-claude--setup-performance-options () ...)
(defun ecc-term-claude--setup-mode-line () ...)
(defun ecc-term-claude--setup-hooks () ...)
(defun ecc-term-claude--register-buffer (buffer) ...)

;; State detection integration
(defun ecc-term-claude--mode-line-state-indicator () ...)
(defun ecc-term-claude-check-state () ...)

;; Auto-response integration
(defun ecc-term-claude--auto-send-respond () ...)
(defun ecc-term-claude--auto-send-y/n () ...)
(defun ecc-term-claude--auto-send-y/y/n () ...)
(defun ecc-term-claude--auto-send-continue () ...)
(defun ecc-term-claude--auto-send-initial-waiting () ...)

;; Follow-bottom scrolling
(defun ecc-term-claude-follow-bottom-after-output () ...)
(defun ecc-term-claude-scroll-to-bottom () ...)

;; Backward compatibility
(defalias 'ecc-register-buffer 'ecc-term-claude--register-buffer ...)
(defalias 'ecc-term-claude-auto-send-accept 'ecc-term-claude--auto-send-respond ...)
```

## Usage Examples

### Setting up Auto-Response

```elisp
;; Configure response settings
(setq ecc-auto-response-y/n "1")
(setq ecc-auto-response-y/y/n "2")
(setq ecc-auto-response-waiting "/auto")
(setq ecc-auto-response-initial-waiting "/user:understand-guidelines")

;; Start auto-response in the current buffer
(ecc-auto-response-start)

;; Stop auto-response
(ecc-auto-response-stop)

;; Toggle auto-response
(ecc-auto-response-toggle)
```

### Debugging

```elisp
;; Enable global debugging
(ecc-debug-toggle-global)

;; Enable buffer-specific debugging
(ecc-debug-toggle-buffer)

;; Enable specific debug categories
(ecc-debug-toggle-category 'auto-response)
(ecc-debug-toggle-category 'state)

;; View debug information
(ecc-debug-print-state-info)
(ecc-debug-view-log)
```

### Using Term Claude Mode

```elisp
;; Create new Claude vterm buffer
(ecc-term-claude)

;; Enable Claude features in an existing vterm buffer
(ecc-term-claude-enable)

;; Toggle auto-response mode
(ecc-term-claude-auto-mode-toggle)

;; Manual responses
(ecc-term-claude-yes)
(ecc-term-claude-no)

;; Toggle follow-bottom scrolling
(ecc-term-claude-toggle-follow-bottom)

;; Debug operations
(ecc-debug-toggle-buffer)
(ecc-debug-print-state-info)
```

### Manual State Detection

```elisp
;; Check current buffer state
(ecc-detect-state)

;; Check specific buffer state
(ecc-detect-state (get-buffer "*CLAUDE-VTERM-1*"))

;; Check state in specific region
(ecc-detect-prompt-in-region (point-min) (point-max))
```

## Testing

To run tests for the consolidated modules:

```elisp
;; Run a specific test
(ert-run-tests-interactively "test-ecc-variables-customization-groups")

;; Run all tests
(ert-run-tests-interactively "^test-ecc-")

;; Run test function
(ecc-test-run-all-consolidated-tests)
```

## Extension Points

The consolidated modules have several key extension points:

1. **Custom State Detection**: Add custom prompt detection by modifying prompt patterns in `ecc-variables`.

2. **Auto-Response Callbacks**: Create custom callbacks to handle different Claude states.

3. **Debug Categories**: Define new debug categories for specific modules or features.

4. **Buffer Processing**: Create custom buffer processing by registering buffers and providing callbacks.

## Best Practices

1. **Module Dependencies**: Maintain the dependency hierarchy to avoid circular dependencies.

2. **Error Handling**: Always check buffer liveness before operating on buffers.

3. **Throttling**: Use the built-in throttling mechanism to prevent response flooding.

4. **Debugging**: Use appropriate debug categories to make logs easier to filter.

5. **Testing**: Add tests for any new functionality to ensure it works with the rest of the system.

## Migration from Old Modules

The consolidated modules maintain backward compatibility through aliases:

```elisp
;; Old function calls still work
(ecc-toggle-debug)          ; -> ecc-debug-toggle-global
(ecc-detect-prompt-state)   ; -> ecc-detect-state
(ecc-auto--get-timer)       ; -> ecc-auto-core-timer-active-p
```

## Future Improvements

1. **Buffer-Local Configuration**: Expand buffer-local customization options.

2. **Enhanced State Detection**: Add more state types and detection methods.

3. **Performance Optimization**: Optimize state detection for very large buffers.

4. **UI Integration**: Improve integration with Emacs UI components.