# Auto-Notify Consolidation Plan

## Overview

This document outlines the plan for consolidating the auto-notify modules in the emacs-claude-code project. Building on the successful consolidation of the auto-response module, we will now follow a similar approach for the notification system.

## Current State Analysis

We currently have multiple versions of the auto-notify module:

1. **ecc-auto-notify.el** - Original implementation with basic notification functionality
2. **ecc-auto-notify-improved.el** - Enhanced documentation and minor improvements
3. **ecc-auto-notify-fix.el** - Bug fixes and compatibility with new state detection module
4. **ecc-auto-notify-consolidated.el** - Previous consolidation attempt

## Consolidation Strategy

We will create a single, comprehensive auto-notify module with the following characteristics:

### 1. Core Features to Include

- All notification methods (bell, flash, message)
- Support for different bell methods (audible, visible, both, external)
- Throttling mechanisms to prevent excessive notifications
- Support for both global and buffer-local configurations
- Clean integration with state detection system
- Debug messaging for troubleshooting
- Clear customization options with good documentation

### 2. Implementation Approach

1. **Start with ecc-auto-notify-consolidated.el** as the foundation
2. **Enhance documentation** with detailed docstrings and examples
3. **Improve buffer-local support** with clear naming and interface
4. **Add improved error handling** for robustness
5. **Ensure backward compatibility** with older usage patterns
6. **Streamline dependencies** for better module organization

### 3. API Structure

```elisp
;; Core functionality
(ecc-auto-notify-check-state state)                ; Check state and notify if needed
(ecc-auto-notify-prompt type)                      ; Notify about prompt type
(ecc-auto-notify-ring-bell)                        ; Ring the bell using configured method
(ecc-auto-notify-flash-mode-line)                  ; Flash the mode line

;; User commands
(ecc-auto-notify-toggle)                           ; Toggle notifications on/off
(ecc-auto-notify-toggle-bell)                      ; Toggle bell notifications
(ecc-auto-notify-toggle-flash)                     ; Toggle flash notifications

;; Buffer setup
(ecc-auto-notify-setup-for-buffer)                 ; Set up notifications for buffer
(ecc-auto-notify-setup-hooks)                      ; Set up global notification hooks
(ecc-auto-notify-buffer-change-handler)            ; Handler for buffer content changes

;; Buffer-local functionality
(ecc-auto-notify-buffer-local-init)                ; Initialize buffer-local settings
(ecc-auto-notify-buffer-local-toggle)              ; Toggle buffer-local notifications
(ecc-auto-notify-check-unified)                    ; Check using appropriate settings
```

## Implementation Plan

### 1. Prepare

- Review all versions for unique features
- Identify any interdependencies with other modules
- Create a test plan

### 2. Consolidate Core Functionality

- Implement all notification methods
- Ensure proper integration with state detection
- Add comprehensive error handling and debug output

### 3. Enhance Buffer-Local Support

- Implement buffer-local variables with proper documentation
- Create toggle functions for buffer-local settings
- Ensure clean interaction between global and buffer-local modes

### 4. Documentation and Testing

- Add detailed docstrings for all functions
- Include examples in commentary section
- Document all customization options
- Implement comprehensive tests

### 5. Backward Compatibility

- Add aliases for old function names
- Ensure compatibility with older usage patterns
- Add multiple provide statements for different module names

## Testing Strategy

We will implement a comprehensive testing strategy:

1. **Unit tests**: Each notification function will be tested individually
2. **Integration tests**: Test interactions with state detection module
3. **Backward compatibility tests**: Verify legacy code works
4. **Edge case tests**: Test throttling and error conditions
5. **Performance tests**: Verify efficiency with multiple buffers

## Implementation Timeline

1. **Core Consolidation**: 1 day
2. **Documentation and Testing**: 1 day
3. **Review and Final Tweaks**: 1 day

## Success Criteria

1. **No duplicated code**: All functionality unified in a single module
2. **100% backward compatibility**: All existing use cases still work
3. **Complete documentation**: All features and APIs documented
4. **High test coverage**: All functionality verified by tests
5. **Clean modularization**: Clear interfaces with other modules

## Next Steps

After completing the auto-notify consolidation, we will:

1. Continue with the next modules following the comprehensive cleanup plan
2. Update documentation to reflect the consolidation
3. Update tests to work with the consolidated modules