# Consolidated Architecture Guide

## Overview

This document describes the consolidated architecture of the emacs-claude-code project. The project has undergone a consolidation process to reduce code duplication, improve maintainability, and enhance robustness.

## Consolidated Modules

The following modules have been consolidated:

### Core Modules

1. **ecc-variables-consolidated.el**
   - Centralized variables and customization options
   - Replaces: ecc-variables.el

2. **ecc-debug-utils-consolidated.el**
   - Comprehensive debugging utilities
   - Replaces: ecc-debug-utils.el

### Functional Modules

3. **ecc-state-detection-consolidated.el**
   - Enhanced state detection for Claude prompts
   - Replaces: ecc-state-detection.el, ecc-state-detection-improved.el

4. **ecc-auto-notify-consolidated.el**
   - Robust notification system for Claude prompts
   - Replaces: ecc-auto-notify.el, ecc-auto-notify-improved.el, ecc-auto-notify-fix.el

### User Interface Modules

5. **ecc-term-claude-mode-consolidated.el**
   - Optimized vterm mode for Claude interaction
   - Replaces: ecc-term-claude-mode.el, ecc-term-claude-mode-improved.el, ecc-term-claude-mode-v2.el

## How to Use Consolidated Modules

The main package file `emacs-claude-code.el` has been updated to use consolidated modules when available, with fallbacks to the original versions:

```elisp
;; Example pattern
(if (locate-library "ecc-module-consolidated")
    (require 'ecc-module-consolidated)
  (require 'ecc-module))
```

### Direct Usage

To directly use consolidated modules:

```elisp
(require 'ecc-variables-consolidated)
(require 'ecc-debug-utils-consolidated)
(require 'ecc-state-detection-consolidated)
(require 'ecc-auto-notify-consolidated)
(require 'ecc-term-claude-mode-consolidated)
```

### Backward Compatibility

All consolidated modules provide backward compatibility by also providing the original module name. This means existing code that requires the original module name will continue to work:

```elisp
;; This still works with consolidated modules
(require 'ecc-variables)
(require 'ecc-debug-utils)
(require 'ecc-state-detection)
(require 'ecc-auto-notify)
(require 'ecc-term-claude-mode)
```

## Module Dependencies

```
ecc-variables-consolidated
    |
    +--> ecc-debug-utils-consolidated
    |        |
    |        v
    +--> ecc-state-detection-consolidated
    |        |
    |        v
    +--> ecc-auto-notify-consolidated
             |
             v
ecc-term-claude-mode-consolidated
```

## Consolidated vs. Original

### Advantages of Consolidated Modules

1. **Enhanced Functionality**: More robust features and error handling
2. **Better Documentation**: Comprehensive docstrings and comments
3. **Improved Performance**: Optimized for better performance
4. **Consistent API**: Clean, consistent, and well-documented API
5. **Comprehensive Testing**: More thorough test coverage

### Backward Compatibility

Each consolidated module maintains backward compatibility through:

1. **Function Aliases**: All original function names still work
2. **Variable Aliases**: All original variable names still work
3. **Module Provides**: Both consolidated and original module names are provided

## Migration Guide

### For Package Users

You don't need to change anything! The package automatically uses consolidated modules when available.

### For Package Developers

1. **Prefer Consolidated Modules**: Use the consolidated version when available:

```elisp
(if (locate-library "ecc-module-consolidated")
    (require 'ecc-module-consolidated)
  (require 'ecc-module))
```

2. **Use Consolidated Functions**: Prefer functions from consolidated modules:

```elisp
;; Preferred
(ecc-debug-message "Important debug info")

;; Instead of
(ecc-debug "Important debug info")
```

## Future Development

All new development should focus on consolidated modules:

1. **Bug Fixes**: Apply to consolidated modules
2. **New Features**: Add to consolidated modules
3. **Documentation**: Update for consolidated modules
4. **Tests**: Write for consolidated modules