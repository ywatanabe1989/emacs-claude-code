# Emacs Claude Code Variables System

## Overview

The Emacs Claude Code variables system provides a centralized location for all configuration values and runtime state used throughout the package. It's designed to support both customization by users and efficient state management for the application.

## Architecture

The variables system is organized into several key components:

1. **Customization Groups** - Organized hierarchy of user-configurable settings
2. **Buffer-Local Variables** - Per-buffer state management
3. **Global State Variables** - System-wide settings and state
4. **Constants** - Fixed values used throughout the system

## Customization Groups

The variables are organized into the following customization groups:

- `emacs-claude-code` - Main configuration group
  - `ecc-buffers` - Buffer management and naming settings
  - `ecc-auto-response` - Auto-response system settings
  - `ecc-state-detection` - Claude prompt detection settings
  - `ecc-vterm` - VTerm integration settings
  - `ecc-notification` - Notification system settings

Users can customize these settings via:
- M-x customize-group RET emacs-claude-code RET
- Direct assignment in their init.el file

## Key Variable Categories

### Buffer Management

Variables for managing Claude buffers, including:
- Buffer naming patterns
- Buffer registration
- Current buffer tracking
- Buffer properties

```elisp
;; Example buffer variables
(defcustom ecc-buffer-prefix "*CLAUDE-VTERM-"
  "Prefix for Claude vterm buffer names."
  :type 'string
  :group 'ecc-buffers)

(defvar ecc-buffer-registered-buffers-alist nil
  "Alist of registered Claude buffers and their properties.")
```

### Auto-Response System

Variables controlling how the system automatically responds to Claude prompts:
- Response texts for different prompt types
- Throttling and timing settings
- State tracking

```elisp
;; Example auto-response variables
(defcustom ecc-auto-response-throttle-time 5.0
  "Minimum seconds between auto-responses to the same state."
  :type 'float
  :group 'ecc-auto-response)

(defcustom ecc-auto-response-waiting "/auto"
  "Response to send for waiting state."
  :type 'string
  :group 'ecc-auto-response)
```

### State Detection

Variables for detecting Claude's current state:
- Prompt patterns
- Buffer size settings for detection
- Alternative detection patterns

```elisp
;; Example state detection variables
(defcustom ecc-state-prompt-y/n "❯ 1. Yes"
  "Y/n prompt pattern to match when Claude asks for confirmation."
  :type 'string
  :group 'ecc-state-detection)

(defcustom ecc-state-detection-buffer-size 2000
  "Number of characters to check from the end of buffer for prompt detection."
  :type 'integer
  :group 'ecc-state-detection)
```

### VTerm Integration

Variables for VTerm-specific behavior:
- Visual behavior settings
- Terminal output processing

```elisp
;; Example VTerm variables
(defcustom ecc-vterm-always-follow-bottom t
  "Whether to always follow bottom in vterm buffers."
  :type 'boolean
  :group 'ecc-vterm)
```

### Notification System

Variables controlling user notifications:
- Notification types
- Visual and auditory settings

```elisp
;; Example notification variables
(defcustom ecc-auto-notify-on-claude-prompt t
  "Whether to notify when claude asks for user response."
  :type 'boolean
  :group 'ecc-notification)
```

## Backward Compatibility

The variables system includes a compatibility layer to ensure that code depending on the old variables structure continues to work without modification. This is implemented through the `ecc-variables.el` file, which loads `ecc-variables-refactored.el` and provides the same feature name.

## Best Practices

When working with the variables system:

1. **Use defcustom for user options** - Any setting that users might want to change should be defined with `defcustom` and assigned to the appropriate customization group.

2. **Use defvar for internal state** - Variables used for internal state tracking should use `defvar` and include comprehensive documentation.

3. **Use buffer-local variables for buffer state** - When a variable relates to the state of a specific buffer, make it buffer-local using `defvar-local` or `make-local-variable`.

4. **Document default values** - Always provide meaningful default values and document their significance.

5. **Provide type information** - When using `defcustom`, always specify the appropriate `:type` to ensure proper customization interface.

## Testing

The variables system is thoroughly tested through:

1. **Basic Loading Tests** - Ensuring all modules load properly
2. **Variable Existence Tests** - Checking that all required variables exist
3. **Type and Value Tests** - Verifying variables have correct types and reasonable values
4. **Backward Compatibility Tests** - Ensuring the compatibility layer works seamlessly

Tests can be run using:

```bash
emacs -Q --batch --eval "(add-to-list 'load-path \"/path/to/emacs-claude-code/src\")" \
  --eval "(add-to-list 'load-path \"/path/to/emacs-claude-code/tests\")" \
  --eval "(require 'ert)" \
  --load "tests/test-ecc-variables-refactored.el" \
  --load "tests/test-ecc-variables-compatibility.el" \
  --eval "(ert-run-tests-batch-and-exit)"
```

## Future Improvements

Planned enhancements to the variables system:

1. **Complete migration** - Gradually update all references to use `ecc-variables-refactored` directly
2. **Enhanced validation** - Add validation for variable values to prevent misconfigurations
3. **Improved documentation** - Expand documentation with more usage examples
4. **Serialization** - Add support for saving and loading variable states