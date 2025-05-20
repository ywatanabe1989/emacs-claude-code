# Naming Conventions for Emacs Claude Code

## Overview

This document defines the standard naming conventions for the emacs-claude-code project. Following these guidelines will ensure consistency across the codebase and make it easier to understand and maintain.

## File Organization

### File Names
- All source files should use the prefix `ecc-` (for "emacs-claude-code")
- Filenames should reflect the main functionality they provide
- Use hyphens to separate words in filenames
- Examples: `ecc-auto-response.el`, `ecc-term-claude-mode.el`

### Directory Structure
- Source files should be placed in `/src/` with subdirectories matching module organization:
  - `/src/ecc-auto/` for auto-response related files
  - `/src/ecc-state/` for state detection related files
  - `/src/ecc-term/` for terminal mode related files
  - `/src/ecc-vterm/` for vterm integration files

- Test files should be placed in `/tests/` with matching subdirectories:
  - `/tests/ecc-auto/` for auto-response tests
  - `/tests/ecc-state/` for state detection tests
  - etc.

## Function Naming

### Public Functions
- All public functions should use the `ecc-` prefix
- Follow the pattern `ecc-[module]-[action]`
- Interactive functions should be marked with `###autoload`
- Examples:
  - `ecc-auto-response-send`
  - `ecc-vterm-yank-as-file`
  - `ecc-term-claude-setup`

### Private Functions
- Internal/private functions should use a double-dash `--` after the module name
- Follow the pattern `ecc-[module]--[action]`
- Examples:
  - `ecc-auto--send-response`
  - `ecc-term--setup-keys`
  - `ecc-vterm--detect-file-type`

### Toggle Functions
- Functions that toggle features should use the verb `toggle`
- Follow the pattern `ecc-[module]-toggle-[feature]`
- Examples:
  - `ecc-auto-toggle-response`
  - `ecc-term-toggle-follow-bottom`

## Variable Naming

### Customization Variables
- User-customizable variables should use `defcustom`
- Follow the pattern `ecc-[module]-[descriptor]`
- Always include `:type` and `:group` specifications
- Examples:
  - `ecc-term-claude-line-numbers`
  - `ecc-vterm-yank-default-dir`

### Internal Variables
- Internal variables should use `defvar`
- Follow the pattern `ecc-[module]-[descriptor]`
- Always include meaningful documentation strings
- Examples:
  - `ecc-auto-response-timer`
  - `ecc-vterm-follow-bottom-margin`

### Flags and State Variables
- Boolean variables should have names that suggest true/false values
- Use `enabled`, `active`, or similar adjectives
- Examples:
  - `ecc-buffer-auto-response-enabled`
  - `ecc-vterm-always-follow-bottom`

### Constants
- Constants (rarely changed values) should use `defconst`
- Use the same naming pattern as variables
- Examples:
  - `ecc-auto-default-response-delay`

## Customization Groups

- Groups should follow a consistent pattern using `defgroup`
- Use the pattern `ecc-[module]`
- Examples:
  - `ecc-state-detect`
  - `ecc-term-claude`
  - `ecc-vterm-yank`

## Docstrings

### Function Docstrings
- All functions should have clear docstrings
- For functions with parameters, document each parameter
- Include usage examples for interactive functions
- Follow the format:
  ```elisp
  (defun ecc-module-action (param1 param2)
    "One-line description of what the function does.
  More detailed explanation if needed.
  
  PARAM1 is used for ...
  PARAM2 is used for ...
  
  Returns ..."
    ...)
  ```

### Variable Docstrings
- All variables should have clear docstrings
- For customization variables, include example settings
- Follow the format:
  ```elisp
  (defvar ecc-module-name nil
    "One-line description of what the variable represents.
  More detailed explanation if needed.
  
  Example: ...")
  ```

## Mode Definitions

- Mode functions should be named `ecc-[name]-mode`
- Mode maps should be named `ecc-[name]-mode-map`
- Mode hooks should be named `ecc-[name]-mode-hook`
- Examples:
  - `ecc-term-claude-mode`
  - `ecc-term-claude-mode-map`
  - `ecc-term-claude-mode-hook`

## Commands

- Interactive commands should have descriptive names
- Prefer verb-noun structure for commands
- Examples:
  - `ecc-vterm-yank-as-file`
  - `ecc-term-claude-clear`
  - `ecc-auto-response-continue`

## Prefixes to Avoid

- Avoid `claude-` prefix, use `ecc-` consistently
- Avoid redundant module information in function names
  - Bad: `ecc-term-claude-auto-send-y/n`
  - Good: `ecc-term-send-yes`

## Examples of Correct Naming

### Files
```
src/ecc-auto/ecc-auto-response.el
src/ecc-term/ecc-term-claude-mode.el
src/ecc-vterm/ecc-vterm-yank-as-file.el
```

### Functions
```elisp
(defun ecc-auto-response-send (buffer &optional state) ...)
(defun ecc-auto--send-response (buffer response type) ...)
(defun ecc-term-claude-toggle-follow-bottom () ...)
```

### Variables
```elisp
(defcustom ecc-term-claude-line-numbers nil
  "Whether to display line numbers in Claude vterm buffers."
  :type 'boolean
  :group 'ecc-term-claude)

(defvar ecc-vterm-always-follow-bottom t
  "Whether to always follow bottom in vterm buffers.")
```