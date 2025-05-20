# Naming Convention Standardization

## Current Naming Patterns

The term-claude mode module and related files currently use several different naming patterns:

1. **ecc-term-claude-**: The main prefix for term-claude mode functions
   - Example: `ecc-term-claude-mode`, `ecc-term-claude-yes`, `ecc-term-claude-setup-mode-line`

2. **ecc-vterm-**: Used for vterm-related functions that aren't strictly Claude-specific
   - Example: `ecc-vterm-yank-as-file`, `ecc-vterm-follow-bottom-margin`

3. **ecc-**: Generic prefix used for various functions across modules
   - Example: `ecc-register-buffer`, `ecc-detect-simple-state`

4. **ecc-term-visual-aid-**: Used for visual enhancement functions
   - Example: `ecc-term-visual-aid-update`, `ecc-term-visual-aid-add-frame`

5. **ecc-detect-**: Used for state detection functions
   - Example: `ecc-detect-simple-state`, `ecc-detect-prompt-in-last-lines`

## Problems with Current Naming

1. **Inconsistent Namespacing**: Functions with similar purposes use different prefixes
2. **Unclear Module Boundaries**: Hard to determine which file a function belongs to
3. **Ambiguous Terminology**: Mixed use of "term" and "vterm" is confusing
4. **Inconsistent Verb Usage**: Some functions use imperative verbs, others don't

## Proposed Naming Convention

### Core Namespaces

1. **ecc-term-claude-**: Primary namespace for Claude vterm mode functions
   - Used for functions in `ecc-term-claude-mode.el`
   - Pertains to the major mode and its core functionality

2. **ecc-vterm-**: Namespace for general vterm enhancements
   - Used for functions in vterm-related utilities like `ecc-vterm-yank-as-file.el`
   - Functions that could be useful outside the Claude context

3. **ecc-term-visual-**: Namespace for visual enhancements
   - Simplifies from `ecc-term-visual-aid-` to be more consistent
   - Used for functions in `ecc-term-visual-aid.el`

4. **ecc-state-**: Namespace for state detection functions
   - Used for functions in `ecc-state-detection.el`
   - Clarifies the primary purpose is state management

### Function Naming Patterns

1. **Setup Functions**: Use `setup-` prefix
   - Example: `ecc-term-claude-setup-mode-line`, `ecc-term-claude-setup-hooks`

2. **Toggle Functions**: Use `toggle-` prefix
   - Example: `ecc-term-claude-toggle-auto-mode`, `ecc-term-visual-toggle`

3. **Action Functions**: Use imperative verbs
   - Example: `ecc-term-claude-send-yes`, `ecc-vterm-yank-as-file`

4. **Query Functions**: Use `get-` prefix for information retrieval
   - Example: `ecc-term-claude-get-state`, `ecc-state-get-name`

5. **Check Functions**: Use `check-` prefix for boolean tests
   - Example: `ecc-term-claude-check-state`, `ecc-state-check-throttle`

6. **Utility Functions**: Use descriptive noun phrases
   - Example: `ecc-vterm-file-type-detection`, `ecc-term-claude-mode-line-indicator`

## Implementation Plan

### 1. Function Renaming

Below is a mapping of functions that should be renamed for consistency:

| Current Function | Proposed Function | Reason |
|------------------|------------------|--------|
| `ecc-term-claude-auto-mode-toggle` | `ecc-term-claude-toggle-auto-mode` | Consistent toggle pattern |
| `ecc-detect-simple-state` | `ecc-state-detect` | Move to state namespace |
| `ecc-detect-prompt-in-last-lines` | `ecc-state-detect-in-lines` | Move to state namespace |
| `ecc-term-claude-yes` | `ecc-term-claude-send-yes` | Use imperative verb |
| `ecc-term-claude-no` | `ecc-term-claude-send-no` | Use imperative verb |
| `ecc-term-visual-aid-toggle` | `ecc-term-visual-toggle` | Simplify namespace |
| `ecc-term-visual-aid-update` | `ecc-term-visual-update` | Simplify namespace |
| `ecc-register-buffer` | `ecc-term-claude-register-buffer` | Clarify namespace |

### 2. Backward Compatibility

For backward compatibility, we'll provide aliases for all renamed functions:

```elisp
(define-obsolete-function-alias 'ecc-term-claude-auto-mode-toggle
  'ecc-term-claude-toggle-auto-mode "May 2025")

(define-obsolete-function-alias 'ecc-detect-simple-state
  'ecc-state-detect "May 2025")

;; Additional aliases...
```

### 3. Documentation Updates

Update all documentation to reflect the new naming conventions:

1. Update docstrings to reference new function names
2. Update comments that reference old function names
3. Update README and other documentation

### 4. Variable Naming

Apply similar conventions to variable names:

1. **Mode-specific variables**: Use `ecc-term-claude-` prefix
   - Example: `ecc-term-claude-auto-mode`, `ecc-term-claude-scroll-conservatively`

2. **Visual enhancement variables**: Use `ecc-term-visual-` prefix
   - Example: `ecc-term-visual-frame-width` (was `ecc-term-visual-aid-frame-width`)

3. **State detection variables**: Use `ecc-state-` prefix
   - Example: `ecc-state-prompt-y/n` (was `ecc-state-prompt-y/n`)

## Benefits of Standardized Naming

1. **Improved Code Navigation**: Easier to find related functions
2. **Clearer Module Boundaries**: Function names indicate which module they belong to
3. **Better Discoverability**: Consistent prefixes make auto-completion more effective
4. **Self-documenting Code**: Function names clearly indicate their purpose and behavior

## Additional Considerations

1. **Future Development**: New functions should follow these conventions
2. **External APIs**: Be cautious when renaming functions used by external code
3. **Documentation**: Provide a naming guide for future contributors

## Conclusion

By standardizing naming conventions, we'll make the codebase more maintainable, readable, and self-documenting. The clear namespace boundaries will make it easier to understand how the different modules interact, and the consistent verb usage will make function purposes more obvious.