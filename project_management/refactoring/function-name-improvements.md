# Function Name Improvements for Emacs Claude Code

This document outlines the function naming improvements to enhance code readability and consistency.

## Auto-Response Module

| Original Function Name | New Function Name | Reason for Change |
|------------------------|-------------------|-------------------|
| `ecc-auto-accept-send` | `ecc-auto-response-send-current-buffer` | More descriptive, matches the pattern of related functions |
| `ecc-auto--send-response` | `ecc-auto-response-send-message` | Removed private prefix (`--`) and made name more explicit |
| `ecc-auto--send-vterm-response` | `ecc-auto-response-send-to-vterm` | Removed private prefix and made purpose clearer |
| `ecc-auto--notify` | `ecc-auto-response-display-notification` | Removed private prefix and made action clearer |
| `ecc-check-and-respond` | `ecc-auto-response-check-and-respond` | Added module prefix for consistency |
| `ecc-start-auto-response` | `ecc-auto-response-start` | Consistent naming with stop and toggle functions |
| `ecc-stop-auto-response` | `ecc-auto-response-stop` | Consistent naming with start and toggle functions |
| `ecc-toggle-auto-response` | `ecc-auto-response-toggle` | Consistent naming with start and stop functions |

## State Detection Module

| Original Function Name | New Function Name | Reason for Change |
|------------------------|-------------------|-------------------|
| `ecc-detect-simple-state` | `ecc-state-detect` | More concise and prefixed with module name |
| `ecc-detect-prompt-in-last-lines` | `ecc-state-detect-in-lines` | More concise and prefixed with module name |
| `ecc-detect-prompt-in-region` | `ecc-state-detect-in-region` | Prefixed with module name |
| `ecc-detect-alternative-initial-waiting` | `ecc-state-detect-initial-waiting-alt` | More concise and standardized format |
| `ecc-detect-enhanced-state` | `ecc-state-detect-enhanced` | More concise and prefixed with module name |
| `ecc-state-notify-if-prompt-detected` | `ecc-state-notify-prompt` | More concise without losing meaning |

## Common Improvements

1. **Consistent Module Prefixes**
   - All functions in a module should begin with the module prefix (e.g., `ecc-auto-response-`, `ecc-state-`)
   
2. **Verb-Noun Pattern**
   - All functions follow a consistent verb-noun pattern (e.g., `send-message`, `detect-state`)
   
3. **Meaningful Names**
   - Names clearly describe the function's purpose
   - Avoids abbreviations and ambiguous terms
   
4. **Appropriate Privacy**
   - Private helper functions should be marked with `--` prefix only when genuinely private
   - Functions used across modules should not have private markers

5. **Consistent Terms**
   - Standardize on terms like "detect" vs "check" for similar operations
   - Standardize on "buffer" vs "current-buffer" for similar parameters

These naming improvements enhance readability, maintainability, and make the codebase more approachable for new contributors.