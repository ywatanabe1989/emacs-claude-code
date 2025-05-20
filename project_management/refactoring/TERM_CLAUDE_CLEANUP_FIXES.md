# Term-Claude Mode Cleanup Follow-up Fixes

Based on test results and byte-compilation warnings, the following issues need to be fixed in a follow-up commit.

## Variable Declaration Issues

### ecc-term-claude-auto.el
- Declare `ecc-term-claude-update-functions` or require proper module

### ecc-term-claude-performance.el
- Declare or require:
  - `ecc-term-claude-scroll-conservatively`
  - `ecc-term-claude-truncate-lines`
  - `ecc-term-claude-line-numbers`
  - `ecc-term-claude-update-functions`

### ecc-term-claude-setup.el
- Declare or require:
  - `ecc-term-claude-line-numbers`
  - `ecc-term-claude-scroll-conservatively`
  - `ecc-term-claude-truncate-lines`
  - `ecc-term-claude-state-timer`
  - `ecc-term-claude-state-update-interval`

## Docstring Formatting Issues

### ecc-term-claude-auto.el
- Fix single quote usage in docstrings:
  - `ecc-term-claude-auto-send-y/n`
  - `ecc-term-claude-auto-send-y/y/n`

### ecc-term-claude-interaction.el
- Fix single quote usage in docstrings:
  - `ecc-term-claude-send-yes`
  - `ecc-term-claude-send-no`

## Module Loading Issues

Fix "End of file during parsing" errors in:
- `ecc-term-claude-mode-consolidated.el`
- `ecc-term-claude-mode-improved.el`
- `ecc-term-claude-mode-v2.el`
- `ecc-term-claude-mode.el`

These appear to be related to issues with loading `ecc-vterm-yank-as-file.el`. 

## Tests to Fix

Fix failing auto-response tests:
- `test-ecc-auto-notify-consolidated-check-state-notifications-disabled`
- `test-ecc-auto-notify-consolidated-check-state-not-in-prompt-types`
- `test-ecc-auto-notify-consolidated-check-state-throttling`

Fix failing consolidated module tests:
- `test-ecc-debug-utils-buffer-local`
- `test-ecc-variables-customization-groups`

## Action Plan

1. Create a focused fix commit for each category:
   - Variable declaration fixes
   - Docstring formatting fixes
   - Module loading fixes
   - Test fixes

2. Test each fix independently before moving to the next.

3. Ensure backward compatibility is maintained throughout.