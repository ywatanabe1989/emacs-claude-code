# Refactoring Implementation Plan

Based on our detailed analysis, this document outlines the specific steps and approaches we'll take for refactoring the Emacs Claude Code codebase.

## 1. State Detection Refactoring

### Target Files
- Create `ecc-state-detection.el` (based on ecc-state-detection-refactored.el)
- Move obsolete files to .old folder:
  - src/ecc-state-detect-prompt.el → src/.old/ecc-state-detect-prompt.el
  - src/ecc-state-detection-refactored.el → src/.old/ecc-state-detection-refactored.el

### Implementation Changes
- Use the refactored version as the base
- Standardize function naming with ecc-detect-* and ecc-state-* prefixes
- Ensure consistent customization variables
- Add appropriate deprecation notices and compatibility aliases
- Update docstrings for clarity

### New File Structure (ecc-state-detection.el)
1. Package information and commentary
2. Customization variables
3. Core detection functions
   - ecc-detect-state
   - ecc-detect-prompt-in-region
   - ecc-detect-prompt-in-last-lines
   - ecc-detect-basic-state
   - ecc-detect-alternative-initial-waiting
4. Utility functions
   - ecc-state-get-name
   - ecc-state-notify-if-prompt-detected
5. Backward compatibility aliases
6. Provide statement

## 2. Auto-Response System Refactoring

### Target Files
- Create `ecc-auto-response.el` (consolidating functionality)
- Create `ecc-notification.el` (from ecc-auto-notify-improved.el)
- Move obsolete files to .old folder:
  - src/ecc-auto-response.el → src/.old/ecc-auto-response.el
  - src/ecc-auto-response-fix.el → src/.old/ecc-auto-response-fix.el
  - src/ecc-auto-response-refactored.el → src/.old/ecc-auto-response-refactored.el
  - src/ecc-auto-response-unified.el → src/.old/ecc-auto-response-unified.el
  - src/ecc-auto-notify.el → src/.old/ecc-auto-notify.el
  - src/ecc-auto-notify-improved.el → src/.old/ecc-auto-notify-improved.el

### Implementation Changes for auto-response.el
- Use ecc-auto-response-refactored.el as the base
- Integrate throttling from ecc-auto-response-fix.el
- Standardize function naming
- Add deprecation notices and compatibility aliases
- Update internal references to notification functions
- Update docstrings for clarity

### Implementation Changes for notification.el
- Use ecc-auto-notify-improved.el as the base
- Rename functions from ecc-auto-notify-* to ecc-notify-*
- Ensure integration with the new auto-response module
- Add deprecation notices and compatibility aliases
- Update docstrings for clarity

### New File Structure (ecc-auto-response.el)
1. Package information and commentary
2. Customization variables
3. Core auto-response functions
   - ecc-auto-response-start
   - ecc-auto-response-stop
   - ecc-auto-response-toggle
   - ecc-auto-response-check
4. Response sending functions
   - ecc-auto-response-send
   - ecc-auto-response-send-message
   - ecc-auto-response-send-to-vterm
5. Throttling functions
   - ecc-auto-response-throttled-p
   - ecc-auto-response-update-time
   - ecc-auto-response-initial-check
6. Utility functions
   - ecc-register-buffer
   - ecc-auto-response-connect-to-vterm-hook
7. Convenience functions
   - ecc-auto-response-yes
   - ecc-auto-response-yes-plus
   - ecc-auto-response-continue
   - ecc-auto-response-template
8. Backward compatibility aliases
9. Provide statement

### New File Structure (ecc-notification.el)
1. Package information and commentary
2. Customization variables
3. Core notification functions
   - ecc-notify-check-state
   - ecc-notify-prompt
   - ecc-notify-ring-bell
   - ecc-notify-flash-mode-line
4. Utility functions
   - ecc-notify-toggle
   - ecc-notify-toggle-bell
5. Backward compatibility aliases
6. Provide statement

## 3. Buffer-Local System Refactoring

### Target Files
- Create `ecc-buffer-manager.el` (from ecc-buffer-api.el and ecc-buffer-local.el)
- Move obsolete files to .old folder:
  - src/ecc-buffer-api.el → src/.old/ecc-buffer-api.el
  - src/ecc-buffer-local.el → src/.old/ecc-buffer-local.el

### Implementation Changes
- Consolidate functionality
- Standardize function naming
- Add deprecation notices and compatibility aliases
- Update docstrings for clarity

## 4. Variables Refactoring

### Target Files
- Create `ecc-variables.el` (based on ecc-variables-refactored.el)
- Move obsolete files to .old folder:
  - src/ecc-variables.el → src/.old/ecc-variables.el
  - src/ecc-variables-refactored.el → src/.old/ecc-variables-refactored.el

### Implementation Changes
- Use the refactored version as the base
- Standardize variable naming
- Add deprecation notices and compatibility aliases
- Update docstrings for clarity

## Testing Strategy

After each refactoring step:
1. Run tests to ensure no regressions
2. Update test files to match new function names
3. Document any issues encountered

## Implementation Order

1. **First Phase**: Core infrastructure
   - ecc-variables.el
   - ecc-state-detection.el
   
2. **Second Phase**: Auto-response system
   - ecc-notification.el
   - ecc-auto-response.el
   
3. **Third Phase**: Buffer management
   - ecc-buffer-manager.el
   
4. **Fourth Phase**: Update main file
   - emacs-claude-code.el

## Completion Criteria

- All refactored files properly replace their predecessors
- All tests pass with the refactored codebase
- Documentation is updated to reflect the new structure
- Obsolete files are moved to the .old directory
- Main emacs-claude-code.el file is updated to load the new modules