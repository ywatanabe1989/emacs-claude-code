# Code Duplication and Naming Inconsistencies Analysis

This document identifies duplicated code, functions, and naming inconsistencies in the emacs-claude-code project.

## Duplicated Modules and Functions

1. **State Detection and Response Functions**
   - Files: `/src/ecc-state/ecc-state-send.el` and `/src/ecc-send/ecc-send-accept.el`
   - Functions: `ecc-auto-accept-send` vs `ecc-send-accept`
   - Description: Both functions perform the same actions - detecting Claude prompts and sending automated responses based on the detected state.

2. **Detection Helper Functions**
   - Files: `/src/ecc-state/ecc-state-detect-prompt.el`
   - Functions: Multiple `--ecc-detect-prompt-*` functions have almost identical patterns
   - Description: Several detection functions use very similar patterns and could be consolidated.

3. **Mode Line Update Functions**
   - Files: `/src/ecc-ui/ecc-ui-update-mode-line.el` and `/src/ecc-auto/ecc-auto-toggle.el`
   - Functions: `ecc-ui-update-mode-line` vs `ecc-auto-update-mode-line`
   - Description: Both include similar logic for modifying the mode line to show status indicators.

4. **Buffer Registration Logic**
   - Files: `/src/ecc-buffer/ecc-buffer-registry.el` and `/src/ecc-buffer/ecc-buffer-current.el`
   - Functions: `--ecc-buffer-register-buffer` and `ecc-buffer-register-buffer`
   - Description: Duplicated buffer registration logic with similar buffer validation checks.

5. **Buffer Switching Logic**
   - Files: `/src/ecc-buffer/ecc-buffer-switch.el`
   - Functions: `ecc-buffer-switch-next-buffer` and `ecc-buffer-switch-previous-buffer`
   - Description: Almost identical functions with significant code duplication, differing only in direction of movement.

6. **Auto-Response Functions**
   - Files: `/src/ecc-state/ecc-state-send.el` and `/src/ecc-send/ecc-send-accept.el`
   - Functions: 
     - `--ecc-auto-send-y` vs `--ecc-auto-send-1-y/n`
     - `--ecc-auto-send-yy` vs `--ecc-auto-send-2-y/y/n`
   - Description: These functions perform the same action but exist in separate files.

7. **Global Variables Duplication**
   - Files: `/src/ecc-state/ecc-state-variables.el` and `/src/ecc-send/ecc-send-accept.el`
   - Variables: Buffer state tracking variables are defined in multiple places
   - Description: State-related variables appear in both files, causing confusion about the source of truth.

8. **Mode Line Status Indicator Logic**
   - Files: `/src/ecc-ui/ecc-ui-update-mode-line.el`
   - Functions: Two versions of `ecc-update-mode-line-all-buffers` (one commented out)
   - Description: The first version is commented out but shows near-identical code to the active version.

9. **Buffer Management Variables**
   - Files: `/src/ecc-buffer/ecc-buffer-current.el` and `/src/ecc-send/ecc-send-accept.el`
   - Variables: `ecc-buffer-current-buffer` vs commented out `ecc-buffer-current-active-buffer`
   - Description: Duplicate buffer tracking variables with similar functionality.

10. **File Processing Logic**
    - Files: `/src/ecc-repository/ecc-repository.el`
    - Functions: Similar file filtering logic between `ecc-get-repository-files` and `ecc-repository-blacklisted-p`
    - Description: Similar file filtering logic that could be consolidated.

## Naming Inconsistencies

### File Naming Conventions

1. **Inconsistent location of term-related files**:
   - `/src/ecc-term/ecc-run-vterm.el` (in term directory)
   - `/src/ecc-term/ecc-vterm.el` (in term directory)
   - `/src/ecc-term/ecc-claude-vterm-mode.el` (in term directory)
   - **Recommendation**: Keep all term-related files in the `ecc-term` directory with consistent naming prefixes like `ecc-term-*`.

2. **Inconsistent file path in header vs. actual location**:
   - `/src/ecc-term/ecc-run-vterm.el` has header showing `/home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-run-vterm.el` (missing term directory)
   - **Recommendation**: Ensure file path in headers matches actual file location.

3. **Underscore vs. no underscore in filenames**:
   - `/src/ecc-auto/_ecc-auto-toggle.el` (with underscore)
   - `/src/ecc-auto/ecc-auto-toggle.el` (without underscore)
   - **Recommendation**: Standardize on no leading underscore for normal files.

### Function Naming

1. **Inconsistent VTERM vs vterm spelling**:
   - `ecc-run-vterm-claude` (lowercase vterm)
   - `ecc-claude-VTERM-next-buffer` (uppercase VTERM)
   - `ecc-claude-vterm-mode` (lowercase vterm)
   - **Recommendation**: Standardize on lowercase `vterm` for consistency.

2. **Inconsistent function prefixes for similar functionality**:
   - `ecc-claude-vterm-*` functions in ecc-claude-vterm-mode.el
   - `ecc-vterm-*` functions in ecc-vterm.el 
   - `ecc-run-vterm-*` functions in ecc-run-vterm.el
   - **Recommendation**: Use consistent prefixes, preferably `ecc-vterm-*` for all vterm-related functions.

3. **Inconsistent use of double dashes in private functions**:
   - `ecc-run-vterm--vterm-available` (uses double dash)
   - `ecc-vterm-setup-state-detection` (no double dash despite being internal)
   - **Recommendation**: Use double dash consistently for all private/internal functions.

### Variable Naming

1. **Duplicate variable definitions with different values**:
   - `ecc-buffer-name` defined in both `/src/ecc-ui/ecc-variables.el` and `/src/ecc-state/ecc-state-variables.el`
   - `ecc-prompt-waiting` with different values in both files
   - **Recommendation**: Define each variable once in a central location.

2. **Inconsistent naming for timers**:
   - `ecc-auto-timer` (in ecc-variables.el)
   - `ecc-auto-accept-timer` (in ecc-state-variables.el)
   - `ecc-claude-vterm-state-timer` (in ecc-claude-vterm-mode.el)
   - `ecc-vterm--timers` (hash table in ecc-vterm.el)
   - **Recommendation**: Standardize on either `ecc-*-timer` or add type suffix like `ecc-*-timer-object`.

3. **Inconsistent prefix for state variables**:
   - `ecc-prompt-waiting` (in ecc-variables.el)
   - `ecc-prompt-pattern-waiting` (in same file)
   - **Recommendation**: Use consistent prefixes, like `ecc-prompt-pattern-*` for all regex patterns.

4. **Inconsistent use of double dashes in private variables**:
   - `ecc-vterm--timers` (uses double dash)
   - `ecc-claude-vterm-state-timer` (no double dash despite being internal)
   - **Recommendation**: Use double dash consistently for all private/internal variables.

### Module Organization

1. **Duplicated customization groups**:
   - `(defgroup emacs-claude nil ...)` defined in both `/src/ecc-ui/ecc-variables.el` and `/src/ecc-state/ecc-state-variables.el`
   - **Recommendation**: Define each customization group once in a dedicated file.

2. **Inconsistent require statements**:
   - `/src/ecc-term/ecc-claude-vterm-mode.el` requires `ecc-state-detect`
   - `/src/ecc-term/ecc-vterm.el` requires `ecc-state-engine`
   - **Recommendation**: Standardize on consistent module names.

3. **Inconsistent module structure**:
   - Some modules have both functional and state/variable code
   - Some divide functionality into separate files
   - **Recommendation**: Use consistent patterns, e.g., separate *-variables.el, *-core.el, and *-ui.el.

## Recommendations for Standardization

1. **File naming**: `ecc-{module}-{component}.el` (e.g., `ecc-vterm-mode.el`)
2. **Function naming**: 
   - Public: `ecc-{module}-{action}` (e.g., `ecc-vterm-create`)
   - Private: `ecc-{module}--{action}` (with double dash)
3. **Variable naming**:
   - Public: `ecc-{module}-{purpose}` (e.g., `ecc-vterm-auto-detect-state`)
   - Private: `ecc-{module}--{purpose}` (with double dash)
4. **Module organization**:
   - Core functionality in `ecc-{module}.el`
   - Variables in `ecc-{module}-variables.el`
   - UI components in `ecc-{module}-ui.el`