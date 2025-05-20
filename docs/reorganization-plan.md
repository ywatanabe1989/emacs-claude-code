# Code Reorganization Plan

This document outlines the plan for reorganizing the codebase to address duplication and inconsistency issues identified in the [code-duplication-analysis.md](./code-duplication-analysis.md).

## Priorities

1. **Standardize Variable Organization** - Highest priority as it affects all modules
2. **Fix Naming Conventions** - Critical for maintaining codebase consistency
3. **Consolidate Duplicate Functionality** - Reduce code duplication and improve maintainability
4. **Move Misplaced Files and Functions** - Ensure logical grouping of functionality
5. **Clean up Transition Artifacts** - Remove transitional and backup files

## 1. Standardize Variable Organization

### 1.1. Centralize Variable Definitions
- Create a dedicated variables file for each module
- Move shared variables to a single central location
- Ensure each variable is defined once only

### 1.2. Variables to Reorganize

| Current Location | Variable | Target Location |
|------------------|----------|----------------|
| `ecc-ui/ecc-variables.el` | `ecc-buffer-name` | `ecc-buffer/ecc-buffer-variables.el` |
| `ecc-state/ecc-state-variables.el` | `ecc-buffer-name` (duplicate) | Remove |
| `ecc-ui/ecc-variables.el` | `ecc-prompt-waiting` | `ecc-state/ecc-state-variables.el` |
| `ecc-ui/ecc-variables.el` | `ecc-prompt-pattern-*` | `ecc-state/ecc-state-variables.el` |
| `ecc-send/ecc-send-accept.el` | `ecc-buffer-current-active-buffer` | `ecc-buffer/ecc-buffer-variables.el` |
| Multiple locations | Timer variables | Consolidate in respective module variables |

### 1.3 Customization Group Consolidation
- Move all `defgroup` and `defcustom` declarations to a single file
- Create logical sub-groups for different module types

## 2. Fix Naming Conventions

### 2.1. Standardize File Naming
- Rename files to follow the pattern: `ecc-{module}-{component}.el`
- Remove leading underscores from filenames
- Ensure file path in headers matches actual location

### 2.2. Function Naming Standardization

| Current Function | Corrected Function Name |
|------------------|------------------------|
| `ecc-claude-VTERM-next-buffer` | `ecc-vterm-next-buffer` |
| `ecc-run-vterm-claude` | `ecc-vterm-run-claude` |
| `ecc-vterm-setup-state-detection` | `ecc-vterm--setup-state-detection` |
| `ecc-send-accept` | `ecc-auto-send-accept` |

### 2.3. Variable Naming Standardization

| Current Variable | Corrected Variable Name |
|-----------------|------------------------|
| `ecc-auto-timer` | `ecc-auto--timer` |
| `ecc-auto-accept-timer` | `ecc-auto--accept-timer` |
| `ecc-claude-vterm-state-timer` | `ecc-vterm--state-timer` |
| `ecc-prompt-waiting` | `ecc-state-prompt-waiting` |

## 3. Consolidate Duplicate Functionality

### 3.1. State Detection and Response

| Duplicated Functions | Consolidated Function | Location |
|---------------------|----------------------|----------|
| `ecc-auto-accept-send` & `ecc-send-accept` | `ecc-auto-send-accept` | `ecc-state/ecc-state-send.el` |
| Various `--ecc-detect-prompt-*` functions | Create helper function with pattern parameter | `ecc-state/ecc-state-detect-prompt.el` |
| Multiple auto response functions | Standardize on one set in `ecc-state-send.el` | `ecc-state/ecc-state-send.el` |

### 3.2. Mode Line Updates

| Duplicated Functions | Consolidated Function | Location |
|---------------------|----------------------|----------|
| `ecc-ui-update-mode-line` & `ecc-auto-update-mode-line` | `ecc-ui-update-mode-line` | `ecc-ui/ecc-ui-update-mode-line.el` |
| Multiple versions of `ecc-update-mode-line-all-buffers` | Single implementation | `ecc-ui/ecc-ui-update-mode-line.el` |

### 3.3. Buffer Management

| Duplicated Functions | Consolidated Function | Location |
|---------------------|----------------------|----------|
| `--ecc-buffer-register-buffer` & `ecc-buffer-register-buffer` | `ecc-buffer-register` | `ecc-buffer/ecc-buffer-registry.el` |
| `ecc-buffer-switch-next-buffer` & `ecc-buffer-switch-previous-buffer` | Create parameterized function | `ecc-buffer/ecc-buffer-switch.el` |

## 4. Move Misplaced Files and Functions

| Current Location | Target Location | Items to Move |
|-----------------|-----------------|---------------|
| `ecc-term/ecc-run-vterm.el` | `ecc-term/ecc-term-run.el` | All functions |
| `ecc-send/ecc-send-accept.el` | `ecc-state/ecc-state-send.el` | Auto-accept functions |
| `ecc-ui/ecc-variables.el` | Module-specific variables files | Variables specific to each module |

## 5. Clean Up Transition Artifacts

- Remove `src/ecc-auto/_ecc-auto-toggle.el` (keep `ecc-auto-toggle.el`)
- Remove commented out code in `ecc-ui-update-mode-line.el`
- Remove duplicate variable definitions
- Ensure all require statements point to current file locations
- Update any reference to moved/renamed functions

## Implementation Strategy

1. Start with variable organization to establish a solid foundation
2. Fix naming conventions next since they affect file paths
3. Consolidate duplicate functions after standardizing names
4. Move misplaced functions after consolidating duplicates
5. Finally, clean up any remaining transition artifacts

Each change should be committed separately with clear commit messages to make the changes trackable and reviewable.