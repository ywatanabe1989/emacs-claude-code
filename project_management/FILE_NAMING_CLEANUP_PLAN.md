# File Naming Cleanup Plan

## Overview

This document outlines the plan to standardize file naming conventions by removing development-stage suffixes like `-fix`, `-improved`, `-refactored`, and `-updated`. These suffixes indicate iterative development rather than production-ready code organization.

## Files to Rename

### Source Files

| Current Filename | New Filename | Notes |
|------------------|--------------|-------|
| ecc-auto-notify-fix.el | ecc-auto-notify.el | Replace existing file |
| ecc-auto-notify-improved.el | ecc-auto-notify-enhanced.el | New name to avoid collision |
| ecc-variables-refactored.el | ecc-variables-core.el | Core variables implementation |

### Test Files

| Current Filename | New Filename | Notes |
|------------------|--------------|-------|
| ecc-auto-response-test-updated.el | ecc-auto-response-test.el | Replace existing file |
| test-initial-waiting-updated.el | test-initial-waiting.el | Replace existing file |
| test-notification-format-updated.el | test-notification-format.el | Replace existing file |
| test-buffer-state-refactored.el | test-buffer-state-core.el | Core state tests |

## Implementation Steps

1. **For each file**:
   - Verify content is production-ready
   - Create git branch for renaming operation
   - Use git mv to rename file
   - Update all references to the old filename
   - Commit changes with clear message

2. **Testing verification**:
   - After each rename, run tests to ensure functionality remains intact
   - Fix any broken references or imports

3. **Documentation updates**:
   - Update any documentation that references old filenames
   - Update project management records

## Standards for Future Development

To maintain consistent naming going forward:

1. **Avoid temporary suffixes**:
   - No `-fix`, `-updated`, `-refactored`, `-improved` in production code
   - Use semantic names that describe purpose rather than development stage

2. **Module naming conventions**:
   - Core functionality: `ecc-{module}.el`
   - Specialized sub-modules: `ecc-{module}-{subtype}.el`
   - Utils and helpers: `ecc-{module}-utils.el`

3. **Version control for iterations**:
   - Use git branches and commits to track iterations
   - Archive superseded files in `.old` directories

## Implementation Priority

1. Variables system (highest priority due to dependencies)
2. Auto-notify system
3. Test files
4. Documentation updates

## Benefits

- Cleaner, more professional codebase
- Easier navigation and maintenance
- Better alignment with clean code principles
- Elimination of confusion about which file is current