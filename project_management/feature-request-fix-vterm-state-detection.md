# Feature Request: Fix vterm State Detection Error

## Error Description
```
Error running timer 'ecc-vterm--detect-state': (void-function ecc-vterm--detect-state) [32 times]
```

## Current Implementation
The current vterm module attempts to implement its own state detection system instead of leveraging the existing ecc-state module. This has led to errors and redundant implementations.

## Analysis
After analyzing the code, I found the following issues:

1. The `ecc-vterm--detect-state` function in `src/ecc-term/ecc-vterm.el` at line 199 is either missing or not properly defined despite being referenced in timers.
2. The vterm module is implementing a redundant state detection system instead of using the robust ecc-state engine.
3. This is part of the term/vterm module that has 14 failing tests.

## Proposed Solution
1. Remove redundant state detection in the vterm module
2. Leverage the existing ecc-state module for state detection
3. Simplify the vterm integration to use the established state engine
4. Fix timer-related code to prevent cascading errors

## Implementation Plan
1. Create a feature branch: `feature/fix-vterm-state-detection`
2. Modify the ecc-vterm.el file to remove redundant state detection
3. Update the vterm module to use the existing ecc-state engine
4. Simplify the timer-based state detection to use the established APIs
5. Add proper error handling to prevent cascading timer errors
6. Test the changes with real vterm buffers
7. Update tests to ensure they pass

## Expected Outcome
1. No more `(void-function ecc-vterm--detect-state)` errors
2. Simpler, more consistent code with less duplication
3. Improved stability in the vterm module
4. Better maintenance through standardization with the rest of the codebase