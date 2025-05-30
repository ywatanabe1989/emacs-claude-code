# Bug Report: Modeline Highlight Not Buffer-Aware

## Issue Description
The modeline highlight when a state is detected is not buffer-aware. When switching between buffers, the modeline highlight appears in all buffers instead of only the buffer where the detection occurred.

## Expected Behavior
- Modeline highlight should only appear in the buffer where the state was detected
- When switching to other buffers, they should show their own state or no highlight
- Each buffer should maintain its own modeline state independently

## Root Cause
The issue is in `--ecc-notification--flash-mode-line` function in `ecc-notification.el`:
- Line 111: `(invert-face 'mode-line)` inverts the mode-line face globally
- This affects all buffers, not just the target buffer
- The face inversion is not buffer-local

## Bug Fix Progress
- [x] Create bug report
- [x] Analyze current modeline implementation
- [x] Identify root cause
- [x] Implement buffer-local modeline handling
- [x] Test the fix

## Solution Implemented
Changed the modeline flashing implementation to:
1. Use buffer-local variable `--ecc-notification--mode-line-format` to store the original mode-line
2. Temporarily prepend a highlighted indicator instead of inverting the global face
3. Restore the original mode-line format after the flash duration