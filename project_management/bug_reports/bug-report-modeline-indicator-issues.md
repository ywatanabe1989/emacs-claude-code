# Bug Report: Mode-line Indicator Issues

## Issue Description
The mode-line indicator for auto-response has two problems:
1. The indicator appears white instead of the configured orange color (#ff8c00)
2. The indicator is not buffer-aware - it shows in all buffers instead of only the buffer where auto-response is enabled

## Symptoms
- Mode-line shows `[AUTO]` in white background instead of orange
- When auto-response is enabled in one buffer, the indicator appears in all buffers
- Switching between buffers doesn't update the indicator visibility correctly

## Expected Behavior
1. The `[AUTO]` indicator should have an orange background (#ff8c00) as configured
2. The indicator should only appear in buffers where auto-response is actually enabled
3. Buffer switching should correctly show/hide the indicator based on each buffer's auto-response state

## Root Cause Analysis
1. **Color Issue**: The face definition might not be properly applied or the face-spec-set might be overridden
2. **Buffer-awareness Issue**: The `:eval` form in mode-line might not be evaluating the buffer-local variable correctly

## Affected Components
- `ecc-auto-response.el` - specifically the `--ecc-auto-response--update-mode-line` function
- Face definition for `ecc-auto-indicator-face`

## Priority
High - Visual feedback is important for users to know which buffers have auto-response enabled

## Reproduction Steps
1. Enable auto-response in a vterm buffer
2. Observe the mode-line indicator is white instead of orange
3. Switch to another buffer
4. Observe the indicator incorrectly appears in the other buffer too

## Bug Fix Progress
- [x] Identify root cause
- [x] Fix face application to use predefined face
- [x] Fix buffer-local evaluation
- [ ] Test the fix

## Solution Implemented
1. Changed the face application to use the predefined `ecc-auto-indicator-face` instead of constructing an inline face
2. Removed `bound-and-true-p` and use direct variable reference since it's buffer-local
3. Also updated notification flash duration from 0.5s to 0.3s per user request