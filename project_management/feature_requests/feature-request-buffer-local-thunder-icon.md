# Feature Request: Buffer-Local Thunder Icon Display

## Summary
The red Claude thunder icon (⚡) should only be displayed on the specific buffer where auto-mode is enabled, not globally across all buffers.

## Current Behavior
- The AUTO indicator in the mode-line is already buffer-local and works correctly
- However, the notification flash (⚡ CLAUDE) appears to be global when Claude prompts are detected
- This can be confusing when multiple buffers are open

## Expected Behavior
- The thunder icon (⚡) notification should only appear on the buffer where auto-response is actually enabled
- Other buffers should not show the notification flash

## Analysis
The issue is in `ecc-notification.el`:
- The `--ecc-notification--flash-mode-line` function modifies the mode-line of the target buffer
- However, it seems to be applied globally rather than just to the specific buffer

## Implementation Notes
1. The AUTO indicator is already working correctly as buffer-local
2. Need to ensure the notification flash is also buffer-specific
3. The fix should be in the notification module, not the auto-response module

## Priority
Medium - This is a UI/UX improvement that would make the feature less confusing when working with multiple buffers.