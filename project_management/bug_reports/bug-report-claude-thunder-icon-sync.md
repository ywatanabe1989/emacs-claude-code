# Bug Report: CLAUDE THUNDER Icon Not Synchronized with Auto-Response Status

## Description
The CLAUDE THUNDER icon (⚡ CLAUDE with reddish background) in the mode-line is not properly synchronized with the auto-response toggle status. When auto-response is toggled off, the thunder icon remains visible instead of being removed.

## Steps to Reproduce
1. Enable auto-response in a vterm buffer
2. The CLAUDE THUNDER icon appears in the mode-line with red background
3. Disable auto-response (via toggle command or buffer list)
4. The CLAUDE THUNDER icon remains visible even though auto-response is disabled

## Expected Behavior
- When auto-response is ENABLED: Show CLAUDE THUNDER icon (⚡ CLAUDE) with red background
- When auto-response is DISABLED: Remove CLAUDE THUNDER icon completely

## Actual Behavior
The CLAUDE THUNDER icon persists in the mode-line after disabling auto-response.

## Root Cause
The thunder icon was being displayed through the notification system but was not being removed when auto-response was disabled. The enable/disable functions were missing the calls to show/hide the thunder icon.

## Bug Fix Progress
- [x] Identify root cause
- [x] Add thunder icon display when enabling auto-response
- [x] Add thunder icon removal when disabling auto-response
- [ ] Test the fix across different scenarios

## Solution Implemented
Updated `ecc-auto-response.el`:

1. In `--ecc-auto-response-enable-buffer`:
   - Added call to `--ecc-notification--flash-mode-line` to show thunder icon

2. In `--ecc-auto-response-disable-buffer`:
   - Added call to `--ecc-notification--remove-thunder-icon` to remove thunder icon

This ensures the CLAUDE THUNDER icon is always consistent with the auto-response status.

## Priority
High - Visual consistency is important for user understanding of the current state.