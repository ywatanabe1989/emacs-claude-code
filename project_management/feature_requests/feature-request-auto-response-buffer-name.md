# Feature Request: Auto-Response Buffer Name Display

## Description
When auto-response system sends automatic responses to Claude prompts, include the buffer name in the notification message to clearly show which buffer is currently working.

## Current Behavior
Auto-response notifications show messages like:
- "Auto-response to Y/N: 1"
- "Auto-response to Continue: /user:auto"

## Requested Behavior
Auto-response notifications should show messages like:
- "[*claude-term*] Auto-response to Y/N: 1"
- "[*my-project*] Auto-response to Continue: /user:auto"

## Benefits
- Clear identification of which buffer is responding
- Better debugging when multiple buffers are active
- Enhanced user awareness of auto-response activity
- Improved workflow transparency

## Implementation Notes
- Should work for both global and buffer-local auto-response modes
- Buffer name should be displayed in brackets at the beginning of message
- Should maintain backward compatibility with existing notification system

## Status
**COMPLETED** - This feature has been enhanced to ensure consistent buffer name display across all notification paths.

## Implementation Details
The feature was partially implemented but has been enhanced to ensure consistency:

1. **Auto-response notifications** (already implemented):
   - `ecc-auto-response--send-to-buffer` in `src/ecc-auto-response.el` displays:
   ```elisp
   (message "[%s] Auto-response to %s: %s" (buffer-name buffer) state-name text)
   ```

2. **Claude prompt notifications** (enhanced):
   - Updated `ecc-notification-dispatch` to accept optional buffer parameter
   - Updated `ecc-notification-dispatch` and `ecc-notification-display-message` to include buffer names
   - Now displays: `[*buffer-name*] Claude prompt detected: yes/no prompt`

3. **Consistent format across all notifications**:
   - All Claude-related notifications now include `[buffer-name]` prefix when buffer context is available
   - Maintains backward compatibility when buffer is not specified

## Testing
The feature is tested in:
- `tests/test-ecc-auto-response.el` - Test `test-auto-response-buffer-name-in-notification`
- `tests/ecc-notification/test-notification.el` - Test `test-notification-buffer-name-display`

All 91 tests pass confirming the feature works correctly.

## Progress
- [x] Create feature branch (`feature/auto-response-buffer-name`)
- [x] Analyze current auto-response notification code
- [x] Enhance notification functions to consistently include buffer names
- [x] Write tests for enhanced functionality
- [x] Test with multiple buffers
- [x] All tests passing (91/91)
- [x] Update documentation
- [x] Merge to develop branch