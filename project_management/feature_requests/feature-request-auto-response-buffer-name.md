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

## Progress
- [ ] Create feature branch
- [ ] Analyze current auto-response notification code
- [ ] Implement buffer name in notification messages
- [ ] Write tests for new functionality
- [ ] Test with multiple buffers
- [ ] Update documentation if needed
- [ ] Merge to develop branch