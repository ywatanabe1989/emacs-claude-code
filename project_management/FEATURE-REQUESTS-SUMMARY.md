# Feature Requests Summary
Last Updated: 2025-05-31

## Active Feature Requests

### 1. Auto-Response System Improvements

#### a. **Timing and Send Detection** (`feature-request-auto-response-timing-detection.md`)
- **Priority**: High
- **Summary**: Detect when commands are actually sent by monitoring prompt state
- **Key Feature**: Use empty prompt as confirmation of successful send
- **Status**: Design phase

#### b. **Incomplete Text Sending** (`feature-request-incomplete-auto-response-sending.md`)
- **Priority**: High  
- **Summary**: Fix issue where auto-response text appears but isn't sent completely
- **Key Feature**: State-based verification using `:running` state
- **Status**: Design phase

#### c. **Centralized Keyword Detection** (`feature-request-centralized-keyword-detection.md`)
- **Priority**: Medium
- **Summary**: Organize all safety keywords like state detection patterns
- **Key Feature**: Multi-layer safety architecture
- **Status**: Design phase

### 2. UI/UX Enhancements

#### a. **Multi-Buffer Split View** (`feature-request-auto-response-tab-view.md`)
- **Priority**: Medium
- **Summary**: View multiple Claude sessions in vertical splits
- **Key Feature**: Interactive selection or dedicated dashboard command
- **Status**: Design phase

#### b. **Buffer Name Enhancement** (`feature-request-auto-response-buffer-name.md`)
- **Priority**: Low
- **Summary**: Add visual indicators to buffer names for auto-response state
- **Status**: Pending

### 3. Automation Features

#### a. **Auto-Periodical Commands** (`feature-request-auto-periodical-commands.md`)
- **Priority**: Low
- **Summary**: Execute commands periodically during sessions
- **Status**: Pending

## Implementation Priority

1. **Immediate**: Timing detection and incomplete send fixes (reliability)
2. **Next**: Centralized keyword system (maintainability)
3. **Future**: UI enhancements and automation features

## Related Bug Reports
- `bug-report-return-key-timing.md` - Related to timing issues
- `bug-report-timer-lambda-argument-mismatch.md` - Fixed
- Various modeline and state detection issues

## Next Steps
1. Implement state-based send verification
2. Create centralized keyword management system
3. Add comprehensive send retry logic
4. Develop multi-buffer dashboard