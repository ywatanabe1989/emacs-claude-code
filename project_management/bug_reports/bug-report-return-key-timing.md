# Bug Report: Return Key Not Sent/Recognized Properly

## Issue Description
Sometimes the return key is not sent or recognized properly after auto-responses. This might be a timing issue or a problem with the periodical response mechanism.

## Symptoms
- Auto-response text appears but doesn't get submitted
- Return key seems to be missing or not recognized
- May be more prevalent with certain response types

## Potential Causes
1. **Timing Issues**: The `sit-for` delays might be too short
2. **vterm-send-return**: The function might need additional delay or double-sending
3. **Buffer State**: The buffer might not be ready to receive input
4. **Race Condition**: Between text sending and return key

## Bug Fix Progress
- [x] Identify potential causes
- [x] Implement more robust return sending
- [x] Add configurable delays
- [x] Add :running state detection
- [x] Block auto-response during :running state
- [ ] Test the fix

## Proposed Solution
1. Add a small delay between text and return
2. Make return sending more robust with retries
3. Add option to send double returns for certain modes