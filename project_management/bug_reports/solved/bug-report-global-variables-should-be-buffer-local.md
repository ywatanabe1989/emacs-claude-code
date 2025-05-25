<!-- Bug Report: Global Variables Should Be Buffer-Local -->
<!-- Author: ywatanabe -->
<!-- Date: 2025-05-26 -->
<!-- Status: Solved -->

# Bug Report: Remaining Global Variables Should Be Buffer-Local

## Summary
Several global variables in the auto-response system track per-buffer state but are not buffer-local. This causes incorrect behavior when multiple buffers are active.

## Location
Multiple files across the auto-response system:
- `src/ecc-auto-response.el`
- `src/ecc-auto-core.el`
- `src/ecc-auto-notify.el`

## Description
The auto-response system maintains both global and buffer-local state tracking. However, several variables that logically represent per-buffer state are defined globally, causing state leakage between buffers.

## Variables That Should Be Buffer-Local

### In `ecc-auto-response.el`
```elisp
;; Currently global, should be buffer-local:
(defvar ecc-auto-response--last-state nil
  "Last Claude state that received an auto-response.")

(defvar ecc-auto-response--last-response-time 0
  "Timestamp of last auto-response.")

(defvar ecc-auto-response--accumulation-count 0
  "Counter for responses sent within current accumulation window.")

(defvar ecc-auto-response--accumulation-start-time 0
  "Start time of current accumulation tracking window.")
```

### In `ecc-auto-core.el`
```elisp
;; Currently global, should be buffer-local:
(defvar ecc-auto-core--last-state nil
  "Last state detected in the auto-core system.")

(defvar ecc-auto-core--last-response-time 0
  "Timestamp of the last response sent.")

(defvar ecc-auto-core--initial-check-count 0
  "Count of initial state checks performed.")
```

### In `ecc-auto-notify.el`
```elisp
;; Currently global, should be buffer-local:
(defvar ecc-auto-notify--last-time 0
  "Time of last notification.")

(defvar ecc-auto-notify--last-state nil
  "Last state for which notification was sent.")
```

## Current Behavior
1. State from one buffer affects behavior in other buffers
2. Throttling/timing decisions are made based on global state instead of per-buffer state
3. Accumulation detection spans across all buffers instead of per-buffer
4. Notification timing is global instead of per-buffer

## Expected Behavior
Each buffer should maintain its own:
- Last response state and time
- Accumulation counters
- Notification history
- Initial check counts

## Impact
- **Response Throttling**: A response in buffer A prevents responses in buffer B
- **State Tracking**: State changes in one buffer affect state detection in others
- **Accumulation Detection**: Responses across different buffers are counted together
- **Notification Spam**: Notifications in one buffer suppress notifications in others

## Root Cause
The system was initially designed with global mode as primary, and buffer-local support was added later. The global variables were retained for backward compatibility but create conflicts when multiple buffers are active.

## Proposed Solution

### 1. Make All State Variables Buffer-Local
Convert all state-tracking variables to buffer-local:

```elisp
;; In ecc-auto-response.el
(defvar-local ecc-auto-response--last-state nil
  "Last Claude state that received an auto-response.")

(defvar-local ecc-auto-response--last-response-time 0
  "Timestamp of last auto-response.")

(defvar-local ecc-auto-response--accumulation-count 0
  "Counter for responses sent within current accumulation window.")

(defvar-local ecc-auto-response--accumulation-start-time 0
  "Start time of current accumulation tracking window.")

;; Similarly for other files...
```

### 2. Remove Global Mode Support
Since global mode is unnecessary (as per previous bug report), remove:
- Global state tracking variables
- Global mode logic
- `ecc-auto-response-default` variable

### 3. Update All Functions
Update all functions to work exclusively with buffer-local variables:
- Remove checks for `ecc-auto-response-default`
- Remove global mode code paths
- Ensure all state access is buffer-local

## Test Case
```elisp
(ert-deftest test-buffer-local-state-isolation ()
  "Test that state in one buffer doesn't affect another."
  (let ((buf1 (generate-new-buffer "*test1*"))
        (buf2 (generate-new-buffer "*test2*")))
    (unwind-protect
        (progn
          ;; Set state in buf1
          (with-current-buffer buf1
            (setq-local ecc-auto-response--last-state :y/n)
            (setq-local ecc-auto-response--last-response-time (float-time)))
          
          ;; Check buf2 has independent state
          (with-current-buffer buf2
            (should (null ecc-auto-response--last-state))
            (should (= 0 ecc-auto-response--last-response-time))))
      (kill-buffer buf1)
      (kill-buffer buf2))))
```

## Related Issues
- Bug Report: "C-c c a toggles global status instead of buffer-local status"
- Global mode creates unnecessary complexity
- State leakage between buffers causes unpredictable behavior

## Priority
High - Causes incorrect behavior with multiple buffers

## Solution Implemented  
✅ **Fixed in commit 8f69e03** - `feature/make-variables-buffer-local`

Converted all state tracking variables to buffer-local:
- `ecc-auto-response--last-state/time` → buffer-local
- `ecc-auto-response--accumulation-count/start-time` → buffer-local  
- `ecc-auto-core--last-state/response-time/initial-check-count` → buffer-local
- `ecc-auto-notify--last-time/state` → buffer-local

Each buffer now maintains independent:
- Response throttling state
- Accumulation detection counters
- Notification timing history
- Initial check progress

## Recommendation
Implement a complete transition to buffer-local only operation:
1. Convert all state variables to buffer-local ✅ **DONE**
2. Remove global mode entirely
3. Simplify the codebase by removing dual-mode logic
4. Update documentation to reflect buffer-local only operation

This aligns with the previous recommendation to make the system buffer-local only, providing a cleaner and more predictable implementation.