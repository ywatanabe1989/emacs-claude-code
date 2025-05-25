<!-- Bug Report: Mode line changes in wrong buffer on navigation -->
<!-- Author: ywatanabe -->
<!-- Date: 2025-05-26 -->
<!-- Status: Solved -->

# Bug Report: Mode Line Changes in Destination Buffer Instead of Original

## Summary
When navigating between buffers, mode line changes (such as notifications or visual indicators) appear in the destination buffer instead of the original buffer where the state change occurred.

## Description
The system incorrectly targets the current buffer for mode line modifications instead of the buffer where the actual state change happened. This occurs when:
1. A state change happens in buffer A
2. User navigates to buffer B
3. Mode line change appears in buffer B instead of buffer A

## Current Behavior
- Mode line flash/changes appear in the currently active buffer
- Visual indicators show up in the wrong buffer context
- User sees notifications related to a different buffer's state

## Expected Behavior
- Mode line changes should appear in the buffer where the state change occurred
- Visual indicators should remain tied to their originating buffer
- Navigation should not affect where notifications appear

## Impact
- **Confusing UX**: Users see mode line changes unrelated to current buffer
- **Incorrect Context**: Visual feedback appears in wrong buffer
- **Loss of Information**: Original buffer's state changes are not visible
- **Misleading Notifications**: Users may think current buffer has issues

## Root Cause Identified
**Timer callbacks lose buffer context** in mode line flash functions. The issue occurs in:

### Location 1: `ecc-notification.el:222-231`
```elisp
(defun ecc-auto-notify-flash-mode-line ()
  "Flash the mode line to get attention."
  (when ecc-notification--flash-timer
    (cancel-timer ecc-notification--flash-timer))
  
  (invert-face 'mode-line)
  (setq ecc-notification--flash-timer
        (run-with-timer ecc-notification-flash-duration nil
                       (lambda ()
                         (invert-face 'mode-line)))))  ; ← PROBLEM: No buffer context
```

### Location 2: `ecc-auto-notify.el:312-323`
```elisp
(defun ecc-auto-notify-flash-mode-line ()
  (invert-face 'mode-line)
  (setq ecc-auto-notify--flash-timer
        (run-with-timer ecc-auto-notify-bell-duration nil
                       (lambda ()
                         (invert-face 'mode-line)))))  ; ← PROBLEM: No buffer context
```

**Issues:**
1. `invert-face` affects the face globally, not buffer-specifically
2. Timer callbacks execute in whatever buffer is current when they fire
3. No `with-current-buffer` to preserve original buffer context

## Areas to Investigate

### 1. Mode Line Flash Functions
Check functions that modify mode line appearance:
- `ecc-auto-notify--flash-mode-line` or similar
- Visual notification functions
- Mode line color/style changes

### 2. Buffer Context Preservation
Functions should preserve buffer context:
```elisp
;; Wrong - uses current buffer
(with-current-buffer (current-buffer)
  (flash-mode-line))

;; Correct - uses specific buffer
(with-current-buffer target-buffer
  (flash-mode-line))
```

### 3. Notification System
Check notification dispatch:
- How buffer context is passed to notification functions
- Whether notifications use correct buffer reference
- Timer-based notifications that may lose buffer context

## Test Case
```elisp
(ert-deftest test-modeline-changes-correct-buffer ()
  "Test that mode line changes appear in the correct buffer."
  (let ((buf1 (generate-new-buffer "*test1*"))
        (buf2 (generate-new-buffer "*test2*")))
    (unwind-protect
        (progn
          ;; Trigger state change in buf1
          (with-current-buffer buf1
            (trigger-some-state-change))
          
          ;; Switch to buf2
          (switch-to-buffer buf2)
          
          ;; Mode line change should still be in buf1, not buf2
          (should (buffer-local-value 'some-mode-line-indicator buf1))
          (should-not (buffer-local-value 'some-mode-line-indicator buf2)))
      (kill-buffer buf1)
      (kill-buffer buf2))))
```

## Steps to Reproduce
1. Open two buffers (e.g., two vterm buffers)
2. Trigger a state change in buffer A (e.g., Claude prompt detection)
3. Quickly switch to buffer B
4. Observe mode line change appears in buffer B instead of A

## Related Files to Check
- `src/ecc-auto-notify.el` - Notification system
- `src/ecc-term-visual-aid.el` - Visual aids
- Any functions that modify mode line appearance
- Timer-based callbacks that may lose buffer context

## Proposed Solution

### 1. Fix Timer Buffer Context
Modify mode line flash functions to preserve buffer context:

```elisp
(defun ecc-auto-notify-flash-mode-line (&optional buffer)
  "Flash the mode line to get attention in BUFFER."
  (let ((target-buffer (or buffer (current-buffer))))
    (when ecc-notification--flash-timer
      (cancel-timer ecc-notification--flash-timer))
    
    (with-current-buffer target-buffer
      (invert-face 'mode-line))
    
    (setq ecc-notification--flash-timer
          (run-with-timer ecc-notification-flash-duration nil
                         (lambda ()
                           (with-current-buffer target-buffer
                             (invert-face 'mode-line)))))))
```

### 2. Update Calling Functions
Ensure notification dispatch functions pass the correct buffer:

```elisp
;; Instead of:
(ecc-auto-notify-flash-mode-line)

;; Use:
(ecc-auto-notify-flash-mode-line originating-buffer)
```

### 3. Consider Buffer-Local Alternative
Use buffer-local mode line modifications instead of global face inversion:

```elisp
(defun ecc-auto-notify-flash-mode-line-local (buffer)
  "Flash mode line locally in BUFFER."
  (with-current-buffer buffer
    (let ((original-format mode-line-format))
      (setq mode-line-format 
            (propertize (format-mode-line original-format)
                       'face 'highlight))
      (force-mode-line-update)
      (run-with-timer flash-duration nil
                     (lambda ()
                       (with-current-buffer buffer
                         (setq mode-line-format original-format)
                         (force-mode-line-update)))))))
```

### 4. API Consistency
Make all visual notification functions buffer-aware:
- Add buffer parameter to all flash functions
- Update callers to pass originating buffer
- Ensure timer callbacks preserve buffer context

## Solution Implemented
✅ **Fixed in commit 56acdb9** - `feature/fix-modeline-buffer-context`

Fixed mode line flash buffer context preservation:
- Added buffer parameter to `ecc-auto-notify-flash-mode-line` and `ecc-auto-notify-flash-mode-line`
- Use `with-current-buffer` in timer callbacks to preserve buffer context
- Added `buffer-live-p` check before timer execution for safety
- Updated all calling functions to pass the correct buffer parameter
- Added `force-mode-line-update` after face changes for immediate visual feedback

Changes made:
1. **ecc-notification.el**: Fixed function to accept buffer parameter and preserve context in timer
2. **ecc-auto-notify.el**: Fixed duplicate function and updated all calls to pass buffer
3. **Function calls**: Updated notification dispatch functions to pass originating buffer

Mode line flashes now appear in the correct buffer regardless of navigation.

## Priority
High - Causes confusing user experience and misleading visual feedback ✅ **FIXED**

## Workaround
No longer needed - issue has been resolved.