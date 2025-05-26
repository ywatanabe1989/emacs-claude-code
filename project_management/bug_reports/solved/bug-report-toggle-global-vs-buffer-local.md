<!-- Bug Report: C-c c a toggles global status instead of buffer-local status -->
<!-- Author: ywatanabe -->
<!-- Date: 2025-05-26 -->
<!-- Status: Solved -->

# Bug Report: Auto-response Toggle Affects Global State Instead of Buffer-Local

## Summary
The keybinding `C-c c a` (mapped to `ecc-auto-response-buffer-toggle`) toggles global auto-response status instead of buffer-local status when `ecc-auto-response-default` is `nil`.

## Location
- File: `src/ecc-auto-response.el:773-798`
- Function: `ecc-auto-response-buffer-toggle`
- Keybinding: `C-c c a` in `src/ecc-convenience-commands.el:310-311`

## Description
When `ecc-auto-response-default` is `nil` (the default), calling `ecc-auto-response-buffer-toggle` toggles the global auto-response state rather than the buffer-local state. This affects all buffers instead of just the current buffer.

## Current Behavior
```elisp
(defun ecc-auto-response-buffer-toggle (&optional buffer)
  ;; ...
  (if ecc-auto-response-default
      ;; Buffer-local mode: toggle buffer-local state
      (if ecc-auto-response-buffer-enabled
          (ecc-auto-response-buffer-stop buf)
        (progn
          (ecc-auto-response-register-buffer buf)
          (ecc-auto-response-buffer-start buf)))
    ;; Global mode: toggle global state for current buffer
    (if ecc-auto-response-enabled
        ;; Stop global auto-response completely
        (progn
          (ecc-auto-response-stop)
          (ecc-debug-message "Global auto-response disabled"))
      ;; Start global auto-response with current buffer registered
      (progn
        (ecc-auto-response-register-buffer buf)
        (ecc-auto-response-start)
        (ecc-debug-message "Global auto-response enabled")))))
```

## Expected Behavior
The function name `ecc-auto-response-buffer-toggle` suggests it should toggle buffer-local auto-response regardless of the `ecc-auto-response-default` setting.

## Root Cause
The function's behavior depends on `ecc-auto-response-default`:
- When `nil` (default): Toggles global state
- When `t`: Toggles buffer-local state

This is counterintuitive given the function name includes "buffer" and the keybinding documentation suggests buffer-local operation.

## Impact
- Users expecting buffer-local control get global changes
- Disabling auto-response in one buffer affects all buffers
- Confusing behavior that doesn't match function naming

## Proposed Solution

### Remove Global Toggle - Only Buffer-Local Control
Since global toggling is unnecessary and causes confusion, the function should **always** toggle buffer-local state:

```elisp
(defun ecc-auto-response-buffer-toggle (&optional buffer)
  "Toggle buffer-local auto-response for BUFFER on or off."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      (if ecc-auto-response-buffer-enabled
          (ecc-auto-response-buffer-stop buf)
        (progn
          (ecc-auto-response-register-buffer buf)
          (ecc-auto-response-buffer-start buf))))))
```

### Rationale
- Global toggling is not necessary - users want to control auto-response per buffer
- Buffer-local control is more intuitive and matches the function name
- Simplifies the codebase by removing dual-mode complexity
- Prevents unexpected side effects on other buffers

### Additional Changes Needed
1. Remove or deprecate `ecc-auto-response-default` variable
2. Remove global mode logic from the codebase
3. Update all related functions to work only with buffer-local state
4. Simplify the auto-response system to be buffer-local only

## Steps to Reproduce
1. Ensure `ecc-auto-response-default` is `nil` (default)
2. Open two vterm buffers
3. Press `C-c c a` in one buffer
4. Observe that auto-response is toggled globally in both buffers

## Test Case
```elisp
(ert-deftest test-buffer-toggle-should-be-buffer-local ()
  "Test that buffer toggle affects only current buffer."
  (let ((ecc-auto-response-default nil)
        (buf1 (generate-new-buffer "*test1*"))
        (buf2 (generate-new-buffer "*test2*")))
    (unwind-protect
        (progn
          ;; Start with global auto-response off
          (ecc-auto-response-stop)
          
          ;; Toggle in buf1
          (with-current-buffer buf1
            (ecc-auto-response-buffer-toggle))
          
          ;; Check states
          (should (buffer-local-value 'ecc-auto-response-buffer-enabled buf1))
          (should-not (buffer-local-value 'ecc-auto-response-buffer-enabled buf2)))
      (kill-buffer buf1)
      (kill-buffer buf2))))
```

## Related Issues
- Inconsistent naming convention between function and behavior
- User confusion about global vs buffer-local modes
- Help documentation doesn't clearly explain the dual behavior

## Priority
High - Core functionality issue that affects user experience and system design

## Workaround
Set `ecc-auto-response-default` to `t` to force buffer-local mode:
```elisp
(setq ecc-auto-response-default t)
```

## Solution Implemented
✅ **Fixed in commit 8815611** - `feature/fix-buffer-local-toggle`

The `ecc-auto-response-buffer-toggle` function now always operates on buffer-local state:
- Removed dual-mode behavior based on `ecc-auto-response-default`
- Function always toggles buffer-local auto-response regardless of global settings
- Simplified logic eliminates global mode path entirely
- Updated docstring to reflect buffer-local only operation

## Recommendation
Implement the proposed solution to remove global mode entirely and make the system buffer-local only. This will:
- Simplify the codebase
- Match user expectations  
- Prevent confusion
- Make the system more predictable