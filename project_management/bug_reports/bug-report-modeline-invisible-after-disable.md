<!-- Bug Report: Modeline Becomes Invisible After Disabling Auto Mode -->
<!-- Author: ywatanabe -->
<!-- Date: 2025-05-29 -->
<!-- Status: Fixed -->

# Bug Report: Modeline Becomes Invisible When Auto Mode is Turned Off

## Summary
When disabling auto-response mode, the modeline would become invisible or lose important elements like the buffer name.

## Root Cause
The modeline restoration logic had several issues:
1. Stored `(default-value 'mode-line-format)` instead of the buffer's actual mode-line-format
2. Didn't properly clean up the stored original mode-line variable after restoration
3. The stored default might not match the buffer's actual modeline configuration

## Solution Implemented
Fixed in `src/ecc-auto-response.el` by improving the modeline storage and restoration logic:

### Key Changes:
1. **Store actual buffer modeline**: Now stores the buffer's current `mode-line-format` before modification, not the global default
2. **Proper cleanup**: Added `kill-local-variable` for `--ecc-auto-response--original-mode-line` after restoration
3. **Moved storage logic**: Storage now happens when enabling (inside the `if` block) rather than at function start

### Code Changes:
```elisp
(defun --ecc-auto-response--update-mode-line ()
  "Update mode-line to show auto-response status."
  (if --ecc-auto-response--enabled
      ;; Add AUTO indicator to mode-line
      (progn
        ;; Store the current mode-line-format before modifying
        (unless (local-variable-p '--ecc-auto-response--original-mode-line)
          (setq-local --ecc-auto-response--original-mode-line
                      (if (local-variable-p 'mode-line-format)
                          mode-line-format
                        (default-value 'mode-line-format))))
        ;; ... add indicator logic ...
        )
    ;; Remove AUTO indicator and restore original
    (when (local-variable-p '--ecc-auto-response--original-mode-line)
      (setq mode-line-format --ecc-auto-response--original-mode-line)
      (kill-local-variable 'ecc-auto-indicator)
      (kill-local-variable '--ecc-auto-response--original-mode-line)))
  (force-mode-line-update))
```

## Test Results
✅ All tests pass (48/48 tests, 100% pass rate)
✅ Modeline properly shows [AUTO] indicator when enabled
✅ Modeline fully restored (including buffer name) when disabled
✅ No lingering state after disable/enable cycles

## Impact
- **Fixed**: Modeline remains fully visible after disabling auto mode
- **Fixed**: All modeline elements (buffer name, mode indicators, etc.) are preserved
- **Fixed**: Clean state management prevents accumulation of old variables

## Status
✅ **FIXED** - Modeline now properly restores to its original state when auto mode is disabled