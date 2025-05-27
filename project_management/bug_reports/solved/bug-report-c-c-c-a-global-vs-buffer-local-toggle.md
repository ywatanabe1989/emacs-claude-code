<!-- ---
!-- Timestamp: 2025-05-27 14:30:00
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/project_management/bug_reports/bug-report-c-c-c-a-global-vs-buffer-local-toggle.md
!-- --- -->

# Bug Report: C-c c a Toggles Auto Response Globally Instead of Buffer-Locally

## Problem Description
The keybinding `C-c c a` is supposed to toggle auto-response behavior for the current buffer only, but it appears to be applying the toggle globally instead of being buffer-local.

## Expected Behavior
- `C-c c a` should toggle auto-response only for the current buffer
- Other buffers should maintain their existing auto-response state
- Each buffer should have independent auto-response configuration

## Actual Behavior
- `C-c c a` toggles auto-response but applies it globally
- All buffers are affected by the toggle, not just the current buffer

## Investigation

### Keybinding Analysis
From `src/ecc-convenience-commands.el:311`:
```elisp
(define-key map (kbd "C-c c a") 'ecc-auto-response-buffer-toggle)
```

The keybinding correctly maps to `ecc-auto-response-buffer-toggle`, which should be buffer-local.

### Function Analysis
From `src/ecc-auto-response.el:798-810`:
```elisp
(defun ecc-auto-response-buffer-toggle (&optional buffer)
  "Toggle buffer-local auto-response for BUFFER on or off.
  Always operates on buffer-local state regardless of global settings."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      ;; Always use buffer-local mode
      (if ecc-auto-response-buffer-enabled
          (ecc-auto-response-buffer-stop buf)
        ;; When starting, ensure buffer is registered
        (progn
          (ecc-auto-response-register-buffer buf)
          (ecc-auto-response-buffer-start buf))))))
```

### Potential Root Causes

1. **Default Mode Configuration**: The variable `ecc-auto-response-default` (line 172) controls whether buffer-local mode is used by default. If this is `nil`, the system might fall back to global mode.

2. **Global System Interference**: The `ecc-auto-response-buffer-start` function enables the global auto-response system if not already enabled (lines 760-763):
```elisp
;; Enable global auto-response system if not already enabled
(unless ecc-auto-response-enabled
  (setq ecc-auto-response-enabled t)
  (ecc-auto-response--debug "Enabled global auto-response system"))
```

3. **Processing Logic**: In `ecc-auto-response--process-single-buffer` (line 551), the mode is determined by `ecc-auto-response-default` rather than the actual buffer state.

## Bug Fix Progress
- [x] Identify root cause: Global/buffer-local mode switching logic
- [x] Fix buffer-local toggle to operate independently of global state
- [x] Ensure `ecc-auto-response-default` properly controls mode selection
- [x] Test buffer-local independence across multiple buffers
- [x] Verify backward compatibility

## Solution Implemented

### Key Changes Made:
1. **Enhanced `ecc-auto-response-buffer-toggle`**: Modified to force buffer-local mode by setting `ecc-auto-response-default` to `t` during the toggle operation.

2. **Improved Buffer Processing Logic**: Updated `ecc-auto-response--process-single-buffer` to check actual buffer state rather than relying solely on global defaults.

3. **Independent State Management**: Each buffer now maintains its own auto-response state independently of global settings.

4. **Safe Dependency Loading**: Made `ecc-optimize-vterm` call conditional to prevent test failures.

### Files Modified:
- `src/ecc-auto-response.el`: Core logic improvements
- `tests/test-auto-response-buffer-local-toggle.el`: Comprehensive test suite

### Test Results:
All 3 buffer-local toggle tests pass:
- `test-buffer-local-toggle-independence`: ✅ PASSED
- `test-buffer-local-toggle-no-global-interference`: ✅ PASSED  
- `test-buffer-local-processing-independence`: ✅ PASSED

## Reproduction Steps
1. Open multiple vterm buffers with Claude
2. Use `C-c c a` in one buffer to enable auto-response
3. Check if auto-response is enabled in other buffers (it should not be)
4. Use `C-c c a` again to disable auto-response
5. Verify other buffers are not affected

## Expected Fix
The buffer-local toggle should:
1. Only affect the current buffer's auto-response state
2. Not interfere with global auto-response settings
3. Maintain independent state per buffer
4. Properly respect `ecc-auto-response-default` configuration

## Related Files
- `src/ecc-auto-response.el` - Main auto-response logic
- `src/ecc-convenience-commands.el` - Keybinding definitions
- `src/ecc-variables.el` - Variable definitions

<!-- EOF -->