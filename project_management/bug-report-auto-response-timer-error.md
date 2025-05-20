# Bug Report: Wrong Number of Arguments in Auto-Response Timer

## Description
When the auto-response timer (`ecc-check-and-respond`) runs, it throws an error:

```
Error running timer 'ecc-check-and-respond': (wrong-number-of-arguments #[(orig-fun) ((if (and (boundp 'ecc-buffer-auto-response-enabled) ecc-buffer-auto-response-enabled (boundp 'ecc-buffer-current-buffer) (buffer-live-p ecc-buffer-current-buffer)) (progn (save-current-buffer (set-buffer ecc-buffer-current-buffer) (if (boundp 'ecc-auto-notify-on-claude-prompt) (progn (let ((state (ecc-detect-simple-state))) (if (fboundp 'ecc-auto-notify-check-state) (progn (ecc-auto-notify-check-state state)))))) (let ((state (ecc-detect-simple-state))) (if (and state (not (ecc-auto-response-throttled-p state))) (progn (let ((ecc-auto-response-active-state state)) (ecc-auto-response-update-time state) (cond ((eq state :y/y/n) (ecc-auto--send-response ecc-buffer-current-buffer ecc-auto-response-y/y/n "Y/Y/N")) ((eq state :y/n) (ecc-auto--send-response ecc-buffer-current-buffer ecc-auto-response-y/n "Y/N")) ((eq state :initial-waiting) (ecc-auto--send-response ecc-buffer-current-buffer ecc-auto-response-initial-waiting "Initial-Waiting")) ((eq state :waiting) (ecc-auto--send-response ecc-buffer-current-buffer ecc-auto-response-waiting "Continue"))))))))))) (t) nil "Advice around `ecc-check-and-respond' to add throttling.
ORIG-FUN is the original function."] 0)
```

After the error, the auto-response system is disabled:
```
Auto-response stopped
Claude interaction counter reset to 0
Auto-response disabled
```

## Steps to Reproduce
1. Call `(ecc-start-auto-response)` to enable auto-response
2. Wait for the auto-response timer to fire

## Expected Behavior
The auto-response timer should run without errors and handle Claude prompts as expected.

## Current Behavior
The auto-response timer fails with a "wrong number of arguments" error when trying to call the advice function that overrides `ecc-check-and-respond`.

## Root Cause
The error is in `ecc-auto-response-fix.el`. When defining the advice for `ecc-check-and-respond`, the advice is applied with `advice-add` using `:override` but the advised function is not being called correctly.

The specific issue is in the advice definition:
1. The function `ecc-check-and-respond-advised` receives `orig-fun` as a parameter (as it would for `:around` advice).
2. However, since it's used with `:override`, the original function parameter isn't needed and isn't passed.
3. When the timer tries to call `ecc-check-and-respond`, it's calling the advised function with 0 arguments, but the advised function expects 1 argument.

## Fix Plan
1. Modify the `ecc-check-and-respond-advised` function to remove the unnecessary `orig-fun` parameter.
2. This will make the function signature compatible with the way it's being called by the timer.

## Priority and Severity
**Priority: High**

This is a critical issue that breaks the auto-response functionality, which is a core feature of the package. The error prevents users from using the auto-response system at all.

**Reasoning:**
1. The bug completely prevents the auto-response system from working
2. It's a simple fix that won't require extensive changes
3. This affects core functionality of the package