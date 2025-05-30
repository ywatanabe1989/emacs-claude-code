# Bug Report: Error in Periodic Return Timer Function

## Issue Summary
Timer error when running `--ecc-auto-response--send-periodic-return`: `(no-catch --cl-block---ecc-auto-response--send-periodic-return-- nil)`

## Root Cause
The function used `cl-return-from` without a proper `cl-block` wrapper. This caused an error when the timer tried to execute the function.

## Location
File: `src/ecc-auto-response.el`
Function: `--ecc-auto-response--send-periodic-return` (line 285-313)

## Solution
Replaced `cl-return-from` with a simple `unless` conditional to avoid the need for block returns:

```elisp
;; Old code (line 294-296):
(when (eq current-state :running)
  (--ecc-debug-message "Claude is running, skipping periodic return")
  (cl-return-from --ecc-auto-response--send-periodic-return))

;; New code:
(unless (eq current-state :running)
  ;; ... rest of the function logic ...
)
```

## Status
Fixed - The function now uses standard conditional logic instead of block returns.