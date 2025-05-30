# URGENT: Auto-Response Fix Instructions

## Problem
Auto-response is not detecting Claude prompts correctly.

## Immediate Solution

1. **In your Claude buffer**, run these commands:

```elisp
;; Step 1: Load the fix
(load-file "/home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/fix-auto-response.el")

;; Step 2: Enable debug mode
(setq --ecc-debug-enabled t)

;; Step 3: Force enable auto-response
(--ecc-auto-response-enable-buffer)

;; Step 4: Test detection
(message "Current state: %s" (--ecc-state-detection-detect))
```

2. **If still not working**, try:

```elisp
;; Refresh all timers
(ecc-refresh-timers)

;; Check what's at the end of buffer
(message "Last 100 chars: %S" 
         (buffer-substring-no-properties 
          (max (- (point-max) 100) (point-min))
          (point-max)))
```

## Permanent Fix Needed

The issue is that the Claude interface format has changed. We need to:
1. Capture the actual prompt format when you see it
2. Update the patterns in `ecc-state-detection.el`

## Report Back

Please let me know:
1. What the output of the debug commands shows
2. What the actual Claude prompt looks like in your buffer

This will help create a permanent fix.