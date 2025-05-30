<!-- ---
!-- Timestamp: 2025-05-31 07:35:00
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/project_management/feature_requests/feature-request-ecc-refresh-timers.md
!-- --- -->

# Feature Request: ECC Refresh Timers Function

## Description
Create a centralized function `ecc-refresh-timers` that can refresh/restart all timers used in the emacs-claude-code system to resolve timer-related issues.

## Motivation
There are multiple timer-related issues occurring:
- Auto-response timer stops working
- Pulse timer for mode-line indicator stops
- Periodic return timer issues
- Timer lambda argument mismatches

Having a single function to refresh all timers would help users quickly recover from timer issues.

## Proposed Implementation

```elisp
(defun ecc-refresh-timers ()
  "Refresh all timers in the emacs-claude-code system."
  (interactive)
  ;; 1. Stop all existing timers
  ;; 2. Restart timers for active features
  ;; 3. Update mode-lines
  ;; 4. Display status message
  )
```

## Timers to Handle
1. **Auto-response main timer** (`--ecc-auto-response--timer`)
2. **Periodic return timers** (buffer-local `--ecc-auto-response--periodic-timer`)
3. **Pulse timers** (buffer-local `--ecc-auto-response--pulse-timer`)
4. **Auto-periodical timers** (from `ecc-auto-periodical.el`)
5. **Notification flash timer** (`--ecc-notification--flash-timer`)

## Benefits
- Single command to fix timer issues
- Easier debugging
- Better user experience
- Prevents accumulation of dead timers

<!-- EOF -->