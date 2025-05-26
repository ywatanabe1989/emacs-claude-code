<!-- ---
!-- Timestamp: 2025-05-25 02:58:00
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/docs/debug-usage.md
!-- --- -->

# Debug Message Usage in emacs-claude-code

## Overview

This project implements buffer-local debug messaging that writes to `*Messages*` buffer without interrupting the minibuffer, following the Debug Message Implementation Guidelines.

## Enabling Debug Messages

### Global Debug (All Buffers)
```elisp
;; Enable global debug
M-x ecc-debug-toggle-global

;; Or programmatically
(setq ecc-debug-enabled t)
```

### Buffer-Local Debug (Specific Buffer)
```elisp
;; In the buffer you want to debug
M-x ecc-debug-toggle-buffer

;; Or programmatically
(setq-local ecc-debug-buffer-enabled t)

;; For specific modules
(setq-local ecc-term-claude-buffer-debug-enabled t)
(setq-local ecc-auto-response-buffer-debug-enabled t)
```

### Category-Specific Debug
```elisp
;; Enable only specific categories
M-x ecc-debug-toggle-category RET auto-response RET
M-x ecc-debug-toggle-category RET buffer RET

;; View available categories
(describe-variable 'ecc-debug-categories)
```

## Debug Message Format

All debug messages follow the pattern:
```
[MODULE DEBUG buffer-name] message
```

Examples:
```
[ECC-BUFFER DEBUG *CLAUDE-VTERM*] Registering buffer: *CLAUDE-VTERM*
[ECC-AUTO-RESPONSE DEBUG *CLAUDE-VTERM*] Sending response: 1
[ECC-TEST DEBUG test-buffer] Running test: test-buffer-registration
```

## Module-Specific Debug Functions

### Buffer Management (`ecc-term-claude-buffer.el`)
```elisp
;; Enable debug for buffer operations
(setq-local ecc-term-claude-buffer-debug-enabled t)
```

### Auto-Response (`ecc-auto-response.el`)
```elisp
;; Enable debug for auto-response
(setq-local ecc-auto-response-buffer-debug-enabled t)
```

### Test Execution (`test-debug-helpers.el`)
```elisp
;; Use debug-enabled test definitions
(ert-deftest-with-debug test-my-feature ()
  "Test with automatic debug logging."
  (ecc-test-debug "Setting up test data")
  (should (equal (my-function) expected-value)))
```

## Viewing Debug Output

### Messages Buffer
```elisp
;; View all debug messages
C-h e  ; View *Messages* buffer

;; Or
M-x view-echo-area-messages
```

### Debug Log Buffer
```elisp
;; Enable logging to dedicated buffer
(setq ecc-debug-log-buffer-name "*Claude Debug Log*")

;; View debug log
M-x ecc-debug-view-log

;; Clear debug log
M-x ecc-debug-clear-log
```

## Best Practices

1. **Enable Locally**: Use buffer-local debug for specific issues
   ```elisp
   (with-current-buffer "*CLAUDE-VTERM*"
     (setq-local ecc-debug-buffer-enabled t))
   ```

2. **Disable When Done**: Turn off debug to reduce noise
   ```elisp
   M-x ecc-debug-toggle-buffer
   ```

3. **Use Categories**: Enable only relevant debug categories
   ```elisp
   (setq ecc-debug-enabled-categories '(auto-response buffer))
   ```

4. **Add Context**: Include relevant information in debug messages
   ```elisp
   (ecc-term-claude-buffer-debug "Registering buffer: %s (mode: %s)" 
                                 (buffer-name) major-mode)
   ```

## Troubleshooting

### No Debug Output
1. Check if debug is enabled: `M-x ecc-debug-print-state-info`
2. Verify buffer-local settings: `(describe-variable 'ecc-debug-buffer-enabled)`
3. Check category settings: `(describe-variable 'ecc-debug-enabled-categories)`

### Too Much Debug Output
1. Disable global debug: `M-x ecc-debug-toggle-global`
2. Use category filtering: `M-x ecc-debug-toggle-category`
3. Disable for specific buffers: `(setq-local ecc-debug-buffer-enabled nil)`

### Debug in Minibuffer
If debug messages appear in minibuffer:
1. Check implementation uses `(let ((inhibit-message t)) ...)`
2. Report as bug - all debug should go only to `*Messages*`

## Adding Debug to New Modules

```elisp
;;; my-new-module.el

;; 1. Add debug support
(defvar-local my-module-debug-enabled nil
  "Whether debug messages are enabled for my module.")

(defun my-module-debug (format-string &rest args)
  "Send debug message for my module operations."
  (when my-module-debug-enabled
    (let ((inhibit-message t))  ; Only to *Messages*, not minibuffer
      (message "[MY-MODULE DEBUG %s] %s" 
               (buffer-name) 
               (apply #'format format-string args)))))

;; 2. Use in critical functions
(defun my-critical-function (arg)
  "Do something critical with ARG."
  (my-module-debug "Starting critical operation with: %s" arg)
  ;; ... function body ...
  (my-module-debug "Critical operation completed"))
```

<!-- EOF -->