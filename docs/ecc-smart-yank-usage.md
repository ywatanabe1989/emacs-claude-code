# ECC Smart Yank

Smart yank functionality with visual diff when replacing regions.

## Overview

`ecc-smart-yank` enhances the standard Emacs yank (`C-y`) command by showing a visual diff when you paste over selected text. This helps you see exactly what changed.

## Features

- **Normal yank**: Works like standard `yank` when no region is selected
- **Replace with diff**: When a region is selected, shows a diff of what changed
- **Auto-hide**: Diff buffer automatically closes after idle time
- **Read-only diff**: Diff buffer is read-only and can be closed with `q`
- **Prefix argument support**: Use `C-u` prefix to yank from kill ring history

## Usage

### Basic Usage

```elisp
;; Bind to C-y (or any other key)
(global-set-key (kbd "C-y") 'ecc-smart-yank)
```

### Interactive Usage

1. **Normal paste** (no selection):
   ```
   C-y              → Paste from kill ring
   ```

2. **Replace with diff** (with selection):
   ```
   Select text
   C-y              → Replace and show diff
   q (in diff)      → Close diff buffer
   ```

3. **Yank from history**:
   ```
   C-u C-y          → Yank previous kill
   C-u 2 C-y        → Yank 2nd previous kill
   ```

## Configuration

### Auto-hide Delay

```elisp
;; Set delay before diff buffer auto-hides (default: 5 seconds)
(setq ecc-smart-yank-diff-hide-delay 5)

;; Disable auto-hide
(setq ecc-smart-yank-diff-hide-delay nil)
```

## Diff Buffer

The diff buffer (`*ECC Yank Diff*`) shows:
- Red lines (`-`): Text that was removed
- Green lines (`+`): Text that was added
- Context lines: Unchanged surrounding text

### Diff Buffer Keys

- `q`: Close the diff buffer
- Standard diff-mode keys work

## Example Scenarios

### Scenario 1: Update a variable value

**Before**:
```elisp
(setq my-var "old value")
      ^^^^^^^^^^^^^^^^
      (selected)
```

**After yanking `"new value"`**:
```elisp
(setq my-var "new value")
```

**Diff shown**:
```diff
--- old
+++ new
@@ -1 +1 @@
-(setq my-var "old value")
+(setq my-var "new value")
```

### Scenario 2: Identical content

If you yank the same text that's already selected:
- Text is replaced (no change)
- Message: "Region content was identical to yanked text."
- No diff buffer shown

## Integration

The smart-yank feature is automatically loaded when you load `emacs-claude-code`:

```elisp
(require 'ecc)  ; Includes ecc-smart-yank

;; Then bind it
(global-set-key (kbd "C-y") 'ecc-smart-yank)
```

## See Also

- `ecc-vterm-yank-as-file`: Yank clipboard content as a file in vterm
- Standard Emacs `yank` command: `C-h f yank`
