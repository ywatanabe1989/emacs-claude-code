# Term Claude Mode User Guide

## Overview

Term Claude Mode (ecc-term-claude-mode) is a specialized Emacs mode for interacting with Claude AI in a terminal environment. It enhances the standard vterm experience with features specifically designed for Claude interaction:

- **State Detection**: Automatically detects different Claude prompt states
- **Auto-Response**: Optional automatic responses to common Claude prompts
- **Visual Indicators**: Mode line displays current Claude state
- **Content Extraction**: Easily save Claude's responses to files
- **Performance Optimizations**: Tuned for handling large streaming outputs

## Installation

1. Ensure you have `vterm` installed in your Emacs environment
2. Load the term-claude-mode files:

```elisp
(add-to-list 'load-path "/path/to/emacs-claude-code/src")
(require 'ecc-term-claude-mode-v2)
```

## Getting Started

To open a new Claude terminal:

```
M-x ecc-term-claude
```

This will create a new buffer named `*CLAUDE-VTERM*` with all Claude features enabled.

If you're already in a vterm buffer and want to add Claude features without changing the major mode:

```
M-x ecc-term-claude-enable
```

## Key Features

### Claude State Detection

Term Claude Mode automatically detects different prompt states:

- **Y/N Prompts**: When Claude asks for yes/no input (`[y/n]`)
- **Y/Y/N Prompts**: Yes/Yes+/No choices (`[Y/y/n]`)
- **Waiting Prompts**: When Claude is waiting for you to continue (`continue>`)
- **Initial Prompts**: At the start of a Claude session

The current state is displayed in the mode line for easy reference.

### Auto-Response System

You can enable automatic responses to Claude prompts:

```
M-x ecc-term-claude-toggle-auto-mode
```

When enabled, term-claude will automatically:
- Send "y" to Yes/No prompts
- Send "y" to Yes/Yes+/No prompts
- Send an empty line to continue prompts

You can customize the auto-responses by setting these variables:
- `ecc-auto-response-y/n` - Response to Y/N prompts (default: "y")
- `ecc-auto-response-y/y/n` - Response to Y/Y/N prompts (default: "y")
- `ecc-auto-response-waiting` - Response to continue prompts (default: "")
- `ecc-auto-response-initial-waiting` - Response to initial prompts (default: "")

### Content Extraction

Extract Claude's responses to files with these commands:

- `C-c C-f` (`ecc-vterm-yank-as-file`): Save selected region to a file
- `C-c C-b` (`ecc-vterm-yank-buffer-as-file`): Save entire buffer to a file
- `C-c C-q` (`ecc-vterm-quick-yank-region`): Quick save region with auto-generated filename

The extract tool automatically detects content type and suggests appropriate file extensions.

### Key Bindings

| Key Binding | Command | Description |
|-------------|---------|-------------|
| `C-c C-y` | `ecc-term-claude-send-yes` | Send 'y' response |
| `C-c C-n` | `ecc-term-claude-send-no` | Send 'n' response |
| `C-c C-l` | `ecc-term-claude-clear-buffer` | Clear the buffer |
| `C-c C-a` | `ecc-term-claude-toggle-auto-mode` | Toggle auto-response mode |
| `C-c C-v` | `ecc-term-claude-toggle-follow-bottom` | Toggle follow-bottom behavior |
| `C-c C-f` | `ecc-vterm-yank-as-file` | Save region to file |
| `C-c C-b` | `ecc-vterm-yank-buffer-as-file` | Save buffer to file |
| `C-c C-q` | `ecc-vterm-quick-yank-region` | Quick save region |
| `C-c C-s` | `ecc-term-claude-switch-to-buffer` | Switch to another Claude buffer |

## Menu Access

Term Claude Mode provides a menu (Claude) in the menu bar with access to all major features.

## Customization

### Performance Settings

```elisp
;; Enable/disable performance mode
(setq ecc-term-claude-performance-mode t)

;; Adjust GC threshold for high-output operations (in bytes)
(setq ecc-term-claude-gc-threshold (* 64 1024 1024))  ; 64MB

;; Maximum buffer size to search for state detection
(setq ecc-term-claude-max-search-size 2000)

;; Delay fontification during rapid updates (in seconds)
(setq ecc-term-claude-jit-lock-defer-time 0.05)
```

### Visual Settings

```elisp
;; Show/hide line numbers
(setq ecc-term-claude-line-numbers nil)

;; Adjust scroll settings
(setq ecc-term-claude-scroll-conservatively 10000)

;; Enable/disable line truncation
(setq ecc-term-claude-truncate-lines t)

;; Default buffer name
(setq ecc-term-claude-buffer-name "*CLAUDE-VTERM*")

;; Always follow bottom margin (in lines)
(setq ecc-vterm-follow-bottom-margin 2)
```

### Auto-Response Settings

```elisp
;; Default auto-response mode state
(setq ecc-term-claude-auto-mode nil)

;; Response for Y/N prompts
(setq ecc-auto-response-y/n "y")

;; Response for Y/Y/N prompts
(setq ecc-auto-response-y/y/n "y")

;; Response for continue prompts
(setq ecc-auto-response-waiting "")

;; Response for initial prompts
(setq ecc-auto-response-initial-waiting "")

;; Delay before sending auto-response (in seconds)
(setq ecc-term-claude-auto-delay 0)
```

## Managing Multiple Claude Buffers

You can have multiple Claude buffers open simultaneously:

```elisp
;; Create a new Claude buffer with a custom name
(let ((ecc-term-claude-buffer-name "*CLAUDE-PROJECT-1*"))
  (ecc-term-claude))
```

Switch between Claude buffers with:
```
M-x ecc-term-claude-switch-to-buffer
```

## Troubleshooting

### Performance Issues

If you experience performance issues with large outputs:

1. Enable performance optimizations:
   ```elisp
   (setq ecc-term-claude-performance-mode t)
   ```

2. Increase the GC threshold:
   ```elisp
   (setq ecc-term-claude-gc-threshold (* 128 1024 1024))  ; 128MB
   ```

3. Reduce the state detection search size:
   ```elisp
   (setq ecc-term-claude-max-search-size 1000)
   ```

### State Detection Issues

If Claude states aren't being detected correctly, customize the prompt patterns:

```elisp
;; Custom patterns for state detection
(setq ecc-state-prompt-y/n "[y/n]")
(setq ecc-state-prompt-y/y/n "[Y/y/n]")
(setq ecc-state-prompt-waiting "continue>")
(setq ecc-state-prompt-initial-waiting "Enter 'y' to begin:")
```

## Advanced Features

### Customizing Auto-Response Behavior

You can create more sophisticated auto-responses by adding advice to the auto-response function:

```elisp
(defun my-custom-auto-response (state)
  "Custom auto-response based on buffer content and STATE."
  (when (eq state :y/n)
    (let ((buffer-text (buffer-string)))
      (cond
       ;; Different response for specific prompts
       ((string-match-p "dangerous operation" buffer-text)
        (vterm-send-string "n")
        (vterm-send-return)
        t)  ; Return t to indicate we handled it
       (t nil)))))  ; Return nil to use default handling

;; Add the advice
(advice-add 'ecc-term-claude-auto-send :before-until #'my-custom-auto-response)
```

### Creating Custom Visual Indicators

You can create custom mode line indicators:

```elisp
(defun my-custom-mode-line-indicator ()
  "Custom mode line indicator for Claude state."
  (let ((state (ecc-term-claude-get-state)))
    (cond
     ((eq state :waiting) " [⏩]")  ; Unicode symbols
     ((eq state :y/n) " [⁇]")
     (t ""))))

;; Replace the standard indicator
(setq-default mode-line-process
              '(:eval (my-custom-mode-line-indicator)))
```

## Extension API

Term Claude Mode provides several extension points:

### Hooks

- `ecc-term-claude-mode-hook`: Run after mode initialization
- `ecc-term-claude-update-functions`: Run after vterm output updates

### Example: Adding Custom Post-Processing

```elisp
(defun my-claude-post-processing ()
  "Process Claude output after updates."
  (let ((state (ecc-term-claude-get-state)))
    (when (eq state :waiting)
      (message "Claude is waiting for input"))))

(add-hook 'ecc-term-claude-update-functions #'my-claude-post-processing)
```