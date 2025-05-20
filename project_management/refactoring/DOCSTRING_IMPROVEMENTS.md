# Docstring and Comment Improvements

## Current Documentation State

The current documentation in the term-claude mode module and related files is inconsistent:

1. **Inconsistent Docstring Format**: Some functions have detailed docstrings while others have minimal documentation
2. **Missing Parameter Documentation**: Many functions don't document their parameters
3. **Undocumented Return Values**: Return values are often not mentioned
4. **Inconsistent Style**: Different files use different documentation styles
5. **Missing Module-Level Documentation**: Some files lack comprehensive module documentation

## Docstring Standards for Emacs Lisp

According to Emacs Lisp conventions:

1. The first line should be a complete sentence that starts with a third-person verb
2. The first line should stand alone as a summary
3. A blank line should follow the first line if more documentation follows
4. Parameter descriptions should indicate name, type, and purpose
5. Return values should be documented
6. Optional behavior or edge cases should be noted
7. Examples should be provided for complex functions

## Examples of Current Docstrings

### Example 1: Minimal Documentation

```elisp
(defun ecc-term-claude-yes ()
  "Send 'y' response to Claude prompt."
  (interactive)
  (vterm-send-string "y")
  (vterm-send-return))
```

### Example 2: Better Documentation

```elisp
(defun ecc-vterm-yank-as-file (start end filename)
  "Yank the region between START and END to a file named FILENAME.
Extracts the selected region from the buffer and saves it to a file,
auto-detecting the appropriate file type based on content analysis.

START is the beginning position of the region.
END is the ending position of the region.
FILENAME is the target filename. When called interactively, prompts for this value.

When called interactively:
- Uses the currently active region
- Prompts for a filename with a default based on detected content type
- Offers to open the saved file in a new buffer

Auto-detects the file type based on content if no extension is provided."
  (interactive 
   ;; Interactive form documentation...
   )
  ;; Implementation...
  )
```

## Proposed Docstring Improvements

### Function Docstring Template

```elisp
(defun function-name (arg1 arg2 &optional opt-arg)
  "Does something specific with ARG1 and ARG2, optionally using OPT-ARG.

Detailed description of what the function does and how it behaves.
Any special cases or important notes should be mentioned here.

Arguments:
  ARG1: The first argument, which should be a string representing X.
  ARG2: The second argument, which should be a number indicating Y.
  OPT-ARG: Optional. When provided, changes behavior to Z. Default is nil.

Returns:
  A value representing the result, or nil if the operation failed.

Examples:
  (function-name \"example\" 42)        => \"example-42\"
  (function-name \"test\" 10 t)         => \"TEST-10\""
  ;; Implementation...
  )
```

### File Header Template

```elisp
;;; ecc-term-claude-mode.el --- Optimized vterm mode for Claude interaction -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Timestamp: <YYYY-MM-DD HH:MM:SS>
;; Package-Version: 1.0
;; Package-Requires: ((emacs "27.1") (vterm "0.0.1"))
;; Keywords: convenience, ai, claude
;; URL: https://github.com/ywatanabe/emacs-claude-code

;;; Commentary:

;; This module provides a specialized vterm mode optimized for interaction with
;; Claude AI.  It enhances the standard vterm with state detection, auto-response
;; capabilities, visual aids, and content management tools.
;;
;; Key features:
;; - State detection for different Claude prompts (waiting, y/n, etc.)
;; - Auto-response system to handle prompts automatically
;; - Visual enhancements for better Claude interaction
;; - Content extraction and management tools
;;
;; Usage:
;;   M-x ecc-term-claude RET          ; Create or switch to a Claude vterm buffer
;;   C-c C-a                          ; Toggle auto-response mode
;;   C-c C-y / C-c C-n                ; Send yes/no response
;;   C-c C-f                          ; Yank region to file
```

## Specific Improvements Needed

### 1. Major Functions

#### ecc-term-claude-mode

```elisp
(define-derived-mode ecc-term-claude-mode vterm-mode "Claude-VTerm"
  "Major mode for optimized Claude interaction in vterm.

This mode enhances vterm with specialized features for interacting with
Claude AI, including state detection, automatic responses to prompts,
and optimized performance settings for streaming output.

Claude-specific features include:
- Detection of different prompt types (yes/no, waiting, etc.)
- Auto-response to common prompts
- Visual indicators of Claude state
- Content extraction tools

You can also use `ecc-term-claude-enable' in an existing vterm buffer
to apply Claude features without changing the major mode.

\\{ecc-term-claude-mode-map}"
  ;; Implementation...
  )
```

#### ecc-term-claude

```elisp
(defun ecc-term-claude ()
  "Create a new Claude vterm buffer or enhance an existing one.

This function serves as the primary entry point for working with Claude
in vterm. It handles two scenarios:

1. When called from an existing vterm buffer, applies Claude settings to
   that buffer without changing the major mode.

2. When called from any other context, creates a new buffer with
   `ecc-term-claude-mode' as the major mode.

The function returns the buffer that was created or enhanced."
  (interactive)
  ;; Implementation...
  )
```

### 2. Core Functionality

#### ecc-term-claude-check-state

```elisp
(defun ecc-term-claude-check-state ()
  "Check and update the state of the current Claude vterm buffer.

Detects the current prompt state (if any) using the state detection
system and updates the mode line indicator accordingly.

When called interactively, forces an immediate state check.

Returns:
  A symbol representing the detected state (:y/n, :y/y/n, :waiting,
  :initial-waiting), or nil if no prompt state is detected."
  (interactive)
  ;; Implementation...
  )
```

#### ecc-term-claude-auto-send-accept

```elisp
(defun ecc-term-claude-auto-send-accept ()
  "Automatically respond to Claude prompts when auto-mode is enabled.

This function:
1. Checks if auto-mode is enabled
2. Detects the current Claude prompt state
3. Sends the appropriate response based on the state

The response sent depends on the detected state:
- For Y/N prompts: Sends `ecc-auto-response-y/n'
- For Y/Y/N prompts: Sends `ecc-auto-response-y/y/n'
- For waiting prompts: Sends `ecc-auto-response-waiting'
- For initial prompts: Sends `ecc-auto-response-initial-waiting'

This function is typically called from the vterm update hook when
new content appears in the buffer."
  ;; Implementation...
  )
```

### 3. Secondary Functions

#### ecc-term-claude-setup-mode-line

```elisp
(defun ecc-term-claude-setup-mode-line ()
  "Set up the mode line indicator for Claude state.

Configures the mode line to display the current Claude prompt state
by adding an indicator to `mode-line-process'. The indicator shows
different states:
- [Waiting] - Claude is waiting for input
- [Y/N] - Claude is asking a yes/no question
- [Y/Y/N] - Claude is offering multiple options"
  ;; Implementation...
  )
```

#### ecc-term-visual-aid-clear-all

```elisp
(defun ecc-term-visual-aid-clear-all ()
  "Clear all visual aid overlays from the current buffer.

Removes all overlays created by the visual aid system, including:
- Frame overlays around the buffer content
- Prompt highlighting overlays
- State indicator overlays

This function is typically called:
- When the buffer state changes
- When visual aids are toggled off
- When the buffer is killed"
  ;; Implementation...
  )
```

## Comment Improvements

### 1. Section Comments

Use consistent section comments to organize code:

```elisp
;;;; Major mode definition

;; Define keymap, menu, etc.
;; ...

;;;; State detection functions

;; Functions for detecting Claude prompt states
;; ...

;;;; Auto-response functionality

;; Functions for automatically responding to prompts
;; ...
```

### 2. Implementation Comments

Add comments to explain non-obvious implementation details:

```elisp
(defun ecc-term-claude-check-state ()
  "Check and update the state of the Claude VTERM buffer."
  (interactive)
  (when (eq major-mode 'ecc-term-claude-mode)
    (let ((state (ecc-term-claude-get-state)))
      ;; Force mode line update to reflect the new state
      (force-mode-line-update)
      ;; Return the state for potential use by callers
      state)))
```

### 3. Variable Comments

Improve comments for customization variables:

```elisp
(defcustom ecc-term-claude-state-update-interval 1.0
  "Interval in seconds for updating Claude state detection.
Lower values provide more responsive state detection but may impact
performance. Higher values reduce CPU usage but may delay state
detection. Values between 0.5 and 2.0 seconds are recommended."
  :type 'number
  :group 'ecc-term-claude)
```

## Implementation Plan

1. **Improve File Headers**:
   - Add comprehensive headers to all files
   - Include package metadata and usage information

2. **Update Major Function Docstrings**:
   - Focus on main entry points and user-facing functions
   - Add detailed descriptions of behavior and options

3. **Update Core Function Docstrings**:
   - Add parameter and return value documentation
   - Clarify function relationships and dependencies

4. **Add Section Comments**:
   - Organize code with consistent section headings
   - Group related functions with explanatory comments

5. **Improve Variable Documentation**:
   - Enhance customization variable descriptions
   - Document default values and usage implications

## Conclusion

Improving docstrings and comments throughout the term-claude mode module will:

1. Make the code more maintainable and easier to understand
2. Improve developer experience with better auto-completion documentation
3. Help users understand how to use the features
4. Facilitate future enhancements by clearly documenting current behavior

These improvements align with Emacs coding standards and best practices for documentation.