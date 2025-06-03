# Yank as File Usage Guide

## Overview
The `ecc-vterm-yank-as-file` feature allows you to save kill-ring contents to a temporary file and automatically send a Read command to Claude in vterm.

## Usage

1. **Copy content to kill-ring**: Select and copy any text in Emacs (M-w or C-w)

2. **In vterm buffer**: Switch to your Claude vterm buffer

3. **Execute the command**: 
   - Use `M-x ecc-vterm-yank-as-file` (creates file in current directory)
   - Use `C-u M-x ecc-vterm-yank-as-file` (creates file in default directory)
   - Or use the keybinding `C-c C-y` (current dir) / `C-u C-c C-y` (default dir)

4. **Result**: 
   - A temp file `kill-ring-YYYYMMDD-HHMMSS.tmp` is created
   - The command `Read /path/to/kill-ring-YYYYMMDD-HHMMSS.tmp` is sent to vterm
   - Claude will read and process the file contents

## SSH Sessions
For SSH sessions where the current directory is remote:
- Use `C-u C-c C-y` to create the file in your local home directory
- Configure `ecc-vterm-yank-default-directory` to set a different default location

## Configuration
```elisp
;; Set default directory for yank files (useful for SSH)
(setq ecc-vterm-yank-default-directory "~/tmp/")
```

## Example
```
# Copy some code or text
# Switch to vterm with Claude
# Press C-c C-y (local) or C-u C-c C-y (default dir)
# Claude receives: "Read /path/to/kill-ring-20250604-064424.tmp"
```

## Notes
- The temp file is created locally on your Emacs machine
- Only works when in vterm-mode
- File naming includes timestamp for uniqueness
- For SSH sessions, use prefix arg to ensure file is accessible