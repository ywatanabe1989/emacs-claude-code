# Yank as File Usage Guide

## Overview
The `ecc-vterm-yank-as-file` feature allows you to save kill-ring contents to a temporary file and automatically send a Read command to Claude in vterm.

## Usage

1. **Copy content to kill-ring**: Select and copy any text in Emacs (M-w or C-w)

2. **In vterm buffer**: Switch to your Claude vterm buffer

3. **Execute the command**: 
   - Use `M-x ecc-vterm-yank-as-file`
   - Or use the keybinding `C-c C-y` (if in vterm-mode)

4. **Result**: 
   - A temp file `kill-ring-YYYYMMDD-HHMMSS.tmp` is created in the current directory
   - The command `Read /path/to/kill-ring-YYYYMMDD-HHMMSS.tmp` is sent to vterm
   - Claude will read and process the file contents

## Example
```
# Copy some code or text
# Switch to vterm with Claude
# Press C-c C-y
# Claude receives: "Read ./kill-ring-20250604-064424.tmp"
```

## Notes
- The temp file is created in the vterm buffer's current directory
- Only works when in vterm-mode
- File naming includes timestamp for uniqueness