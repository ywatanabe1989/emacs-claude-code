# Feature Request: Yank as File

## Description
Create a simple yank-as-file function that:
1. Creates a .tmp file in the current directory
2. Writes the latest kill-ring content to the file
3. Sends a message to the vterm buffer with the file path

## Requirements
- Function should work in vterm-mode
- Create temporary file with unique name
- Send "Read <temp-path>" message to vterm
- This signals Claude to read and understand kill-ring contents

## Implementation Plan
- [ ] Create new file `ecc-vterm-yank-as-file.el`
- [ ] Implement `ecc-vterm-yank-as-file` function
- [ ] Add keybinding for easy access
- [ ] Create tests for the new functionality
- [ ] Update main ecc.el to load the new module

## Progress
- [x] Implementation - Created ecc-vterm-yank-as-file.el with main function
- [x] Testing - Added comprehensive tests in test-ecc-vterm-yank-as-file.el
- [x] Documentation - Added docstrings with examples
- [x] Integration - Updated ecc.el to load the new module