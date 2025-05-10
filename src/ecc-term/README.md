# Claude Code Terminal Integration

This directory contains modules for integrating Claude Code with terminal emulation in Emacs.

## Modules

### ecc-claude-vterm-mode.el

This module provides an optimized VTerm mode for Claude interaction. It enhances the standard vterm-mode with:

- Performance optimizations for handling large outputs
- Visual indicators for Claude state in the mode line
- Special keybindings for common Claude interactions (yes/no responses, interrupts, etc.)
- Fallback to special-mode when vterm is not available

#### Key Features

- Graceful degradation when vterm is not available
- Performance optimizations for streaming output
- Integration with the Claude buffer registry
- Custom mode-line indicators showing Claude's state

#### Keybindings

| Keybinding | Function | Description |
|------------|----------|-------------|
| C-c C-k    | ecc-claude-vterm-interrupt | Interrupt the current Claude process |
| C-c C-c    | ecc-claude-vterm-interrupt | Alternative binding for interrupt |
| C-c C-y    | ecc-claude-vterm-yes | Send 'y' response to Claude prompt |
| C-c C-n    | ecc-claude-vterm-no | Send 'n' response to Claude prompt |
| C-c C-r    | ecc-claude-vterm-retry | Send 'r' response to retry |
| C-c C-l    | ecc-claude-vterm-clear | Clear the vterm buffer |