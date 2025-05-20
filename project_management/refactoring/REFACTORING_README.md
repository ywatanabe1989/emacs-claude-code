# Emacs Claude Code - Refactored Modules

This document provides an overview of the refactored modules in the Emacs Claude Code project, explaining their organization, key functions, and usage patterns.

## Table of Contents

1. [Auto-Response System](#auto-response-system)
2. [State Detection](#state-detection)
3. [Variables](#variables)
4. [Migration Guide](#migration-guide)
5. [Testing](#testing)

## Auto-Response System

The auto-response system (`ecc-auto-response-refactored.el`) provides mechanisms to automatically respond to Claude prompts.

### Core Functions

- `ecc-auto-response-start`: Start the auto-response system
- `ecc-auto-response-stop`: Stop the auto-response system
- `ecc-auto-response-toggle`: Toggle auto-response on/off
- `ecc-auto-response-check`: Check current buffer state and respond automatically
- `ecc-auto-response-send`: Explicitly send appropriate response based on buffer state
- `ecc-auto-response-send-current-buffer`: Convenience function for current buffer

### Response Functions

- `ecc-auto-response-send-yes`: Send Y response to Y/N prompt
- `ecc-auto-response-send-yes-plus`: Send Y response to Y/Y/N prompt
- `ecc-auto-response-send-continue`: Send continue response to waiting prompt
- `ecc-auto-response-send-template`: Send custom template text

### Internal Functions

- `ecc-auto-response-send-message`: Core function to send a response
- `ecc-auto-response-send-to-vterm`: Send response to vterm buffer
- `ecc-auto-response-display-notification`: Display notification about sent response
- `ecc-auto-response-process-state`: Process auto-response for detected state
- `ecc-auto-response-throttled-p`: Check if response should be throttled
- `ecc-auto-response-update-time`: Update last response time
- `ecc-auto-response-initial-check`: Perform initial prompt check

### Connection to Vterm

- `ecc-auto-response-connect-to-vterm-hook`: Connect auto-response to vterm output

### Buffer Registration

- `ecc-register-buffer`: Register current buffer for auto-response

## State Detection

The state detection system (`ecc-state-detection-refactored.el`) provides reliable detection of Claude prompt states.

### Core Functions

- `ecc-detect-state`: Main detection function that selects the best method
- `ecc-detect-basic-state`: Basic detection using buffer content
- `ecc-detect-prompt-in-last-lines`: Line-based detection for precise results
- `ecc-detect-prompt-in-region`: Detection in a specific buffer region

### Helper Functions

- `ecc-detect-alternative-initial-waiting`: Check for alternative waiting patterns
- `ecc-state-notify-if-prompt-detected`: Check and notify about prompts
- `ecc-state-get-name`: Convert state symbol to human-readable name

### Aliases for Backward Compatibility

- `ecc-detect-simple-state`: Alias to `ecc-detect-state`
- `ecc-detect-enhanced-state`: Alias to `ecc-detect-state`

## Variables

The variables module (`ecc-variables-refactored.el`) provides a centralized location for all configuration options and variables.

### Customization Groups

- `emacs-claude-code`: Main customization group
- `ecc-buffers`: Buffer management settings
- `ecc-auto-response`: Auto-response settings
- `ecc-state-detection`: State detection settings
- `ecc-vterm`: VTerm integration settings
- `ecc-notification`: Notification settings

### Key Variables

#### Buffer Variables

- `ecc-buffer-prefix`, `ecc-buffer-suffix`: Buffer name formatting
- `ecc-buffer-counter`: Counter for buffer numbering
- `ecc-buffer-current-buffer`: Currently active buffer
- `ecc-buffer-auto-response-enabled`: Whether auto-response is enabled

#### Auto-Response Variables

- `ecc-auto-response-y/n`: Response for Y/N prompts
- `ecc-auto-response-y/y/n`: Response for Y/Y/N prompts
- `ecc-auto-response-waiting`: Response for waiting state
- `ecc-auto-response-initial-waiting`: Response for initial waiting state
- `ecc-auto-response-throttle-time`: Minimum seconds between responses

#### State Detection Variables

- `ecc-state-detection-buffer-size`: Characters to check for detection
- `ecc-state-detection-line-count`: Lines to check for detection
- `ecc-state-prompt-y/n`: Pattern for Y/N prompts
- `ecc-state-prompt-y/y/n`: Pattern for Y/Y/N prompts
- `ecc-state-prompt-waiting`: Pattern for waiting prompts
- `ecc-state-prompt-initial-waiting`: Pattern for initial waiting

#### Debugging

- `ecc-debug-enabled`: Whether to enable debugging output
- `ecc-debug-message`: Macro for debug messages
- `ecc-toggle-debug`: Function to toggle debugging

## Migration Guide

### Function Name Changes

| Old Function Name | New Function Name |
|-------------------|-------------------|
| `ecc-auto-accept-send` | `ecc-auto-response-send-current-buffer` |
| `ecc-auto--send-response` | `ecc-auto-response-send-message` |
| `ecc-auto--send-vterm-response` | `ecc-auto-response-send-to-vterm` |
| `ecc-auto--notify` | `ecc-auto-response-display-notification` |
| `ecc-auto-response-yes` | `ecc-auto-response-send-yes` |
| `ecc-auto-response-yes-plus` | `ecc-auto-response-send-yes-plus` |
| `ecc-auto-response-continue` | `ecc-auto-response-send-continue` |
| `ecc-auto-response-template` | `ecc-auto-response-send-template` |
| `ecc-detect-simple-state` | `ecc-detect-state` |
| `ecc-start-auto-response` | `ecc-auto-response-start` |
| `ecc-stop-auto-response` | `ecc-auto-response-stop` |
| `ecc-toggle-auto-response` | `ecc-auto-response-toggle` |
| `ecc-check-and-respond` | `ecc-auto-response-check` |

## Testing

Updated test files are provided to ensure compatibility with the refactored code:

- `ecc-auto-response-test-updated.el`: Tests for auto-response functionality
- `test-notification-format-updated.el`: Tests for notification formatting
- `test-initial-waiting-updated.el`: Tests for initial waiting detection

To run tests with the refactored code:

1. Load the refactored files
2. Load the updated test files
3. Run the tests with `ert-run-tests-batch`