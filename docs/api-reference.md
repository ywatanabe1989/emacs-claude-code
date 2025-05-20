# Claude Code API Reference

This document provides a comprehensive reference for the Claude Code API, focusing on the refactored modules for state detection and auto-response.

## Core API Modules

The Claude Code API is organized into the following core modules:

1. **ecc-api**: Public API for interacting with Claude buffers, detecting prompt states, and sending responses
2. **ecc-state-detection**: Centralized state detection functionality
3. **ecc-auto-response-unified**: Unified auto-response system
4. **ecc-variables**: Global variables and configuration

## Buffer Management API

### Register and Set Buffers

```elisp
(ecc-buffer-register BUFFER)
```
Register a buffer as a Claude buffer and make it the current active buffer.

```elisp
(ecc-buffer-set-current BUFFER)
```
Set a buffer as the current active Claude buffer without changing other settings.

### Buffer Information

```elisp
(ecc-buffer-list)
```
Return a list of all registered Claude buffers.

```elisp
(ecc-buffer-current)
```
Return the current active Claude buffer.

## State Detection API

### Core Detection

```elisp
(ecc-state-detect &optional BUFFER)
```
Detect Claude prompt state in BUFFER or current buffer. Returns one of:
- `:y/y/n` for yes/yes/no prompts
- `:y/n` for yes/no prompts
- `:waiting` for continue prompts
- `:initial-waiting` for initial prompts
- `nil` if no prompt is detected

### State Predicates

```elisp
(ecc-state-waiting-p &optional BUFFER)
```
Return non-nil if BUFFER has a waiting prompt.

```elisp
(ecc-state-y/n-p &optional BUFFER)
```
Return non-nil if BUFFER has a Y/N prompt.

```elisp
(ecc-state-y/y/n-p &optional BUFFER)
```
Return non-nil if BUFFER has a Y/Y/N prompt.

```elisp
(ecc-state-initial-waiting-p &optional BUFFER)
```
Return non-nil if BUFFER has an initial waiting prompt.

## Auto-Response API

### Enable/Disable Auto-Response

```elisp
(ecc-auto-response-enable &optional BUFFER)
(ecc-auto-start &optional BUFFER)  ; Alias
```
Enable auto-response for Claude prompts in BUFFER or current buffer.

```elisp
(ecc-auto-response-disable)
(ecc-auto-stop)  ; Alias
```
Disable auto-response for Claude prompts.

```elisp
(ecc-auto-response-toggle)
(ecc-auto-toggle)  ; Alias
```
Toggle auto-response on/off.

### Send Responses

```elisp
(ecc-auto-response-send RESPONSE &optional BUFFER)
```
Send a custom RESPONSE to Claude in BUFFER or current buffer.

```elisp
(ecc-auto-response-send-state-based &optional BUFFER)
```
Send appropriate response based on current state in BUFFER.

### Convenience Functions

```elisp
(ecc-auto-response-yes &optional BUFFER)
(ecc-yes)  ; Alias
```
Automatically send Y response to Claude Y/N prompt.

```elisp
(ecc-auto-response-yes-plus &optional BUFFER)
(ecc-yes-plus)  ; Alias
```
Automatically send Y response to Claude Y/Y/N prompt.

```elisp
(ecc-auto-response-continue &optional BUFFER)
(ecc-continue)  ; Alias
```
Automatically send continue to Claude waiting prompt.

```elisp
(ecc-auto-response-template TEMPLATE-TEXT)
```
Send custom TEMPLATE-TEXT to Claude.

## Debugging API

```elisp
(ecc-debug-toggle)
```
Toggle debug message output for Claude.

## Internal Modules

These modules are not meant to be used directly but provide the implementation for the API:

### State Detection Module (ecc-state-detection.el)

Core state detection functions:
- `ecc-detect-state`: Main detection function that selects the best method
- `ecc-detect-basic-state`: Basic content-based detection
- `ecc-detect-alternative-initial-waiting`: Check for alternate initial prompt patterns
- `ecc-state-get-name`: Convert state symbol to human-readable name

### Auto-Response Module (ecc-auto-response-unified.el)

Core auto-response functions:
- `ecc-auto-response-check`: Check buffer state and respond if appropriate
- `ecc-auto-response-process-state`: Process response based on detected state
- `ecc-auto-response-send-response`: Send response to Claude
- `ecc-auto-response-send-vterm-response`: Send response in vterm mode
- `ecc-auto-response-initial-check`: Initial check with throttling bypass
- `ecc-auto-response-throttled-p`: Check if response should be throttled

## Configuration Variables

Important variables that can be customized:

### State Detection Variables

```elisp
ecc-state-detection-buffer-size
```
Number of characters to check for basic prompt detection (default: 2000).

```elisp
ecc-state-detection-line-count
```
Number of lines to check for line-based prompt detection (default: 50).

### Response Text Variables

```elisp
ecc-auto-response-y/n
```
Response for Y/N prompts (default: "1").

```elisp
ecc-auto-response-y/y/n
```
Response for Y/Y/N prompts (default: "2").

```elisp
ecc-auto-response-waiting
```
Response for waiting prompts (default: "/auto").

```elisp
ecc-auto-response-initial-waiting
```
Response for initial waiting state (default: "/user:understand-guidelines").

### Auto-Response Timing Variables

```elisp
ecc-auto-response-throttle-time
```
Minimum seconds between auto-responses (default: 5.0).

```elisp
ecc-auto-response-timer-interval
```
Interval in seconds for auto-response timer checks (default: 0.5).

### Debugging Variables

```elisp
ecc-debug-enabled
```
Whether to enable debugging output (default: nil).

## Integration with VTerm

For VTerm integration, the Claude API provides:

```elisp
(ecc-auto-response-connect-to-vterm-hook)
```
Connect auto-response to vterm output hooks for automatic checking.