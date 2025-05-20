# Buffer-Local Configuration System

## Overview

The buffer-local configuration system allows each Claude buffer to have its own independent settings and state tracking. This is crucial for environments where multiple Claude instances are active simultaneously, each potentially requiring different response patterns and behaviors.

## Key Features

1. **Independent State Tracking**: Each buffer tracks its own Claude prompt state
2. **Buffer-Specific Response Patterns**: Customize response patterns per buffer
3. **Isolated Throttling**: Throttling is applied independently to each buffer
4. **Local Auto-Response Timers**: Each buffer has its own auto-response timer
5. **Buffer-Specific Debugging**: Enable/disable debug messages per buffer

## Architecture

The buffer-local configuration system is built on three main components:

1. **ecc-buffer-local.el**: Core implementation of buffer-local variables and state tracking
2. **ecc-buffer-api.el**: Public API for working with buffer-specific settings
3. **ecc-auto-response-buffer-local.el**: Auto-response system that uses buffer-local configuration

### Buffer-Local Variables

Each Claude buffer maintains its own set of buffer-local variables:

```elisp
;; State tracking variables
ecc-buffer-state                 ; Current detected prompt state
ecc-buffer-last-state-time       ; Timestamp of last state detection
ecc-buffer-last-time-alist       ; Throttling timestamps
ecc-buffer-active-state          ; Currently active state being processed

;; Configuration variables
ecc-buffer-auto-response-enabled      ; Whether auto-response is enabled
ecc-buffer-auto-response-y/n          ; Response for Y/N prompts
ecc-buffer-auto-response-y/y/n        ; Response for Y/Y/N prompts
ecc-buffer-auto-response-waiting      ; Response for waiting prompts
ecc-buffer-auto-response-initial-waiting  ; Response for initial waiting
ecc-buffer-auto-notify-on-prompt      ; Whether to notify on prompts
ecc-buffer-auto-notify-completions    ; Whether to notify on completions
ecc-buffer-debug-enabled              ; Whether debugging is enabled
```

## Usage

### Registering a Buffer

To use buffer-local configuration, first register the buffer:

```elisp
(ecc-buffer-register my-buffer)
```

This initializes all buffer-local variables with default values from the global configuration.

### Setting Buffer-Specific Configuration

Set buffer-specific response patterns:

```elisp
;; Setting Y/N response for a specific buffer
(ecc-buffer-auto-response-set-y/n "1" my-buffer)

;; Setting initial waiting response for a specific buffer
(ecc-buffer-auto-response-set-initial "/start" my-buffer)
```

### Enabling Auto-Response

Enable auto-response with buffer-local configuration:

```elisp
;; Enable for a specific buffer
(ecc-buffer-auto-response-enable my-buffer)

;; Or toggle on/off
(ecc-buffer-auto-response-toggle my-buffer)
```

### Checking Buffer State

Get the current state of a specific buffer:

```elisp
;; Detect and update buffer-local state
(ecc-buffer-state-detect my-buffer)

;; Check for specific states
(ecc-buffer-state-waiting-p my-buffer)
(ecc-buffer-state-y/n-p my-buffer)
```

### General Settings

Access and modify any buffer-local setting:

```elisp
;; Get a buffer-local setting
(ecc-buffer-settings-get 'ecc-buffer-auto-response-y/n my-buffer)

;; Set a buffer-local setting
(ecc-buffer-settings-set 'ecc-buffer-debug-enabled t my-buffer)
```

## Examples

### Working with Multiple Claude Instances

```elisp
;; Set up two Claude buffers with different configurations
(let ((buffer-a (get-buffer "*CLAUDE-A*"))
      (buffer-b (get-buffer "*CLAUDE-B*")))
  
  ;; Register both buffers
  (ecc-buffer-register buffer-a)
  (ecc-buffer-register buffer-b)
  
  ;; Configure buffer A for automated scientific research
  (ecc-buffer-auto-response-set-initial "/research-mode" buffer-a)
  (ecc-buffer-auto-response-set-y/n "1" buffer-a)
  
  ;; Configure buffer B for code generation
  (ecc-buffer-auto-response-set-initial "/code-mode" buffer-b)
  (ecc-buffer-auto-response-set-y/n "y" buffer-b)
  
  ;; Enable auto-response for both
  (ecc-buffer-auto-response-enable buffer-a)
  (ecc-buffer-auto-response-enable buffer-b))
```

### Debugging Specific Buffers

```elisp
;; Enable debug messages only for a specific buffer
(ecc-buffer-debug-toggle my-buffer)

;; Logging with buffer-local control
(with-current-buffer my-buffer
  (ecc-buffer-local-debug-message "Debug message for %s" (buffer-name)))
```

## Extending the System

To add new buffer-local settings:

1. Declare the buffer-local variable with `defvar-local`
2. Initialize it in `ecc-buffer-local-init`
3. Consider adding accessor functions in `ecc-buffer-api.el`

Example:

```elisp
;; 1. Declare the variable
(defvar-local ecc-buffer-custom-setting nil
  "Custom buffer-local setting.")

;; 2. Add to initialization
(defun ecc-buffer-local-init (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    ;; ... existing initialization ...
    
    ;; Initialize new setting
    (setq-local ecc-buffer-custom-setting 
                (and (boundp 'ecc-global-custom-setting)
                     ecc-global-custom-setting))))

;; 3. Add accessor (optional)
(defun ecc-buffer-custom-setting-set (value &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (setq-local ecc-buffer-custom-setting value)))
```

## Interaction with Global Configuration

When a buffer is registered, it inherits initial values from the global configuration. After that, each buffer's settings are entirely independent. Changing global settings after buffer registration will not affect existing buffers.

To update all buffers with new global settings, you can use:

```elisp
(mapc (lambda (buf-cons)
        (with-current-buffer (car buf-cons)
          (ecc-buffer-local-init)))
      ecc-buffer-registered-buffers-alist)
```

## Performance Considerations

The buffer-local system adds slightly more overhead than global configuration, but provides much greater flexibility. Each buffer maintains its own timer for auto-response checks, which are only active when the buffer has auto-response enabled.

For optimal performance:
- Disable auto-response for inactive buffers
- Use appropriate timer intervals (typically 0.5s is sufficient)
- Disable debug logging when not needed