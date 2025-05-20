# Auto-Response Function Consolidation

## Current Implementation

The current implementation in `ecc-term-claude-mode.el` has separate functions for each type of auto-response:

```elisp
(defun ecc-term-claude-auto-send-accept ()
  "Automatically respond to Claude prompts in vterm mode."
  (when ecc-term-claude-auto-mode
    (let ((state (ecc-detect-simple-state)))
      (cond
       ((eq state :y/y/n)
        (ecc-term-claude-auto-send-y/y/n))
       ((eq state :y/n)
        (ecc-term-claude-auto-send-y/n))
       ((eq state :initial-waiting)
        (ecc-term-claude-auto-send-initial-waiting))
       ((eq state :waiting)
        (ecc-term-claude-auto-send-continue))))))

(defun ecc-term-claude-auto-send-y/n ()
  "Automatically respond with 'y' to Y/N prompts."
  (vterm-send-string ecc-auto-response-y/n)
  (vterm-send-return)
  (message "Auto-responded: %s" ecc-auto-response-y/n))

(defun ecc-term-claude-auto-send-y/y/n ()
  "Automatically respond with 'y' to Y/Y/N prompts."
  (vterm-send-string ecc-auto-response-y/y/n)
  (vterm-send-return)
  (message "Auto-responded: %s" ecc-auto-response-y/y/n))

(defun ecc-term-claude-auto-send-continue ()
  "Automatically respond to continue prompts."
  (vterm-send-string ecc-auto-response-waiting)
  (vterm-send-return)
  (message "Auto-responded: %s" ecc-auto-response-waiting))

(defun ecc-term-claude-auto-send-initial-waiting ()
  "Automatically respond to initial waiting prompts."
  (vterm-send-string ecc-auto-response-initial-waiting)
  (vterm-send-return)
  (message "Auto-responded: %s" ecc-auto-response-initial-waiting))
```

## Problems with Current Implementation

1. **Code Duplication**: The functions have identical structure but different response variables
2. **Inconsistent Naming**: `ecc-term-claude-auto-send-continue` vs. others (naming inconsistency)
3. **No Error Handling**: No validation of response variables or state
4. **Limited Extensibility**: Adding a new state type requires creating a new function
5. **No Throttling Integration**: Time-based throttling is handled elsewhere

## Proposed Consolidated Implementation

```elisp
;;;; Auto-response system

(defvar ecc-term-claude-auto-response-map
  '((:y/n . ecc-auto-response-y/n)
    (:y/y/n . ecc-auto-response-y/y/n)
    (:waiting . ecc-auto-response-waiting)
    (:initial-waiting . ecc-auto-response-initial-waiting))
  "Mapping from state symbols to auto-response variables.")

(defun ecc-term-claude-get-response-variable (state)
  "Get the appropriate response variable for STATE.
STATE should be one of: :y/y/n, :y/n, :waiting, :initial-waiting.
Returns the variable containing the response text."
  (let ((var-symbol (cdr (assq state ecc-term-claude-auto-response-map))))
    (if (and var-symbol (boundp var-symbol))
        var-symbol
      (user-error "No response defined for state %s" state))))

(defun ecc-term-claude-auto-send (state)
  "Automatically respond based on STATE.
STATE should be one of: :y/y/n, :y/n, :waiting, :initial-waiting.
Uses the appropriate response variable based on the state."
  (let* ((var (ecc-term-claude-get-response-variable state))
         (response (symbol-value var))
         (state-name (if (fboundp 'ecc-state-get-name)
                         (ecc-state-get-name state)
                       (format "%s" state))))
    (ecc-debug-message "Auto-sending response for %s state: %s" state-name response)
    (vterm-send-string response)
    (vterm-send-return)
    (message "Auto-responded to %s: %s" state-name response)
    (run-hooks 'ecc-auto-response-hooks)))

(defun ecc-term-claude-auto-send-accept ()
  "Check for Claude prompts and automatically respond if auto-mode is enabled.
Uses the unified state detection system to identify the current prompt type."
  (when ecc-term-claude-auto-mode
    (let ((state (ecc-term-claude-get-state)))
      (when state
        ;; Check for throttling if supported
        (if (and (featurep 'ecc-auto-response)
                 (fboundp 'ecc-auto-response-check-throttle))
            ;; Use the throttling mechanism
            (when (ecc-auto-response-check-throttle state)
              (ecc-term-claude-auto-send state))
          ;; No throttling, send immediately
          (ecc-term-claude-auto-send state))))))
```

## Benefits of Consolidated Approach

1. **Reduced Duplication**: The core response logic is defined once
2. **Consistent Structure**: All responses follow the same pattern
3. **Better Error Handling**: Added validation of response variables
4. **Enhanced Extensibility**: Adding a new state only requires updating the mapping
5. **Integration Support**: Optional integration with throttling mechanism
6. **Debug Support**: Added debug messages for troubleshooting
7. **Hook Support**: Runs hooks after sending for extensibility

## Integration with Other Modules

The consolidated implementation provides clear integration points:

1. **State Detection**: Uses `ecc-term-claude-get-state` for unified state detection
2. **Throttling**: Optional integration with `ecc-auto-response-check-throttle`
3. **Hooks**: Runs `ecc-auto-response-hooks` for extensibility
4. **Debugging**: Uses `ecc-debug-message` for troubleshooting

## Migration Plan

1. Add the new consolidated functions
2. Replace references to the individual functions with the consolidated version
3. Add aliases for backward compatibility
4. Update tests to verify functionality

## Testing Considerations

The consolidated approach should be tested for:

1. Each state type responds correctly
2. Error conditions are properly handled
3. Throttling integration works as expected
4. Hook execution happens correctly
5. Backward compatibility is maintained