# Auto-Core Module Implementation Plan

## Module Purpose
The `ecc-auto-core.el` module serves as the foundation for the auto-response system. It provides core infrastructure for:
1. Managing auto-response timers and intervals
2. Tracking state across buffers
3. Throttling responses to prevent excessive activity
4. Registering and managing buffers for auto-response

## Key Dependencies
- `ecc-state-detection.el` - For detecting Claude prompt states
- `ecc-variables.el` - For shared variables and customization

## Variables

### Customization Variables
```elisp
(defgroup ecc-auto-core nil
  "Core settings for Claude auto-response functionality."
  :group 'ecc
  :prefix "ecc-auto-core-")

(defcustom ecc-auto-core-interval 1.0
  "Interval in seconds for checking Claude state and auto-responding."
  :type 'number
  :group 'ecc-auto-core)

(defcustom ecc-auto-core-throttle-time 2.0
  "Minimum time in seconds between auto-responses to prevent rapid firing."
  :type 'number
  :group 'ecc-auto-core)

(defcustom ecc-auto-core-initial-wait-time 1.0
  "Time to wait for the initial check after starting auto-response."
  :type 'number
  :group 'ecc-auto-core)

(defcustom ecc-auto-core-debug nil
  "Whether to show debug messages for auto-response activity."
  :type 'boolean
  :group 'ecc-auto-core)
```

### Internal Variables
```elisp
(defvar ecc-auto-core--timer nil
  "Timer object for auto-response checks.")

(defvar ecc-auto-core--last-state nil
  "The last detected Claude prompt state.")

(defvar ecc-auto-core--last-response-time 0
  "Timestamp of the last auto-response, used for throttling.")

(defvar ecc-auto-core--registered-buffers nil
  "List of buffers registered for auto-response.")

(defvar ecc-auto-core--initial-check-count 0
  "Counter for initial checking attempts.")
```

## Functions

### Timer Management
```elisp
(defun ecc-auto-core-timer-active-p ()
  "Return non-nil if the auto-response timer is active."
  (and ecc-auto-core--timer
       (timerp ecc-auto-core--timer)
       (timer-activate-when-idle ecc-auto-core--timer)))

(defun ecc-auto-core-timer-start (callback)
  "Start the auto-response timer with CALLBACK function.
Cancels any existing timer first."
  (ecc-auto-core-timer-stop)
  (setq ecc-auto-core--timer
        (run-with-timer ecc-auto-core-initial-wait-time
                        ecc-auto-core-interval
                        callback)))

(defun ecc-auto-core-timer-stop ()
  "Stop the auto-response timer if it exists."
  (when (timerp ecc-auto-core--timer)
    (cancel-timer ecc-auto-core--timer)
    (setq ecc-auto-core--timer nil)))
```

### State and Throttling Management
```elisp
(defun ecc-auto-core-update-state (state)
  "Update the tracked state to STATE and record timestamp."
  (setq ecc-auto-core--last-state state
        ecc-auto-core--last-response-time (float-time)))

(defun ecc-auto-core-throttled-p (state)
  "Return non-nil if auto-response for STATE should be throttled.
Prevents responses that are too frequent for the same state."
  (and ecc-auto-core--last-state
       (eq ecc-auto-core--last-state state)
       (< (- (float-time) ecc-auto-core--last-response-time)
          ecc-auto-core-throttle-time)))

(defun ecc-auto-core-reset-state ()
  "Reset the auto-response state tracking."
  (setq ecc-auto-core--last-state nil
        ecc-auto-core--last-response-time 0
        ecc-auto-core--initial-check-count 0))
```

### Buffer Management
```elisp
(defun ecc-auto-core-register-buffer (buffer)
  "Register BUFFER for auto-response.
Returns the buffer if registered, nil otherwise."
  (when (buffer-live-p buffer)
    (add-to-list 'ecc-auto-core--registered-buffers buffer)
    buffer))

(defun ecc-auto-core-unregister-buffer (buffer)
  "Remove BUFFER from auto-response registry."
  (setq ecc-auto-core--registered-buffers
        (delq buffer ecc-auto-core--registered-buffers)))

(defun ecc-auto-core-registered-buffers ()
  "Return list of currently registered buffers that are still live."
  (setq ecc-auto-core--registered-buffers
        (seq-filter #'buffer-live-p ecc-auto-core--registered-buffers)))

(defun ecc-auto-core-cleanup-buffers ()
  "Clean up the buffer registry by removing dead buffers."
  (ecc-auto-core-registered-buffers))
```

### Core Processing
```elisp
(defun ecc-auto-core-process-buffer (buffer callback)
  "Process BUFFER for auto-response, calling CALLBACK with state if needed.
The callback should take two arguments: the buffer and the detected state."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((state (ecc-detect-state)))
        (when (and state (not (ecc-auto-core-throttled-p state)))
          (when ecc-auto-core-debug
            (message "[Auto Core] Detected state %s in %s"
                     (ecc-state-get-name state)
                     (buffer-name buffer)))
          (funcall callback buffer state)
          (ecc-auto-core-update-state state))))))

(defun ecc-auto-core-initial-check (buffer callback)
  "Perform initial check for BUFFER, calling CALLBACK if state detected.
This function specifically targets the initial-waiting state with more
aggressive checking to catch Claude's initial prompt."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((state (ecc-detect-state)))
        (when (and state 
                   (eq state :initial-waiting)
                   (not (ecc-auto-core-throttled-p state)))
          (when ecc-auto-core-debug
            (message "[Auto Core] Detected initial state in %s"
                     (buffer-name buffer)))
          (funcall callback buffer state)
          (ecc-auto-core-update-state state)))))
  
  ;; Increment counter and schedule next check if needed
  (setq ecc-auto-core--initial-check-count (1+ ecc-auto-core--initial-check-count))
  (when (< ecc-auto-core--initial-check-count 5)
    (run-with-timer 0.5 nil #'ecc-auto-core-initial-check buffer callback)))

(defun ecc-auto-core-process-all-buffers (callback)
  "Process all registered buffers for auto-response using CALLBACK."
  (dolist (buffer (ecc-auto-core-registered-buffers))
    (ecc-auto-core-process-buffer buffer callback)))
```

### Initialization and Shutdown
```elisp
(defun ecc-auto-core-initialize ()
  "Initialize the auto-core system.
This resets state tracking and cleans up any existing resources."
  (ecc-auto-core-timer-stop)
  (ecc-auto-core-reset-state)
  (ecc-auto-core-cleanup-buffers))

(defun ecc-auto-core-shutdown ()
  "Shut down the auto-core system, cleaning up all resources."
  (ecc-auto-core-timer-stop)
  (ecc-auto-core-reset-state)
  (setq ecc-auto-core--registered-buffers nil))
```

### Debugging
```elisp
(defun ecc-auto-core-debug-status ()
  "Return a string with debug status information for auto-core."
  (format "Auto-Core Status:
  Timer Active: %s
  Last State: %s
  Last Response: %.2f seconds ago
  Registered Buffers: %d
  Initial Check Count: %d"
          (if (ecc-auto-core-timer-active-p) "Yes" "No")
          (if ecc-auto-core--last-state 
              (ecc-state-get-name ecc-auto-core--last-state)
            "None")
          (- (float-time) ecc-auto-core--last-response-time)
          (length (ecc-auto-core-registered-buffers))
          ecc-auto-core--initial-check-count))

(defun ecc-auto-core-toggle-debug ()
  "Toggle debug output for auto-core."
  (interactive)
  (setq ecc-auto-core-debug (not ecc-auto-core-debug))
  (message "Auto-core debug %s" (if ecc-auto-core-debug "enabled" "disabled"))
  (when ecc-auto-core-debug
    (message "%s" (ecc-auto-core-debug-status))))
```

## Backward Compatibility
```elisp
;; Add compatibility aliases for existing code
(defalias 'ecc-auto--get-timer 'ecc-auto-core-timer-active-p)
(defalias 'ecc-auto--start-timer 'ecc-auto-core-timer-start)
(defalias 'ecc-auto--stop-timer 'ecc-auto-core-timer-stop)
```

## Module Exports (provide)
```elisp
(provide 'ecc-auto-core)
```

## Test Plan
1. **Timer Tests**
   - Verify timer creation
   - Verify timer cancellation 
   - Test timer interval accuracy

2. **State Management Tests**
   - Test state updates
   - Verify throttling works correctly
   - Test state reset functionality

3. **Buffer Management Tests**
   - Test buffer registration
   - Test buffer unregistration
   - Verify dead buffer cleanup

4. **Processing Tests**
   - Test single buffer processing
   - Test multi-buffer processing
   - Verify initial check behavior

5. **Integration Tests**
   - Test with real callbacks
   - Verify proper state detection integration
   - Test debug output functionality

## Implementation Sequence
1. Create internal variables and customization options
2. Implement timer management functions
3. Add state and throttling management
4. Build buffer management functions
5. Create core processing functions
6. Add initialization and shutdown
7. Implement debugging utilities
8. Add backward compatibility aliases
9. Create comprehensive tests
10. Review and refine

This implementation plan provides a detailed guide for creating the ecc-auto-core.el module, ensuring all necessary functionality is included with proper organization and backward compatibility.