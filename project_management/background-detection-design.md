# Background Detection System Design

## Overview

The current Claude Code state detection system has two key limitations:
1. States are shared globally across buffers
2. Detection is often tied to cursor position (particularly in vterm mode)

This design document outlines a comprehensive approach to implementing:
1. Enhanced buffer-local state tracking
2. Background detection that works without relying on cursor position

## Current Architecture Analysis

The current system has:
- **State Detection**: In `ecc-state-detection.el`, functions analyze buffer content
- **Buffer-Local Variables**: In `ecc-buffer-local.el`, basic buffer-local state tracking
- **Auto-Response**: In `ecc-auto-response.el` and `ecc-auto-core.el`, timer-based checks

The primary limitations:
- Detection operates on visible buffer content near point-max
- State tracking is mixed between global and buffer-local approaches
- Timer system doesn't have a buffer-independent mechanism for detection

## Proposed Architecture

### 1. Enhanced Buffer-Local State Module

Create a true buffer-local state container with:
- Buffer-local variables for each detected state
- Timestamp tracking per buffer
- Independent throttling mechanisms per buffer

```elisp
;; Buffer-local state container
(defvar-local ecc-buffer-state-container
  (make-hash-table :test 'eq)
  "Hash table for buffer-local state information.")

;; Buffer-local state accessors
(defun ecc-buffer-state-get (buffer key &optional default)
  "Get buffer-local state value for KEY in BUFFER."
  (with-current-buffer buffer
    (gethash key ecc-buffer-state-container default)))

(defun ecc-buffer-state-set (buffer key value)
  "Set buffer-local state KEY to VALUE in BUFFER."
  (with-current-buffer buffer
    (puthash key value ecc-buffer-state-container)))
```

### 2. Background Detection Worker

Create a dedicated worker system that:
1. Registers buffers for background monitoring
2. Uses idle timers or background processing for state detection
3. Operates on buffer content without changing point position

```elisp
;; Background worker management
(defvar ecc-background-detection-active nil
  "Whether background detection is active.")

(defvar ecc-background-detection-timer nil
  "Timer for background detection.")

(defun ecc-background-detection-start ()
  "Start background detection for all registered buffers."
  (interactive)
  (setq ecc-background-detection-active t)
  (ecc-background-detection-setup-timer))

(defun ecc-background-detection-process-buffer (buffer)
  "Process BUFFER for state detection without changing point."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        ;; Detect state without affecting cursor
        (let ((state (ecc-detect-state-in-background)))
          (when state
            ;; Update buffer-local state
            (ecc-buffer-local-update-state state buffer)))))))
```

### 3. Cursor-Independent Detection

Modify detection methods to:
1. Use the entire buffer content or work with specific regions
2. Avoid relying on point/max-point relative positions
3. Support background scanning without UI disruption

```elisp
(defun ecc-detect-state-in-background (&optional buffer)
  "Detect Claude state in BUFFER without affecting point.
Returns detected state or nil."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      ;; Move to a safe detection position
      (goto-char (point-min))
      ;; Check the full buffer content
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        (ecc-detect-state-from-content content)))))

(defun ecc-detect-state-from-content (content)
  "Analyze CONTENT string for Claude prompt patterns.
Returns one of: :y/y/n, :y/n, :waiting, :initial-waiting, or nil."
  ;; Implementation using string matching instead of buffer positions
  ;; ...
  )
```

### 4. Integration with Auto-Response

Modify the auto-response system to:
1. Use the background detection worker for state detection
2. Operate on buffer-local state independently for each buffer
3. Handle multiple buffers concurrently with proper throttling

```elisp
(defun ecc-auto-response-buffer-local-check (buffer)
  "Check BUFFER for Claude prompts and respond if needed."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((state (ecc-buffer-local-state buffer)))
        (when (and state (not (ecc-buffer-local-throttled-p state buffer)))
          ;; Respond based on buffer-local configuration
          ;; ...
          )))))
```

## Implementation Plan

1. **Phase 1: Enhanced Buffer-Local State**
   - Extend `ecc-buffer-local.el` with robust state container
   - Update state tracking functions to use the container
   - Modify throttling mechanism to be buffer-local

2. **Phase 2: Background Detection Engine**
   - Create new module `ecc-background-detection.el`
   - Implement timer-based worker system
   - Add buffer registration and processing

3. **Phase 3: Cursor-Independent Detection**
   - Enhance detection functions to work without cursor position
   - Create content-based detection alternatives
   - Ensure performance for large buffers

4. **Phase 4: Integration**
   - Connect auto-response system to background detection
   - Update APIs for backward compatibility
   - Ensure all systems work together

## Testing Strategy

1. **Unit Tests**
   - Test buffer-local state container operations
   - Test background detection with various buffer contents
   - Test cursor-independent detection accuracy

2. **Integration Tests**
   - Test multiple buffers with different Claude states
   - Test detection and response across different buffer types
   - Verify throttling works correctly per buffer

3. **Performance Tests**
   - Measure CPU usage with many buffers
   - Test with large buffers to ensure efficiency
   - Verify detection works reliably with high buffer count

## Conclusion

This design provides a comprehensive approach to implementing buffer-local state tracking and cursor-independent background detection for Claude Code. The new architecture will support scaling to multiple buffers while maintaining performance and correctness.