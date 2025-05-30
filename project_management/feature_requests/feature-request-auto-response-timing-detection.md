# Feature Request: Auto-Response Timing and Send Detection

## Summary
Improve auto-response reliability by detecting when commands are actually sent (prompt box becomes empty) rather than relying on timing alone.

## Problem Description
Currently, auto-response sends commands based on timing intervals, but sometimes the return key press is not recognized properly. This can lead to commands like `/user:auto` sitting in the prompt without being sent.

## Proposed Solution

### 1. Prompt State Detection
- Monitor the prompt/input area to detect when it becomes empty
- When text is entered (e.g., `/user:auto`), track its position
- After sending, verify the text has moved up into the conversation history
- Only consider a command "sent" when the prompt is empty again

### 2. Send Status Tracking
- Introduce a "pending" state for auto-responses
- States: `pending` → `sending` → `sent`
- If still in `sending` state after timeout, retry the return key

### 3. Visual Feedback
- Show send status in the buffer (e.g., "Sending..." indicator)
- Change color/style when successfully sent
- Alert if send appears stuck

## Implementation Details

```elisp
(defvar-local --ecc-auto-response--send-status nil
  "Current send status: nil, 'pending, 'sending, or 'sent")

(defun --ecc-auto-response--check-prompt-empty-p ()
  "Check if the prompt area is empty."
  ;; Implementation depends on terminal mode
  )

(defun --ecc-auto-response--verify-send ()
  "Verify that the last command was actually sent."
  (when (eq --ecc-auto-response--send-status 'sending)
    (if (--ecc-auto-response--check-prompt-empty-p)
        (setq --ecc-auto-response--send-status 'sent)
      ;; Retry sending
      (--ecc-auto-response--retry-send))))
```

## Benefits
- More reliable auto-response execution
- No missed commands due to timing issues
- Clear feedback about send status
- Automatic retry on failure

## Technical Considerations
- Need to identify prompt area in different terminal modes (vterm, comint, etc.)
- Handle edge cases like multi-line input
- Performance impact of frequent prompt checking
- Integration with existing auto-response timer logic

## Priority
High - This addresses a core reliability issue with auto-response functionality