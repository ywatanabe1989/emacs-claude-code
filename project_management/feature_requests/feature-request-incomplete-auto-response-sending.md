# Feature Request: Fix Incomplete Auto-Response Text Sending

## Summary
Auto-response text sometimes appears in the textbox but is not sent completely, requiring improved text insertion and sending mechanisms.

## Problem Description
When auto-response is enabled, the response text (like `/user:auto`) is written to the textbox but occasionally:
- Only part of the text is sent
- The text appears but doesn't get submitted at all
- Characters may be dropped or truncated

This is likely related to timing issues between text insertion and return key sending.

## Current Behavior
1. Auto-response detects Claude prompt
2. Text is inserted into the buffer
3. Return key is sent
4. **Issue**: Sometimes text is incomplete or not fully processed before return

## Proposed Solutions

### 1. Text Verification Before Sending
```elisp
(defun --ecc-auto-response--verify-text-complete (expected-text)
  "Verify that EXPECTED-TEXT is fully present in the prompt."
  ;; Check if the full text is in the prompt area
  ;; Wait briefly if text is still being inserted
  )
```

### 2. Chunked Text Insertion
- Insert text character by character with micro-delays
- Ensures each character is properly registered
- More reliable for terminal emulators

### 3. Buffer State Synchronization
- Wait for buffer to be "idle" before inserting
- Check for any pending operations
- Ensure terminal is ready to receive input

### 4. Send Confirmation Loop
```elisp
(defun --ecc-auto-response--send-with-verification (text)
  "Send TEXT and verify it was sent completely."
  (let ((attempts 0)
        (max-attempts 3))
    (while (and (< attempts max-attempts)
                (not (--ecc-auto-response--text-sent-p text)))
      ;; Insert text
      ;; Wait
      ;; Check if complete
      ;; Send return if complete
      (cl-incf attempts))))
```

## Benefits
- Reliable auto-response execution
- No partial or dropped commands
- Better handling of slow or busy terminals
- Reduced need for manual intervention

## Technical Considerations
- Different terminal modes may require different approaches
- Network latency in remote sessions
- CPU load affecting timing
- Integration with existing timing detection feature request

## Related Issues
- `bug-report-return-key-timing.md`
- `feature-request-auto-response-timing-detection.md`

## Priority
High - This directly impacts the reliability of the auto-response feature