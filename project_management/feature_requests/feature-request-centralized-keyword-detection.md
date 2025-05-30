# Feature Request: Centralized Keyword Detection Variables

## Summary
Centralize all keyword patterns as variables (like state detection patterns) to improve maintainability and consistency in determining when to send auto-responses.

## Current Situation
State detection patterns are well-organized:
```elisp
(defvar --ecc-state-detection-patterns
  '((:initial-waiting . "│ > Try ")
    (:waiting . "│ >                            ")
    (:y/n . "❯ 1. Yes")
    (:y/y/n . " 2. Yes, and")
    (:running . " tokens · esc to interrupt")))
```

But safety check keywords are scattered throughout the code.

## Proposed Solution

### 1. Create Centralized Safety Keywords
```elisp
(defvar --ecc-auto-response-safety-keywords
  '((:running-indicator . "esc to interrupt")
    (:processing-indicator . "Herding…")
    (:thinking-indicator . "Thinking…")
    (:generating-indicator . "Generating")
    (:typing-indicator . "typing")
    (:error-indicator . "Error:")
    (:warning-indicator . "Warning:"))
  "Keywords that indicate Claude is busy or in an error state.")

(defvar --ecc-auto-response-completion-keywords
  '((:message-sent . "Message sent")
    (:command-executed . "Command executed")
    (:response-complete . "│ H ")  ; Human prompt appears
    (:assistant-done . "│ A "))    ; Assistant response complete
  "Keywords that indicate a command was successfully processed.")

(defvar --ecc-auto-response-prompt-keywords
  '((:continue-prompt . "continue>")
    (:continue-capital . "Continue>")
    (:yn-prompt . "[y/n]")
    (:Yn-prompt . "[Y/n]")
    (:yyn-prompt . "[Y/y/n]"))
  "Keywords that indicate specific prompt types.")
```

### 2. Unified Detection Function
```elisp
(defun --ecc-auto-response--check-safety-keywords (text)
  "Check TEXT for safety keywords that should block sending."
  (catch 'found
    ;; Check all safety keywords
    (dolist (keyword-pair --ecc-auto-response-safety-keywords)
      (let ((type (car keyword-pair))
            (keyword (cdr keyword-pair)))
        (when (string-match-p (regexp-quote keyword) text)
          (--ecc-debug-message "Found safety keyword %s: %s" type keyword)
          (throw 'found type))))
    nil))

(defun --ecc-auto-response--should-send-p (buffer-text state)
  "Determine if auto-response should be sent based on BUFFER-TEXT and STATE."
  (and
   ;; State indicates a prompt is waiting
   (memq state '(:waiting :y/n :y/y/n :initial-waiting))
   ;; No safety keywords present
   (not (--ecc-auto-response--check-safety-keywords buffer-text))
   ;; Not already sent
   (not (--ecc-auto-response--already-sent-p))
   ;; Not throttled
   (not (--ecc-auto-response--should-throttle-p state))))
```

### 3. Configuration Options
```elisp
(defcustom --ecc-auto-response-custom-safety-keywords nil
  "Additional safety keywords defined by user.
Each element should be a cons cell (TYPE . KEYWORD)."
  :type '(alist :key-type symbol :value-type string)
  :group 'ecc)

(defun --ecc-auto-response--get-all-safety-keywords ()
  "Get all safety keywords including custom ones."
  (append --ecc-auto-response-safety-keywords
          --ecc-auto-response-custom-safety-keywords))
```

## Benefits

1. **Maintainability**: All keywords in one place, easy to update
2. **Extensibility**: Users can add custom safety keywords
3. **Consistency**: Same pattern as state detection
4. **Debugging**: Clear logging of which keyword triggered
5. **Flexibility**: Easy to add new safety conditions

## Throttling Integration

The centralized keyword system works with existing throttling mechanisms:

### 1. Time-Based Throttling
```elisp
(defvar --ecc-auto-response-throttle-keywords
  '((:recent-send . "Message sent")  ; Just sent something
    (:recent-response . "│ A ")      ; Assistant just responded
    (:recent-activity . "ago"))      ; Timestamp indicators
  "Keywords indicating recent activity that should trigger throttling.")

(defun --ecc-auto-response--check-throttle-keywords (text)
  "Check if TEXT contains indicators of recent activity."
  ;; If these keywords appear with timestamps < threshold, throttle
  )
```

### 2. Multi-Layer Safety Architecture
```
Layer 1: State Detection (WHAT to send)
   ↓
Layer 2: Keyword Safety (Is Claude busy?)
   ↓
Layer 3: Throttling (Did we just send?)
   ↓
Layer 4: Accumulation (Too many recent sends?)
   ↓
Layer 5: Send Verification (Did it actually work?)
```

### 3. Unified Decision Function
```elisp
(defun --ecc-auto-response--make-send-decision (buffer)
  "Make a comprehensive decision about whether to send."
  (let* ((text (--ecc-get-buffer-tail-text buffer))
         (state (--ecc-state-detection-detect buffer))
         (safety-check (--ecc-auto-response--check-safety-keywords text))
         (throttle-check (--ecc-auto-response--should-throttle-p state))
         (accumulation-check (--ecc-auto-response--accumulation-detected-p)))
    
    (list :state state
          :safety safety-check
          :throttled throttle-check
          :accumulated accumulation-check
          :decision (and state
                        (not safety-check)
                        (not throttle-check)
                        (not accumulation-check)))))
```

This provides complete visibility into why a send decision was made or blocked.

## Implementation Notes

- Keywords should be checked after state detection
- Use regexp-quote to handle special characters
- Consider case-sensitivity options
- Allow for partial matches vs exact matches
- Performance: compile patterns if needed

## Related Components
- `ecc-state-detection.el` - Current pattern matching
- `ecc-auto-response.el` - Where this would be implemented