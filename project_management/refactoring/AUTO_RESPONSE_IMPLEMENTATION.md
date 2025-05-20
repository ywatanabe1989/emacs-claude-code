# Auto-Response Module Implementation Plan

## Module Purpose
The `ecc-auto-response.el` module serves as the API layer for the auto-response system. It provides:
1. Public API functions for starting, stopping, and configuring the auto-response system
2. Response action functions for different Claude prompt states
3. Buffer management for tracked response targets
4. Convenience commands for users

This module builds on the `ecc-auto-core.el` infrastructure and focuses exclusively on the response action logic, leaving the timing, state detection, and throttling to the core module.

## Key Dependencies
- `ecc-auto-core.el` - For core infrastructure (timers, throttling, state tracking)
- `ecc-state-detection.el` - For detecting Claude prompt states
- `ecc-variables.el` - For shared variables and customization

## Variables

### Customization Variables
```elisp
(defgroup ecc-auto-response nil
  "Automatic response settings for Claude."
  :group 'ecc
  :prefix "ecc-auto-response-")

(defcustom ecc-auto-response-enabled nil
  "Whether auto-response functionality is enabled."
  :type 'boolean
  :group 'ecc-auto-response)

(defcustom ecc-auto-response-yes "1"
  "Response to send for Y/N prompts (typically \"1\" for \"yes\")."
  :type 'string
  :group 'ecc-auto-response)

(defcustom ecc-auto-response-yes-plus "2"
  "Response to send for Y/Y/N prompts (typically \"2\" for second option)."
  :type 'string
  :group 'ecc-auto-response)

(defcustom ecc-auto-response-continue "/auto"
  "Response to send for waiting state (typically \"/auto\" or \"/continue\")."
  :type 'string 
  :group 'ecc-auto-response)

(defcustom ecc-auto-response-initial-waiting "/user:understand-guidelines"
  "Response to send for initial waiting state."
  :type 'string
  :group 'ecc-auto-response)

(defcustom ecc-auto-response-notify t
  "Whether to show notifications when automatic responses are sent."
  :type 'boolean
  :group 'ecc-auto-response)
```

### Internal Variables
```elisp
(defvar ecc-auto-response--registered-callback nil
  "Callback function to process detected states in registered buffers.")
```

## Functions

### Public API Functions

```elisp
(defun ecc-auto-response-start ()
  "Start the auto-response system.
Initializes and activates the auto-response system that automatically responds
to different types of Claude prompts.

Sets up a timer using the core infrastructure that periodically checks
buffers with Claude interactions for prompts and sends appropriate responses.

Displays a message with the configured response values when started."
  (interactive)
  
  ;; Enable auto-response
  (setq ecc-auto-response-enabled t)
  
  ;; Set up callback for processing detected states
  (setq ecc-auto-response--registered-callback 
        (lambda (buffer state)
          (ecc-auto-response-send buffer state)))
  
  ;; Register currently active buffer
  (when-let ((buf (current-buffer)))
    (ecc-auto-core-register-buffer buf))
  
  ;; Initialize core timer for periodic checking
  (ecc-auto-core-timer-start 
   (lambda () 
     (ecc-auto-core-process-all-buffers ecc-auto-response--registered-callback)))
  
  ;; Also do an immediate initial check for waiting prompts
  (when-let ((buf (current-buffer)))
    (ecc-auto-core-initial-check buf ecc-auto-response--registered-callback))
  
  (message "Auto-response started: Y/N=%s, Y/Y/N=%s, Continue=%s" 
           ecc-auto-response-yes
           ecc-auto-response-yes-plus
           ecc-auto-response-continue))

(defun ecc-auto-response-stop ()
  "Stop the auto-response system.
Deactivates the auto-response system and cancels any pending timers."
  (interactive)
  
  ;; Disable auto-response
  (setq ecc-auto-response-enabled nil)
  
  ;; Clear callback
  (setq ecc-auto-response--registered-callback nil)
  
  ;; Stop core timer
  (ecc-auto-core-timer-stop)
  
  ;; Reset core state
  (ecc-auto-core-reset-state)
  
  (message "Auto-response stopped"))

(defun ecc-auto-response-toggle ()
  "Toggle auto-response on/off."
  (interactive)
  (if ecc-auto-response-enabled
      (ecc-auto-response-stop)
    (ecc-auto-response-start)))

(defun ecc-auto-response-register-buffer (&optional buffer)
  "Register BUFFER for auto-response.
If BUFFER is nil, use current buffer.
Returns the buffer if registered successfully."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (prog1 (ecc-auto-core-register-buffer buf)
      (when (called-interactively-p 'any)
        (message "Buffer %s registered for auto-response" (buffer-name buf))))))
```

### Response Functions

```elisp
(defun ecc-auto-response-send (buffer &optional state)
  "Send appropriate response to Claude prompts in BUFFER.
Examines the buffer content to detect Claude's current prompt state, then
sends an appropriate pre-configured response based on that state.

BUFFER is the buffer containing Claude's output to respond to.
Optional STATE can be provided to override automatic state detection.
Valid states are: `:y/n`, `:y/y/n`, `:initial-waiting`, `:waiting`

Responds with configured variables for different prompt types.
Returns t if a response was sent, nil otherwise."
  (interactive (list (current-buffer)))
  
  ;; Skip if auto-response is disabled
  (unless ecc-auto-response-enabled
    (ecc-debug-message "Auto-response is disabled, not sending response")
    (cl-return-from ecc-auto-response-send nil))
  
  ;; Skip if buffer is not live
  (unless (buffer-live-p buffer)
    (ecc-debug-message "Buffer is not live, not sending response")
    (cl-return-from ecc-auto-response-send nil))
  
  (with-current-buffer buffer
    ;; Get current state if not provided
    (let ((current-state (or state (ecc-detect-state))))
      (cond
       ((eq current-state :y/y/n)
        (ecc-auto-response--send-message buffer ecc-auto-response-yes-plus "Y/Y/N"))
       
       ((eq current-state :y/n)
        (ecc-auto-response--send-message buffer ecc-auto-response-yes "Y/N"))
       
       ((eq current-state :initial-waiting)
        (ecc-auto-response--send-message buffer ecc-auto-response-initial-waiting "Initial-Waiting"))
       
       ((eq current-state :waiting)
        (ecc-auto-response--send-message buffer ecc-auto-response-continue "Continue"))
       
       (t ;; No recognized state
        (ecc-debug-message "No recognized prompt state detected")
        nil)))))

(defun ecc-auto-response--send-message (buffer response type)
  "Send RESPONSE to Claude in BUFFER.
TYPE is used for notification messages."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      ;; Check if we're in a terminal mode
      (cond
       ;; vterm mode
       ((derived-mode-p 'vterm-mode)
        (ecc-auto-response--send-to-vterm response))
       
       ;; comint mode (e.g., shell)
       ((derived-mode-p 'comint-mode)
        (comint-send-string
         (get-buffer-process buffer)
         (concat response "\n")))
       
       ;; Default fallback - just insert at point
       (t
        (insert response)
        (message "Inserted response in buffer %s" (buffer-name buffer))))))
  
  ;; Notify user about the response if notifications are enabled
  (when ecc-auto-response-notify
    (ecc-auto-response--notify type response))
  
  ;; Return t to indicate success
  t)

(defun ecc-auto-response--send-to-vterm (response)
  "Send RESPONSE to Claude in a vterm buffer."
  ;; Only proceed if vterm is available
  (when (fboundp 'vterm-send-string)
    ;; Save current point position to potentially restore it later
    (let ((distance-from-end (- (point-max) (point))))
      ;; Output debug information
      (ecc-debug-message "Sending response to vterm: %s" response)
      (ecc-debug-message "Point: %d, Max: %d, Distance from end: %d"
                      (point) (point-max) distance-from-end)
      
      ;; If user is more than 40 chars from the end, they might be reading
      ;; earlier content, so use save-excursion to avoid disturbing their view
      (if (< distance-from-end 40)
          ;; User is at end of buffer - send directly
          (progn
            (ecc-debug-message "Sending directly at current point")
            (sit-for 0.2) ; Small delay for stability
            (vterm-send-string response)
            (sit-for 0.2)
            (vterm-send-return))
        
        ;; User might be reading earlier content, so don't move point
        (save-excursion
          (ecc-debug-message "Using save-excursion to send at end")
          (goto-char (point-max))
          (sit-for 0.2)
          (vterm-send-string response)
          (sit-for 0.2)
          (vterm-send-return)))
      
      (ecc-debug-message "Send complete. Final point: %d" (point)))))

(defun ecc-auto-response--notify (type response)
  "Display notification about auto-response of TYPE with actual RESPONSE string.
TYPE is a description of the response context (e.g., \"Y/N\").
RESPONSE is the actual string sent to Claude."
  (let ((msg (format "Auto-responded: %s (\"%s\")" type response)))
    (message msg)))
```

### Convenience Functions

```elisp
(defun ecc-auto-response-yes (&optional buffer)
  "Automatically send Y response to Claude Y/N prompt.
Sends the yes response (defined by `ecc-auto-response-yes`) to
Claude when it's in a Y/N prompt state.

If BUFFER is nil, use current buffer."
  (interactive)
  (ecc-auto-response--send-message 
   (or buffer (current-buffer))
   ecc-auto-response-yes
   "Y/N"))

(defun ecc-auto-response-yes-plus (&optional buffer)
  "Automatically send Y response to Claude Y/Y/N prompt.
Sends the yes-plus response (defined by `ecc-auto-response-yes-plus`)
to Claude when it's in a Y/Y/N prompt state.

If BUFFER is nil, use current buffer."
  (interactive)
  (ecc-auto-response--send-message
   (or buffer (current-buffer))
   ecc-auto-response-yes-plus
   "Y/Y/N"))

(defun ecc-auto-response-continue (&optional buffer)
  "Automatically send continue to Claude waiting prompt.
Sends the continue response (defined by `ecc-auto-response-continue`) to
Claude when it's in a waiting state, prompting for more output.

If BUFFER is nil, use current buffer."
  (interactive)
  (ecc-auto-response--send-message
   (or buffer (current-buffer))
   ecc-auto-response-continue
   "Continue"))

(defun ecc-auto-response-custom (response-text)
  "Send custom RESPONSE-TEXT to Claude.
This allows sending natural language responses instead of just command options."
  (interactive "sEnter your response: ")
  (let ((buffer (current-buffer)))
    (when (buffer-live-p buffer)
      (ecc-auto-response--send-message
       buffer
       response-text
       (format "Custom: %s" response-text)))))
```

### Backward Compatibility

```elisp
;; Aliases for compatibility with old code
(defalias 'ecc-start-auto-response 'ecc-auto-response-start)
(defalias 'ecc-stop-auto-response 'ecc-auto-response-stop)
(defalias 'ecc-toggle-auto-response 'ecc-auto-response-toggle)
(defalias 'ecc-auto-accept-send 'ecc-auto-response-send)
(defalias 'ecc-check-and-respond 
  (lambda ()
    "Compatibility function for old auto-response checking."
    (when ecc-auto-response-enabled
      (ecc-auto-core-process-all-buffers ecc-auto-response--registered-callback))))
```

## Module Exports (provide)
```elisp
(provide 'ecc-auto-response)
```

## Test Plan

1. **Basic Functionality Tests**
   - Test starting and stopping auto-response system
   - Verify toggling behavior works correctly
   - Test buffer registration and management

2. **Response Function Tests**
   - Test response functions for each state type (Y/N, Y/Y/N, waiting, etc.)
   - Verify correct response values are sent
   - Test notification behavior

3. **Integration Tests**
   - Test interaction with core infrastructure
   - Verify timer management and state detection
   - Test backwoard compatibility with existing code

4. **Edge Case Tests**
   - Test behavior with invalid states
   - Test with disabled auto-response
   - Test with dead buffers

5. **Mode-specific Tests**
   - Test vterm support
   - Test comint mode support
   - Test default fallback behavior

## Implementation Sequence

1. Create customization variables and groups
2. Implement public API functions
3. Add response sending core functions
4. Build mode-specific response handling
5. Create convenience wrapper functions
6. Add backward compatibility functions
7. Create comprehensive tests
8. Review and refine

This implementation plan provides a detailed guide for creating the `ecc-auto-response.el` module, ensuring all necessary functionality is included with proper organization and backward compatibility.