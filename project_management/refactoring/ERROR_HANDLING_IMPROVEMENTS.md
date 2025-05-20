# Error Handling and Input Validation Improvements

## Current Error Handling State

The term-claude mode module has inconsistent error handling and input validation:

1. **Inconsistent Validation**: Some functions validate inputs, others don't
2. **Missing Buffer Checks**: Some functions assume buffers exist or are in the correct mode
3. **Limited Variable Validation**: Customization options lack type validation
4. **Error Recovery**: Few functions have proper error recovery mechanisms
5. **Inconsistent Error Messaging**: Different error reporting styles across functions

## Error Handling Best Practices in Emacs Lisp

In Emacs Lisp, robust error handling should follow these principles:

1. **Validate inputs** before operating on them
2. **Use appropriate error functions**: `user-error` for user-facing errors, `error` for programming errors
3. **Provide helpful error messages** that guide the user on how to fix the problem
4. **Add condition-case** for critical operations that might fail
5. **Clean up resources** when errors occur

## Examples of Current Error Handling

### Example 1: Good Error Handling

```elisp
(defun ecc-register-buffer (&optional buffer)
  "Register BUFFER as a Claude buffer."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (unless (buffer-live-p buf)
      (user-error "Buffer is not alive"))
    
    ;; Register the buffer
    (unless (assoc buf ecc-buffer-registered-buffers-alist)
      (push (cons buf nil) ecc-buffer-registered-buffers-alist)
      (message "Buffer '%s' registered as Claude buffer" (buffer-name buf)))
    
    ;; Set as current Claude buffer
    (setq ecc-buffer-current-buffer buf)
    buf))
```

### Example 2: Insufficient Error Handling

```elisp
(defun ecc-term-visual-aid-highlight-prompt (prompt-regexp face)
  "Highlight text matching PROMPT-REGEXP with FACE."
  (save-excursion
    (goto-char (point-max))
    (let ((search-end-line (max (line-number-at-pos (point-min))
                             (- (line-number-at-pos) 10)))
          (search-end (save-excursion
                        (goto-char (point-min))
                        (forward-line (- search-end-line 1))
                        (point))))
      (when (re-search-backward prompt-regexp search-end t)
        (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
          (overlay-put overlay 'face face)
          (overlay-put overlay 'priority 200)
          (overlay-put overlay 'ecc-term-visual-aid t)
          (push overlay ecc-term-visual-aid--overlays))))))
```

## Areas for Improvement

### 1. Buffer Validation

Add consistent buffer validation to functions that operate on buffers:

```elisp
(defun ecc-term-claude-validate-buffer (buffer &optional require-mode)
  "Validate that BUFFER is alive and optionally check its major mode.
When REQUIRE-MODE is non-nil, verify the buffer is in the specified mode.
REQUIRE-MODE can be a mode symbol (like 'vterm-mode) or a list of
allowed mode symbols.

Returns the validated buffer object if checks pass, otherwise signals an error."
  (let ((buf (or buffer (current-buffer))))
    ;; Check buffer is alive
    (unless (buffer-live-p buf)
      (user-error "Buffer does not exist or has been killed"))
    
    ;; Check buffer mode if required
    (when require-mode
      (with-current-buffer buf
        (let ((modes (if (listp require-mode) require-mode (list require-mode))))
          (unless (cl-some (lambda (mode) (eq major-mode mode)) modes)
            (user-error "Buffer %s is not in %s mode (current: %s)"
                       (buffer-name buf)
                       (if (listp require-mode)
                           (format "one of %s" require-mode)
                         require-mode)
                       major-mode)))))
    buf))
```

Then update functions to use this validation:

```elisp
(defun ecc-term-claude-setup-existing-buffer ()
  "Setup current vterm buffer with Claude enhancements without changing major mode."
  ;; Validate buffer is in vterm mode
  (ecc-term-claude-validate-buffer (current-buffer) 'vterm-mode)
  
  ;; Rest of the function...
  )
```

### 2. Parameter Validation

Add parameter validation to functions with complex parameters:

```elisp
(defun ecc-term-claude-auto-send (state)
  "Automatically respond based on STATE.
STATE should be one of: :y/y/n, :y/n, :waiting, :initial-waiting."
  ;; Validate state parameter
  (unless (memq state '(:y/y/n :y/n :waiting :initial-waiting))
    (error "Invalid state: %s (must be one of :y/y/n, :y/n, :waiting, :initial-waiting)"
           state))
  
  ;; Rest of the function...
  )
```

### 3. Resource Cleanup

Ensure resources are properly cleaned up, especially with visual overlay operations:

```elisp
(defun ecc-term-visual-aid-update ()
  "Update visual aids based on current Claude state."
  (when (and ecc-term-visual-aid-enabled
             (derived-mode-p 'vterm-mode))
    (condition-case err
        (let ((state (ecc-detect-simple-state)))
          ;; Only update if state has changed
          (unless (eq state ecc-term-visual-aid--last-state)
            ;; Clear existing overlays before adding new ones
            (ecc-term-visual-aid-clear-all)
            (setq ecc-term-visual-aid--last-state state)
            
            (cond
             ((eq state :waiting)
              (ecc-term-visual-aid-apply-waiting))
             ((eq state :y/n)
              (ecc-term-visual-aid-apply-yes-no))
             ((eq state :y/y/n)
              (ecc-term-visual-aid-apply-yes-no)))))
      ;; If an error occurs, clean up overlays to prevent visual corruption
      (error
       (ecc-term-visual-aid-clear-all)
       (ecc-debug-message "Error in visual aid update: %s" (error-message-string err))))))
```

### 4. Boundary Checking

Add boundary checks for functions that operate on buffer regions:

```elisp
(defun ecc-term-visual-aid-highlight-prompt (prompt-regexp face)
  "Highlight text matching PROMPT-REGEXP with FACE."
  (save-excursion
    ;; Validate parameters
    (unless (stringp prompt-regexp)
      (error "Prompt regexp must be a string"))
    (unless (facep face)
      (error "Face must be a valid face symbol"))
    
    ;; Ensure buffer has content
    (when (= (point-min) (point-max))
      (ecc-debug-message "Cannot highlight prompt in empty buffer")
      (cl-return-from ecc-term-visual-aid-highlight-prompt nil))
    
    ;; Calculate safe search boundaries
    (goto-char (point-max))
    (let* ((max-lines-to-search 10)
           (current-line (line-number-at-pos))
           (min-line (line-number-at-pos (point-min)))
           (search-end-line (max min-line (- current-line max-lines-to-search)))
           (search-end (save-excursion
                         (goto-char (point-min))
                         (forward-line (- search-end-line 1))
                         (point))))
      
      ;; Search and add overlay
      (when (re-search-backward prompt-regexp search-end t)
        (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
          (overlay-put overlay 'face face)
          (overlay-put overlay 'priority 200)
          (overlay-put overlay 'ecc-term-visual-aid t)
          (push overlay ecc-term-visual-aid--overlays))))))
```

### 5. Error Recovery

Add error recovery mechanisms where appropriate:

```elisp
(defun ecc-term-claude-check-state ()
  "Check and update the state of the Claude VTERM buffer."
  (interactive)
  (when (eq major-mode 'ecc-term-claude-mode)
    (condition-case err
        (let ((state (ecc-detect-simple-state)))
          (force-mode-line-update)
          state)
      (error
       (ecc-debug-message "Error checking Claude state: %s" 
                         (error-message-string err))
       ;; Return nil to indicate no recognized state
       nil))))
```

### 6. Customization Validation

Improve customization variable validation:

```elisp
(defcustom ecc-term-claude-state-update-interval 1.0
  "Interval in seconds for updating Claude state.
Must be a positive number. Lower values make detection more responsive
but increase CPU usage."
  :type '(restricted-sexp :match-alternatives
                        ((lambda (val) (and (numberp val) (> val 0)))))
  :group 'ecc-term-claude)
```

## Comprehensive Improvements by Function Category

### State Detection Functions

```elisp
(defun ecc-term-claude-get-state ()
  "Get the current Claude prompt state for this buffer.
Returns one of: :y/y/n, :y/n, :waiting, :initial-waiting, or nil."
  (condition-case err
      (if (featurep 'ecc-state-detection)
          ;; Use enhanced state detection if available
          (ecc-detect-state)
        ;; Fallback to basic detection
        (let ((buffer-text (buffer-substring-no-properties 
                           (max (- (point-max) 1000) (point-min))
                           (point-max))))
          (cond
           ;; Detection logic...
           )))
    (error
     (ecc-debug-message "Error in state detection: %s" (error-message-string err))
     nil)))
```

### Setup Functions

```elisp
(defun ecc-term-claude-setup-common (buffer)
  "Set up common Claude features in BUFFER."
  ;; Validate buffer
  (let ((buf (ecc-term-claude-validate-buffer buffer 'vterm-mode)))
    (condition-case err
        (with-current-buffer buf
          ;; Setup logic...
          )
      (error
       (ecc-debug-message "Error setting up Claude features: %s" 
                         (error-message-string err))
       (message "Failed to set up Claude features in buffer %s: %s" 
                (buffer-name buf) (error-message-string err))))))
```

### Auto-Response Functions

```elisp
(defun ecc-term-claude-auto-send-accept ()
  "Automatically respond to Claude prompts when auto-mode is enabled."
  (when ecc-term-claude-auto-mode
    (condition-case err
        (let ((state (ecc-term-claude-get-state)))
          (when state
            ;; Auto-response logic...
            ))
      (error
       (ecc-debug-message "Error in auto-response: %s" (error-message-string err))
       (message "Auto-response failed: %s" (error-message-string err))))))
```

### Yank-as-File Functions

```elisp
(defun ecc-vterm-yank-as-file (start end filename)
  "Yank the region between START and END to a file named FILENAME."
  (interactive 
   (if (region-active-p)
       ;; Interactive form...
     (user-error "No active region")))
  
  ;; Validate parameters
  (unless (and start end)
    (user-error "Start and end positions required"))
  (unless (< start end)
    (user-error "Start position must be before end position"))
  (unless (and start end (>= start (point-min)) (<= end (point-max)))
    (user-error "Region positions out of buffer bounds"))
  
  ;; Generate the file with error handling
  (condition-case err
      (let ((file-path (ecc-vterm-generate-file-from-region start end filename)))
        ;; Handle successful file creation...
        )
    (file-error
     (message "Failed to write file: %s" (error-message-string err)))
    (error
     (message "Error creating file: %s" (error-message-string err)))))
```

## Implementation Plan

1. **Phase 1 - Critical Validation**:
   - Add buffer validation to core functions
   - Protect against basic errors in main user-facing functions
   
2. **Phase 2 - Resource Management**:
   - Improve cleanup of overlays, timers, and hooks
   - Add condition-case for failure recovery
   
3. **Phase 3 - Enhanced Validation**:
   - Add thorough parameter and boundary checking
   - Improve customization variable validation

## Testing Strategy

For each improvement:
1. Test with valid inputs to ensure normal operation is preserved
2. Test with invalid inputs to verify appropriate error handling
3. Test edge cases like empty buffers or unusual inputs
4. Verify cleanup happens when errors occur

## Conclusion

These error handling and input validation improvements will make the term-claude mode module more robust and maintainable by:

1. Preventing unexpected crashes
2. Providing clear error messages to users
3. Ensuring proper cleanup of resources
4. Protecting against edge cases
5. Making debugging easier with better error reporting

The improvements follow Emacs best practices and focus on enhancing reliability without significant performance impact.