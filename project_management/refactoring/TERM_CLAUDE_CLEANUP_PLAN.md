# Term-Claude Mode Cleanup Plan

## Current State Analysis

The term-claude mode module provides a specialized VTerm environment for Claude AI interaction. While functional, it has several areas that would benefit from code cleanup to improve maintainability, reduce duplication, and enhance consistency.

## State Detection Refactoring

### Current Issues:
- State detection is split between `ecc-term-claude-mode.el` and `ecc-state-detection.el`
- The `ecc-detect-simple-state` function in term-claude-mode implements its own logic rather than fully delegating to the state detection module
- Redundant detection patterns across files
- Inconsistent treatment of custom patterns and fallbacks

### Proposed Solution:
1. Remove the `ecc-detect-simple-state` function from `ecc-term-claude-mode.el`
2. Update all references to use the consolidated `ecc-detect-state` function from `ecc-state-detection.el`
3. Add feature detection to safely handle dependency on the state detection module
4. Ensure mode-line and auto-response functions refer to the same state information

### Implementation Plan:
```elisp
;; In ecc-term-claude-mode.el, replace direct state detection with:
(defun ecc-term-claude-get-state ()
  "Get the current Claude prompt state for this buffer.
Returns one of: :y/y/n, :y/n, :waiting, :initial-waiting, or nil."
  (if (featurep 'ecc-state-detection)
      ;; Use enhanced state detection if available
      (ecc-detect-state)
    ;; Fallback to basic detection if module is not loaded
    (let ((buffer-text (buffer-substring-no-properties 
                      (max (- (point-max) 1000) (point-min))
                      (point-max))))
      (cond
       ;; Minimal detection for critical states
       ((string-match-p "\\[Y/y/n\\]" buffer-text) :y/y/n)
       ((string-match-p "\\[y/n\\]\\|\\[Y/n\\]" buffer-text) :y/n)
       ((string-match-p "continue>\\|Continue>" buffer-text) :waiting)
       (t nil)))))
```

## Auto-Response Consolidation

### Current Issues:
- Separate functions for auto-responding to different prompt types that share identical structure
- Duplicated code for sending responses and messaging
- Inconsistent message formatting

### Proposed Solution:
1. Create a single parameterized auto-response function
2. Use conditional logic to select appropriate response type
3. Ensure consistent messaging and error handling

### Implementation Plan:
```elisp
(defun ecc-term-claude-auto-send (state)
  "Automatically respond based on STATE.
STATE should be one of: :y/y/n, :y/n, :waiting, :initial-waiting."
  (let ((response (pcase state
                    (:y/n ecc-auto-response-y/n)
                    (:y/y/n ecc-auto-response-y/y/n)
                    (:waiting ecc-auto-response-waiting)
                    (:initial-waiting ecc-auto-response-initial-waiting)
                    (_ (user-error "Unknown state for auto-response: %s" state))))
        (state-name (ecc-state-get-name state)))
    (vterm-send-string response)
    (vterm-send-return)
    (message "Auto-responded to %s: %s" state-name response)))
```

## Common Setup Logic Extraction

### Current Issues:
- Duplicated setup code between mode definition and existing buffer setup
- Timer creation/cancellation logic repeated
- Hook setup duplicated

### Proposed Solution:
1. Extract common setup logic into shared functions
2. Create clear separation between mode-specific and general setup
3. Ensure proper cleanup in all cases

### Implementation Plan:
```elisp
(defun ecc-term-claude-setup-common (buffer)
  "Set up common Claude features in BUFFER."
  (with-current-buffer buffer
    ;; Register buffer
    (ecc-register-buffer)
    
    ;; Set up visual indicators
    (ecc-term-claude-setup-mode-line)
    
    ;; Set up state detection timer
    (ecc-term-claude-setup-timer)
    
    ;; Connect to vterm hooks
    (ecc-term-claude-setup-hooks)))

(defun ecc-term-claude-setup-timer ()
  "Set up or reset the state detection timer for current buffer."
  (when ecc-term-claude-state-timer
    (cancel-timer ecc-term-claude-state-timer))
  
  (setq ecc-term-claude-state-timer
        (run-with-timer 0 ecc-term-claude-state-update-interval
                      'ecc-term-claude-check-state)))

(defun ecc-term-claude-setup-hooks ()
  "Set up hooks for Claude term mode in current buffer."
  ;; Connect to vterm hooks
  (add-hook 'vterm-update-functions
            (lambda (&rest _)
              (run-hooks 'ecc-term-claude-update-functions))
            nil t)
  
  ;; Enable follow bottom by default
  (add-hook 'ecc-term-claude-update-functions
            'ecc-term-claude-follow-bottom-after-output)
  
  ;; Add hook to clean up when buffer is killed
  (add-hook 'kill-buffer-hook 'ecc-term-claude-cleanup-buffer nil t))
```

## Naming Convention Standardization

### Current Issues:
- Inconsistent prefixes between modules
- Unclear function naming in some cases
- Mixing of `ecc-term-` and `ecc-vterm-` prefixes

### Proposed Solution:
1. Standardize on `ecc-term-claude-` prefix for term-claude mode functions
2. Standardize on `ecc-vterm-` prefix for general vterm enhancement functions
3. Ensure all related modules follow consistent naming

### Implementation Note:
- Update function names but maintain backward compatibility with aliases
- Document namespace standards for future development

## Documentation Improvements

### Current Issues:
- Inconsistent docstring format and detail
- Some functions missing proper documentation
- Unclear parameter descriptions

### Proposed Solution:
1. Ensure all functions have standardized docstrings with:
   - Purpose description
   - Parameter documentation
   - Return value documentation
   - Usage examples where appropriate
2. Add module-level documentation

### Example Implementation:
```elisp
(defun ecc-term-claude-check-state ()
  "Check and update the state of the Claude VTERM buffer.
Detects the current prompt state using the enhanced detection
system and updates the mode line indicator accordingly.

Returns the detected state symbol (:y/n, :y/y/n, :waiting,
:initial-waiting) or nil if no prompt is detected."
  (interactive)
  (when (eq major-mode 'ecc-term-claude-mode)
    (let ((state (ecc-term-claude-get-state)))
      (force-mode-line-update)
      state)))
```

## Error Handling Improvements

### Current Issues:
- Inconsistent error handling across modules
- Missing validation in some functions
- No defensive programming in string search functions

### Proposed Solution:
1. Add consistent error checking for user input
2. Validate buffer state before operations
3. Add defensive programming to handle edge cases

### Implementation Example:
```elisp
(defun ecc-term-claude-setup-existing-buffer ()
  "Setup current vterm buffer with Claude enhancements without changing major mode."
  ;; Validate buffer mode
  (unless (eq major-mode 'vterm-mode)
    (user-error "Current buffer is not in vterm-mode"))
  
  ;; Validate buffer is live
  (unless (buffer-live-p (current-buffer))
    (user-error "Cannot setup Claude features in a non-live buffer"))
    
  ;; Register buffer as a Claude buffer
  (ecc-register-buffer)
  
  ;; Proceed with common setup
  (ecc-term-claude-setup-common (current-buffer))
  
  ;; Setup local keybindings
  (ecc-term-claude-setup-keys)
  
  (message "Claude features applied to current vterm buffer"))
```

## Implementation Timeline

1. **Phase 1 - Core Refactoring**
   - State detection consolidation
   - Auto-response function merger
   - Common setup extraction
   
2. **Phase 2 - Consistency Improvements**
   - Naming convention standardization
   - Documentation improvements
   
3. **Phase 3 - Robustness Enhancements**
   - Error handling enhancement
   - Test cases for refactored code

## Testing Strategy

For each refactoring step:
1. Update existing unit tests to match the new structure
2. Add new tests to verify correct behavior
3. Ensure test coverage for common edge cases
4. Verify backward compatibility

## Conclusion

This cleanup plan addresses the major issues identified in the term-claude module while preserving core functionality. The goal is to create a more maintainable, consistent, and robust implementation that follows Emacs conventions and clean code principles.