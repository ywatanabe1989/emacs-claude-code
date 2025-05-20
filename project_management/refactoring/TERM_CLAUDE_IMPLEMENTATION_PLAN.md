# Term-Claude Mode Comprehensive Implementation Plan

## Overview

This document provides a comprehensive implementation plan for refactoring and improving the term-claude mode module based on the detailed analysis conducted in the TERM_CLAUDE_CLEANUP_REPORT.md. The plan outlines a phased approach, prioritizing changes based on impact and dependency relationships, with specific code changes and testing approaches for each phase.

## Implementation Phases

### Phase 1: Core Architecture Improvements (High Priority)

#### 1.1. State Detection Consolidation

**Goal**: Remove duplicated state detection logic and create a unified interface

**Changes**:
1. Implement `ecc-term-claude-get-state` function with feature detection and fallback mechanisms
2. Remove redundant state detection from `ecc-term-claude-mode.el`
3. Update all state consumers to use the new interface
4. Add backward compatibility aliases

**Implementation**:
```elisp
(defun ecc-term-claude-get-state (&optional buffer)
  "Get the current Claude prompt state for BUFFER or current buffer.
Returns one of: :y/y/n, :y/n, :waiting, :initial-waiting, or nil."
  (with-current-buffer (or buffer (current-buffer))
    (cond
     ;; First try enhanced detection from state-detection module if available
     ((and (featurep 'ecc-state-detection) 
           (fboundp 'ecc-detect-state))
      (ecc-detect-state))
     
     ;; Next try alternative detection module if available
     ((and (featurep 'ecc-state-detect-prompt)
           (fboundp 'ecc-detect-prompt-in-last-lines))
      (ecc-detect-prompt-in-last-lines 20))
     
     ;; Fall back to basic detection
     (t (ecc-term-claude-detect-basic-state)))))

;; Define backward compatibility alias
(defalias 'ecc-detect-simple-state 'ecc-term-claude-get-state)
```

**Files to modify**:
- `ecc-term-claude-mode.el` - Replace `ecc-detect-simple-state` with `ecc-term-claude-get-state`
- Update all dependencies of state detection

#### 1.2. Auto-Response Function Consolidation

**Goal**: Replace multiple similar auto-response functions with a unified approach

**Changes**:
1. Create a mapping from state to response variables
2. Implement unified `ecc-term-claude-auto-send` function
3. Refactor the coordinator `ecc-term-claude-auto-send-accept` function
4. Add backward compatibility wrappers for old function names

**Implementation**:
```elisp
(defvar ecc-term-claude-auto-response-map
  '((:y/n . ecc-auto-response-y/n)
    (:y/y/n . ecc-auto-response-y/y/n)
    (:waiting . ecc-auto-response-waiting)
    (:initial-waiting . ecc-auto-response-initial-waiting))
  "Mapping from state symbols to auto-response variables.")

(defun ecc-term-claude-auto-send (state)
  "Automatically respond based on STATE.
STATE should be one of: :y/y/n, :y/n, :waiting, :initial-waiting."
  (let* ((response-var (cdr (assq state ecc-term-claude-auto-response-map)))
         (response (and response-var (boundp response-var) (symbol-value response-var)))
         (state-name (ecc-state-get-name state)))
    (when response
      (vterm-send-string response)
      (vterm-send-return)
      (message "Auto-responded to %s prompt: %s" state-name response)
      t)))

(defun ecc-term-claude-auto-send-accept ()
  "Automatically respond to Claude prompts in vterm mode."
  (when ecc-term-claude-auto-mode
    (let ((state (ecc-term-claude-get-state)))
      (when state
        (ecc-term-claude-auto-send state)))))
```

**Files to modify**:
- `ecc-term-claude-mode.el` - Replace auto-response functions with the consolidated approach

#### 1.3. Common Setup Logic Extraction

**Goal**: Extract duplicated setup code into reusable functions

**Changes**:
1. Create `ecc-term-claude-setup-common` function for shared setup logic
2. Extract component functions for specific setup tasks
3. Refactor the mode definition and existing buffer setup to use common functions

**Implementation**:
```elisp
(defun ecc-term-claude-setup-common (buffer)
  "Set up common Claude features in BUFFER."
  (with-current-buffer buffer
    ;; Register buffer
    (ecc-term-claude-register-buffer)
    
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
  
  (setq-local ecc-term-claude-state-timer
              (run-with-timer 0 ecc-term-claude-state-update-interval
                            'ecc-term-claude-check-state)))

(defun ecc-term-claude-setup-hooks ()
  "Set up vterm hooks for Claude functionality."
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

**Files to modify**:
- `ecc-term-claude-mode.el` - Refactor major mode definition and `ecc-term-claude-setup-existing-buffer`

### Phase 2: Code Quality Enhancements (Medium Priority)

#### 2.1. Naming Convention Standardization

**Goal**: Implement consistent naming conventions across the module

**Changes**:
1. Rename functions to follow standardized patterns
2. Ensure proper namespacing with `ecc-term-claude-` prefix
3. Add backward compatibility aliases for all renamed functions

**Implementation**:
```elisp
;; Example of function renaming
(defun ecc-term-claude-toggle-auto-mode ()
  "Toggle automatic response to Claude prompts."
  (interactive)
  (setq ecc-term-claude-auto-mode (not ecc-term-claude-auto-mode))
  (message "Claude auto-mode %s"
           (if ecc-term-claude-auto-mode "enabled" "disabled"))
  
  ;; Set up hooks for auto-responses
  (if ecc-term-claude-auto-mode
      (add-to-list 'ecc-term-claude-update-functions
                  'ecc-term-claude-auto-send-accept)
    (setq ecc-term-claude-update-functions
          (remove 'ecc-term-claude-auto-send-accept
                  ecc-term-claude-update-functions))))

;; Add backward compatibility alias
(defalias 'ecc-term-claude-auto-mode-toggle 'ecc-term-claude-toggle-auto-mode)
```

**Files to modify**:
- All term-claude module files - Rename functions consistently

#### 2.2. Documentation Enhancement

**Goal**: Improve docstrings and comments throughout the module

**Changes**:
1. Update docstrings to follow standardized format
2. Document parameters, return values, and edge cases
3. Add module-level commentary
4. Improve inline comments for complex code

**Implementation**:
```elisp
(defun ecc-term-claude-get-state (&optional buffer)
  "Get the current Claude prompt state for BUFFER or current buffer.
Returns one of: :y/y/n, :y/n, :waiting, :initial-waiting, or nil.

This function serves as the unified interface for Claude prompt state
detection. It automatically uses the best available detection method,
falling back to simpler methods if necessary.

Arguments:
  BUFFER: Optional. The buffer to check for Claude prompt state.
          If nil, uses the current buffer.

Returns:
  A symbol representing the detected state:
    - `:y/y/n' - Claude is asking a yes/yes+/no question
    - `:y/n' - Claude is asking a yes/no question
    - `:waiting' - Claude is waiting for the user to continue
    - `:initial-waiting' - Claude is showing an initial waiting prompt
    - `nil' - No recognized prompt state was detected"
  (with-current-buffer (or buffer (current-buffer))
    ;; Implementation...
    ))
```

**Files to modify**:
- All term-claude module files - Update docstrings and comments

#### 2.3. Error Handling Improvement

**Goal**: Enhance error handling and input validation

**Changes**:
1. Add buffer and parameter validation functions
2. Implement error recovery mechanisms
3. Add consistent error signaling and messaging
4. Ensure proper resource cleanup

**Implementation**:
```elisp
(defun ecc-term-claude-validate-buffer (buffer-or-name &optional require-mode)
  "Validate that BUFFER-OR-NAME is a live buffer and optionally check its mode.
BUFFER-OR-NAME can be a buffer or a buffer name.
REQUIRE-MODE can be a mode symbol or list of allowed modes.

Returns the buffer object if valid, otherwise signals an error."
  (let ((buf (if (bufferp buffer-or-name)
                buffer-or-name
              (get-buffer buffer-or-name))))
    ;; Check buffer exists
    (unless (buffer-live-p buf)
      (user-error "Buffer %s does not exist or has been killed"
                 (if (stringp buffer-or-name)
                     buffer-or-name
                   "specified")))
    
    ;; Check buffer mode if specified
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

**Files to modify**:
- All term-claude module files - Add validation to key functions

### Phase 3: Verification and Optimization (Lower Priority)

#### 3.1. Test Suite Enhancement

**Goal**: Update and expand the test suite for term-claude mode

**Changes**:
1. Add test cases for refactored functions
2. Create edge case tests for error conditions
3. Implement integration tests for full workflows
4. Add tests for backward compatibility

**Implementation**:
```elisp
(ert-deftest test-ecc-term-claude-get-state ()
  "Test the unified state detection function."
  ;; Test basic detection
  (with-temp-buffer
    (insert "Some content with [y/n] prompt")
    (should (eq (ecc-term-claude-get-state (current-buffer)) :y/n)))
  
  ;; Test with no prompt
  (with-temp-buffer
    (insert "Regular content with no prompt")
    (should (eq (ecc-term-claude-get-state (current-buffer)) nil)))
  
  ;; Test different prompt types
  (with-temp-buffer
    (insert "Content with continue> prompt")
    (should (eq (ecc-term-claude-get-state (current-buffer)) :waiting))))
```

**Files to modify**:
- `tests/ecc-vterm/test-ecc-term-claude-mode.el` - Add and update tests

#### 3.2. Performance Optimization

**Goal**: Improve performance for large outputs and real-time operation

**Changes**:
1. Analyze and optimize state detection for large buffers
2. Improve overlay management for better rendering performance
3. Optimize hook execution patterns

**Implementation**:
```elisp
(defun ecc-term-claude-get-state (&optional buffer)
  "Get the current Claude prompt state for BUFFER or current buffer."
  (with-current-buffer (or buffer (current-buffer))
    ;; Only examine the last part of large buffers for performance
    (let* ((buffer-size (buffer-size))
           (search-size (min buffer-size 2000))
           (start-pos (max (point-min) (- (point-max) search-size))))
      ;; Implementation with optimized search region
      )))
```

**Files to modify**:
- `ecc-term-claude-mode.el` and related files - Optimize performance-critical functions

#### 3.3. Final Documentation Updates

**Goal**: Complete comprehensive documentation for the module

**Changes**:
1. Update README and user documentation
2. Create developer documentation for extension
3. Add examples for common use cases
4. Document implementation details

**Files to create/modify**:
- `README.md` - Update with new features and changes
- Create developer documentation in appropriate locations

## Integration Testing

After each phase, conduct thorough integration testing to ensure:

1. **Functional Correctness**: All features work as expected
2. **Backward Compatibility**: Existing code continues to work
3. **Error Handling**: Edge cases are handled gracefully
4. **Resource Management**: No leaks or leftover resources
5. **User Experience**: Behavior remains consistent and intuitive

## Rollout Strategy

1. **Development**: Implement changes in a feature branch
2. **Testing**: Conduct thorough testing of each phase
3. **Code Review**: Perform detailed code review of changes
4. **Documentation**: Update documentation for users and developers
5. **Merge**: Merge the changes into the main branch
6. **Announcement**: Communicate changes to users

## Time Estimates

| Phase | Component | Estimated Time |
|-------|-----------|----------------|
| 1.1   | State Detection Consolidation | 3-4 hours |
| 1.2   | Auto-Response Function Consolidation | 2-3 hours |
| 1.3   | Common Setup Logic Extraction | 3-4 hours |
| 2.1   | Naming Convention Standardization | 4-5 hours |
| 2.2   | Documentation Enhancement | 5-6 hours |
| 2.3   | Error Handling Improvement | 4-5 hours |
| 3.1   | Test Suite Enhancement | 5-6 hours |
| 3.2   | Performance Optimization | 3-4 hours |
| 3.3   | Final Documentation Updates | 2-3 hours |
| Integration Testing | | 4-6 hours |
| **Total** | | **35-46 hours** |

## Success Criteria

The refactoring will be considered successful when:

1. All duplicated code has been consolidated
2. Naming conventions are consistent throughout
3. Error handling is comprehensive and robust
4. Documentation is complete and accurate
5. All tests pass, including new edge case tests
6. Performance is maintained or improved
7. Backward compatibility is preserved

## Conclusion

This comprehensive implementation plan provides a structured approach to refactoring the term-claude mode module while ensuring backward compatibility and code quality. By following this phased approach, we can systematically improve the codebase while minimizing risks and ensuring a smooth transition for users.