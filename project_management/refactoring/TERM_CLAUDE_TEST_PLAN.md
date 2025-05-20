# Test Plan for Consolidated Term-Claude Modules

## Overview

This document outlines a comprehensive test plan for the newly consolidated term-claude modules. The goal is to ensure each module functions correctly, both independently and as part of the integrated system.

## Test Strategy

1. **Unit Tests**: For individual functions and features
2. **Integration Tests**: For interactions between modules
3. **Mock-Based Tests**: For functions that interact with external systems or have side effects
4. **Regression Tests**: To ensure existing functionality is preserved

## Test Coverage Goals

- Aim for ≥80% code coverage for each module
- 100% coverage for critical state detection functions
- Cover all public API functions and interactive commands

## Module-Specific Test Plans

### 1. ecc-term-claude-state.el

Test file: `tests/ecc-vterm/test-ecc-term-claude-state.el`

Key focus areas:
- State detection for all prompt types (y/n, y/y/n, waiting, initial-waiting)
- Custom pattern handling
- Optimized detection in large buffers
- State metadata functions (name, etc.)

Test cases:
```elisp
(ert-deftest test-ecc-term-claude-get-state-basic ()
  "Test basic state detection for different prompt types.")

(ert-deftest test-ecc-term-claude-get-state-custom-patterns ()
  "Test state detection with custom patterns.")

(ert-deftest test-ecc-term-claude-get-state-optimized ()
  "Test optimized state detection in large buffers.")

(ert-deftest test-ecc-term-claude-state-name ()
  "Test state name retrieval and formatting.")
```

### 2. ecc-term-claude-auto.el

Test file: `tests/ecc-vterm/test-ecc-term-claude-auto.el`

Key focus areas:
- Auto-response dispatching to correct functions
- Response mapping for each state
- Auto-mode enabling and disabling
- Error handling for invalid states
- Delayed response functionality

Test cases:
```elisp
(ert-deftest test-ecc-term-claude-auto-send ()
  "Test auto-sending responses for each state type.")

(ert-deftest test-ecc-term-claude-auto-send-delay ()
  "Test delayed auto-responses.")

(ert-deftest test-ecc-term-claude-toggle-auto-mode ()
  "Test toggling auto-mode on and off.")

(ert-deftest test-ecc-term-claude-auto-error-handling ()
  "Test error handling for invalid states and responses.")
```

### 3. ecc-term-claude-setup.el

Test file: `tests/ecc-vterm/test-ecc-term-claude-setup.el`

Key focus areas:
- Common setup across different initialization paths
- Timer and hook configuration
- Cleanup on buffer kill
- Error handling for missing dependencies
- Buffer validation

Test cases:
```elisp
(ert-deftest test-ecc-term-claude-setup-common ()
  "Test common setup functionality.")

(ert-deftest test-ecc-term-claude-setup-timer ()
  "Test timer setup and cancellation.")

(ert-deftest test-ecc-term-claude-setup-hooks ()
  "Test hook setup.")

(ert-deftest test-ecc-term-claude-cleanup-buffer ()
  "Test cleanup when buffer is killed.")
```

### 4. ecc-term-claude-buffer.el

Test file: `tests/ecc-vterm/test-ecc-term-claude-buffer.el`

Key focus areas:
- Buffer registration and tracking
- Buffer customization
- Multi-buffer support
- Buffer local variable handling

Test cases:
```elisp
(ert-deftest test-ecc-term-claude-register-buffer ()
  "Test buffer registration.")

(ert-deftest test-ecc-term-claude-buffer-local-vars ()
  "Test buffer-local variables setup.")

(ert-deftest test-ecc-term-claude-multi-buffer ()
  "Test handling multiple Claude buffers.")
```

### 5. ecc-term-claude-interaction.el

Test file: `tests/ecc-vterm/test-ecc-term-claude-interaction.el`

Key focus areas:
- Yes/No responses
- Custom text sending
- Scrolling behavior 
- Follow-bottom functionality

Test cases:
```elisp
(ert-deftest test-ecc-term-claude-send-yes-no ()
  "Test yes/no response functions.")

(ert-deftest test-ecc-term-claude-send-string ()
  "Test sending arbitrary strings.")

(ert-deftest test-ecc-term-claude-follow-bottom ()
  "Test follow-bottom functionality.")
```

### 6. ecc-term-claude-performance.el

Test file: `tests/ecc-vterm/test-ecc-term-claude-performance.el`

Key focus areas:
- Buffer optimizations
- GC threshold management
- Optimized state detection 
- Hook consolidation

Test cases:
```elisp
(ert-deftest test-ecc-term-claude-optimize-buffer ()
  "Test buffer performance optimizations.")

(ert-deftest test-ecc-term-claude-with-gc-optimization ()
  "Test GC optimization wrapper.")

(ert-deftest test-ecc-term-claude-get-state-optimized ()
  "Test optimized state detection.")
```

## Testing Approach

### Mocking Strategy

For testing functions that interact with the terminal or have side effects:

1. Use `cl-letf` to temporarily replace function definitions
2. Create stub/mock buffers with controlled content 
3. Track function calls to verify behavior

Example:
```elisp
(ert-deftest test-with-mock ()
  (let ((send-called nil))
    (cl-letf (((symbol-function 'vterm-send-string)
              (lambda (str) (setq send-called str))))
      (ecc-term-claude-send-yes)
      (should (equal send-called "y")))))
```

### Test Fixtures

1. Create standardized buffer fixtures for tests:
```elisp
(defun ecc-test-create-buffer-with-content (content)
  "Create a test buffer with CONTENT."
  (let ((buf (generate-new-buffer "*ecc-test*")))
    (with-current-buffer buf
      (insert content)
      (vterm-mode))
    buf))
```

2. Ensure proper cleanup:
```elisp
(unwind-protect
    (do-test-with-buffer buf)
  (kill-buffer buf))
```

## Test Execution Plan

1. Implement tests from most critical to least critical modules:
   - State detection (core functionality)
   - Auto-response (most visible user feature)
   - Setup (affects all other modules)
   - Performance (important but more isolated)

2. Run tests with increased verbosity during development:
```bash
emacs -Q --batch -L . -L src -L tests -l tests/ecc-vterm/test-ecc-term-claude-state.el --eval '(ert-run-tests-batch-and-exit)'
```

3. Incorporate into CI/CD pipeline for automated testing

## Timeline

1. Week 1: Implement state and auto tests
2. Week 2: Implement setup and buffer tests
3. Week 3: Implement interaction and performance tests
4. Week 4: Integration tests and coverage analysis

## Conclusion

This test plan provides a comprehensive approach to ensuring the quality and reliability of the consolidated term-claude modules. By focusing on both unit and integration testing, we can verify the correct functioning of individual components as well as their interactions.