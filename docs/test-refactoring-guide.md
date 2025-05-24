<!-- ---
!-- Timestamp: 2025-05-25 02:48:00
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/docs/test-refactoring-guide.md
!-- --- -->

# Test Refactoring Guide: One Assertion Per Test

## Overview

This guide demonstrates how to refactor tests to follow the "one assertion per test" principle, a cornerstone of Test-Driven Development (TDD).

## Why One Assertion Per Test?

1. **Clear Failure Messages**: When a test fails, you know exactly what went wrong
2. **Better Test Names**: Forces descriptive names that explain the specific behavior
3. **Easier Debugging**: No need to figure out which assertion failed
4. **Independent Tests**: Each test can run in isolation
5. **Better Coverage**: Ensures all behaviors are explicitly tested

## Refactoring Example

### Before: Multi-Assertion Test

```elisp
(ert-deftest test-debug-toggle-category ()
  "Test toggling debug categories."
  (let ((ecc-debug-enabled t)
        (ecc-debug-categories '(cat1 cat2 cat3))
        (ecc-debug-enabled-categories nil))
    
    ;; Test initial state (all enabled)
    (should (ecc-debug--category-enabled-p 'cat1))
    (should (ecc-debug--category-enabled-p 'cat2))
    
    ;; Disable cat1
    (ecc-debug-toggle-category 'cat1)
    (should-not (ecc-debug--category-enabled-p 'cat1))
    (should (ecc-debug--category-enabled-p 'cat2))
    
    ;; Re-enable cat1
    (ecc-debug-toggle-category 'cat1)
    (should (ecc-debug--category-enabled-p 'cat1))))
```

**Problems**:
- 5 assertions in one test
- Multiple behaviors tested together
- Unclear which behavior failed when test breaks

### After: Atomic Tests

```elisp
(ert-deftest test-debug-toggle-category-all-enabled-initially ()
  "Test that all debug categories are enabled when enabled-categories is nil."
  (let ((ecc-debug-enabled t)
        (ecc-debug-categories '(cat1 cat2 cat3))
        (ecc-debug-enabled-categories nil))
    (should (ecc-debug--category-enabled-p 'cat1))))

(ert-deftest test-debug-toggle-category-disables-specific-category ()
  "Test that toggling a category disables it when all are initially enabled."
  (let ((ecc-debug-enabled t)
        (ecc-debug-categories '(cat1 cat2 cat3))
        (ecc-debug-enabled-categories nil))
    (ecc-debug-toggle-category 'cat1)
    (should-not (ecc-debug--category-enabled-p 'cat1))))

(ert-deftest test-debug-toggle-category-preserves-other-categories ()
  "Test that toggling one category doesn't affect others."
  (let ((ecc-debug-enabled t)
        (ecc-debug-categories '(cat1 cat2 cat3))
        (ecc-debug-enabled-categories nil))
    (ecc-debug-toggle-category 'cat1)
    (should (ecc-debug--category-enabled-p 'cat2))))
```

**Benefits**:
- Each test has one clear purpose
- Test names describe specific behaviors
- Failures are immediately understandable
- Tests can be run independently

## Refactoring Process

1. **Identify Multi-Assertion Tests**: Look for tests with multiple `should`, `should-not`, or `should-error`
2. **Extract Behaviors**: Each assertion usually represents a distinct behavior
3. **Create Atomic Tests**: One test per behavior with descriptive name
4. **Share Setup**: Use helper functions or macros for common setup
5. **Verify Independence**: Ensure each test can run alone

## Common Patterns to Split

### State Transitions
```elisp
;; BAD
(ert-deftest test-toggle ()
  (feature-enable)
  (should enabled-p)      ; Test 1: enable works
  (feature-disable)  
  (should-not enabled-p)) ; Test 2: disable works

;; GOOD
(ert-deftest test-feature-enable-sets-enabled-state ()
  (feature-enable)
  (should enabled-p))

(ert-deftest test-feature-disable-clears-enabled-state ()
  (feature-enable)  ; Setup
  (feature-disable) ; Action
  (should-not enabled-p))
```

### Multiple Outputs
```elisp
;; BAD
(ert-deftest test-parse-result ()
  (let ((result (parse-input "test")))
    (should (equal (car result) 'success))    ; Test 1
    (should (equal (cdr result) "parsed"))    ; Test 2
    (should (equal (length result) 2))))      ; Test 3

;; GOOD
(ert-deftest test-parse-input-returns-success-status ()
  (should (equal (car (parse-input "test")) 'success)))

(ert-deftest test-parse-input-returns-parsed-value ()
  (should (equal (cdr (parse-input "test")) "parsed")))
```

### Error Conditions
```elisp
;; BAD
(ert-deftest test-validation ()
  (should (valid-p "good"))          ; Test 1
  (should-not (valid-p ""))          ; Test 2
  (should-not (valid-p nil)))        ; Test 3

;; GOOD
(ert-deftest test-validation-accepts-valid-string ()
  (should (valid-p "good")))

(ert-deftest test-validation-rejects-empty-string ()
  (should-not (valid-p "")))

(ert-deftest test-validation-rejects-nil ()
  (should-not (valid-p nil)))
```

## Test Naming Convention

Use the format: `test-<module>-<function>-<specific-behavior>`

Examples:
- `test-debug-toggle-buffer-enables-when-disabled`
- `test-auto-response-send-includes-buffer-name-in-notification`
- `test-state-detection-y-n-matches-standard-prompt`

## Conclusion

While refactoring to one assertion per test creates more test functions, it results in:
- More maintainable test suite
- Clearer test failures
- Better documentation of expected behaviors
- Easier debugging and fixing of issues

The initial effort pays off through reduced debugging time and increased confidence in the test suite.

<!-- EOF -->