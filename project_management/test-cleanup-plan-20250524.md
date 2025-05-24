# Test Suite Cleanup Plan

Date: 2025-05-24
Branch: feature/test-cleanup-2025-0524-203437
Checkpoint: checkpoint/before-test-cleanup-2025-0524-203437

## Current State

Based on TEST-QUALITY-REVIEW.md:
- 43 active test files (excluding .old)
- 62 obsolete test files in .old directories
- 27 duplicate test names
- 75% test pass rate (69/91 tests)
- 22 failing tests

## Cleanup Tasks Priority

### Phase 1: Remove Obsolete Files (High Priority)
1. Remove all .old directories containing 62 obsolete test files
   - tests/ecc-auto/.old/
   - tests/ecc-state/.old/
   - tests/ecc-vterm/.old-test-*
   - tests/.old/
2. Remove test-consolidated-modules.el.skip

### Phase 2: Fix Duplicate Test Names (High Priority)
1. Rename 27 duplicate test names to be unique
2. Most duplicates between:
   - ecc-term/ and ecc-vterm/ directories
   - test-buffer-state-refactored.el and test-buffer-state-core.el

### Phase 3: Consolidate Duplicate Test Suites (Medium Priority)
1. Merge ecc-term and ecc-vterm test suites
2. Remove redundant mock implementations
3. Keep only necessary test variations

### Phase 4: Remove Other Obsolete Files (Medium Priority)
1. Remove unused mock files if functionality can be tested directly
2. Remove any commented-out or disabled tests

### Phase 5: Verify and Test (High Priority)
1. Run all tests to ensure nothing breaks
2. Update test documentation
3. Commit with comprehensive test report

## Acceptance Criteria

- [ ] Zero .old directories in tests/
- [ ] Zero duplicate test names
- [ ] All tests have unique, meaningful names
- [ ] Mock usage reduced to external dependencies only
- [ ] 100% test files follow naming convention: test-[module]-[functionality].el
- [ ] All tests pass after cleanup

## Commands to Use

```bash
# For safe file removal
./docs/to_claude/bin/safe_rm.sh <file>

# To run tests
./run_tests.sh

# To check for duplicates
rg "^\\(ert-deftest\\s+\\(test-[a-zA-Z0-9-]+\\)" tests/ -r "$2" --no-filename | sort | uniq -d
```

## Post-Cleanup Verification

1. Run tree to verify structure
2. Run all tests
3. Generate new test report
4. Update TEST-QUALITY-REVIEW.md with results