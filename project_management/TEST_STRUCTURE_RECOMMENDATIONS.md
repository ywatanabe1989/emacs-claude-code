# Test Structure Recommendations

## Current Issues

During the consolidation of the auto modules, we identified several issues with the test structure:

1. **Duplicate Test Definitions**: Many tests were defined in multiple files with the same name but different implementations. For example:
   - `/tests/test-ecc-auto-notify.el`
   - `/tests/ecc-auto/test-ecc-auto-notify.el`

2. **Obsolete Module References**: Some tests referenced modules that no longer exist or have been consolidated, such as `ecc-auto-response-fix`.

3. **Mix of Test Approaches**: Some tests followed a proper TDD approach, while others were written after implementation.

## Recommendations

### File Organization

1. **Consolidated Root-Level Tests**:
   - Keep main test files in the root `/tests` directory
   - Use a simple, consistent naming convention: `test-<module-name>.el`
   - Example: `test-ecc-auto-core.el`

2. **Subdirectory Organization**:
   - Group related tests in subdirectories like `/tests/ecc-auto`, `/tests/ecc-term`, etc.
   - Avoid duplicate test definitions across directories
   - Use these directories for component-specific tests that don't belong in the main module tests

3. **Obsolete Tests Management**:
   - Move obsolete or redundant tests to `.old` directories
   - Use timestamped filenames when moving to maintain history
   - Example: `test-ecc-auto-notify-20250521_014607.el`

### Test Naming and Implementation

1. **Unique Test Names**:
   - Ensure each test has a unique name across the entire codebase
   - Use prefixes to differentiate similar tests (e.g., `test-ecc-auto-notify-global-` vs `test-ecc-auto-notify-buffer-local-`)

2. **Clear Test Documentation**:
   - Include clear docstrings explaining the purpose of each test
   - Indicate which module and functionality is being tested

3. **Module Dependencies**:
   - Always specify required modules at the top of test files
   - Keep dependencies updated when modules are consolidated or renamed

### Test Script Improvements

1. **Explicit Skip Logic**:
   - The current `run_tests.sh` script has some hardcoded skips for obsolete test files
   - Implement a more systematic approach, such as a `.skip` file or pattern

2. **Test Discovery**:
   - Implement better test discovery to prevent loading duplicate tests
   - Use a manifest file to specify which tests should be run

3. **Failure Reporting**:
   - Improve error reporting to provide clearer information about test failures
   - Include context and suggestions for fixing common test issues

## Implementation Plan

1. **Short Term**:
   - Continue moving duplicate and obsolete tests to `.old` directories
   - Fix remaining test failures for consolidated modules
   - Update test dependencies to reference the new consolidated modules

2. **Medium Term**:
   - Refactor the test script to better handle test discovery and skipping
   - Create a test manifest to explicitly control which tests are run
   - Update all tests to follow consistent naming and documentation standards

3. **Long Term**:
   - Implement a comprehensive test framework specific to this project
   - Add automated test coverage reporting
   - Integrate test runs with continuous integration