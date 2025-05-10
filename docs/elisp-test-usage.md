<!-- ---
!-- Timestamp: 2025-05-08 20:45:25
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/docs/elisp-test-usage.md
!-- --- -->

# Elisp-Test Usage Guide

This document explains how to use the `elisp-test.sh` script to run ERT tests for Emacs Lisp projects.

## Overview

The `elisp-test.sh` script provides a convenient way to run Emacs Lisp tests from the command line. It can:

- Run all tests in a directory
- Run a single test file
- Generate detailed test reports in Org format
- Create PDF reports (when pdflatex is available)
- Filter tests by pattern
- Generate mock reports with failed, timed-out and skipped tests for development

## Basic Usage

### Running All Tests in a Directory

To run all tests in the current directory:

```bash
./elisp-test.sh
```

To run all tests in a specific directory:

```bash
./elisp-test.sh -p /path/to/tests
```

### Running a Single Test File

To run tests from a specific file:

```bash
./elisp-test.sh -s tests/test-ecc-elisp-test.el
```

## Options

The script accepts the following options:

| Option | Description |
|--------|-------------|
| `-d, --debug` | Enable debug output |
| `-p, --path DIRECTORY` | Path to directory containing tests (default: current directory) |
| `-t, --timeout SECONDS` | Timeout per test in seconds (default: 10) |
| `-o, --output FILE` | Output report file path (default: ./ELISP-TEST-REPORT.org) |
| `-m, --match PATTERN` | Only run tests matching pattern |
| `-s, --single FILE` | Run a single test file |
| `--mock` | Run in mock mode with synthetic test data (for development) |
| `-h, --help` | Display help message |

## Examples

Run tests with a 5-second timeout:

```bash
./elisp-test.sh -t 5
```

Run only tests matching "buffer" in their filename:

```bash
./elisp-test.sh -m buffer
```

Save the test report to a custom location:

```bash
./elisp-test.sh -o ~/reports/test-report.org
```

Debug mode with a single test file:

```bash
./elisp-test.sh -d -s tests/test-ecc-buffer.el
```

Generate a mock test report with failing, timed-out, and skipped tests:

```bash
./elisp-test.sh --mock
```

## Test Reports

Test reports are generated in Org-mode format and saved to the specified output file. The report includes:

- Summary statistics (passed, failed, skipped, timeout)
- Success rate percentage
- Total time spent
- File-by-file breakdown of test results
- Detailed error messages for failed tests
- Timeout details for tests that exceeded the time limit
- Skip reason for tests that were skipped

The report filename includes a timestamp and the success rate percentage (e.g., `ELISP-TEST-REPORT-20250508-202830-85-PERCENT.org`), making it easy to track progress over time.

The script automatically creates symlinks to the latest reports:
- `LATEST-TEST-REPORT.org`: Points to the most recent org-mode report
- `LATEST-TEST-REPORT.pdf`: Points to the most recent PDF report

If pdflatex is available, PDF versions of the reports are also generated automatically for better readability and sharing.

The script keeps only the 5 most recent reports and automatically cleans up older ones to prevent accumulation of report files.

## Integrating with CI/CD

The script can be easily integrated with CI/CD pipelines. It uses exit codes to indicate success or failure:

- Exit code 0: Tests completed successfully (even if some tests failed)
- Exit code 1: Script execution failed (e.g., incorrect parameters, Emacs error)

Example in GitHub Actions:

```yaml
steps:
  - name: Run Tests
    run: |
      ./elisp-test.sh -p ./tests
```

## Customizing the Output

For more advanced customization of the test report format, you can modify the following files:

- `ecc-elisp-test.el`: Core module for finding, running, and reporting on tests
- `tests/mock-elisp-test.el`: Mock implementation for generating test reports with synthetic data
- `elisp-test.sh`: Shell script that integrates everything for command-line usage

## Internal Components

The elisp-test functionality consists of several components:

1. **ecc-elisp-test.el**: Core Emacs Lisp module that provides functions for:
   - Finding test files in a directory (`ecc-elisp-test-find-test-files`)
   - Extracting test names from files (`ecc-elisp-test-find-tests-in-file`)
   - Running individual tests (`ecc-elisp-test-run-single-test`)
   - Generating test reports (`ecc-elisp-test-generate-report`)

2. **mock-elisp-test.el**: Provides functions for generating synthetic test data:
   - Creates mock test data with fixed pass/fail rates
   - Generates different types of failure modes (errors, timeouts, skips)
   - Formats reports identically to the real test runner

3. **elisp-test.sh**: Shell script that ties everything together:
   - Provides a command-line interface
   - Handles options and arguments
   - Manages report files and cleanup
   - Generates PDF reports when possible

## Further Development

The elisp-test system can be extended in various ways:

- Add test coverage reporting
- Implement test profiling to identify slow tests
- Create test visualization tools
- Add integration with notification systems (e.g., Slack)
- Implement historical test result tracking

<!-- EOF -->