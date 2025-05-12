#!/bin/bash
# -*- coding: utf-8 -*-
# Timestamp: "2025-05-13 02:52:54 (ywatanabe)"
# File: ./run_tests.sh

THIS_DIR="$(cd $(dirname ${BASH_SOURCE[0]}) && pwd)"
LOG_PATH="$THIS_DIR/.$(basename $0).log"
echo > "$LOG_PATH"

BLACK='\033[0;30m'
LIGHT_GRAY='\033[0;37m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

echo_info() { echo -e "${LIGHT_GRAY}$1${NC}"; }
echo_success() { echo -e "${GREEN}$1${NC}"; }
echo_warning() { echo -e "${YELLOW}$1${NC}"; }
echo_error() { echo -e "${RED}$1${NC}"; }
# ---------------------------------------

#
# Enhanced test runner for emacs-claude-code
# Fixes selector issue to run all tests

# Color codes for terminal output

# Helper functions for colored output

# Utility functions
strip_ansi() { sed 's/\x1b\[[0-9;]*m//g'; }

# Get the project root directory
PROJECT_NAME=$(basename "$THIS_DIR")

# Parse command-line arguments
DEBUG=false
SKIP_FAIL=false
NO_REPORT=false
VERBOSE=false
SRC_DIR="src"
TESTS_DIR="tests"
TEST_SELECTOR="t"  # Default to run all tests

while [[ $# -gt 0 ]]; do
  case $1 in
    --debug)
      DEBUG=false
      shift
      ;;
    --skip-fail-test)
      SKIP_FAIL=true
      shift
      ;;
    --no-report)
      NO_REPORT=true
      shift
      ;;
    --verbose)
      VERBOSE=true
      shift
      ;;
    --src-dir=*)
      SRC_DIR="${1#*=}"
      shift
      ;;
    --tests-dir=*)
      TESTS_DIR="${1#*=}"
      shift
      ;;
    --selector=*)
      TEST_SELECTOR="${1#*=}"
      shift
      ;;
    *)
      # Check if this looks like a test selector without flag
      if [[ $1 =~ ^[a-zA-Z0-9-]+$ ]]; then
        TEST_SELECTOR="'(name . \".*$1.*\")"
        echo_info "Using selector for tests matching: $1"
      fi
      shift
      ;;
  esac
done

# Auto-detect project structure
detect_project_structure() {
  # Create arrays to store discovered source and test directories
  src_dirs=()
  test_dirs=()

  # Check if the specified source directory exists
  if [ -d "$THIS_DIR/$SRC_DIR" ]; then
    # Add the main source directory
    src_dirs+=("$THIS_DIR/$SRC_DIR")
    # Find all subdirectories in the source directory
    while IFS= read -r dir; do
      if [ -d "$dir" ]; then
        src_dirs+=("$dir")
      fi
    done < <(find "$THIS_DIR/$SRC_DIR" -type d -not -path "*/\.*" 2>/dev/null | sort)
  fi

  # Check if the specified tests directory exists
  if [ -d "$THIS_DIR/$TESTS_DIR" ]; then
    # Add the main tests directory
    test_dirs+=("$THIS_DIR/$TESTS_DIR")
    # Find all subdirectories in the tests directory
    while IFS= read -r dir; do
      if [ -d "$dir" ]; then
        test_dirs+=("$dir")
      fi
    done < <(find "$THIS_DIR/$TESTS_DIR" -type d -not -path "*/\.*" 2>/dev/null | sort)
  fi

  # Return discovered directories as space-separated strings
  echo "SOURCE_DIRS=\"${src_dirs[*]}\""
  echo "TEST_DIRS=\"${test_dirs[*]}\""
}

# Run the tests
run_tests() {
  # Get project structure
  eval "$(detect_project_structure)"

  if $VERBOSE; then
    echo_info "Detected source directories: $SOURCE_DIRS"
    echo_info "Detected test directories: $TEST_DIRS"
  fi

  # Create a file to collect information about available tests
  TEST_INFO_FILE="$THIS_DIR/.test_info.el"
  cat > "$TEST_INFO_FILE" << EOF
(defun ecc-list-all-tests ()
  "List all available ERT tests."
  (message "\\nAVAILABLE TESTS:\\n=============")
  (let ((tests (ert--list-all-tests)))
    (message "Found %d total tests" (length tests))
    (dolist (test tests)
      (message "  * %s" (ert-test-name test)))))
EOF

  # Prepare test runner command
  emacs_cmd="emacs -Q --batch"

  # Add project root to load path
  emacs_cmd+=" --eval \"(add-to-list 'load-path \\\"$THIS_DIR\\\")\" "

  # Handle paths by finding all source and test directories
  for src_subdir in $(find "$THIS_DIR/src" -type d 2>/dev/null | grep -v .old); do
    emacs_cmd+=" --eval \"(add-to-list 'load-path \\\"$src_subdir\\\")\" "
  done

  for test_subdir in $(find "$THIS_DIR/tests" -type d 2>/dev/null | grep -v .old); do
    emacs_cmd+=" --eval \"(add-to-list 'load-path \\\"$test_subdir\\\")\" "
  done

  # Load all test files explicitly
  echo_warning "Loading all test files..."

  # First, reset any potential selector from test files
  emacs_cmd+=" --eval \"(setq-default ert-selector t)\" "

  # First load our test info file
  emacs_cmd+=" --load \"$TEST_INFO_FILE\" "

  # Load top-level test files
  for test_file in $(find "$THIS_DIR/tests" -maxdepth 1 -name "test-*.el" 2>/dev/null | sort | grep -v .old); do
    if $VERBOSE; then
      echo_info "Loading $test_file"
    fi
    emacs_cmd+=" --load \"$test_file\" "
  done

  # Load test files from subdirectories
  for test_dir in $(find "$THIS_DIR/tests" -mindepth 1 -type d 2>/dev/null | sort | grep -v .old); do
    for test_file in $(find "$test_dir" -maxdepth 1 -name "test-*.el" 2>/dev/null | sort); do
      if $VERBOSE; then
        echo_info "Loading $test_file"
      fi
      emacs_cmd+=" --load \"$test_file\" "
    done
  done

  # CRITICAL: Reset any ert-selector that may have been set by loaded files
  emacs_cmd+=" --eval \"(message \\\"Current ert-selector before reset: %S\\\"
                         (if (boundp 'ert-selector) ert-selector 'not-bound))\" "

  emacs_cmd+=" --eval \"(setq-default ert-selector t)\" "
  emacs_cmd+=" --eval \"(makunbound 'ert-selector)\" "


  # Skip intentional failures if requested
  if $SKIP_FAIL; then
    echo_warning "Skipping intentional failing tests..."
    # Use ERT's test selector to exclude tests with "fail" in their name
    TEST_SELECTOR="'(not (name . \".*fail.*\"))"
  fi

  # # Display what selector we're using
  # echo_warning "Using test selector: $TEST_SELECTOR"

  # Run ERT with our specified selector
  emacs_cmd+=" --eval \"(message \\\"Running tests with selector: %S\\\" '$TEST_SELECTOR)\" "
  emacs_cmd+=" --eval \"(ert-run-tests-batch-and-exit $TEST_SELECTOR)\" "

  # Execute the command
  echo_warning "Running $PROJECT_NAME tests..."
  if $DEBUG || $VERBOSE; then
    echo_warning "Command: $emacs_cmd"
  fi

  TEST_OUTPUT_FILE="$THIS_DIR/.test_output_temp.log"
  eval $emacs_cmd 2>&1 | tee -a $LOG_PATH > $TEST_OUTPUT_FILE
  TEST_EXIT_CODE=${PIPESTATUS[0]}

  if [ $TEST_EXIT_CODE -eq 0 ]; then
    echo_success "Tests completed successfully!"
  else
    echo_error "Tests failed with exit code: $TEST_EXIT_CODE"
  fi

  return $TEST_EXIT_CODE
}

# Generate the report
generate_report() {
  echo_info "Generating test report..."

  # Extract test statistics
  reporting_line=$(grep -o "Ran [0-9]\+ tests, [0-9]\+ results as expected, [0-9]\+ unexpected" "$TEST_OUTPUT_FILE")
  TOTAL_TESTS=$(echo $reporting_line | awk '{print $2}' )
  PASSED_TESTS=$(echo $reporting_line | awk '{print $4}' )
  FAILED_TESTS=$(echo $reporting_line | awk '{print $8}' )
  SKIPPED_TESTS=0
  TIMEOUT_TESTS=0
  DUPLICATE_TESTS=0

  # Generate timestamp and report names
  TIMESTAMP=$(date +%Y%m%d-%H%M%S)

  # Calculate success rate for report (with decimal) and filename (integer)
  if [ "x$TOTAL_TESTS" = "x0" ] || [ -z "$TOTAL_TESTS" ]; then
    SUCCESS_RATE="0.0"
    SUCCESS_PERCENT=0
  else
    if command -v bc >/dev/null 2>&1; then
      SUCCESS_RATE=$(echo "scale=1; $PASSED_TESTS * 100 / $TOTAL_TESTS" | bc)
      SUCCESS_PERCENT=$(echo "scale=0; $PASSED_TESTS * 100 / $TOTAL_TESTS" | bc)
    else
      SUCCESS_RATE="100.0"
      SUCCESS_PERCENT=100
    fi
  fi

  # Delete existing reports
  find $THIS_DIR -maxdepth 1 -type f -name "*-TEST-REPORT-*.org" | xargs rm -f 2>/dev/null

  # Create filename with metrics
  REPORT_FILE="ELISP-TEST-REPORT-${TIMESTAMP}-${PASSED_TESTS}-PASSED-${TOTAL_TESTS}-TOTAL-${SUCCESS_PERCENT}-PERCENT.org"
  REPORT_PATH="$THIS_DIR/$REPORT_FILE"
  LATEST_REPORT="$THIS_DIR/LATEST-ELISP-REPORT.org"

  # Extract test duration
  TOTAL_TIME=$(grep -o "[0-9]\+\.[0-9]\+ sec)" "$TEST_OUTPUT_FILE" | tail -1 | cut -d'(' -f2 | cut -d' ' -f1)
  if [ -z "$TOTAL_TIME" ]; then
    TOTAL_TIME="0.00"
  fi

  # Create the report
  cat > "$REPORT_PATH" << EOF
#+TITLE: $PROJECT_NAME Test Report
#+AUTHOR: $(whoami)
#+DATE: $(date '+%Y-%m-%d %H:%M:%S') Generated by $(basename $0)

* Test Results Summary
- Passed: $PASSED_TESTS
- Failed: $FAILED_TESTS
- Skipped: $SKIPPED_TESTS
- Timeout (= 10 s): $TIMEOUT_TESTS
- Duplicates: $DUPLICATE_TESTS
- Total: $TOTAL_TESTS
- Total Time: $TOTAL_TIME seconds
- Success Rate: $SUCCESS_RATE%
EOF

  # Add detailed test results
  process_test_results >> "$REPORT_PATH"

  # Update latest report link
  if [ -f "$LATEST_REPORT" ]; then
    rm -f "$LATEST_REPORT"
  fi
  ln -sfr "$REPORT_PATH" "$LATEST_REPORT"

  echo_success "Report generated: $REPORT_FILE"
}

process_test_results() {
  # First, create a clean version of the output file
  strip_ansi < "$TEST_OUTPUT_FILE" > "$THIS_DIR/.test_output_clean.txt"

  # Process passed tests
  echo
  echo "* Passed Tests ($PASSED_TESTS)"
  if [ "$PASSED_TESTS" -gt 0 ]; then
    # Create a list of passed tests
    grep -E "^[ ]+passed" "$THIS_DIR/.test_output_clean.txt" | awk '{print $3}' > "$THIS_DIR/.passed_tests.txt"

    # Create a test-to-file mapping for better organization
    > "$THIS_DIR/.test_file_mapping.txt"

    # Scan the test directories to build the mapping
    find "$THIS_DIR/$TESTS_DIR" -type f -name "*.el" | while read -r file_path; do
      rel_path=${file_path#"$THIS_DIR/"}
      # Extract test names from the file
      grep -o "(ert-deftest [a-zA-Z0-9-]*" "$file_path" | cut -d' ' -f2 | while read -r test_name; do
        if [ -n "$test_name" ]; then
          echo "$test_name|$rel_path" >> "$THIS_DIR/.test_file_mapping.txt"
        fi
      done
    done

    # Group tests by test file
    if [ -s "$THIS_DIR/.test_file_mapping.txt" ]; then
      # Get all unique test files
      test_files=$(cut -d'|' -f2 "$THIS_DIR/.test_file_mapping.txt" | sort | uniq)

      for test_file in $test_files; do
        # Get passed tests for this file
        file_tests=$(cat "$THIS_DIR/.passed_tests.txt" | while read -r test_name; do
          grep "^$test_name|$test_file$" "$THIS_DIR/.test_file_mapping.txt" | cut -d'|' -f1
        done)

        file_test_count=$(echo "$file_tests" | grep -v "^$" | wc -l)
        if [ "$file_test_count" -gt 0 ]; then
          # Display file with test count
          echo
          echo "** $test_file ($file_test_count tests)"
          # Display each test with link
          echo "$file_tests" | grep -v "^$" | sort | while read -r test_name; do
            echo "- [[file:$test_file::$test_name][$test_name]]"
          done
        fi
      done
    else
      # Fallback to simpler format if mapping failed
      echo_warning "Test-to-file mapping not available, using simple listing:"
      cat "$THIS_DIR/.passed_tests.txt" | sort | while read -r test_name; do
        echo "- $test_name"
      done
    fi
  else
    echo "No passed tests found."
  fi

  # Process failed tests
  echo
  echo "* Failed Tests ($FAILED_TESTS)"
  if [ "$FAILED_TESTS" -gt 0 ]; then
    # Extract all relevant sections for failed tests
    sed -n '/Test.*backtrace:/,/Test.*condition:/p' "$THIS_DIR/.test_output_clean.txt" > "$THIS_DIR/.failed_traces.txt"

    # Extract failed tests with their file locations
    grep -A 1 "FAILED" "$THIS_DIR/.test_output_clean.txt" > "$THIS_DIR/.failed_lines.txt"

    # Extract test names and their files
    grep "FAILED" "$THIS_DIR/.failed_lines.txt" |
    sed -n 's/.*FAILED[^a-zA-Z]*\([a-zA-Z0-9-]*\).* at \(.*\):\([0-9]*\)/\1|\2/p' > "$THIS_DIR/.failed_tests_files.txt"

    # If the above extraction doesn't work (no "at file:line" format), try alternate method
    if [ ! -s "$THIS_DIR/.failed_tests_files.txt" ]; then
      # Alternative extraction method
      grep "FAILED" "$THIS_DIR/.failed_lines.txt" | awk '{print $3}' > "$THIS_DIR/.failed_tests.txt"

      # Try to use the test-to-file mapping we created for passed tests
      if [ -s "$THIS_DIR/.test_file_mapping.txt" ]; then
        while read -r test_name; do
          if [ -n "$test_name" ]; then
            file=$(grep "^$test_name|" "$THIS_DIR/.test_file_mapping.txt" | cut -d'|' -f2 | head -1)
            if [ -n "$file" ]; then
              echo "$test_name|$file" >> "$THIS_DIR/.failed_tests_files.txt"
            fi
          fi
        done < "$THIS_DIR/.failed_tests.txt"
      fi

      # If still no mapping, use simple listing
      if [ ! -s "$THIS_DIR/.failed_tests_files.txt" ]; then
        echo_warning "Test-to-file mapping not available for failed tests, using simple listing:"
        cat "$THIS_DIR/.failed_tests.txt" | sort | while read -r test_name; do
          echo "- $test_name"

          # Try to extract error info
          backtrace=$(grep -A 10 "$test_name.*FAILED" "$THIS_DIR/.test_output_clean.txt")
          if [ -n "$backtrace" ]; then
            echo "  + Error details:"
            echo "$backtrace" | sed 's/^/    /'
          fi
        done

        # We've already handled output, so return early
        return
      fi
    fi

    # Group failed tests by file
    failed_files=$(cat "$THIS_DIR/.failed_tests_files.txt" | cut -d'|' -f2 | sort | uniq)
    for file in $failed_files; do
      # Get tests for this file
      file_tests=$(grep "|$file$" "$THIS_DIR/.failed_tests_files.txt" | cut -d'|' -f1)
      file_count=$(echo "$file_tests" | grep -v "^$" | wc -l)

      if [ "$file_count" -gt 0 ]; then
        # Display file with test count
        echo
        echo "** $file ($file_count tests)"

        # Display each failed test with link and detailed error info
        echo "$file_tests" | grep -v "^$" | sort | while read -r test_name; do
          echo "- [[file:$file::$test_name][$test_name]]"

          # Extract full backtrace for this test
          echo "  + Error details:"

          # First try to get the backtrace
          backtrace=$(sed -n "/Test $test_name backtrace:/,/Test $test_name condition:/p" "$THIS_DIR/.failed_traces.txt")

          # If no backtrace found, try extracting error from the general context
          if [ -z "$backtrace" ]; then
            backtrace=$(grep -A 15 "$test_name.*FAILED" "$THIS_DIR/.test_output_clean.txt")
          fi

          # Display error details with proper indentation
          if [ -n "$backtrace" ]; then
            echo "$backtrace" | sed 's/^/    /'
          else
            echo "    (No detailed error information available)"
          fi
        done
      fi
    done
  else
    echo "No failed tests found."
  fi
}

# Main execution
run_tests
TEST_EXIT_CODE=$?

# Generate report unless --no-report flag was provided
if ! $NO_REPORT; then
  generate_report
fi

# Clean up temporary files
rm -f "$TEST_INFO_FILE"
# Leave other temp files for debugging if verbose or debug mode
if ! $VERBOSE && ! $DEBUG; then
  rm -f "$THIS_DIR/.test_output_clean.txt" "$THIS_DIR/.passed_tests.txt" "$THIS_DIR/.test_file_mapping.txt" \
        "$THIS_DIR/.failed_traces.txt" "$THIS_DIR/.failed_lines.txt" "$THIS_DIR/.failed_tests_files.txt" \
        "$THIS_DIR/.failed_tests.txt" "$THIS_DIR/.test_output_temp.log"
fi

echo_info "Logged to: $LOG_PATH"
exit $TEST_EXIT_CODE

# EOF