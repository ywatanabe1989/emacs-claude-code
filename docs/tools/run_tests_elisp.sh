#!/bin/bash
# -*- coding: utf-8 -*-
# Timestamp: "2025-05-12 13:31:40 (ywatanabe)"
# File: ./.claude/tools/run_tests_elisp.sh

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

echo_info() { echo -e "${BLACK}$1${NC}"; }
# Colors

REPO_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
# Paths and config
ELISP_TEST_PATH="$HOME/.emacs.d/lisp/elisp-test"
TESTS_DIR="$REPO_ROOT/tests"
TEST_TIMEOUT=10
DEBUG_MODE=false
SINGLE_TEST_FILE=""

usage() {
    echo_info "Usage: $0 [OPTIONS]"
    echo_info ""
    echo_info "Options:"
    echo_info "  -t, --tests-dir DIR       Directory containing test files (default: $TESTS_DIR)"
    echo_info "  -d, --debug               Enable debug output"
    echo_info "  -s, --single FILE         Run a single test file"
    echo_info "  --elisp-test PATH         Path to elisp-test (default: $ELISP_TEST_PATH)"
    echo_info "  --timeout SECONDS         Timeout in seconds (default: ${TEST_TIMEOUT}s)"
    echo_info "  -h, --help                Display this help message"
    exit 1
}

# Parse command line arguments
TESTS_DIR_ARG=""
while [[ $# -gt 0 ]]; do
    case $1 in
        -t|--tests-dir)
            TESTS_DIR_ARG="$2"
            shift 2
            ;;
        --timeout)
            TEST_TIMEOUT="$2"
            shift 2
            ;;
        --elisp-test)
            ELISP_TEST_PATH="$2"
            shift 2
            ;;
        -d|--debug)
            DEBUG_MODE=true
            shift
            ;;
        -s|--single)
            SINGLE_TEST_FILE="$2"
            shift 2
            ;;
        -h|--help)
            usage
            ;;
        *)
            TESTS_DIR_ARG="$1"
            shift
            ;;
    esac
done

# Function to run tests
run_tests_elisp() {
    local test_target="$1"

    if [ -f "$test_target" ]; then
        echo_info "Running single test file: $test_target..."
    elif [ -d "$test_target" ]; then
        echo_info "Running tests in directory: $test_target..."
    fi

    # Prepare command
    local emacs_cmd="emacs -Q --batch"

    # Add load paths
    emacs_cmd+=" --eval \"(add-to-list 'load-path \\\"$(pwd)\\\")\" "
    emacs_cmd+=" --eval \"(add-to-list 'load-path \\\"$THIS_DIR\\\")\" "
    emacs_cmd+=" --eval \"(add-to-list 'load-path \\\"$TESTS_DIR\\\")\" "
    emacs_cmd+=" --eval \"(add-to-list 'load-path \\\"$test_target\\\")\" "
    emacs_cmd+=" --eval \"(add-to-list 'load-path \\\"$ELISP_TEST_PATH\\\")\" "

    # Load elisp-test
    emacs_cmd+=" --eval \"(require 'elisp-test)\" "
    
    # Allow duplicate test loading - set special variable to skip duplication checks
    emacs_cmd+=" --eval \"(setq ert-test-dont-check-redefinition t)\" "

    # Load Package
    local package_name="$(basename $(pwd))"
    emacs_cmd+=" --eval \"(require '$package_name)\" "

    # Set debug level if needed
    if $DEBUG_MODE; then
        emacs_cmd+=" --eval \"(setq debug-on-error t)\" "
    fi

    # Run tests
    emacs_cmd+=" --eval \"(elisp-test-run \\\"$test_target\\\" $TEST_TIMEOUT t)\" "

    # Execute the command
    if $DEBUG_MODE; then
        echo "Running Command:\n$emacs_cmd"
        eval $emacs_cmd 2>&1 | tee -a "$LOG_PATH"
    else
        eval $emacs_cmd >> "$LOG_PATH" 2>&1
    fi

    return ${PIPESTATUS[0]}
}


main() {
    # Determine the target to test
    if [ -n "$SINGLE_TEST_FILE" ]; then
        TEST_TARGET="$SINGLE_TEST_FILE"
    else
        TEST_TARGET="${TESTS_DIR_ARG:-$TESTS_DIR}"
    fi

    # Validate target
    if [ ! -e "$TEST_TARGET" ]; then
        echo_error "Error: Target '$TEST_TARGET' does not exist"
        exit 1
    fi

    $REPO_ROOT/docs/tools/find_incorrect_require_provide_statements.sh

    # Execute tests and log output
    echo_info "Running tests..."
    run_tests_elisp "$TEST_TARGET" | tee -a "$LOG_PATH"
    exit_code=$?

    # # Show summary and errors
    # echo -e "\n\nTest Summary:"

    if [ $exit_code -eq 0 ]; then
        # echo_success "Tests completed successfully"

        # Find report files
        report_file=$(find "$THIS_DIR" -maxdepth 1 -mmin -1 -name "*ELISP-TEST-REPORT*" | head -n 1)
        if [ -n "$report_file" ]; then
            echo_success "Report created: $report_file"
        fi

        exit 0
    else
        echo_error "Tests failed with exit code $exit_code. Please check $LOG_PATH"

        # Show errors from log
        if grep -q "Error\\|No such file" "$LOG_PATH"; then
            grep -A 5 -B 2 "Error\\|No such file" "$LOG_PATH" --color=always
        fi

        exit 1
    fi
}

main "$@"

# EOF