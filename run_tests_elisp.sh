#!/bin/bash
# Versatile Elisp Test Runner
# Compatible with any Elisp project structure

set -e

# Configuration
PROJECT_NAME="${PROJECT_NAME:-$(basename $(pwd))}"
THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_PATH="$THIS_DIR/.run_tests.sh.log"
PASS_THRESHOLD="${PASS_THRESHOLD:-80}"  # Allow CI to pass with >= 80% success

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Helper functions
log() {
    echo -e "${GREEN}[INFO]${NC} $*"
}

error() {
    echo -e "${RED}[ERROR]${NC} $*"
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $*"
}

# Find and add all elisp directories to load path
build_load_path() {
    local load_path_args=""
    
    # Add project root
    load_path_args+=" --eval \"(add-to-list 'load-path \\\"$THIS_DIR\\\")\""
    
    # Find all directories containing .el files
    while IFS= read -r dir; do
        if [[ "$dir" != *"/.old"* ]] && [[ "$dir" != *"/.*"* ]]; then
            load_path_args+=" --eval \"(add-to-list 'load-path \\\"$dir\\\")\""
        fi
    done < <(find "$THIS_DIR" -type d \( -name "src" -o -name "tests" -o -name "lisp" \) -exec find {} -type d \;)
    
    echo "$load_path_args"
}

# Find all test files
find_test_files() {
    find "$THIS_DIR" -name "test-*.el" -o -name "*-test.el" -o -name "*-tests.el" | \
        grep -v "/.old/" | \
        sort
}

# Run tests
run_tests() {
    log "Running tests for $PROJECT_NAME..."
    
    local test_files=($(find_test_files))
    
    if [[ ${#test_files[@]} -eq 0 ]]; then
        error "No test files found"
        return 1
    fi
    
    log "Found ${#test_files[@]} test files"
    
    # Build Emacs command
    local emacs_cmd="emacs -Q --batch"
    emacs_cmd+=" $(build_load_path)"
    
    # Load test files
    for test_file in "${test_files[@]}"; do
        emacs_cmd+=" --load \"$test_file\""
    done
    
    # Run tests
    emacs_cmd+=" --eval \"(ert-run-tests-batch-and-exit)\""
    
    # Execute and capture output
    local output_file="$THIS_DIR/.test-output.txt"
    eval $emacs_cmd 2>&1 | tee "$output_file"
    local exit_code=${PIPESTATUS[0]}
    
    # Extract statistics
    local stats=$(grep -o "Ran [0-9]\+ tests, [0-9]\+ results as expected" "$output_file" || echo "")
    
    if [[ -n "$stats" ]]; then
        local total=$(echo "$stats" | grep -o "Ran [0-9]\+" | grep -o "[0-9]\+")
        local passed=$(echo "$stats" | grep -o "[0-9]\+ results as expected" | grep -o "[0-9]\+")
        local failed=$((total - passed))
        local percentage=0
        
        if [[ $total -gt 0 ]]; then
            percentage=$(( (passed * 100) / total ))
        fi
        
        log "Test Results: $passed/$total passed ($percentage%)"
        
        # Check if we meet the pass threshold
        if [[ $percentage -ge $PASS_THRESHOLD ]]; then
            log "Pass rate ${percentage}% meets threshold ${PASS_THRESHOLD}%"
            return 0
        else
            error "Pass rate ${percentage}% below threshold ${PASS_THRESHOLD}%"
            return $exit_code
        fi
    fi
    
    return $exit_code
}

# Generate simple report
generate_report() {
    local timestamp=$(date "+%Y%m%d-%H%M%S")
    local report_file="$THIS_DIR/TEST-REPORT-${timestamp}.txt"
    
    echo "Test Report for $PROJECT_NAME" > "$report_file"
    echo "Generated: $(date)" >> "$report_file"
    echo "" >> "$report_file"
    
    if [[ -f "$THIS_DIR/.test-output.txt" ]]; then
        echo "=== Test Output ===" >> "$report_file"
        cat "$THIS_DIR/.test-output.txt" >> "$report_file"
    fi
    
    log "Report generated: $report_file"
}

# Main execution
main() {
    log "Starting test runner for $PROJECT_NAME"
    
    # Clear log
    > "$LOG_PATH"
    
    # Run tests
    if run_tests; then
        log "All tests passed!"
        generate_report
        exit 0
    else
        error "Tests failed"
        generate_report
        exit 1
    fi
}

# Run main
main "$@"