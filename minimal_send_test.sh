#!/bin/bash
# A minimal test script that only tests ecc-send functions

cd "$(dirname "$0")"

# Process arguments
SCRIPT="./tests/simple-test-ecc-send.el"
DEBUG=false

usage() {
    echo "Usage: $0 [OPTIONS]"
    echo
    echo "Options:"
    echo "  -d, --debug      Enable debug output"
    echo "  -h, --help       Display this help message"
    echo
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -d|--debug)
            DEBUG=true
            shift
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            usage
            exit 1
            ;;
    esac
done

echo "Running minimal tests for ecc-send..."

# Build the command
CMD="emacs -Q --batch"

# Add load paths
CMD+=" --eval \"(add-to-list 'load-path \\\"$(pwd)/tests\\\")\" "
CMD+=" --eval \"(add-to-list 'load-path \\\"$(pwd)/tests/modules\\\")\" "
CMD+=" --eval \"(add-to-list 'load-path \\\"$(pwd)/tests/term\\\")\" "
CMD+=" --eval \"(add-to-list 'load-path \\\"$(pwd)/src\\\")\" "

# Debug mode if requested
if [[ "$DEBUG" == "true" ]]; then
    CMD+=" --eval \"(setq debug-on-error t)\" "
    CMD+=" --eval \"(setq debug-on-signal t)\" "
fi

# Load required modules first
CMD+=" --eval \"(load \\\"$(pwd)/tests/modules/fix-variables.el\\\")\" "
CMD+=" --eval \"(load \\\"$(pwd)/tests/term/vterm-mock.el\\\")\" "

# Load the simple test file (it will run automatically when loaded)
CMD+=" --load \"$SCRIPT\" "

# Print command in debug mode
if [[ "$DEBUG" == "true" ]]; then
    echo "Running command: $CMD"
fi

# Run the command
eval $CMD
EXIT_CODE=$?

# Print result based on exit code
if [ $EXIT_CODE -eq 0 ]; then
    echo -e "\033[0;32mSUCCESS: Tests completed with exit code $EXIT_CODE\033[0m"
else
    echo -e "\033[0;31mFAILURE: Tests failed with exit code $EXIT_CODE\033[0m"
fi

exit $EXIT_CODE