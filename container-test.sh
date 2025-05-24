#!/bin/bash
# Container testing wrapper for emacs-claude-code
# Supports both Docker and Apptainer

set -e

# Default values
CONTAINER_RUNTIME="docker"
EMACS_VERSION="28.2"
INTERACTIVE=false

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --apptainer|--singularity)
            CONTAINER_RUNTIME="apptainer"
            shift
            ;;
        --docker)
            CONTAINER_RUNTIME="docker"
            shift
            ;;
        --emacs-version)
            EMACS_VERSION="$2"
            shift 2
            ;;
        --interactive|-i)
            INTERACTIVE=true
            shift
            ;;
        --help|-h)
            echo "Usage: $0 [OPTIONS]"
            echo "Options:"
            echo "  --docker           Use Docker (default)"
            echo "  --apptainer        Use Apptainer/Singularity"
            echo "  --emacs-version    Emacs version to test with (default: 28.2)"
            echo "  --interactive, -i  Run interactive shell instead of tests"
            echo "  --help, -h         Show this help message"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Docker implementation
if [ "$CONTAINER_RUNTIME" = "docker" ]; then
    IMAGE="silex/emacs:${EMACS_VERSION}-ci"
    
    echo "Using Docker with image: $IMAGE"
    
    if [ "$INTERACTIVE" = true ]; then
        docker run -it --rm \
            -v "$(pwd):/workspace" \
            -w /workspace \
            "$IMAGE" \
            bash
    else
        docker run --rm \
            -v "$(pwd):/workspace" \
            -w /workspace \
            "$IMAGE" \
            bash run_tests.sh
    fi
    
# Apptainer implementation
elif [ "$CONTAINER_RUNTIME" = "apptainer" ]; then
    SIF_FILE="emacs-${EMACS_VERSION}.sif"
    
    # Build image if it doesn't exist
    if [ ! -f "$SIF_FILE" ]; then
        echo "Building Apptainer image: $SIF_FILE"
        apptainer build "$SIF_FILE" "docker://silex/emacs:${EMACS_VERSION}-ci"
    fi
    
    echo "Using Apptainer with image: $SIF_FILE"
    
    if [ "$INTERACTIVE" = true ]; then
        apptainer shell --bind "$(pwd):/workspace" "$SIF_FILE"
    else
        apptainer exec --bind "$(pwd):/workspace" "$SIF_FILE" \
            bash -c "cd /workspace && bash run_tests.sh"
    fi
fi