#!/bin/bash
# -*- coding: utf-8 -*-
# Author: ywatanabe
# Timestamp: <2025-05-20 23:50:00>
# File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/docs/tools/safe_rm.sh

# This script safely "removes" files by moving them to a .old directory
# rather than actually deleting them.

set -e  # Exit on error

# Display usage information
function show_usage {
    echo "Usage: $0 [OPTIONS] FILE..."
    echo
    echo "Safely 'remove' files by moving them to a .old directory."
    echo
    echo "Options:"
    echo "  -h, --help     Show this help message and exit"
    echo "  -v, --verbose  Display more information during operation"
    echo "  -f, --force    Force operation even if destination exists"
    echo
    echo "Examples:"
    echo "  $0 file.txt           # Move file.txt to .old/file.txt"
    echo "  $0 -v dir/file.txt    # Move with verbose output"
    echo "  $0 *.txt              # Move all .txt files to .old/"
}

# Initial parameter values
VERBOSE=0
FORCE=0

# Parse command line options
while [[ $# -gt 0 ]]; do
    case "$1" in
        -h|--help)
            show_usage
            exit 0
            ;;
        -v|--verbose)
            VERBOSE=1
            shift
            ;;
        -f|--force)
            FORCE=1
            shift
            ;;
        -*)
            echo "Error: Unknown option: $1" >&2
            show_usage
            exit 1
            ;;
        *)
            break
            ;;
    esac
done

# Check if at least one file is specified
if [[ $# -eq 0 ]]; then
    echo "Error: No files specified" >&2
    show_usage
    exit 1
fi

# Function to safely move a file
function safe_move {
    local src="$1"
    
    # Check if source file exists
    if [[ ! -e "$src" ]]; then
        echo "Warning: $src does not exist, skipping" >&2
        return 1
    fi
    
    # Get the directory and filename
    local dir=$(dirname "$src")
    local filename=$(basename "$src")
    
    # Create .old directory if it doesn't exist
    local old_dir="$dir/.old"
    if [[ ! -d "$old_dir" ]]; then
        if [[ $VERBOSE -eq 1 ]]; then
            echo "Creating directory: $old_dir"
        fi
        mkdir -p "$old_dir"
    fi
    
    # Check for timestamp format
    local timestamp=$(date '+%Y%m%d_%H%M%S')
    local dst="$old_dir/$filename"
    
    # If file already exists and force is not set, create a timestamped version
    if [[ -e "$dst" && $FORCE -eq 0 ]]; then
        dst="${old_dir}/${filename}-${timestamp}"
    fi
    
    # Move the file
    if [[ $VERBOSE -eq 1 ]]; then
        echo "Moving: $src → $dst"
    fi
    
    mv "$src" "$dst"
    return 0
}

# Process each file
SUCCESS=0
FAILURE=0

for file in "$@"; do
    if safe_move "$file"; then
        ((SUCCESS++))
    else
        ((FAILURE++))
    fi
done

# Report results
if [[ $VERBOSE -eq 1 ]]; then
    echo "Operation complete: $SUCCESS files moved, $FAILURE files skipped"
fi

# Exit with appropriate status
if [[ $FAILURE -gt 0 ]]; then
    exit 1
else
    exit 0
fi