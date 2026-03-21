#!/bin/bash
# -*- coding: utf-8 -*-
# Timestamp: "2026-03-21 12:00:00 (ywatanabe)"
# File: ./check_requires.sh
#
# Validates that all (require 'ecc-*) in src/ have corresponding .el files.
# Runs without Emacs — pure shell, suitable for CI pre-checks.

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$THIS_DIR/.." && pwd)"
SRC_DIR="$PROJECT_ROOT/src"
EXIT_CODE=0

echo "Checking require integrity in $SRC_DIR..."

# Extract all (require 'ecc-*) and verify the file exists
while IFS= read -r line; do
    file=$(echo "$line" | cut -d: -f1)
    feature=$(echo "$line" | grep -oP "require '\\K[a-zA-Z0-9_-]+")
    target="$SRC_DIR/$feature.el"

    if [ ! -f "$target" ]; then
        echo "ERROR: $(basename "$file") requires '$feature' but $feature.el does not exist"
        EXIT_CODE=1
    fi
done < <(grep -rn "^(require 'ecc-" "$SRC_DIR" --include="*.el" 2>/dev/null || true)

# Check that no src/*.el files are gitignored
if command -v git >/dev/null 2>&1 && git -C "$PROJECT_ROOT" rev-parse --git-dir >/dev/null 2>&1; then
    ignored=$(git -C "$PROJECT_ROOT" check-ignore "$SRC_DIR"/*.el 2>/dev/null || true)
    if [ -n "$ignored" ]; then
        echo "ERROR: The following .el files are gitignored:"
        echo "$ignored"
        EXIT_CODE=1
    fi
fi

if [ $EXIT_CODE -eq 0 ]; then
    echo "All require statements have matching files."
else
    echo "Require integrity check FAILED."
fi

exit $EXIT_CODE
