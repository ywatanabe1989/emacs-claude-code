#!/bin/bash
# -*- coding: utf-8 -*-
# Timestamp: "2025-05-11 13:31:36 (ywatanabe)"
# File: ./.claude/scripts/count_parens.sh

THIS_DIR="$(cd $(dirname ${BASH_SOURCE[0]}) && pwd)"
LOG_PATH="$THIS_DIR/.$(basename $0).log"
echo > "$LOG_PATH"

GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color
# ---------------------------------------
count_parens() {
    # Usage examples:
    # count_parens file.el                 # Count all parens in file
    # count_parens file.el 10 50           # Count parens between lines 10-50
    # count_parens file.el "$(head -1 file.el | grep -n "defun" | cut -d: -f1)" # From first defun to end

    local file="$1"
    local start_line="${2:-1}"
    local end_line="${3:-$(wc -l < "$file")}"

    # Define color codes
    GREEN='\033[0;32m'
    RED='\033[0;31m'
    BLACK='\033[0;30m'
    NC='\033[0m' # No Color

    # Define echo functions
    echo_success() { echo -e "${GREEN}$1${NC}"; }
    echo_error() { echo -e "${RED}$1${NC}"; }
    echo_info() { echo -e "${BLACK}$1${NC}"; }

    # Extract lines within range
    local content=$(sed -n "${start_line},${end_line}p" "$file")

    # Count each type of parentheses
    local round_count=$(echo "$content" | grep -o "(" | wc -l)
    local round_close_count=$(echo "$content" | grep -o ")" | wc -l)
    local square_count=$(echo "$content" | grep -o "[" | wc -l)
    local square_close_count=$(echo "$content" | grep -o "]" | wc -l)
    local curly_count=$(echo "$content" | grep -o "{" | wc -l)
    local curly_close_count=$(echo "$content" | grep -o "}" | wc -l)

    # Calculate status
    local round_status=$([ $round_count -eq $round_close_count ] && echo_success "BALANCED" || echo_error "UNBALANCED")
    local square_status=$([ $square_count -eq $square_close_count ] && echo_success "BALANCED" || echo_error "UNBALANCED")
    local curly_status=$([ $curly_count -eq $curly_close_count ] && echo_success "BALANCED" || echo_error "UNBALANCED")

    # Report results
    echo -e "($round_count, $round_close_count) matches: $round_status"
    echo -e "[$square_count, $square_close_count] matches: $square_status"
    echo -e "{$curly_count, $curly_close_count} matches: $curly_status"

    # Set exit code based on balance
    if [ $round_count -eq $round_close_count ] && [ $square_count -eq $square_close_count ] && [ $curly_count -eq $curly_close_count ]; then
        return 0
    else
        return 1
    fi
}

# EOF