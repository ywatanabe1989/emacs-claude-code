#!/bin/bash
# -*- coding: utf-8 -*-
# Timestamp: "2025-05-25 16:10:00 (ywatanabe)"
# File: ./analyze-duplicates.sh
# Description: Analyze duplicate functions to determine which to keep

SCRIPT_DIR="$(cd $(dirname ${BASH_SOURCE[0]}) && pwd)"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${GREEN}=== Analyzing Duplicate Function Definitions ===${NC}\n"

# Function to extract function definition
extract_function() {
    local file="$1"
    local func_name="$2"
    local context_lines=20
    
    echo -e "${BLUE}File: $file${NC}"
    grep -n -A "$context_lines" "(defun $func_name" "$file" | head -30
}

# 1. Analyze ecc-auto-response-send
echo -e "${YELLOW}1. Analyzing ecc-auto-response-send${NC}"
echo -e "${GREEN}First definition:${NC}"
extract_function "$SCRIPT_DIR/src/ecc-api.el" "ecc-auto-response-send"
echo -e "\n${GREEN}Second definition:${NC}"
extract_function "$SCRIPT_DIR/src/ecc-auto-response.el" "ecc-auto-response-send"

# 2. Analyze ecc-buffer-state-get
echo -e "\n\n${YELLOW}2. Analyzing ecc-buffer-state-get${NC}"
echo -e "${GREEN}Searching in ecc-buffer-state.el:${NC}"
grep -n "defun ecc-buffer-state-get" "$SCRIPT_DIR/src/ecc-buffer-state.el"

# 3. Analyze ecc-term-claude-check-state
echo -e "\n\n${YELLOW}3. Analyzing ecc-term-claude-check-state${NC}"
echo -e "${GREEN}First definition:${NC}"
extract_function "$SCRIPT_DIR/src/ecc-term-claude-mode.el" "ecc-term-claude-check-state"
echo -e "\n${GREEN}Second definition:${NC}"
extract_function "$SCRIPT_DIR/src/ecc-term-claude-state.el" "ecc-term-claude-check-state"

# Check for references to these functions
echo -e "\n\n${YELLOW}=== Checking References ===${NC}"

check_references() {
    local func_name="$1"
    echo -e "\n${GREEN}References to $func_name:${NC}"
    grep -r "$func_name" "$SCRIPT_DIR/src" "$SCRIPT_DIR/tests" --include="*.el" | \
        grep -v "defun $func_name" | \
        grep -v "defalias" | \
        head -10
}

check_references "ecc-auto-response-send"
check_references "ecc-buffer-state-get"
check_references "ecc-term-claude-check-state"

echo -e "\n${GREEN}=== Recommendations ===${NC}"
echo "1. ecc-auto-response-send: Keep the one in ecc-auto-response.el (main module)"
echo "2. ecc-buffer-state-get: Need to check which implementation is used"
echo "3. ecc-term-claude-check-state: Keep the one in ecc-term-claude-state.el (proper module)"

# EOF