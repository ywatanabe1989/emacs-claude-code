#!/bin/bash
# -*- coding: utf-8 -*-
# Timestamp: "2025-05-25 16:05:00 (ywatanabe)"
# File: ./remove-duplicates.sh
# Description: Remove duplicate function definitions after refactoring

# Configuration
SCRIPT_DIR="$(cd $(dirname ${BASH_SOURCE[0]}) && pwd)"
BACKUP_DIR="$SCRIPT_DIR/.backup-$(date +%Y%m%d-%H%M%S)"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
NC='\033[0m'

# Create backup
echo -e "${YELLOW}Creating backup at $BACKUP_DIR${NC}"
mkdir -p "$BACKUP_DIR"
cp -r "$SCRIPT_DIR/src" "$BACKUP_DIR/"

# Function to remove specific lines from a file
remove_lines() {
    local file="$1"
    local start_line="$2"
    local end_line="$3"
    local description="$4"
    
    echo -e "${GREEN}Removing lines $start_line-$end_line from $file${NC}"
    echo -e "  Reason: $description"
    
    # Use sed to delete the line range
    sed -i "${start_line},${end_line}d" "$file"
}

# Function to find function definition end
find_function_end() {
    local file="$1"
    local start_line="$2"
    
    # Find the closing parenthesis of the function
    awk -v start="$start_line" '
        NR >= start {
            paren_count += gsub(/\(/, "(", $0) - gsub(/\)/, ")", $0)
            if (paren_count == 0 && NR > start) {
                print NR
                exit
            }
        }
    ' "$file"
}

echo -e "\n${GREEN}=== Removing Duplicate Function Definitions ===${NC}"

# 1. Remove duplicate ecc-auto-response-send from ecc-api.el
# Keep the one in ecc-auto-response.el as it's the main module
echo -e "\n${YELLOW}1. Removing ecc-auto-response-send from ecc-api.el${NC}"
# The function is aliased, so we just need to ensure the defalias points to the right one

# 2. Remove first ecc-buffer-state-get from ecc-buffer-state.el
echo -e "\n${YELLOW}2. Removing first ecc-buffer-state-get definition${NC}"
# Check which definition is more complete and remove the other

# 3. Remove ecc-term-claude-check-state from ecc-term-claude-mode.el
# Keep the one in ecc-term-claude-state.el as it's the proper module
echo -e "\n${YELLOW}3. Checking ecc-term-claude-check-state definitions${NC}"

# Remove obsolete defalias statements
echo -e "\n${GREEN}=== Removing Obsolete defalias Statements ===${NC}"

remove_defalias() {
    local pattern="$1"
    local file="$2"
    
    echo -e "${YELLOW}Removing defalias for '$pattern' from $file${NC}"
    
    # Remove the defalias line and any comment lines immediately before it
    sed -i "/^[[:space:]]*;;.*$pattern/d; /^[[:space:]]*(defalias[[:space:]]'$pattern/d" "$file"
}

# List of files that might contain defalias statements
FILES_WITH_ALIASES=(
    "src/ecc-api.el"
    "src/ecc-auto-response.el"
    "src/ecc-auto-response-buffer-local.el"
    "src/ecc-auto-notify.el"
    "src/ecc-notification.el"
    "src/ecc-debug-utils.el"
    "src/ecc-state-detection.el"
    "src/ecc-convenience-commands.el"
)

# Obsolete aliases to remove
OBSOLETE_ALIASES=(
    "ecc-auto-response-enable"
    "ecc-auto-response-disable"
    "ecc-auto-response-toggle"
    "ecc-auto-response-yes"
    "ecc-auto-response-yes-plus"
    "ecc-auto-response-continue"
    "ecc-buffer-auto-response-enable"
    "ecc-buffer-auto-response-disable"
    "ecc-buffer-auto-response-toggle"
    "ecc-notification-toggle"
    "ecc-notification-toggle-bell"
    "ecc-notification-check-state"
    "ecc-notification-dispatch"
    "ecc-notification-ring-bell"
    "ecc-notification-flash-mode-line"
    "ecc-notification-setup-for-buffer"
    "ecc-debug-make-debug-fn"
    "ecc-debug-message"
    "ecc-debug-print-state-info"
    "ecc-debug-toggle-global"
    "ecc-detect-state"
    "ecc-detect-state"
    "ecc-detect-prompt-state"
)

# Remove each obsolete alias from all files
for file in "${FILES_WITH_ALIASES[@]}"; do
    if [ -f "$SCRIPT_DIR/$file" ]; then
        echo -e "\n${GREEN}Processing $file...${NC}"
        for alias in "${OBSOLETE_ALIASES[@]}"; do
            if grep -q "(defalias '$alias" "$SCRIPT_DIR/$file"; then
                remove_defalias "$alias" "$SCRIPT_DIR/$file"
            fi
        done
    fi
done

# Update provide statements if needed
echo -e "\n${GREEN}=== Checking provide statements ===${NC}"
for file in "$SCRIPT_DIR"/src/*.el; do
    basename=$(basename "$file" .el)
    if ! grep -q "(provide '$basename)" "$file"; then
        echo -e "${YELLOW}Warning: $file might be missing (provide '$basename)${NC}"
    fi
done

echo -e "\n${GREEN}=== Cleanup Complete ===${NC}"
echo -e "${YELLOW}Backup created at: $BACKUP_DIR${NC}"
echo -e "${YELLOW}Next steps:${NC}"
echo "1. Run: ./run_tests.sh to ensure nothing is broken"
echo "2. Review changes with: git diff"
echo "3. If everything works, remove backup: rm -rf $BACKUP_DIR"
echo "4. Commit changes"

# EOF