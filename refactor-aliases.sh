#!/bin/bash
# -*- coding: utf-8 -*-
# Timestamp: "2025-05-25 16:00:00 (ywatanabe)"
# File: ./refactor-aliases.sh
# Description: Refactor duplicate functions and aliases in emacs-claude-code

# Configuration
SCRIPT_DIR="$(cd $(dirname ${BASH_SOURCE[0]}) && pwd)"
RENAME_SCRIPT="$SCRIPT_DIR/docs/to_claude/bin/replace_and_rename.sh"
DRY_RUN=true  # Set to false to actually perform changes

# Colors
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
NC='\033[0m'

# Check if rename script exists
if [ ! -f "$RENAME_SCRIPT" ]; then
    echo -e "${RED}Error: replace_and_rename.sh not found at $RENAME_SCRIPT${NC}"
    exit 1
fi

# Function to perform rename
rename_function() {
    local old_name="$1"
    local new_name="$2"
    local description="$3"
    
    echo -e "\n${YELLOW}Renaming: $old_name → $new_name${NC}"
    echo -e "${GREEN}Reason: $description${NC}"
    
    if [ "$DRY_RUN" = true ]; then
        "$RENAME_SCRIPT" "$old_name" "$new_name" "$SCRIPT_DIR"
    else
        "$RENAME_SCRIPT" -n "$old_name" "$new_name" "$SCRIPT_DIR"
    fi
}

# Main refactoring
echo -e "${GREEN}Starting refactoring of emacs-claude-code aliases...${NC}"

if [ "$DRY_RUN" = true ]; then
    echo -e "${YELLOW}Running in DRY RUN mode. No changes will be made.${NC}"
    echo "To apply changes, set DRY_RUN=false in this script."
else
    echo -e "${RED}Running in LIVE mode. Changes will be applied!${NC}"
    read -p "Are you sure you want to continue? (y/N): " confirm
    if [ "$confirm" != "y" ]; then
        echo "Aborted."
        exit 1
    fi
fi

# Phase 1: Standardize notification naming (ecc-auto-notify-* → ecc-notification-*)
echo -e "\n${GREEN}=== Phase 1: Standardizing notification naming ===${NC}"
rename_function "ecc-notification-toggle" "ecc-notification-toggle" "Standardize notification naming"
rename_function "ecc-notification-toggle-bell" "ecc-notification-toggle-bell" "Standardize notification naming"
rename_function "ecc-notification-check-state" "ecc-notification-check-state" "Standardize notification naming"
rename_function "ecc-notification-dispatch" "ecc-notification-dispatch" "Already aliased, make consistent"
rename_function "ecc-notification-ring-bell" "ecc-notification-ring-bell" "Standardize notification naming"
rename_function "ecc-auto-notify-flash-mode-line" "ecc-auto-notify-flash-mode-line" "Standardize notification naming"
rename_function "ecc-notification-setup-for-buffer" "ecc-notification-setup-for-buffer" "Standardize notification naming"

# Phase 2: Remove short aliases in favor of descriptive names
echo -e "\n${GREEN}=== Phase 2: Replacing short aliases with descriptive names ===${NC}"
rename_function "ecc-auto-response-enable" "ecc-auto-response-enable" "Use descriptive name"
rename_function "ecc-auto-response-disable" "ecc-auto-response-disable" "Use descriptive name"
rename_function "ecc-auto-response-toggle" "ecc-auto-response-toggle" "Use descriptive name"
rename_function "ecc-auto-response-yes" "ecc-auto-response-yes" "Use descriptive name"
rename_function "ecc-auto-response-yes-plus" "ecc-auto-response-yes-plus" "Use descriptive name"
rename_function "ecc-auto-response-continue" "ecc-auto-response-continue" "Use descriptive name"

# Phase 3: Buffer-local aliases
echo -e "\n${GREEN}=== Phase 3: Replacing buffer-local aliases ===${NC}"
rename_function "ecc-buffer-auto-response-enable" "ecc-buffer-auto-response-enable" "Use descriptive name"
rename_function "ecc-buffer-auto-response-disable" "ecc-buffer-auto-response-disable" "Use descriptive name"
rename_function "ecc-buffer-auto-response-toggle" "ecc-buffer-auto-response-toggle" "Use descriptive name"

# Phase 4: Debug utils aliases
echo -e "\n${GREEN}=== Phase 4: Standardizing debug utils naming ===${NC}"
rename_function "ecc-debug-make-debug-fn" "ecc-debug-make-debug-fn" "Simplify naming"
rename_function "ecc-debug-message" "ecc-debug-message" "Simplify naming"
rename_function "ecc-debug-print-state-info" "ecc-debug-print-state-info" "Simplify naming"
rename_function "ecc-debug-toggle-global" "ecc-debug-toggle-global" "Already aliased, make consistent"

# Phase 5: State detection consolidation
echo -e "\n${GREEN}=== Phase 5: Consolidating state detection functions ===${NC}"
rename_function "ecc-detect-state" "ecc-detect-state" "Consolidate state detection"
rename_function "ecc-detect-state" "ecc-detect-state" "Consolidate state detection"
rename_function "ecc-detect-prompt-state" "ecc-detect-state" "Consolidate state detection"

echo -e "\n${GREEN}=== Refactoring complete ===${NC}"
echo -e "${YELLOW}Note: After running with DRY_RUN=false, you will need to:${NC}"
echo "1. Remove duplicate function definitions manually"
echo "2. Remove defalias statements that are no longer needed"
echo "3. Run tests to ensure everything still works: ./run_tests.sh"
echo "4. Review and commit changes"

# EOF