#!/bin/bash
# -*- coding: utf-8 -*-
# Timestamp: "2025-05-25 16:25:00 (ywatanabe)"
# File: ~/.claude/to_claude/bin/refactor-rename.sh
# Description: Orchestrate systematic renaming across codebases

# Configuration
SCRIPT_DIR="$(cd $(dirname ${BASH_SOURCE[0]}) && pwd)"
RENAME_SCRIPT="${RENAME_SCRIPT:-$SCRIPT_DIR/replace_and_rename.sh}"
TARGET_DIR="${1:-.}"
MAPPING_FILE="${2:-rename-mappings.txt}"
DRY_RUN=${DRY_RUN:-true}

# Colors
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
NC='\033[0m'

# Function to display usage
show_usage() {
    echo "Usage: $0 [target_directory] [mapping_file]"
    echo ""
    echo "Environment variables:"
    echo "  DRY_RUN=true|false    Whether to perform dry run (default: true)"
    echo "  RENAME_SCRIPT=path    Path to replace_and_rename.sh"
    echo ""
    echo "Mapping file format:"
    echo "  old_name:new_name:reason"
    echo ""
    echo "Examples:"
    echo "  $0                    # Use current dir and rename-mappings.txt"
    echo "  $0 ./src mappings.txt # Specific directory and mapping file"
    echo "  DRY_RUN=false $0      # Actually perform the renaming"
}

# Parse arguments
if [[ "$1" == "-h" ]] || [[ "$1" == "--help" ]]; then
    show_usage
    exit 0
fi

# Check prerequisites
if [ ! -f "$RENAME_SCRIPT" ]; then
    echo -e "${RED}Error: replace_and_rename.sh not found at $RENAME_SCRIPT${NC}"
    echo "Please ensure the script exists or set RENAME_SCRIPT environment variable"
    exit 1
fi

if [ ! -f "$MAPPING_FILE" ]; then
    echo -e "${RED}Error: Mapping file not found: $MAPPING_FILE${NC}"
    echo ""
    echo "Please create a mapping file with format:"
    echo "old_name:new_name:reason"
    echo ""
    echo "Example:"
    cat > "$MAPPING_FILE.example" << 'EOF'
# Function renamings
ecc-auto-start:ecc-auto-response-enable:Use descriptive name
ecc-auto-stop:ecc-auto-response-disable:Use descriptive name
ecc-yes:ecc-auto-response-yes:Use descriptive name

# Notification standardization
ecc-auto-notify-toggle:ecc-notification-toggle:Standardize naming
ecc-auto-notify-ring-bell:ecc-notification-ring-bell:Standardize naming

# Debug utilities
ecc-debug-utils-message:ecc-debug-message:Simplify naming
ecc-toggle-debug:ecc-debug-toggle-global:Consistent naming
EOF
    echo -e "${GREEN}Created example mapping file: $MAPPING_FILE.example${NC}"
    exit 1
fi

# Display configuration
echo -e "${GREEN}=== Refactor Rename Configuration ===${NC}"
echo "Target directory: $TARGET_DIR"
echo "Mapping file: $MAPPING_FILE"
echo "Rename script: $RENAME_SCRIPT"
if [ "$DRY_RUN" = true ]; then
    echo -e "${YELLOW}Mode: DRY RUN (no changes will be made)${NC}"
else
    echo -e "${RED}Mode: LIVE (changes will be applied!)${NC}"
fi
echo ""

# Function to perform rename
rename_function() {
    local old_name="$1"
    local new_name="$2"
    local reason="$3"
    
    echo -e "\n${YELLOW}Renaming: $old_name → $new_name${NC}"
    echo -e "${GREEN}Reason: $reason${NC}"
    
    if [ "$DRY_RUN" = true ]; then
        "$RENAME_SCRIPT" "$old_name" "$new_name" "$TARGET_DIR"
    else
        "$RENAME_SCRIPT" -n "$old_name" "$new_name" "$TARGET_DIR"
    fi
}

# Confirm before proceeding in live mode
if [ "$DRY_RUN" = false ]; then
    echo -e "${RED}WARNING: This will modify files in $TARGET_DIR${NC}"
    read -p "Are you sure you want to continue? (yes/no): " confirm
    if [ "$confirm" != "yes" ]; then
        echo "Aborted."
        exit 1
    fi
    
    # Create backup
    BACKUP_DIR="$TARGET_DIR/.backup-rename-$(date +%Y%m%d-%H%M%S)"
    echo -e "${YELLOW}Creating backup at $BACKUP_DIR${NC}"
    mkdir -p "$BACKUP_DIR"
    find "$TARGET_DIR" -type f \( -name "*.el" -o -name "*.py" -o -name "*.js" -o -name "*.ts" \) \
         -not -path "*/\.*" -exec cp --parents {} "$BACKUP_DIR" \;
fi

# Process mappings
echo -e "\n${GREEN}=== Processing Rename Mappings ===${NC}"

# Group mappings by category
declare -a phase1_mappings
declare -a phase2_mappings
declare -a phase3_mappings

while IFS=: read -r old_name new_name reason; do
    # Skip comments and empty lines
    [[ "$old_name" =~ ^#.*$ ]] && continue
    [[ -z "$old_name" ]] && continue
    
    # Categorize by pattern
    if [[ "$reason" =~ "Standardize" ]]; then
        phase1_mappings+=("$old_name:$new_name:$reason")
    elif [[ "$reason" =~ "descriptive" ]]; then
        phase2_mappings+=("$old_name:$new_name:$reason")
    else
        phase3_mappings+=("$old_name:$new_name:$reason")
    fi
done < "$MAPPING_FILE"

# Execute in phases
if [ ${#phase1_mappings[@]} -gt 0 ]; then
    echo -e "\n${GREEN}=== Phase 1: Standardization ===${NC}"
    for mapping in "${phase1_mappings[@]}"; do
        IFS=: read -r old_name new_name reason <<< "$mapping"
        rename_function "$old_name" "$new_name" "$reason"
    done
fi

if [ ${#phase2_mappings[@]} -gt 0 ]; then
    echo -e "\n${GREEN}=== Phase 2: Descriptive Names ===${NC}"
    for mapping in "${phase2_mappings[@]}"; do
        IFS=: read -r old_name new_name reason <<< "$mapping"
        rename_function "$old_name" "$new_name" "$reason"
    done
fi

if [ ${#phase3_mappings[@]} -gt 0 ]; then
    echo -e "\n${GREEN}=== Phase 3: Other Renamings ===${NC}"
    for mapping in "${phase3_mappings[@]}"; do
        IFS=: read -r old_name new_name reason <<< "$mapping"
        rename_function "$old_name" "$new_name" "$reason"
    done
fi

# Summary
echo -e "\n${GREEN}=== Refactoring Complete ===${NC}"

if [ "$DRY_RUN" = true ]; then
    echo -e "${YELLOW}This was a DRY RUN. No files were modified.${NC}"
    echo "To apply changes, run: DRY_RUN=false $0 $@"
else
    echo -e "${GREEN}Changes have been applied.${NC}"
    echo "Backup created at: $BACKUP_DIR"
    echo ""
    echo "Next steps:"
    echo "1. Review changes: git diff"
    echo "2. Run cleanup-duplicates.sh to remove obsolete code"
    echo "3. Run tests to ensure everything works"
    echo "4. If issues arise, restore from: $BACKUP_DIR"
fi

# EOF