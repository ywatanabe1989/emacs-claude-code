#!/bin/bash
# -*- coding: utf-8 -*-
# Timestamp: "2025-05-25 16:30:00 (ywatanabe)"
# File: ~/.claude/to_claude/bin/cleanup-duplicates.sh
# Description: Remove duplicate definitions and obsolete code after renaming

# Configuration
TARGET_DIR="${1:-.}"
CLEANUP_PLAN="${2:-cleanup-plan.txt}"
DRY_RUN=${DRY_RUN:-true}

# Colors
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m'

# Function to display usage
show_usage() {
    echo "Usage: $0 [target_directory] [cleanup_plan_file]"
    echo ""
    echo "Environment variables:"
    echo "  DRY_RUN=true|false    Whether to perform dry run (default: true)"
    echo ""
    echo "Cleanup plan format:"
    echo "  action:file:pattern:description"
    echo ""
    echo "Actions:"
    echo "  remove-function: Remove entire function definition"
    echo "  remove-alias: Remove defalias statement"
    echo "  remove-lines: Remove specific line range"
    echo ""
    echo "Examples:"
    echo "  $0                    # Use current dir and cleanup-plan.txt"
    echo "  DRY_RUN=false $0      # Actually perform cleanup"
}

# Parse arguments
if [[ "$1" == "-h" ]] || [[ "$1" == "--help" ]]; then
    show_usage
    exit 0
fi

# Create example cleanup plan if not exists
if [ ! -f "$CLEANUP_PLAN" ]; then
    echo -e "${YELLOW}Creating example cleanup plan: $CLEANUP_PLAN${NC}"
    cat > "$CLEANUP_PLAN" << 'EOF'
# Cleanup plan format: action:file:pattern:description
# Actions: remove-function, remove-alias, remove-lines

# Remove duplicate function definitions
remove-function:src/ecc-api.el:ecc-auto-response-send:Keep version in ecc-auto-response.el
remove-function:src/ecc-term-claude-mode.el:ecc-term-claude-check-state:Keep version in ecc-term-claude-state.el

# Remove obsolete aliases
remove-alias:src/ecc-api.el:ecc-auto-start:Replaced with ecc-auto-response-enable
remove-alias:src/ecc-api.el:ecc-auto-stop:Replaced with ecc-auto-response-disable
remove-alias:src/ecc-api.el:ecc-yes:Replaced with ecc-auto-response-yes

# Remove specific line ranges
remove-lines:src/ecc-buffer-state.el:184-200:First duplicate of ecc-buffer-state-get
EOF
    echo "Please edit the cleanup plan and run again."
    exit 1
fi

# Display configuration
echo -e "${GREEN}=== Cleanup Duplicates Configuration ===${NC}"
echo "Target directory: $TARGET_DIR"
echo "Cleanup plan: $CLEANUP_PLAN"
if [ "$DRY_RUN" = true ]; then
    echo -e "${YELLOW}Mode: DRY RUN (no changes will be made)${NC}"
else
    echo -e "${RED}Mode: LIVE (changes will be applied!)${NC}"
fi
echo ""

# Backup if in live mode
if [ "$DRY_RUN" = false ]; then
    BACKUP_DIR="$TARGET_DIR/.backup-cleanup-$(date +%Y%m%d-%H%M%S)"
    echo -e "${YELLOW}Creating backup at $BACKUP_DIR${NC}"
    mkdir -p "$BACKUP_DIR"
    
    # Backup files mentioned in cleanup plan
    while IFS=: read -r action file pattern description; do
        [[ "$action" =~ ^#.*$ ]] && continue
        [[ -z "$action" ]] && continue
        
        if [ -f "$TARGET_DIR/$file" ]; then
            mkdir -p "$BACKUP_DIR/$(dirname "$file")"
            cp "$TARGET_DIR/$file" "$BACKUP_DIR/$file"
        fi
    done < "$CLEANUP_PLAN"
    
    echo -e "${RED}WARNING: This will modify files in $TARGET_DIR${NC}"
    read -p "Are you sure you want to continue? (yes/no): " confirm
    if [ "$confirm" != "yes" ]; then
        echo "Aborted."
        exit 1
    fi
fi

# Function to find function end in various languages
find_function_end() {
    local file="$1"
    local func_name="$2"
    local start_line
    
    # Detect language by extension
    case "$file" in
        *.el)
            # Emacs Lisp - find matching parentheses
            start_line=$(grep -n "(defun $func_name" "$file" | head -1 | cut -d: -f1)
            if [ -n "$start_line" ]; then
                awk -v start="$start_line" '
                    NR >= start {
                        paren_count += gsub(/\(/, "(", $0) - gsub(/\)/, ")", $0)
                        if (paren_count == 0 && NR > start) {
                            print NR
                            exit
                        }
                    }
                ' "$file"
            fi
            ;;
        *.py)
            # Python - find next function or class at same indent level
            start_line=$(grep -n "^def $func_name" "$file" | head -1 | cut -d: -f1)
            if [ -n "$start_line" ]; then
                awk -v start="$start_line" '
                    NR == start { indent = match($0, /[^ ]/) }
                    NR > start && /^[^ ]/ { print NR-1; exit }
                    END { print NR }
                ' "$file"
            fi
            ;;
        *.js|*.ts)
            # JavaScript/TypeScript - find matching braces
            start_line=$(grep -n "function $func_name\|const $func_name" "$file" | head -1 | cut -d: -f1)
            if [ -n "$start_line" ]; then
                awk -v start="$start_line" '
                    NR >= start {
                        brace_count += gsub(/{/, "{", $0) - gsub(/}/, "}", $0)
                        if (brace_count == 0 && NR > start) {
                            print NR
                            exit
                        }
                    }
                ' "$file"
            fi
            ;;
    esac
}

# Function to remove function definition
remove_function() {
    local file="$1"
    local func_name="$2"
    local description="$3"
    
    echo -e "\n${YELLOW}Removing function: $func_name from $file${NC}"
    echo -e "${BLUE}Reason: $description${NC}"
    
    if [ ! -f "$file" ]; then
        echo -e "${RED}File not found: $file${NC}"
        return 1
    fi
    
    # Find function boundaries
    local start_line=$(grep -n "(defun $func_name\|^def $func_name\|function $func_name" "$file" | head -1 | cut -d: -f1)
    if [ -z "$start_line" ]; then
        echo -e "${RED}Function not found: $func_name${NC}"
        return 1
    fi
    
    local end_line=$(find_function_end "$file" "$func_name")
    if [ -z "$end_line" ]; then
        echo -e "${RED}Could not find function end${NC}"
        return 1
    fi
    
    echo "  Lines to remove: $start_line-$end_line"
    
    if [ "$DRY_RUN" = false ]; then
        sed -i "${start_line},${end_line}d" "$file"
        echo -e "${GREEN}Removed function${NC}"
    fi
}

# Function to remove alias
remove_alias() {
    local file="$1"
    local alias_name="$2"
    local description="$3"
    
    echo -e "\n${YELLOW}Removing alias: $alias_name from $file${NC}"
    echo -e "${BLUE}Reason: $description${NC}"
    
    if [ ! -f "$file" ]; then
        echo -e "${RED}File not found: $file${NC}"
        return 1
    fi
    
    # Remove defalias and any comment line immediately before it
    if [ "$DRY_RUN" = true ]; then
        grep -n "defalias.*$alias_name\|;;.*$alias_name" "$file" || echo "Alias not found"
    else
        sed -i "/^[[:space:]]*;;.*$alias_name/d; /^[[:space:]]*(defalias[[:space:]]'$alias_name/d" "$file"
        echo -e "${GREEN}Removed alias${NC}"
    fi
}

# Function to remove specific lines
remove_lines() {
    local file="$1"
    local range="$2"
    local description="$3"
    
    echo -e "\n${YELLOW}Removing lines: $range from $file${NC}"
    echo -e "${BLUE}Reason: $description${NC}"
    
    if [ ! -f "$file" ]; then
        echo -e "${RED}File not found: $file${NC}"
        return 1
    fi
    
    if [ "$DRY_RUN" = true ]; then
        local start_line=$(echo "$range" | cut -d- -f1)
        local end_line=$(echo "$range" | cut -d- -f2)
        sed -n "${start_line},${end_line}p" "$file" | head -10
        echo "... (showing first 10 lines)"
    else
        sed -i "${range}d" "$file"
        echo -e "${GREEN}Removed lines${NC}"
    fi
}

# Process cleanup plan
echo -e "\n${GREEN}=== Processing Cleanup Plan ===${NC}"

while IFS=: read -r action file pattern description; do
    # Skip comments and empty lines
    [[ "$action" =~ ^#.*$ ]] && continue
    [[ -z "$action" ]] && continue
    
    # Trim whitespace
    action=$(echo "$action" | xargs)
    file=$(echo "$file" | xargs)
    pattern=$(echo "$pattern" | xargs)
    
    # Resolve full path
    full_path="$TARGET_DIR/$file"
    
    case "$action" in
        remove-function)
            remove_function "$full_path" "$pattern" "$description"
            ;;
        remove-alias)
            remove_alias "$full_path" "$pattern" "$description"
            ;;
        remove-lines)
            remove_lines "$full_path" "$pattern" "$description"
            ;;
        *)
            echo -e "${RED}Unknown action: $action${NC}"
            ;;
    esac
done < "$CLEANUP_PLAN"

# Summary
echo -e "\n${GREEN}=== Cleanup Complete ===${NC}"

if [ "$DRY_RUN" = true ]; then
    echo -e "${YELLOW}This was a DRY RUN. No files were modified.${NC}"
    echo "To apply changes, run: DRY_RUN=false $0 $@"
else
    echo -e "${GREEN}Changes have been applied.${NC}"
    echo "Backup created at: $BACKUP_DIR"
    echo ""
    echo "Next steps:"
    echo "1. Review changes: git diff"
    echo "2. Run tests to ensure everything works"
    echo "3. Update documentation if needed"
    echo "4. If issues arise, restore from: $BACKUP_DIR"
fi

# Generate summary report
echo -e "\n${GREEN}=== Generating Summary Report ===${NC}"
REPORT_FILE="$TARGET_DIR/cleanup-summary-$(date +%Y%m%d-%H%M%S).md"

cat > "$REPORT_FILE" << EOF
# Cleanup Summary Report

Generated: $(date)

## Configuration
- Target Directory: $TARGET_DIR
- Cleanup Plan: $CLEANUP_PLAN
- Mode: $([ "$DRY_RUN" = true ] && echo "DRY RUN" || echo "LIVE")

## Actions Performed

EOF

while IFS=: read -r action file pattern description; do
    [[ "$action" =~ ^#.*$ ]] && continue
    [[ -z "$action" ]] && continue
    echo "- **$action**: $pattern in $file - $description" >> "$REPORT_FILE"
done < "$CLEANUP_PLAN"

echo -e "\nSummary report saved to: $REPORT_FILE"

# EOF