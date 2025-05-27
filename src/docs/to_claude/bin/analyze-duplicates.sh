#!/bin/bash
# -*- coding: utf-8 -*-
# Timestamp: "2025-05-25 16:20:00 (ywatanabe)"
# File: ~/.claude/to_claude/bin/analyze-duplicates.sh
# Description: Analyze duplicate functions across multiple programming languages

# Configuration
SCRIPT_DIR="${1:-.}"  # Target directory (default: current)
LANG="${2:-auto}"     # Language: auto, elisp, python, js, etc.

# Colors
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m'

# Function to display usage
show_usage() {
    echo "Usage: $0 [directory] [language]"
    echo ""
    echo "Arguments:"
    echo "  directory    Target directory (default: current directory)"
    echo "  language     Programming language: auto, elisp, python, js (default: auto)"
    echo ""
    echo "Examples:"
    echo "  $0                    # Analyze current directory, auto-detect language"
    echo "  $0 ./src elisp        # Analyze ./src for Elisp files"
    echo "  $0 /path/to/project python  # Analyze Python project"
}

# Parse arguments
if [[ "$1" == "-h" ]] || [[ "$1" == "--help" ]]; then
    show_usage
    exit 0
fi

echo -e "${GREEN}=== Analyzing Duplicate Functions ===${NC}"
echo -e "Directory: $SCRIPT_DIR"
echo -e "Language: $LANG\n"

# Function to detect language if auto
detect_language() {
    if [ "$LANG" = "auto" ]; then
        if find "$SCRIPT_DIR" -name "*.el" -type f | head -1 | grep -q .; then
            LANG="elisp"
        elif find "$SCRIPT_DIR" -name "*.py" -type f | head -1 | grep -q .; then
            LANG="python"
        elif find "$SCRIPT_DIR" -name "*.js" -o -name "*.ts" -type f | head -1 | grep -q .; then
            LANG="js"
        else
            echo -e "${RED}Could not auto-detect language${NC}"
            exit 1
        fi
        echo -e "${YELLOW}Auto-detected language: $LANG${NC}\n"
    fi
}

# Language-specific patterns
get_function_pattern() {
    case "$LANG" in
        elisp)
            echo "(defun\s+\([^[:space:]]+\)"
            ;;
        python)
            echo "^\s*def\s+\([^([:space:]]+\)"
            ;;
        js|javascript|typescript)
            echo "^\s*\(function\s+\([^([:space:]]+\)\|const\s+\([^[:space:]]+\)\s*=\s*\(async\s*\)\?function\)"
            ;;
        *)
            echo -e "${RED}Unsupported language: $LANG${NC}"
            exit 1
            ;;
    esac
}

get_alias_pattern() {
    case "$LANG" in
        elisp)
            echo "(defalias\s+'\\([^']+\\)"
            ;;
        python)
            echo "^\s*\([^[:space:]]+\)\s*=\s*\([^[:space:]]+\)\s*#.*alias"
            ;;
        js|javascript|typescript)
            echo "^\s*export\s*{\s*\([^[:space:]]+\)\s*as\s*\([^[:space:]]+\)"
            ;;
    esac
}

get_file_extensions() {
    case "$LANG" in
        elisp)
            echo "*.el"
            ;;
        python)
            echo "*.py"
            ;;
        js|javascript|typescript)
            echo "*.js *.jsx *.ts *.tsx"
            ;;
    esac
}

# Detect language if needed
detect_language

# Get patterns
FUNC_PATTERN=$(get_function_pattern)
ALIAS_PATTERN=$(get_alias_pattern)
FILE_EXTS=$(get_file_extensions)

# Find duplicate function definitions
echo -e "${YELLOW}1. Finding duplicate function definitions...${NC}"
declare -A func_locations

for ext in $FILE_EXTS; do
    find "$SCRIPT_DIR" -name "$ext" -type f -not -path "*/\.*" | while read -r file; do
        # Extract function names based on language
        case "$LANG" in
            elisp)
                grep -n "$FUNC_PATTERN" "$file" | sed -n 's/.*(\s*defun\s\+\([^[:space:]]+\).*/\1/p' | while read -r func; do
                    line=$(grep -n "(defun $func" "$file" | cut -d: -f1 | head -1)
                    if [ -n "$func" ]; then
                        if [ -n "${func_locations[$func]}" ]; then
                            echo -e "${RED}Duplicate found: $func${NC}"
                            echo -e "  First: ${func_locations[$func]}"
                            echo -e "  Also:  $file:$line"
                        else
                            func_locations[$func]="$file:$line"
                        fi
                    fi
                done
                ;;
            python)
                grep -n "$FUNC_PATTERN" "$file" | sed -n 's/.*def\s\+\([^([:space:]]+\).*/\1/p' | while read -r func; do
                    line=$(grep -n "def $func" "$file" | cut -d: -f1 | head -1)
                    if [ -n "$func" ]; then
                        if [ -n "${func_locations[$func]}" ]; then
                            echo -e "${RED}Duplicate found: $func${NC}"
                            echo -e "  First: ${func_locations[$func]}"
                            echo -e "  Also:  $file:$line"
                        else
                            func_locations[$func]="$file:$line"
                        fi
                    fi
                done
                ;;
            js|javascript|typescript)
                # Handle both function declarations and const arrow functions
                grep -nE "(function\s+[^([:space:]]+|const\s+[^[:space:]]+\s*=\s*(async\s*)?function)" "$file" | while read -r match; do
                    line=$(echo "$match" | cut -d: -f1)
                    func=$(echo "$match" | sed -n 's/.*\(function\s\+\([^([:space:]]+\)\|const\s\+\([^[:space:]]+\)\s*=\).*/\2\3/p')
                    if [ -n "$func" ]; then
                        if [ -n "${func_locations[$func]}" ]; then
                            echo -e "${RED}Duplicate found: $func${NC}"
                            echo -e "  First: ${func_locations[$func]}"
                            echo -e "  Also:  $file:$line"
                        else
                            func_locations[$func]="$file:$line"
                        fi
                    fi
                done
                ;;
        esac
    done
done

# Find aliases
echo -e "\n${YELLOW}2. Finding function aliases...${NC}"
for ext in $FILE_EXTS; do
    find "$SCRIPT_DIR" -name "$ext" -type f -not -path "*/\.*" | while read -r file; do
        case "$LANG" in
            elisp)
                grep -n "$ALIAS_PATTERN" "$file" | while read -r match; do
                    echo -e "${BLUE}$file${NC}: $match"
                done
                ;;
            python)
                # Look for alias patterns in Python
                grep -n "=.*#.*alias" "$file" | while read -r match; do
                    echo -e "${BLUE}$file${NC}: $match"
                done
                ;;
            js|javascript|typescript)
                # Look for export aliases
                grep -nE "export\s*{.*as.*}" "$file" | while read -r match; do
                    echo -e "${BLUE}$file${NC}: $match"
                done
                ;;
        esac
    done
done

# Find inconsistent naming patterns
echo -e "\n${YELLOW}3. Finding inconsistent naming patterns...${NC}"
case "$LANG" in
    elisp)
        echo "Looking for mix of - and _ in function names:"
        find "$SCRIPT_DIR" -name "*.el" -type f -exec grep -l "defun.*_" {} \; | while read -r file; do
            echo -e "${BLUE}$file${NC} contains underscore in function names (should use hyphens)"
        done
        ;;
    python)
        echo "Looking for camelCase in function names (should be snake_case):"
        find "$SCRIPT_DIR" -name "*.py" -type f -exec grep -l "def.*[a-z][A-Z]" {} \; | while read -r file; do
            echo -e "${BLUE}$file${NC} may contain camelCase function names"
        done
        ;;
    js|javascript|typescript)
        echo "Looking for snake_case in function names (should be camelCase):"
        find "$SCRIPT_DIR" -name "*.js" -name "*.ts" -type f -exec grep -l "function.*_" {} \; | while read -r file; do
            echo -e "${BLUE}$file${NC} may contain snake_case function names"
        done
        ;;
esac

echo -e "\n${GREEN}=== Analysis Complete ===${NC}"
echo "Next steps:"
echo "1. Review the duplicates and decide which to keep"
echo "2. Create a refactoring plan with replacement mappings"
echo "3. Run refactor-rename.sh to apply changes"

# EOF