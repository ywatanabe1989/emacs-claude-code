#!/bin/bash
# Repository view script for testing
# Author: ywatanabe
# Date: 2025-05-14

# Default settings
DEPTH=3
N_HEAD=50
OUTPUT="viewed.md"
PATH_TO_REPO="."

# Parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --path)
      PATH_TO_REPO="$2"
      shift 2
      ;;
    --depth)
      DEPTH="$2"
      shift 2
      ;;
    --n-head)
      N_HEAD="$2"
      shift 2
      ;;
    --output)
      OUTPUT="$2"
      shift 2
      ;;
    *)
      echo "Unknown option: $1"
      exit 1
      ;;
  esac
done

# Create repository view with tree listing and file contents
{
  echo "# Repository View"
  echo "Generated on $(date)"
  echo ""
  echo "## Directory Structure"
  echo '```'
  find "$PATH_TO_REPO" -type f -not -path "*/\.*" | sort | head -n 50
  echo '```'
  echo ""
  echo "## File Contents"
  
  # List some files and their contents
  find "$PATH_TO_REPO" -type f -not -path "*/\.*" -not -path "*/node_modules/*" |
  sort |
  head -n 5 |
  while read -r file; do
    echo "### $file"
    echo '```'
    head -n "$N_HEAD" "$file"
    echo '```'
    echo ""
  done
} > "$OUTPUT"

echo "Repository view generated at $OUTPUT"
exit 0