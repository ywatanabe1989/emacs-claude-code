#!/bin/bash
# Run GitHub Actions locally using act
# Usage: ./tests/github_actions/run_ci_local.sh [workflow-name]

set -e

# Get project root (two levels up from this script)
PROJECT_ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$PROJECT_ROOT"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${GREEN}🎬 GitHub Actions Local Runner (act)${NC}"
echo "Project Root: $PROJECT_ROOT"
echo

# Check if act is installed
if ! command -v act &> /dev/null; then
    echo -e "${RED}❌ act is not installed${NC}"
    echo "Run: ./tests/github_actions/install_act.sh to install it"
    exit 1
fi

# Check if .github/workflows exists
if [ ! -d ".github/workflows" ]; then
    echo -e "${YELLOW}⚠️  No .github/workflows directory found${NC}"
    echo "This project doesn't have GitHub Actions workflows configured."
    exit 1
fi

# List available workflows
echo -e "${BLUE}📋 Available workflows:${NC}"
find .github/workflows -name "*.yml" -o -name "*.yaml" | while read -r workflow; do
    name=$(basename "$workflow" | sed 's/\.[^.]*$//')
    echo "  - $name ($workflow)"
done
echo

# Run specific workflow if provided, otherwise run all
if [ -n "$1" ]; then
    WORKFLOW_FILE=".github/workflows/$1.yml"
    if [ ! -f "$WORKFLOW_FILE" ]; then
        WORKFLOW_FILE=".github/workflows/$1.yaml" 
    fi
    
    if [ ! -f "$WORKFLOW_FILE" ]; then
        echo -e "${RED}❌ Workflow '$1' not found${NC}"
        exit 1
    fi
    
    echo -e "${GREEN}🚀 Running workflow: $1${NC}"
    act -W "$WORKFLOW_FILE" --env-file="" "$@"
else
    echo -e "${GREEN}🚀 Running all workflows...${NC}"
    act --env-file="" "$@"
fi