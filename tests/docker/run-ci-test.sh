#!/bin/bash
# Run CI tests in Docker environment
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
NC='\033[0m'

echo -e "${YELLOW}Running CI tests in Docker...${NC}"

# Build Docker image
echo "Building Docker image..."
docker build -f "${SCRIPT_DIR}/Dockerfile.ci" -t emacs-claude-code-ci "${PROJECT_ROOT}"

# Run tests
echo -e "\n${YELLOW}Running tests...${NC}"
if docker run --rm emacs-claude-code-ci; then
    echo -e "\n${GREEN}✓ CI tests passed!${NC}"
    exit 0
else
    echo -e "\n${RED}✗ CI tests failed${NC}"
    exit 1
fi