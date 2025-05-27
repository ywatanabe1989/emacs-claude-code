#!/bin/bash
# Run CI tests using Apptainer
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
IMAGE_NAME="${SCRIPT_DIR}/emacs-ci.sif"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
NC='\033[0m'

# Check if apptainer/singularity is available
if command -v apptainer &> /dev/null; then
    RUNNER="apptainer"
elif command -v singularity &> /dev/null; then
    RUNNER="singularity"
else
    echo -e "${RED}Error: Neither apptainer nor singularity found${NC}"
    exit 1
fi

# Check if image exists
if [ ! -f "${IMAGE_NAME}" ]; then
    echo -e "${YELLOW}Image not found. Building...${NC}"
    "${SCRIPT_DIR}/build-image.sh"
fi

echo -e "${YELLOW}Running CI tests with Apptainer...${NC}"

# Run tests
if ${RUNNER} run --bind "${PROJECT_ROOT}:/workspace" "${IMAGE_NAME}"; then
    echo -e "\n${GREEN}✓ CI tests passed!${NC}"
    exit 0
else
    echo -e "\n${RED}✗ CI tests failed${NC}"
    exit 1
fi