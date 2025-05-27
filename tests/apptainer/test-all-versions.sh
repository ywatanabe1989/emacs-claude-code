#!/bin/bash
# Test with multiple Emacs versions using Apptainer
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
CACHE_DIR="${SCRIPT_DIR}/.apptainer-cache"

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Create cache directory
mkdir -p "${CACHE_DIR}"

# Check runner
if command -v apptainer &> /dev/null; then
    RUNNER="apptainer"
elif command -v singularity &> /dev/null; then
    RUNNER="singularity"
else
    echo -e "${RED}Error: Neither apptainer nor singularity found${NC}"
    exit 1
fi

# Emacs versions to test
VERSIONS=(
    "27"
    "28"
    "29"
)

echo -e "${BLUE}Testing with multiple Emacs versions${NC}"

PASSED=0
FAILED=0

for version in "${VERSIONS[@]}"; do
    echo -e "\n${YELLOW}=== Testing with Emacs ${version} ===${NC}"
    
    IMAGE_FILE="${CACHE_DIR}/emacs${version}.sif"
    
    # Pull image if not cached
    if [ ! -f "${IMAGE_FILE}" ]; then
        echo "Pulling silex/emacs:${version} image..."
        if ${RUNNER} pull "${IMAGE_FILE}" "docker://silex/emacs:${version}"; then
            echo "Image pulled successfully"
        else
            echo -e "${YELLOW}Warning: Could not pull Emacs ${version}, using default${NC}"
            IMAGE_FILE="${SCRIPT_DIR}/emacs-ci.sif"
        fi
    fi
    
    # Run tests
    if [ -f "${IMAGE_FILE}" ]; then
        if ${RUNNER} exec --bind "${PROJECT_ROOT}:/workspace" "${IMAGE_FILE}" \
            bash -c "cd /workspace && emacs --version && ./run-tests.sh"; then
            echo -e "${GREEN}✓ Emacs ${version} passed${NC}"
            ((PASSED++))
        else
            echo -e "${RED}✗ Emacs ${version} failed${NC}"
            ((FAILED++))
        fi
    else
        echo -e "${RED}✗ No image available for Emacs ${version}${NC}"
        ((FAILED++))
    fi
done

# Summary
echo -e "\n${BLUE}=== Test Summary ===${NC}"
echo -e "Passed: ${GREEN}${PASSED}${NC}"
echo -e "Failed: ${RED}${FAILED}${NC}"

if [[ ${FAILED} -eq 0 ]]; then
    echo -e "\n${GREEN}All versions passed!${NC}"
    exit 0
else
    echo -e "\n${RED}Some versions failed${NC}"
    exit 1
fi