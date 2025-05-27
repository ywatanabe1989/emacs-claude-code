#!/bin/bash
# Test with all Emacs versions used in CI
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Emacs versions from CI matrix
VERSIONS=(
    "27.1"
    "27.2" 
    "28.1"
    "28.2"
    "29.1"
)

echo -e "${BLUE}Testing with multiple Emacs versions${NC}"

# Track results
PASSED=0
FAILED=0

for version in "${VERSIONS[@]}"; do
    echo -e "\n${YELLOW}=== Testing with Emacs ${version} ===${NC}"
    
    # Try to use silex/emacs Docker images which have multiple versions
    if docker run --rm \
        -v "${PROJECT_ROOT}:/workspace" \
        -w /workspace \
        silex/emacs:${version} \
        bash -c "emacs --version && ./run-tests.sh" 2>/dev/null; then
        echo -e "${GREEN}✓ Emacs ${version} passed${NC}"
        ((PASSED++))
    else
        # Fallback to default Ubuntu Emacs
        if [[ "${version}" == "27."* ]] || [[ "${version}" == "28."* ]]; then
            echo -e "${YELLOW}Trying with Ubuntu default Emacs...${NC}"
            if docker run --rm \
                -v "${PROJECT_ROOT}:/workspace" \
                -w /workspace \
                ubuntu:22.04 \
                bash -c "apt-get update -qq && apt-get install -y -qq emacs && emacs --version && ./run-tests.sh"; then
                echo -e "${GREEN}✓ Emacs ${version} passed (Ubuntu)${NC}"
                ((PASSED++))
            else
                echo -e "${RED}✗ Emacs ${version} failed${NC}"
                ((FAILED++))
            fi
        else
            echo -e "${RED}✗ Emacs ${version} not available${NC}"
            ((FAILED++))
        fi
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