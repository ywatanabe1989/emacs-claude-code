#!/bin/bash
# Test runner that mimics GitHub Actions environment using Docker

set -e

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
NC='\033[0m'

echo -e "${GREEN}Testing emacs-claude-code in Docker (GitHub Actions environment)${NC}"

# Function to test with a specific Emacs version
test_emacs_version() {
    local version=$1
    echo -e "\n${YELLOW}=== Testing with Emacs ${version} ===${NC}"
    
    docker run --rm \
        -v "$(pwd):/workspace" \
        -w /workspace \
        -e "EMACS_VERSION=${version}" \
        silex/emacs:${version} \
        bash -c "
            echo 'Emacs version:' && emacs --version | head -1
            echo 'Running tests...'
            ./run-tests.sh
        "
}

# Test with all versions used in CI
VERSIONS=(
    "27.1"
    "27.2"
    "28.1"
    "28.2"
    "29.1"
    "29.2"
    "29.3"
    "29.4"
)

# Check if specific version requested
if [ $# -eq 1 ]; then
    test_emacs_version "$1"
else
    # Test all versions
    for version in "${VERSIONS[@]}"; do
        if test_emacs_version "$version"; then
            echo -e "${GREEN}✓ Emacs ${version} passed${NC}"
        else
            echo -e "${RED}✗ Emacs ${version} failed${NC}"
        fi
    done
fi

echo -e "\n${GREEN}Docker testing complete!${NC}"