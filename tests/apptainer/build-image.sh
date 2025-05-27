#!/bin/bash
# Build Apptainer image for CI testing
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
IMAGE_NAME="emacs-ci.sif"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
NC='\033[0m'

echo -e "${YELLOW}Building Apptainer image for CI testing...${NC}"

# Check if apptainer is available
if ! command -v apptainer &> /dev/null; then
    # Try singularity as fallback
    if command -v singularity &> /dev/null; then
        echo "Using singularity instead of apptainer..."
        BUILDER="singularity"
    else
        echo -e "${RED}Error: Neither apptainer nor singularity found${NC}"
        echo "Please install Apptainer: https://apptainer.org/docs/user/latest/quick_start.html"
        exit 1
    fi
else
    BUILDER="apptainer"
fi

# Build the image
cd "${SCRIPT_DIR}"
if ${BUILDER} build --fakeroot "${IMAGE_NAME}" emacs-ci.def; then
    echo -e "${GREEN}✓ Successfully built ${IMAGE_NAME}${NC}"
    echo -e "Image size: $(ls -lh ${IMAGE_NAME} | awk '{print $5}')"
else
    echo -e "${RED}✗ Failed to build image${NC}"
    exit 1
fi

echo -e "\n${GREEN}Image ready! Run tests with:${NC}"
echo "  ./run-ci-test.sh"