#!/bin/bash
# CI Test Runner using Docker
# Mimics GitHub Actions environment for local debugging

set -e

PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_NAME="emacs-claude-code"

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}=== CI Test Runner (Docker) ===${NC}"
echo -e "Project: ${PROJECT_NAME}"
echo -e "Directory: ${PROJECT_DIR}"

# Create a temporary Dockerfile for testing
cat > "${PROJECT_DIR}/.ci-test.dockerfile" << 'EOF'
FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive

# Install basic packages
RUN apt-get update && apt-get install -y \
    emacs \
    git \
    curl \
    make \
    && rm -rf /var/lib/apt/lists/*

# Create runner user (like GitHub Actions)
RUN useradd -m runner
USER runner

# Set working directory
WORKDIR /home/runner/work/project

# Copy project files
COPY --chown=runner:runner . .

# Ensure scripts are executable
RUN chmod +x run-tests.sh run_tests_elisp.sh || true

# Run tests
CMD ["./run-tests.sh"]
EOF

# Function to run tests
run_docker_test() {
    echo -e "\n${YELLOW}Building Docker image...${NC}"
    docker build -f .ci-test.dockerfile -t ${PROJECT_NAME}-ci-test . 2>&1 | tail -20
    
    echo -e "\n${YELLOW}Running tests in Docker...${NC}"
    if docker run --rm ${PROJECT_NAME}-ci-test; then
        echo -e "${GREEN}✓ Tests passed in Docker${NC}"
        return 0
    else
        echo -e "${RED}✗ Tests failed in Docker${NC}"
        return 1
    fi
}

# Main execution
if run_docker_test; then
    echo -e "\n${GREEN}CI simulation successful!${NC}"
    EXIT_CODE=0
else
    echo -e "\n${RED}CI simulation failed!${NC}"
    EXIT_CODE=1
fi

# Cleanup
rm -f "${PROJECT_DIR}/.ci-test.dockerfile"

# Show next steps
echo -e "\n${BLUE}Next steps:${NC}"
echo "1. Check the test output above"
echo "2. Fix any failing tests locally"
echo "3. Push changes to trigger real CI"

exit $EXIT_CODE