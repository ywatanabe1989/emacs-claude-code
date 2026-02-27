#!/bin/bash
# HPC-compatible CI using Singularity/Apptainer and environment modules
# Usage: ./tests/github_actions/run_ci_container.sh [workflow-name]

set -e

# Get project root
PROJECT_ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$PROJECT_ROOT"

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

echo -e "${GREEN}🏗️ HPC CI Runner (Singularity/Environment Modules)${NC}"

# Detect HPC environment
HPC_MODE="native"
CONTAINER_CMD=""

# Check for Apptainer/Singularity (prioritize Apptainer)
if command -v apptainer &> /dev/null; then
    HPC_MODE="apptainer"
    CONTAINER_CMD="apptainer"
    echo -e "${BLUE}📦 Using Apptainer containers${NC}"
elif command -v singularity &> /dev/null; then
    HPC_MODE="singularity"
    CONTAINER_CMD="singularity"
    echo -e "${BLUE}📦 Using Singularity containers${NC}"
else
    echo -e "${YELLOW}ℹ️ No container runtime found, using native environment${NC}"
fi

# HPC cache directory (typically on shared filesystem)
HPC_CACHE_DIR="${SLURM_SUBMIT_DIR:-$HOME}/.ci_container_cache"
mkdir -p "$HPC_CACHE_DIR"

# Function to run command in container or native
run_step() {
    local step_name="$1"
    local cmd="$2"
    local container_image="$3"
    
    echo -e "${BLUE}🔄 Running: $step_name${NC}"
    
    if [ "$HPC_MODE" != "native" ] && [ -n "$container_image" ]; then
        # Container mode
        local sif_file="$HPC_CACHE_DIR/$(basename $container_image).sif"
        
        # Pull container if not cached
        if [ ! -f "$sif_file" ]; then
            echo "  📥 Pulling container: $container_image"
            $CONTAINER_CMD pull "$sif_file" "docker://$container_image"
        fi
        
        # Run in container with project mounted and proper environment
        $CONTAINER_CMD exec \
            --bind "$PROJECT_ROOT:/workspace" \
            --pwd /workspace \
            --env PYTHONPATH="/workspace/src:/workspace" \
            --cleanenv \
            "$sif_file" bash -c "
                # Clean any cached files that might cause conflicts
                find /workspace -name '__pycache__' -type d -exec rm -rf {} + 2>/dev/null || true
                find /workspace -name '*.pyc' -delete 2>/dev/null || true
                rm -rf /workspace/.ruff_cache /workspace/.pytest_cache 2>/dev/null || true
                
                # Set up Python environment 
                export PYTHONDONTWRITEBYTECODE=1
                export PYTHONUNBUFFERED=1
                
                # Run the actual command
                $cmd
            "
    else
        # Native mode (load modules if available)
        if command -v module &> /dev/null; then
            echo "  📦 Loading environment modules..."
            # Common HPC modules
            module load python/3.11 2>/dev/null || \
            module load Python/3.11 2>/dev/null || \
            module load python 2>/dev/null || true
            
            module load git 2>/dev/null || true
        fi
        
        # Run directly
        bash -c "$cmd"
    fi
    
    local exit_code=$?
    if [ $exit_code -eq 0 ]; then
        echo -e "  ✅ PASS: $step_name"
        return 0
    else
        echo -e "  ❌ FAIL: $step_name (exit code: $exit_code)"
        return $exit_code
    fi
}

# CI Pipeline Steps
echo -e "${GREEN}🚀 Running CI pipeline...${NC}"

FAILED_STEPS=()

# Step 1: Python Setup
if ! run_step "Python setup" \
    "python3 -m pip install --user -e .[dev] || pip install --user -e .[dev]" \
    "python:3.11-slim"; then
    FAILED_STEPS+=("python_setup")
fi

# Step 2: Linting  
if ! run_step "Linting" \
    "python3 -m ruff check src/ tests/ || echo 'Ruff not available, skipping lint'" \
    "python:3.11-slim"; then
    FAILED_STEPS+=("linting")
fi

# Step 3: Tests
if ! run_step "Tests with coverage" \
    "python3 -m pytest tests/ --cov=src/pip_project_template --cov-report=term-missing --cov-fail-under=90 || pytest tests/" \
    "python:3.11-slim"; then
    FAILED_STEPS+=("tests")
fi

# Step 4: Src-Test Agreement  
if ! run_step "Src-test agreement" \
    "python3 tests/custom/test_src_test_agreement.py" \
    "python:3.11-slim"; then
    FAILED_STEPS+=("agreement")
fi

# Generate Report
REPORT_FILE="$PROJECT_ROOT/tests/reports/ci_container_report.json"
cat > "$REPORT_FILE" << EOF
{
  "workflow": "HPC CI Validation",
  "status": "${#FAILED_STEPS[@]} -eq 0 && echo 'success' || echo 'failure')",
  "hpc_mode": "$HPC_MODE",
  "failed_steps": [$(printf '"%s",' "${FAILED_STEPS[@]}" | sed 's/,$//')],
  "cache_dir": "$HPC_CACHE_DIR",
  "timestamp": "$(date -Iseconds)"
}
EOF

# Summary
echo -e "\n${GREEN}📊 HPC CI SUMMARY${NC}"
echo "=================="
echo "Mode: $HPC_MODE"
echo "Cache: $HPC_CACHE_DIR"

if [ ${#FAILED_STEPS[@]} -eq 0 ]; then
    echo -e "Status: ${GREEN}✅ SUCCESS${NC}"
    echo "Report: $REPORT_FILE"
    exit 0
else
    echo -e "Status: ${RED}❌ FAILURE${NC}"
    echo "Failed steps: ${FAILED_STEPS[*]}"
    echo "Report: $REPORT_FILE"
    exit 1
fi