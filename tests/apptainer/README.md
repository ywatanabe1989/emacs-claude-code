# Apptainer/Singularity Testing for emacs-claude-code

This directory contains Apptainer (formerly Singularity) configurations for testing in HPC environments where Docker is not available.

## Quick Start

```bash
# Build the Apptainer image
./build-image.sh

# Run tests with default Emacs
./run-ci-test.sh

# Test with specific Emacs version
./run-ci-test.sh 28.2

# Test with all versions
./test-all-versions.sh
```

## Files

- `emacs-ci.def` - Apptainer definition file for CI environment
- `build-image.sh` - Builds the Apptainer image
- `run-ci-test.sh` - Runs tests in Apptainer container
- `test-all-versions.sh` - Tests multiple Emacs versions
- `.apptainer-cache/` - Cache directory for images (git-ignored)

## Why Apptainer?

1. **HPC Compatible** - Works on systems where Docker requires root
2. **Same behavior as CI** - Mimics GitHub Actions environment
3. **Reproducible** - Consistent testing environment
4. **No root required** - Can build and run as regular user

## Prerequisites

```bash
# Check if Apptainer is installed
apptainer --version

# If not installed, see: https://apptainer.org/docs/user/latest/quick_start.html
```

## Building Images

The first run will download and convert Docker images, which may take time:

```bash
# Build from definition file
apptainer build emacs-ci.sif emacs-ci.def

# Or pull from Docker Hub
apptainer pull docker://ubuntu:22.04
```

## Usage Examples

```bash
# Interactive shell for debugging
apptainer shell emacs-ci.sif

# Run specific command
apptainer exec emacs-ci.sif emacs --version

# Run with bind mount
apptainer run --bind $(pwd):/workspace emacs-ci.sif
```