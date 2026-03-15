# Local GitHub Actions with `act` + Singularity/Apptainer Support

This directory contains tools for running GitHub Actions workflows locally using [`act`](https://github.com/nektos/act) with **full Singularity/Apptainer support for HPC environments**.

## 🎉 Major Achievement: Act + Singularity Integration Working!

We have successfully implemented a working solution for running GitHub Actions locally on HPC systems using Singularity/Apptainer, addressing the limitations in [nektos/act#1125](https://github.com/nektos/act/issues/1125).

## Why `act` + Our Singularity Solution?

Unlike Docker-only solutions, our implementation provides:
- ✅ **HPC compatibility** - Works on systems that prohibit Docker
- ✅ **Singularity/Apptainer support** - Full container isolation without admin rights
- ✅ **Universal compatibility** - Works with ANY project that has GitHub Actions
- ✅ **True fidelity** - Runs your actual `.github/workflows/*.yml` files
- ✅ **Auto-sync** - Always matches your workflow exactly
- ✅ **Zero maintenance** - No code to keep in sync
- ✅ **Industry standard** - Based on widely used `act` tool

## Setup

### For HPC/Singularity Users (No Docker Required!)

```bash
# No installation needed! Just run:
make act
# Or directly:
./tests/github_actions/run_ci_act_and_container.sh
```

The script automatically:
- Detects Apptainer/Singularity availability
- Downloads GitHub Actions runner images as SIF files
- Caches containers in `~/.act_singularity_cache/`
- Executes workflows in Singularity containers

### For Docker Users

```bash
# Install act (only if using Docker mode)
./tests/github_actions/install_act.sh

# Verify installation
act --version
```

## Usage

### 🚀 Option 1: Singularity/Apptainer Mode (HPC-Friendly)
```bash
# Run GitHub Actions with Singularity (RECOMMENDED for HPC)
make act

# Or directly:
./tests/github_actions/run_ci_act_and_container.sh

# What happens:
# 1. Auto-detects: Apptainer → Singularity → Docker
# 2. Downloads catthehacker/ubuntu:act-latest as SIF
# 3. Runs complete GitHub Actions workflow
# 4. ~10-30 seconds after initial setup
```

### Option 2: HPC Direct CI (Simplified)
```bash
# Direct container execution without workflow parsing
make ci-container
# Or:
./tests/github_actions/run_ci_container.sh
```

### Option 3: Python-Based Local CI
```bash
# Custom CI emulator with container detection
make ci-local
# Or:
./tests/github_actions/run_ci_local.sh
```

### Option 4: Docker Mode (Traditional)
```bash
# Standard act with Docker (requires Docker daemon)
make ci-docker
# Or:
./tests/github_actions/run_with_docker.sh
```

### Option 5: Direct `act` Commands (Docker only)
```bash
# List workflows
act -l

# Run all workflows
act

# Run specific job
act -j test

# Dry run
act -n
```

## How It Works

### Singularity/Apptainer Mode (NEW!)

Our `run_ci_act_and_container.sh` script:

1. **Detects container runtime** - Checks for Apptainer → Singularity → Docker
2. **Downloads runner image** - Pulls `catthehacker/ubuntu:act-latest` 
3. **Converts to SIF** - Creates Singularity Image Format file
4. **Caches locally** - Stores in `~/.act_singularity_cache/`
5. **Executes workflow** - Runs all GitHub Actions steps in container
6. **Provides isolation** - Full container security without Docker daemon

### Traditional Docker Mode

`act` reads your `.github/workflows/*.yml` files and:

1. **Parses workflow syntax** - Understands jobs, steps, actions
2. **Downloads actions** - Gets real GitHub Actions from marketplace  
3. **Creates containers** - Spins up Docker containers matching `runs-on`
4. **Executes steps** - Runs each step exactly as GitHub would
5. **Handles secrets** - Supports environment variables and secrets

## Configuration

### Platform Mapping
Create `.actrc` in project root to customize Docker images:
```
--platform ubuntu-latest=ghcr.io/catthehacker/ubuntu:act-latest
--platform ubuntu-22.04=ghcr.io/catthehacker/ubuntu:act-22.04
```

### Environment Variables
Create `.env` file for secrets:
```
GITHUB_TOKEN=your_token_here
NPM_TOKEN=your_npm_token
```

## Container Runtime Comparison

| Feature | Docker | Singularity/Apptainer |
|---------|--------|----------------------|
| Admin rights | ❌ Required | ✅ Not required |
| HPC support | ❌ Often prohibited | ✅ Native support |
| Security | ⚠️ Daemon access | ✅ User-space only |
| Image format | Layers | Single SIF file |
| Caching | System-wide | User directory |
| GitHub Actions | ✅ Native `act` | ✅ Our integration |

## Benefits of Our Solution

| Feature | Traditional `act` | Our Singularity Solution |
|---------|------------------|-------------------------|
| Works on HPC | ❌ Requires Docker | ✅ Full support |
| No admin rights | ❌ Docker daemon | ✅ User-space only |
| GitHub Actions | ✅ Full support | ✅ Full support |
| Maintenance | ✅ Zero | ✅ Zero |
| Container isolation | ✅ Docker | ✅ Singularity |
| Performance | Good | Excellent (SIF cache) |

## Integration

### Makefile Integration
```makefile
local-ci:
    ./tests/github_actions/run_ci_local.sh

ci-docker:  
    ./tests/github_actions/run_with_docker.sh
```

### Pre-commit Hook
```yaml
- repo: local
  hooks:
    - id: act-ci
      name: GitHub Actions (act)
      entry: ./tests/github_actions/run_ci_local.sh
      language: script
      pass_filenames: false
      stages: [pre-push]
```

## Troubleshooting

### Common Issues

**1. act not found**
```bash
# Make sure act is in PATH
export PATH="$HOME/.local/bin:$PATH"
```

**2. Docker permission denied**
```bash
# Add user to docker group
sudo usermod -aG docker $USER
# Then logout/login
```

**3. Workflow not found**
```bash
# List available workflows
find .github/workflows -name "*.yml" -o -name "*.yaml"
```

**4. Actions fail to download**
```bash
# Run with GitHub token
GITHUB_TOKEN=your_token act
```

## Advanced Usage

### Debug Mode
```bash
act -v  # Verbose output
act --dry-run  # Show what would run
```

### Custom Platforms
```bash
act --platform ubuntu-latest=node:18-bullseye
```

### Specific Events
```bash
act push          # Trigger push event
act pull_request  # Trigger PR event  
```

This `act`-based approach provides a truly universal, maintenance-free solution for local GitHub Actions execution.