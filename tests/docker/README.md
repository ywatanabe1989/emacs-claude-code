# Docker-Based Testing for emacs-claude-code

This directory contains Docker configurations to test the project in environments that mimic GitHub Actions CI.

## Quick Start

```bash
# Test with default Emacs version (mimics CI)
./run-ci-test.sh

# Test with specific Emacs version
./run-ci-test.sh 28.2

# Test with all CI versions
./test-all-versions.sh
```

## Files

- `run-ci-test.sh` - Main test runner that mimics GitHub Actions
- `test-all-versions.sh` - Tests against all Emacs versions used in CI
- `Dockerfile.ci` - Minimal Dockerfile mimicking GitHub Actions environment
- `docker-compose.yml` - Multi-version test orchestration
- `.dockerignore` - Excludes unnecessary files from Docker context

## Why Docker Testing?

1. **Reproduce CI failures locally** - Debug issues without pushing to GitHub
2. **Test multiple Emacs versions** - Ensure compatibility across versions
3. **Clean environment** - No interference from local Emacs configuration
4. **Fast feedback** - Test changes before committing

## Supported Emacs Versions

- 27.1, 27.2 (Ubuntu default)
- 28.1, 28.2 (via PPA)
- 29.1, 29.2, 29.3, 29.4 (if available)

## Troubleshooting

If tests pass locally but fail in Docker:
1. Check for missing dependencies
2. Verify file permissions
3. Look for hardcoded paths
4. Check load-path issues