# Quick Setup Guide for `act`

## 1. Install `act`
```bash
# Run the installer
./tests/github_actions/install_act.sh

# Verify installation
act --version
```

## 2. Test the Setup
```bash
# Test with make command
make local-ci

# Or run directly
./tests/github_actions/run_ci_local.sh
```

## 3. What Changed

### Before (Custom Emulator)
- ❌ **Project-specific** - Only worked for this Python project
- ❌ **Manual maintenance** - Had to update code when workflow changed
- ❌ **Limited** - Basic validation only

### After (`act`)
- ✅ **Universal** - Works with ANY project with GitHub Actions
- ✅ **Zero maintenance** - Reads actual `.github/workflows/*.yml` files  
- ✅ **Complete** - Full GitHub Actions compatibility

## 4. Commands

```bash
# Same as before - but now uses act
make local-ci       # Run GitHub Actions locally
make ci-docker      # Run with Docker platform

# New act-specific options
act -l              # List available workflows
act -j test         # Run specific job
act --dry-run       # Show what would run
```

## 5. Benefits

This transition makes the local CI system:

- **Truly universal** - Copy `tests/github_actions/` to ANY project with GitHub Actions
- **Maintenance-free** - Always matches your actual workflows
- **Industry standard** - Uses the same tool as thousands of other projects

The same setup now works for Python, Node.js, Go, Rust, or any language with GitHub Actions!