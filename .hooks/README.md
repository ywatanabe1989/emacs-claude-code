# Git Hooks for emacs-claude-code

This directory contains Git hooks that can be installed to improve the development workflow.

## Available Hooks

### pre-commit

Runs the test suite before each commit to prevent broken code from being committed. The hook uses a threshold-based approach:

- Extracts test statistics from the test output
- Calculates the success rate (passed tests / total tests)
- Allows commits if the success rate is at least 95%
- Falls back to exit code checking if statistics can't be parsed

This approach ensures code quality while providing flexibility for minor test failures.

## Installation

Run the setup script from the project root:

```bash
./setup-hooks.sh
```

This will copy the hooks to your local `.git/hooks` directory and make them executable.

## Manual Installation

If you prefer to install hooks manually:

1. Copy the hook file to `.git/hooks/`
2. Make it executable: `chmod +x .git/hooks/hook-name`

## Bypassing Hooks

If you need to bypass a hook temporarily, use the `--no-verify` flag:

```bash
git commit --no-verify -m "Your commit message"
```

Note: This should only be used in exceptional circumstances.

## Customizing the Threshold

If you need to adjust the success rate threshold, edit the `SUCCESS_THRESHOLD` variable in the pre-commit hook script. The default is set to 95%.