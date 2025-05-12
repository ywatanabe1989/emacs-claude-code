#!/bin/bash
# Setup script to install git hooks

# Get the directory of this script
THIS_DIR="$(cd $(dirname ${BASH_SOURCE[0]}) && pwd)"

echo "Setting up git hooks for emacs-claude-code..."

# Create the git hooks directory if it doesn't exist
mkdir -p "$THIS_DIR/.git/hooks"

# Copy each hook from .hooks to .git/hooks and make executable
for hook in "$THIS_DIR/.hooks"/*; do
  if [ -f "$hook" ]; then
    hook_name=$(basename "$hook")
    target="$THIS_DIR/.git/hooks/$hook_name"
    
    cp "$hook" "$target"
    chmod +x "$target"
    
    echo "Installed: $hook_name"
  fi
done

echo "Hooks setup complete!"