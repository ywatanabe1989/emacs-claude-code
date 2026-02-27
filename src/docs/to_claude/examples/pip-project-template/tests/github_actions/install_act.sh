#!/bin/bash
# Install act (GitHub Actions runner)
# Usage: ./tests/github_actions/install_act.sh

set -e

echo "🎬 Installing act (GitHub Actions local runner)..."

# Check if act is already installed
if command -v act &> /dev/null; then
    echo "✅ act is already installed: $(act --version)"
    exit 0
fi

# Detect OS and architecture
OS=$(uname -s | tr '[:upper:]' '[:lower:]')
ARCH=$(uname -m)

case $ARCH in
    x86_64) ARCH="x86_64" ;;
    arm64|aarch64) ARCH="arm64" ;;
    *) echo "❌ Unsupported architecture: $ARCH"; exit 1 ;;
esac

# Get latest version
LATEST_VERSION=$(curl -s https://api.github.com/repos/nektos/act/releases/latest | grep '"tag_name"' | cut -d'"' -f4)
if [ -z "$LATEST_VERSION" ]; then
    echo "❌ Failed to get latest version"
    exit 1
fi

echo "📦 Downloading act $LATEST_VERSION for $OS-$ARCH..."

# Download URL
DOWNLOAD_URL="https://github.com/nektos/act/releases/download/${LATEST_VERSION}/act_${OS}_${ARCH}.tar.gz"

# Create temp directory
TMP_DIR=$(mktemp -d)
cd "$TMP_DIR"

# Download and extract
curl -L "$DOWNLOAD_URL" -o act.tar.gz
tar -xzf act.tar.gz

# Install to user bin or /usr/local/bin
INSTALL_DIR="$HOME/.local/bin"
mkdir -p "$INSTALL_DIR"

if [ -w "$INSTALL_DIR" ]; then
    mv act "$INSTALL_DIR/"
    echo "✅ act installed to $INSTALL_DIR/act"
    echo "💡 Make sure $INSTALL_DIR is in your PATH"
elif [ -w "/usr/local/bin" ]; then
    sudo mv act /usr/local/bin/
    echo "✅ act installed to /usr/local/bin/act"
else
    echo "❌ Cannot install act - no writable directory found"
    echo "   Please run: sudo mv act /usr/local/bin/"
    exit 1
fi

# Cleanup
cd - > /dev/null
rm -rf "$TMP_DIR"

# Verify installation
if command -v act &> /dev/null; then
    echo "🎉 act successfully installed: $(act --version)"
else
    echo "⚠️  act installed but not in PATH. Add to your PATH:"
    echo "   export PATH=\"$INSTALL_DIR:\$PATH\""
fi