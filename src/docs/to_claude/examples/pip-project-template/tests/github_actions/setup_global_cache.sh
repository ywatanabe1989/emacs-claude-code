#!/bin/bash
# Setup global act cache for all projects
# Usage: ./tests/github_actions/setup_global_cache.sh

set -e

echo "🌐 Setting up global act cache..."

# Create global cache directory
GLOBAL_ACT_CACHE="$HOME/.act"
mkdir -p "$GLOBAL_ACT_CACHE"

# Pre-pull common Docker images used across projects
echo "📦 Pre-pulling common Docker images..."

# Most common images used by act
COMMON_IMAGES=(
    "catthehacker/ubuntu:act-latest"
    "catthehacker/ubuntu:act-22.04"
    "catthehacker/ubuntu:act-20.04"
    "node:18-bullseye-slim"
    "node:20-bullseye-slim"
    "python:3.11-slim"
    "python:3.12-slim"
)

for image in "${COMMON_IMAGES[@]}"; do
    echo "  Pulling $image..."
    docker pull "$image" &
done

# Wait for all pulls to complete
wait

# Create global .actrc configuration
ACTRC="$HOME/.actrc"
if [ ! -f "$ACTRC" ]; then
    echo "⚙️ Creating global .actrc configuration..."
    cat > "$ACTRC" << 'EOF'
# Global act configuration for all projects
--platform ubuntu-latest=catthehacker/ubuntu:act-latest
--platform ubuntu-22.04=catthehacker/ubuntu:act-22.04
--platform ubuntu-20.04=catthehacker/ubuntu:act-20.04

# Performance optimizations
--rm=false
--reuse

# Skip environment file loading by default
--env-file=""

# Use container networking for better compatibility
--container-architecture linux/amd64
EOF
    echo "✅ Created $ACTRC"
else
    echo "ℹ️ $ACTRC already exists"
fi

# Create cache cleanup script
CLEANUP_SCRIPT="$HOME/.local/bin/act-cleanup"
mkdir -p "$(dirname "$CLEANUP_SCRIPT")"
cat > "$CLEANUP_SCRIPT" << 'EOF'
#!/bin/bash
# Clean up act containers and cache
echo "🧹 Cleaning act cache..."

# Remove stopped act containers
docker container prune -f --filter "label=act"

# Remove old act containers (older than 1 day)
docker ps -a --filter "name=act-" --format "table {{.ID}}\t{{.CreatedAt}}" | \
    awk 'NR>1 && $2 < systime()-86400 {print $1}' | \
    xargs -r docker rm -f

echo "✅ Act cache cleaned"
EOF
chmod +x "$CLEANUP_SCRIPT"

# Setup periodic cleanup (optional)
echo "🔄 Setting up cache maintenance..."
echo "
# Add to your ~/.bashrc or ~/.zshrc for automatic cleanup:
# alias act-clean='$CLEANUP_SCRIPT'
#
# Or add to crontab for weekly cleanup:
# 0 2 * * 0 $CLEANUP_SCRIPT
"

echo "
✅ Global act cache setup complete!

🌟 Benefits:
- Docker images cached globally (shared across all projects)
- Act containers reused for faster runs
- Configuration shared across projects
- Automatic cleanup available

🚀 Usage:
- Copy tests/github_actions/ directory to any project
- Images and containers will be reused automatically
- First run downloads, subsequent runs are fast

📁 Cache locations:
- Docker images: /var/lib/docker/ (global)
- Act actions: ~/.cache/act/ (global)  
- Act config: ~/.actrc (global)
- Containers: Reused when possible

💡 Pro tip: Run 'act-cleanup' occasionally to clean old containers
"