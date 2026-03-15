#!/bin/bash
# Setup symlinks for better organization
# Coverage reports naturally belong with tests but are accessible from mgmt

set -e

PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$PROJECT_ROOT"

echo "Setting up symlinks for coverage reports..."

# Create the mgmt/reports directory if it doesn't exist
mkdir -p mgmt/reports

# Create symlink from mgmt/reports/test_results to tests/reports
if [ ! -L "mgmt/reports/coverage" ]; then
    ln -s ../../tests/reports mgmt/reports/test_results
    echo "✅ Created symlink: mgmt/reports/test_results -> tests/reports"
else
    echo "ℹ️  Symlink already exists: mgmt/reports/coverage"
fi

# Verify the symlink works
if [ -L "mgmt/reports/test_results" ] && [ -d "tests/reports" ]; then
    echo "✅ Symlink verified successfully"
    echo "   Coverage reports in tests/reports are accessible via mgmt/reports/test_results"
else
    echo "⚠️  Warning: tests/reports directory doesn't exist yet"
    echo "   It will be created when tests run with coverage"
fi

echo ""
echo "Directory structure:"
echo "  tests/reports/         # Actual coverage reports (natural location)"
echo "  mgmt/reports/coverage/  # Symlink for management access"
echo ""
echo "Agents can reference coverage as:"
echo "  - tests/reports/report.json (direct)"
echo "  - mgmt/reports/coverage/report.json (via symlink)"