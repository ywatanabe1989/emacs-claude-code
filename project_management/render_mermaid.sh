#!/bin/bash
# -*- coding: utf-8 -*-
# Timestamp: "2025-05-10 01:01:32 (ywatanabe)"
# File: ./project_management/render_mermaid.sh

THIS_DIR="$(cd $(dirname ${BASH_SOURCE[0]}) && pwd)"
LOG_PATH="$THIS_DIR/.$(basename $0).log"
echo > "$LOG_PATH"

GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color
# ---------------------------------------

# render_mermaid.sh
# Author: ywatanabe (ywatanabe@alumni.u-tokyo.ac.jp)
# Date: 2025-05-08

LOG_FILE="$0.log"

mmd2images() {
    local path_mmd=$1
    local base_name=${path_mmd%.mmd}
    local path_svg="${base_name}.svg"
    local path_png="${base_name}.png"
    local path_gif="${base_name}.gif"

    # Step 1: Check if it's a graph type diagram (skip for other types like gantt)
    if grep -q "^graph" "$path_mmd"; then
        echo "Detected graph diagram, checking format..."
        # Ensure graph is in TD (top-down) format
        if ! grep -q "^graph TD" "$path_mmd"; then
            echo "Warning: Graph diagram is not using TD (top-down) format."
            echo "Converting to TD format..."
            sed -i 's/^graph \(LR\|RL\|BT\)/graph TD/' "$path_mmd"
        fi
    else
        echo "Non-graph diagram detected (gantt, pie, sequence, etc.), preserving format..."
    fi

    # Step 2: Convert MMD to SVG (high resolution)
    echo "Converting ${path_mmd} to SVG..."
    mmdc -i "$path_mmd" -o "$path_svg" --backgroundColor white

    # Step 3: Convert SVG to high-res PNG with reasonable size
    echo "Converting ${path_svg} to high-res PNG..."
    convert "$path_svg" -quality 100 -background white -flatten "$path_png"

    # Step 4: Convert PNG to GIF
    echo "Converting ${path_png} to GIF..."
    convert "$path_png" "$path_gif"

    # Output success message
    echo "Created: $path_svg"
    echo "Created: $path_png"
    echo "Created: $path_gif"

    # Return the paths to the created files
    echo "$path_svg $path_png $path_gif"
}

usage() {
    echo "Usage: $0 [mermaid_file] [-h|--help]"
    echo
    echo "Options:"
    echo " -h, --help       Display this help message"
    echo " mermaid_file     Path to the Mermaid file to render (optional)"
    echo
    echo "Examples:"
    echo " $0                               # Renders default progress.mmd file"
    echo " $0 path/to/diagram.mmd           # Renders specified Mermaid file"
    echo
    echo "Purpose:"
    echo " Converts Mermaid files to SVG, PNG and GIF formats"
    echo " and ensures all diagrams are in TD (top-down) format"
    exit 1
}

main() {
    local mermaid_file=""
    
    # Parse command line arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
        -h|--help) 
            usage 
            ;;
        *)
            # If not a recognized option, treat as input file
            if [[ -f "$1" ]]; then
                mermaid_file="$1"
            else
                echo "Unknown option or file not found: $1"
                usage
            fi
            ;;
        esac
        shift
    done

    # Get the directory where this script is located
    local script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    
    # If no file was specified, use the default
    if [[ -z "$mermaid_file" ]]; then
        mermaid_file="${script_dir}/progress.mmd"
    fi

    # Check if the Mermaid file exists
    if [[ ! -f "$mermaid_file" ]]; then
        echo "Error: Mermaid file not found: $mermaid_file"
        exit 1
    fi

    # Check if mmdc command is available
    if ! command -v mmdc &> /dev/null; then
        echo "Error: mmdc command not found. Please install @mermaid-js/mermaid-cli:"
        echo "npm install -g @mermaid-js/mermaid-cli"
        exit 1
    fi

    # Check if convert command is available
    if ! command -v convert &> /dev/null; then
        echo "Error: convert command not found. Please install ImageMagick:"
        echo "sudo apt-get install imagemagick # Ubuntu/Debian"
        echo "brew install imagemagick # macOS with Homebrew"
        exit 1
    fi

    # Generate the images
    echo "Generating images from Mermaid diagram..."
    mmd2images "$mermaid_file"

    echo "Successfully generated SVG, PNG and GIF from Mermaid diagram."
    echo "SVG: ${mermaid_file%.mmd}.svg"
    echo "PNG: ${mermaid_file%.mmd}.png"
    echo "GIF: ${mermaid_file%.mmd}.gif"
}

{ main "$@"; } 2>&1 | tee "$LOG_FILE"

# EOF