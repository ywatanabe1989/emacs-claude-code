# Simple Apptainer Integration for emacs-claude-code

## Overview

This document provides information about the Simple Apptainer integration for the emacs-claude-code project. The Simple Apptainer is a lightweight, streamlined container solution designed to get you up and running with Claude in Emacs quickly and with minimal setup.

## Comparison with Full Apptainer

| Feature | Simple Apptainer | Full Apptainer |
|---------|-----------------|----------------|
| Container Size | ~500MB (25% smaller) | ~700MB |
| Build Time | ~3 minutes | ~5 minutes |
| Dependencies | Minimal, essential only | Comprehensive |
| Emacs Version | emacs-nox (terminal) | Full emacs |
| Command Options | Simplified, essential | Extensive |
| Documentation | Concise quick-start | Comprehensive |

## Quick Start

The easiest way to start using Claude with VTerm is:

```bash
./launch-claude-simple.sh
```

This command will:
1. Build the container if it doesn't exist
2. Launch Emacs with Claude VTerm mode
3. Set up all necessary paths and configurations

## Requirements

- Apptainer or Singularity installed on your system
- Emacs 25+ (already included in the container)
- Basic terminal environment

## Building the Container

If you want to build the container separately:

```bash
./launch-claude-simple.sh --build
```

This compiles the Apptainer definition into a runnable container image.

## Container Details

The Simple Apptainer container includes:

- Ubuntu 22.04 base (minimal)
- emacs-nox (terminal-based Emacs)
- libvterm for VTerm integration
- Python with anthropic API
- Placeholder Claude CLI (to be replaced with the actual one when available)
- Minimal system utilities

## Directory Binding

The container automatically binds:

- The project directory to `/workspace` inside the container

This allows seamless file access between the host and container.

## Customization

You can customize the Simple Apptainer by:

1. Modifying the container definition at `apptainer/claude-vterm-simple.def`
2. Editing the Emacs init file at `apptainer/build/emacs/init/init.el`
3. Updating the Claude CLI script at `apptainer/build/claude/bin/claude`

## Troubleshooting

### Common Issues

- **Container build fails**: Ensure Apptainer/Singularity is installed correctly
- **VTerm not working**: Check if libvterm-dev is available in the container
- **Emacs fails to load**: Verify the init file path in the launcher script
- **File access issues**: Check the bind mount paths in the launcher script

### Debug Commands

```bash
# Start a shell in the container for debugging
./launch-claude-simple.sh --shell

# Build with detailed output
APPTAINER_DEBUG=1 ./launch-claude-simple.sh --build
```

## Next Steps

After getting started with the Simple Apptainer, you might want to:

1. Explore more advanced features by using the full Apptainer (`launch-claude-emacs.sh`)
2. Customize your Emacs configuration
3. Set up a real Claude API key when available

## References

- [Apptainer Documentation](https://apptainer.org/docs/)
- [emacs-claude-code README](../README.md)
- [VTerm Mode Documentation](vterm-mode.md)