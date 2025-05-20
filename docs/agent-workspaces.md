# Agent Workspace System

The Agent Workspace System is an extension of the Claude Code Apptainer integration that allows each agent to work in its own isolated environment while still accessing the core code base.

## Architecture

The system uses a sophisticated binding structure to organize code, workspaces, and configuration:

```
Host                           Container
───────────────              ────────────────
/path/to/project    ─────►   /workspace/emacs-claude-code (read-only)
                                  │
~/.emacs.d/claude-agents  ► /workspace/agents (per-agent workspaces)
                                  │
~/.emacs.d/claude-workspace ► /workspace/current (active workspace)
                                  │
~/.emacs.d/claude-config  ─►  /root/.emacs.d/claude-config (shared config)
```

## Directory Structure

1. `/workspace/emacs-claude-code` - Read-only access to the main codebase
2. `/workspace/agents` - Persistent storage for agent-specific workspaces
3. `/workspace/current` - Current working directory for active tasks
4. `/root/.emacs.d/claude-config` - Shared configuration across all agents

## Usage

### Agent Management

The following commands are available for managing agent workspaces:

- `C-c a c` - Create a new agent workspace
- `C-c a s` - Switch to an existing agent workspace
- `C-c a d` - Display current agent workspace status

### Example Workflow

1. Start the container with `./launch-claude-simple.sh`
2. Use `C-c a c` to create a new agent workspace (e.g., "project-analyzer")
3. The agent will create files in its own workspace at `/workspace/agents/project-analyzer/`
4. Use `C-c a s` to switch between different agent workspaces 

### Benefits

This architecture provides several benefits:

1. **Isolation**: Each agent works in its own workspace without affecting others
2. **Persistence**: Agent workspaces persist between container runs
3. **Read-only Source**: The main codebase is mounted read-only to prevent accidental changes
4. **Shared Configuration**: Configuration can be shared across all agents

## Implementation

The system is implemented at multiple levels:

1. **Launcher Scripts**:
   - `launch-claude-simple.sh` and `launch-claude-emacs.sh` set up the necessary bind mounts
   - They create required directories on the host if they don't exist

2. **Emacs Configuration**:
   - The Emacs init file in the container defines variables and functions for workspace management
   - Special keybindings are set up for agent-related operations

3. **Filesystem Mapping**:
   - Apptainer bind mounts map host directories to container directories
   - Appropriate permissions are set for each directory

## Technical Details

### Directory Creation

The launcher scripts ensure that the necessary directories exist on the host before starting the container:

```bash
AGENTS_DIR="${HOME}/.emacs.d/claude-agents"
CURRENT_WORKSPACE="${HOME}/.emacs.d/claude-workspace"
CONFIG_DIR="${HOME}/.emacs.d/claude-config"
    
mkdir -p "${AGENTS_DIR}" "${CURRENT_WORKSPACE}" "${CONFIG_DIR}"
```

### Apptainer Binding

The container is launched with specific bind mounts:

```bash
apptainer run \
    --bind "${PROJECT_DIR}:/workspace/emacs-claude-code:ro" \
    --bind "${AGENTS_DIR}:/workspace/agents" \
    --bind "${CURRENT_WORKSPACE}:/workspace/current" \
    --bind "${CONFIG_DIR}:/root/.emacs.d/claude-config" \
    "${APPTAINER_IMAGE}" \
    emacs -nw -q -l /root/.emacs.d/init/init.el
```

### Emacs Variables

The Emacs initialization file defines variables for easy access to these directories:

```elisp
(defvar claude-agents-dir "/workspace/agents"
  "Directory for Claude agent workspaces.")
  
(defvar claude-current-workspace "/workspace/current"
  "Directory for current Claude workspace.")
  
(defvar claude-config-dir "/root/.emacs.d/claude-config"
  "Directory for Claude configuration.")
```