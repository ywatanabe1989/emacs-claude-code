<!-- ---
!-- Timestamp: 2025-05-20 16:56:38
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.claude/to_claude/guidelines/IMPORTANT-guidelines-programming-Cleanup-Rules.md
!-- --- -->

## Cleanup Guidelines
Follow these guidelines when cleaning up the codebase:

1. Ensure the current project can be rolled back at any time.
   - Save ALL files in a branch with a descriptive name.

2. Clean up the codebase by following:
   - `./docs/to_claude/guidelines/IMPORTANT-guidelines-programming-Clean-Code-Rules.md`.

3. Target not only source code (e.g., `./src`, `./scripts`) but also:
   - `./docs/`
   - `./examples`
   - `./project_management`
   - `./tests`

4. Identify and delete unnecessary, obsolete files. However, do not use `rm`.
   - Instead, use `./docs/bin/safe_rm.sh` (although this does not strictly delete)
   - This script moves files to the `.old` directory in the same location as the obsolete file.
   - See more details: `./docs/to_claude/guidelines/IMPORTANT-guidelines-Safe-Remove.md`

5. The goal is to polish the repository to PRODUCTION-READY QUALITY.

<!-- EOF -->