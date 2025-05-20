<!-- ---
!-- Timestamp: 2025-05-18 23:19:37
!-- Author: ywatanabe
!-- File: /ssh:ywatanabe@sp:/home/ywatanabe/.dotfiles/.claude/to_claude/guidelines/IMPORTANT_guidelines_safe_rm.md
!-- --- -->

# `rm` Command
- ALWAYS KEEP REPOSITORY CLEAN
- For this, use `./docs/to_claude/bin/safe_rm.sh` below to hide old/unrelated files with timestamp.
- `rm` command is not allowed. 

## Usage
`safe_rm.sh [-h|--help] file_or_directory [file_or_directory...]`

## Examples:
`$(basename $0) file1.txt dir1`
`$(basename $0) *.txt`

## Your Understanding Check
Did you understand the guideline? If yes, please say:
`CLAUDE UNDERSTOOD: <THIS FILE PATH HERE>`

<!-- EOF -->