# Changelog

All notable changes to emacs-claude-code will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [3.2.0] - 2026-01-08

### Added
- New `:suggestion` state detection for Claude Code's inline suggestions (detects "↵ send" indicator)
- Auto-response for suggestion state: sends Tab + delay + Enter to accept suggestions
- Tests for suggestion state detection and naming

### Changed
- License changed from GPL-3.0 to AGPL-3.0
- Removed `docs/to_claude/` from git history to reduce repository size (46M → 34M)

### Fixed
- Test fixes for buffer-local variable detection (use with-temp-buffer)
- Test fixes for smart-yank region handling (enable transient-mark-mode)
- Test fixes for shell-command mock signature (add optional args)
- Test fixes for temp file naming pattern (updated to match new content-preview format)

## [3.1.1] - 2025-07-01

### Fixed
- Various bug fixes and improvements

## [3.0.0] - 2025-06-04

### Changed
- Major repository cleanup and reorganization
- Removed historical project management artifacts
- Streamlined documentation structure
- Improved codebase organization for better maintainability

### Added
- Example bash configuration files for Claude sessions
- Test reports integration for example elisp projects
- Enhanced guidelines structure under docs/to_claude/

### Removed
- Legacy bug reports and feature requests
- Historical progress reports and session summaries
- Phase-specific planning documents
- Redundant documentation files

### Notes
This is a major version release marking a significant cleanup and reorganization of the project structure while maintaining all core functionality.