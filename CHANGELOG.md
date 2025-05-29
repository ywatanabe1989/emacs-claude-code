# Changelog

All notable changes to emacs-claude-code will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [2.1.0] - 2025-05-30

### Added
- **Yank-as-File Feature**: Automatically saves large yanked content to temporary files in vterm buffers
  - Smart file type detection (Python, JavaScript, HTML, CSS, Elisp, Shell)
  - Configurable thresholds (100+ chars prompts, 500+ chars auto-saves)
  - Sends `See <temp-file-path>` to Claude instead of pasting large blocks
  - Commands: `--ecc-vterm-utils-enable-yank-advice`, `--ecc-vterm-utils-cleanup-temp-files`
  
- **Auto-Periodical Command Execution**: Execute commands automatically at regular intervals
  - Tracks interactions per buffer
  - Default: `/git` every 10 interactions, `/user:auto` every 20
  - Prevents duplicate execution
  - Commands: `ecc-auto-periodical-toggle`, `ecc-auto-periodical-status`
  
- **Periodic Return Sending**: Improves auto-response reliability
  - Sends return key every 5 minutes (configurable)
  - Helps resolve timing issues in auto-response
  - Buffer-local periodic timers

### Fixed
- **Modeline Buffer Awareness**: AUTO indicator now properly buffer-local
  - Fixed cross-buffer contamination of mode-line-format
  - Ensures mode-line changes are isolated to individual buffers
  - AUTO indicator appears only where explicitly enabled
  
- **CI/CD Pipeline**:
  - Updated GitHub Actions to work with Ubuntu 22.04
  - Fixed timeout issues with Emacs installation
  - Added proper error handling for known PPA compatibility issues

### Changed
- **Test Quality Improvements**:
  - Split multi-assertion tests into single-assertion tests
  - Improved test naming conventions (removed generic names)
  - Added setup/teardown functions for better test isolation
  - Increased test coverage to 91 tests with 100% pass rate
  
- **Code Organization**:
  - Removed `ecc-vterm-mode` complexity
  - Simplified buffer management
  - Enhanced notification system with consistent buffer name display

### Developer Notes
- All features are backward compatible
- No breaking changes to existing APIs
- Configuration variables follow consistent naming patterns
- Comprehensive test coverage for all new features

## [2.0.0] - 2025-05-26

### Added
- Enhanced buffer management with dired-like marking system
- Major refactoring of core modules
- Improved auto-response system

### Changed
- Complete overhaul of variable system
- Simplified API structure

## [1.0.0] - Initial Release

### Added
- Basic Claude integration with Emacs
- Auto-response capabilities
- vterm integration
- State detection for Claude prompts
- Buffer list management
- Debug utilities