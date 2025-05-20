Complete term-claude-mode cleanup and refactoring

This commit provides a comprehensive cleanup and refactoring of the term-claude-mode
module, transforming it into a modular, well-documented, and optimized system while
maintaining backward compatibility.

Key improvements:

1. Architecture:
   - Split monolithic implementation into 7 specialized modules
   - Created clean interfaces between components
   - Eliminated all code duplication

2. Code quality:
   - Standardized naming conventions across all functions
   - Added comprehensive docstrings to all functions
   - Improved error handling and validation throughout

3. Performance:
   - Optimized state detection for large buffers
   - Added GC optimizations for intensive operations
   - Improved UI responsiveness during streaming

4. Documentation:
   - Created detailed user guide with examples
   - Added API documentation for extension
   - Added comprehensive testing suite

New modules:
- ecc-term-claude-state.el: Consolidated state detection
- ecc-term-claude-auto.el: Unified auto-response system
- ecc-term-claude-setup.el: Common setup logic
- ecc-term-claude-buffer.el: Buffer management
- ecc-term-claude-interaction.el: Claude interaction functions
- ecc-term-claude-performance.el: Performance optimizations
- ecc-term-claude-mode-v2.el: Refined mode definition

This cleanup follows the guidelines in IMPORTANT-guidelines-programming-Clean-Code-Rules.md
and IMPORTANT-guidelines-programming-Cleanup-Rules.md, creating a production-ready
implementation with excellent maintainability.