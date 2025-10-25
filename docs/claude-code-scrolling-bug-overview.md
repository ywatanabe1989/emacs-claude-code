# Claude Code Scrolling Bug: Understanding the Root Cause

**Last Updated:** October 2025

## Summary

The scrolling bug in Claude Code is a fundamental design limitation of the Ink library—not a bug. Ink intentionally clears terminal scrollback when content exceeds terminal height. While no official fix exists as of October 2025, users can implement several effective workarounds depending on their terminal environment.

## Root Cause: Ink's Rendering Architecture

### The Technical Problem

When Ink applications like Claude Code render content that exceeds terminal height, the library makes a deliberate choice to clear the entire terminal, including the scrollback buffer.

**The problematic behavior:**
- Located in `src/ink.tsx` around line 239
- When output height ≥ terminal rows, Ink executes: `ansiEscapes.clearTerminal`
- Sends escape sequence: `\x1b[2J\x1b[3J\x1b[H`
- The critical part: `\x1b[3J` (E3 capability) specifically clears the scrollback buffer

### Why Ink Does This

Ink uses React's reconciler to render to terminals instead of DOM. Unlike browsers that can update specific elements:

1. Terminals require full frame repainting when content changes
2. When content exceeds the visible window, Ink cannot use differential rendering without risking corrupted output
3. Clearing the terminal was chosen as the "safe" approach to ensure rendering correctness

### The Maintainer's Position

From GitHub discussions, the Ink maintainer's stance is clear:

- Ink is designed for "interactive command-line apps" with bounded output
- Not intended for fullscreen applications or apps with unlimited output
- For complex terminal UIs, recommends ncurses-based alternatives like `blessed`
- This behavior is considered a design decision, not a bug

### Status of Issues

**GitHub Issue History:**
- Issue #382: "If terminal height is 10 lines and an Ink app renders 11 lines, the entire terminal history is wiped"
- Issue #359 (August 2020): "flickering badly on updates"
- **No fix has been implemented in any Ink release from 2020 through 2025**

**Claude Code Specific Issues:**
- Issue #826 (151+ reactions): "stroboscope effect" screen flashing at 10 flashes per second
- Issue #1422: "uninterruptible high speed scrolling" that no keyboard input can stop
- Issue #1413: Problem occurs after context compaction with long terminal history
- Issues #3648, #7216: Similar symptoms across VSCode, Cursor, other IDEs

**All are marked as duplicates of #826 from 2024, with no public fix timeline announced.**

## When the Problem Triggers

The pattern is consistent across reports:

1. Conversation history approaches or exceeds 50% of context window
2. Ink's frequent re-renders to manage large output buffers activate the scrollback clearing behavior
3. This manifests as aggressive, uncontrollable scrolling
4. Some users report GPU-accelerated scrolling speeds that make the terminal unusable

## Official Workarounds from Anthropic

**Primary recommendation: Use `/clear` command frequently**
- Every 5-10 interactions
- Resets context and prevents the trigger condition
- Documented in Claude Code best practices
- Most reliable workaround—prevents reaching the problematic threshold

## Cross-Platform Solutions

### For All Users

**1. Aggressive Context Management**
- Use `/clear` every 5-10 interactions
- Monitor conversation length
- Restart sessions before they become unwieldy

**2. Run in Native Terminal Apps**
- Avoid IDE integrated terminals (VSCode, Cursor, IntelliJ)
- Native terminals have better Ink compatibility
- Consider the Claude Code VS Code extension (beta) as an alternative

### For tmux Users

```bash
# Add to ~/.tmux.conf
set -g mouse on
set -ga terminal-overrides ',xterm*:smcup@:rmcup@'
```

The `smcup@:rmcup@` override tells tmux to ignore alternate screen buffer commands, reducing Ink's disruptive behavior.

Reload with: `tmux source-file ~/.tmux.conf`

### For macOS Terminal.app Users

Disable alternate screen scrolling:
1. Terminal → Preferences → Profiles → [Your Profile] → Keyboard
2. Uncheck "Scroll alternate screen"

This prevents the terminal from attempting to scroll when applications are in alternate screen mode.

### For Windows Terminal Users

Add to `settings.json` (affects Windows Terminal only, not Emacs):

```json
{
    "profiles": {
        "defaults": {
            "compatibility.enableUnfocusedAcrylic": false
        }
    }
}
```

This addresses GPU-accelerated rendering issues that can compound flickering in WSL terminals.

**Location of settings.json:**
- Stable: `%LOCALAPPDATA%\Packages\Microsoft.WindowsTerminal_8wekyb3d8bbwe\LocalState\settings.json`
- Preview: `%LOCALAPPDATA%\Packages\Microsoft.WindowsTerminalPreview_8wekyb3d8bbwe\LocalState\settings.json`

### For VSCode/IDE Users

**Best practices:**
1. Increase integrated terminal scrollback to maximum
2. Consider running Claude Code in external terminal instead
3. Try the Claude Code VS Code extension (beta)—bypasses terminal rendering entirely

## Understanding Alternate Screen Buffer

A key concept for understanding Ink's behavior:

When applications enter alternate screen mode (escape sequence `\x1b[?1049h`):
- Saves current screen content
- Switches to a clean alternate buffer with no scrollback
- Fixes display size to visible window
- Returns to original screen on exit (`\x1b[?1049l`)

**This is how vim, less, and htop work.** Ink applications typically use alternate screen, which means scrollback is intentionally disabled by design—even though this feels wrong for conversational CLI tools like Claude Code.

## Current State and Future Outlook

### Claude Code 2.0 (September 29, 2025)

**New features:**
- Refreshed terminal interface
- Searchable prompt history (Ctrl+r)
- Native VS Code extension beta
- Checkpoints for saving state
- Sonnet 4.5 as default model

**The scrolling issue remains unresolved.** Version 2.0.8 has made no changes to address the fundamental Ink rendering problem.

### Ink Library Status

Reviewing major releases:
- **6.1.0** (July 2024): Added backgroundColor support, no scrollback fixes
- **5.0.0** (2023): Improved performance, maintained clearing behavior
- **4.0.0** (2022): Migrated to ESM, no scrollback changes
- **3.0.0** (2020): Rewrote rendering engine (2x performance), kept clearing approach

**No architectural changes to address this issue are planned.**

## Known Limitations

Some challenges have no clean solutions:

1. **Cursor sync issues**: Rapid typing can desynchronize cursor tracking
2. **Input box positioning glitches**: Text input area gets stuck, particularly after resizes
3. **Context window limits**: Even with workarounds, extremely long sessions remain problematic

## Recommendations Summary

**For all users:**
1. Use `/clear` command every 5-10 interactions
2. Run Claude Code in native terminal apps when possible
3. Avoid IDE integrated terminals
4. Monitor conversation length proactively

**Platform-specific configurations available for:**
- tmux users
- macOS Terminal.app
- Windows Terminal
- Emacs vterm (see separate Emacs-specific guide)

The VS Code extension represents a potential long-term solution by bypassing terminal rendering entirely, but currently remains in beta and doesn't help users who prefer terminal workflows.
