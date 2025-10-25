# ECC Anti-Flicker Module

**Version:** 1.0.0
**Last Updated:** 2025-10-24

## Overview

The `ecc-anti-flicker` module provides comprehensive optimizations to address the Claude Code scrolling and flickering issues in Emacs vterm. It implements the community-researched workarounds and best practices documented in the Claude Code scrolling bug guides.

## What It Does

### Core Optimizations

1. **Maximizes vterm scrollback** to 100,000 lines (vterm's hardcoded maximum)
2. **Prevents scrollback clearing** when using C-l
3. **Optimizes terminal settings** for better Ink compatibility
4. **Monitors buffer size** and warns when approaching flicker-prone thresholds
5. **Configures fonts** for proper Unicode rendering of Ink borders

### Why These Help

Claude Code's flicker issues stem from the Ink library's design:
- Ink clears terminal scrollback when content exceeds terminal height
- This causes rapid screen flashing at 10+ flashes per second
- The issue is most severe after conversation context exceeds 50% of the window

The anti-flicker module addresses these through:
- **Large scrollback**: Preserves full conversation history
- **Buffer monitoring**: Proactive warnings before issues occur
- **Optimized settings**: Reduces terminal re-rendering overhead

## Quick Start

### Automatic Setup (Recommended)

When you create a Claude vterm buffer using `--ecc-create-vterm`, anti-flicker mode is **automatically enabled**:

```elisp
M-x --ecc-create-vterm
```

### Manual Control

Enable anti-flicker in current buffer:
```elisp
M-x --ecc-anti-flicker-enable
```

Disable anti-flicker:
```elisp
M-x --ecc-anti-flicker-disable
```

Toggle anti-flicker:
```elisp
M-x --ecc-anti-flicker-toggle
```

### Auto-Enable for All vterm Buffers

```elisp
M-x --ecc-anti-flicker-setup-vterm-hook
```

This adds anti-flicker to `vterm-mode-hook`, so all new vterm buffers get the optimizations.

To remove:
```elisp
M-x --ecc-anti-flicker-remove-vterm-hook
```

## Configuration

### Essential Settings

```elisp
;; Maximum scrollback (default: 100000, vterm's max)
(setq --ecc-anti-flicker-scrollback-size 100000)

;; Warn when buffer reaches this % of capacity (default: 0.5 = 50%)
(setq --ecc-anti-flicker-context-threshold 0.5)

;; How often to check buffer size in seconds (default: 30)
(setq --ecc-anti-flicker-check-interval 30.0)

;; Enable automatic warnings (default: t)
(setq --ecc-anti-flicker-auto-warn t)
```

### Font Configuration

For optimal Ink border rendering:

```elisp
;; Configure fonts for Unicode box-drawing characters
M-x --ecc-anti-flicker-configure-fonts

;; Or set a different font family
(setq --ecc-anti-flicker-font-family "Fira Code")
;; Good alternatives: DejaVu Sans Mono, Iosevka, Cascadia Code
```

## Monitoring and Diagnostics

### Check Current Status

```elisp
M-x --ecc-anti-flicker-status
```

Shows:
- Whether anti-flicker is enabled
- Current buffer size and capacity used
- Scrollback limit
- Monitoring status
- Warnings if threshold exceeded

### Understanding Warnings

When buffer approaches the threshold:
```
[ECC Anti-Flicker] Buffer is 52% full. Consider using /clear to prevent flicker issues.
```

**What to do:**
1. Use `/clear` command in Claude Code to reset context
2. Or restart the Claude Code session
3. The warning cooldown is 5 minutes to avoid spam

## Integration with Auto-Response

Anti-flicker works seamlessly with the auto-response system:

```elisp
;; Create a vterm buffer with both features
M-x --ecc-create-vterm

;; This enables:
;; 1. Anti-flicker optimizations
;; 2. Auto-response to Claude prompts
;; 3. State detection with highlighting
```

## Advanced Usage

### Customizing Warning Behavior

```elisp
;; Warn at 70% instead of 50%
(setq --ecc-anti-flicker-context-threshold 0.7)

;; Check buffer every 60 seconds instead of 30
(setq --ecc-anti-flicker-check-interval 60.0)

;; Disable automatic warnings (manual check only)
(setq --ecc-anti-flicker-auto-warn nil)
```

### Per-Buffer Settings

Anti-flicker settings are buffer-local, so you can:

```elisp
;; In a specific buffer
(setq-local --ecc-anti-flicker-context-threshold 0.3)  ; More aggressive warnings
(setq-local --ecc-anti-flicker--warning-cooldown 60)   ; Warn every minute
```

## Best Practices

### Recommended Workflow

1. **Use anti-flicker for all Claude buffers**
   ```elisp
   (--ecc-anti-flicker-setup-vterm-hook)
   ```

2. **Monitor buffer status periodically**
   ```elisp
   M-x --ecc-anti-flicker-status
   ```

3. **Heed the warnings** - Use `/clear` when prompted

4. **Configure fonts** for best visual results
   ```elisp
   M-x --ecc-anti-flicker-configure-fonts
   ```

### What Anti-Flicker CANNOT Fix

The module addresses vterm-specific optimizations, but remember:

❌ Cannot eliminate Ink's fundamental rendering limitations
❌ Cannot prevent issues if you ignore buffer warnings
❌ Cannot replace the `/clear` command - still needed regularly

✅ Significantly reduces flicker frequency
✅ Preserves conversation history better
✅ Provides early warnings before issues occur
✅ Optimizes vterm for long Claude sessions

## Troubleshooting

### Anti-Flicker Not Working?

1. **Verify it's enabled:**
   ```elisp
   M-x --ecc-anti-flicker-status
   ```

2. **Check you're in vterm:**
   ```elisp
   ;; Anti-flicker only works in vterm-mode
   M-x describe-mode
   ```

3. **Reload the module:**
   ```elisp
   (load-library "ecc-anti-flicker")
   ```

### Still Experiencing Flicker?

Anti-flicker reduces but doesn't eliminate all issues:

1. **Use `/clear` more frequently** (every 5-10 interactions)
2. **Check buffer capacity:** `M-x --ecc-anti-flicker-status`
3. **Verify font configuration:** Some fonts don't render Ink borders correctly
4. **Consider buffer size:** Even with optimizations, extremely long sessions remain problematic

## Technical Details

### How Buffer Monitoring Works

1. Timer checks buffer size every N seconds (default: 30)
2. Calculates ratio: `buffer-size / (scrollback-limit × 80 chars/line)`
3. If ratio ≥ threshold AND cooldown expired → warn user
4. Cooldown period: 5 minutes between warnings

### Scrollback Limit

The 100,000 line limit is vterm's hardcoded maximum. The module uses this because:
- Claude conversations can be extremely long
- Preserving history prevents context loss
- Larger buffer delays when Ink's clearing behavior triggers

### Font Configuration

Sets `use-default-font-for-symbols nil` and configures Unicode fontset because:
- Ink uses box-drawing characters for UI borders
- Improper Unicode rendering compounds visual issues
- Monospace fonts with good Unicode support work best

## Related Documentation

- `claude-code-scrolling-bug-overview.md` - Technical details on the Ink issue
- `claude-code-quick-solutions.md` - Platform-specific workarounds
- `ecc-auto-response` - Auto-response system that works with anti-flicker

## Version History

### 1.0.0 (2025-10-24)
- Initial release
- Scrollback optimization
- Buffer size monitoring
- Font configuration
- Integration with `--ecc-create-vterm`

## Contributing

Found an improvement or workaround? Please update:
1. The source: `src/ecc-anti-flicker.el`
2. The tests: `tests/test-ecc-anti-flicker.el`
3. This documentation

## License

Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
