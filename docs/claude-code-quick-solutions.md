# Claude Code Scrolling Bug: Quick Solutions Reference

**Last Updated:** October 2025

This guide provides actionable solutions organized by user type and technical level.

---

## Universal Solution (Everyone Should Do This)

### Use `/clear` Command Frequently

**What:** Type `/clear` in Claude Code every 5-10 interactions

**Why:** Prevents conversation history from reaching the 50% context window threshold where Ink's rendering issues trigger

**When:**
- Every 5-10 exchanges with Claude
- Before starting new complex tasks
- After Claude generates very long outputs
- When you notice slowdown or scrolling weirdness

**This is Anthropic's official recommendation** and the most reliable workaround.

---

## Solutions by Terminal Type

### General Terminal Users

#### Immediate Actions
1. **Run Claude Code in native terminal apps** instead of IDE integrated terminals
2. **Avoid VSCode/Cursor integrated terminals** - they have worse Ink compatibility
3. **Monitor conversation length** - restart before issues occur

#### For VSCode Users
- Try the **Claude Code VS Code extension** (beta) - bypasses terminal rendering entirely
- Alternative: Run Claude Code in Windows Terminal, iTerm2, or other native terminal

---

### tmux Users

Add to `~/.tmux.conf`:

```bash
# Enable mouse support
set -g mouse on

# Reduce Ink's disruptive behavior with alternate screen
set -ga terminal-overrides ',xterm*:smcup@:rmcup@'
```

Reload: `tmux source-file ~/.tmux.conf`

**What this does:** The `smcup@:rmcup@` override tells tmux to ignore some alternate screen buffer commands that Ink uses.

---

### macOS Terminal.app Users

**Disable alternate screen scrolling:**

1. Terminal → Preferences → Profiles
2. Select your profile → Keyboard tab
3. Uncheck "Scroll alternate screen"

**What this does:** Prevents the terminal from scrolling when applications enter alternate screen mode.

---

### Windows Terminal Users

**For Windows Terminal only** (not Emacs in WSL):

1. Press `Win + R`, paste: `%LOCALAPPDATA%\Packages\Microsoft.WindowsTerminal_8wekyb3d8bbwe\LocalState`
2. Open `settings.json`
3. Add under "profiles":

```json
{
    "profiles": {
        "defaults": {
            "compatibility.enableUnfocusedAcrylic": false
        }
    }
}
```

4. Save and fully close all Terminal windows
5. Reopen Windows Terminal

**What this does:** Disables GPU-accelerated transparency effects that can cause flickering in WSL.

**Note:** This setting does NOT affect Emacs running in WSL.

---

### Emacs vterm Users

#### Minimal Configuration (Quick Start)

Add to your Emacs config:

```elisp
(use-package vterm
  :ensure t
  :config
  ;; CRITICAL: Increase scrollback
  (setq vterm-max-scrollback 100000)
  
  ;; Prevent C-l from clearing scrollback
  (setq vterm-clear-scrollback-when-clearing nil)
  
  ;; Terminal environment
  (setq vterm-term-environment-variable "xterm-256color")
  
  ;; Performance
  (setq vterm-timer-delay 0.1)
  
  ;; Per-buffer settings
  (add-hook 'vterm-mode-hook
    (lambda ()
      (setq-local vterm-max-scrollback 100000))))
```

**What this does:**
- Increases scrollback from 1,000 to 100,000 lines
- Preserves history during long AI conversations
- Optimizes rendering performance

#### For Advanced Users

Consider community packages:

**Option 1: claude-code.el (simpler)**
```elisp
(use-package claude-code
  :vc (:url "https://github.com/stevemolitor/claude-code.el")
  :config
  (setq claude-code-terminal-backend 'vterm)
  (setq claude-code-vterm-buffer-multiline-output t))
```

**Option 2: claude-code-ide.el (more features)**
```elisp
(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el")
  :config
  (setq claude-code-ide-terminal-backend 'vterm)
  (setq claude-code-ide-vterm-anti-flicker t)
  (setq claude-code-ide-prevent-reflow-glitch t))
```

**See the full Emacs vterm guide** for comprehensive configuration including your existing sophisticated setup.

#### Font Configuration (Important for All Emacs Users)

```elisp
;; Ensure proper Unicode rendering for Ink borders
(setq use-default-font-for-symbols nil)
(set-fontset-font t 'unicode (font-spec :family "JuliaMono"))
```

**Good alternatives:** DejaVu Sans Mono, Fira Code, Iosevka, Cascadia Code

---

## Troubleshooting Common Issues

### Symptom: Screen flashing/strobing at 10+ flashes per second

**Solutions:**
1. Use `/clear` immediately
2. If in Emacs vterm: Toggle optimization mode (M-o if configured)
3. Restart Claude Code session
4. Consider running in different terminal app

### Symptom: Uncontrollable scrolling that keyboard can't stop

**Solutions:**
1. Close and restart Claude Code
2. Use `/clear` more frequently (every 5 interactions instead of 10)
3. If in IDE: Switch to native terminal
4. Check if GPU acceleration is causing issues (Windows Terminal setting above)

### Symptom: Input box stuck halfway up buffer

**Solutions:**
1. Restart Claude Code (no other reliable fix)
2. Prevention: Enable reflow glitch prevention (Emacs vterm with integration packages)
3. Avoid resizing terminal window during Claude output

### Symptom: Cursor appearing in wrong position when typing

**Solutions:**
1. Use Claude Code's vim mode for modal editing
2. In Emacs: Consider using minibuffer commands instead of direct terminal input
3. Slower typing helps (cursor sync issue)

---

## Decision Tree: Which Solutions Apply to You?

```
Are you using Emacs?
├─ YES → See "Emacs vterm Users" section above
│        + Full vterm guide document
│        + Still use /clear command frequently
│
└─ NO → What's your terminal?
    ├─ tmux → See "tmux Users" section
    ├─ macOS Terminal.app → See "macOS" section  
    ├─ Windows Terminal → See "Windows Terminal" section
    ├─ VSCode/Cursor integrated → Switch to native terminal OR try VS Code extension
    └─ Other → Use universal solution (/clear frequently)
```

---

## Configuration Priority

**Priority 1 (Do First):**
- Use `/clear` every 5-10 interactions
- Run in native terminal, not IDE

**Priority 2 (Significant Improvement):**
- Terminal-specific configurations (tmux/macOS/Windows Terminal)
- For Emacs: Increase vterm scrollback to 100,000

**Priority 3 (Fine-tuning):**
- For Emacs: Community integration packages
- Font configuration
- Advanced optimizations

---

## What NOT to Do

❌ **Don't rely only on configuration** - the `/clear` command is still essential

❌ **Don't increase scrollback infinitely** - vterm maxes at 100,000 (hardcoded)

❌ **Don't expect complete elimination** - this is a fundamental Ink design limitation

❌ **Don't use IDE integrated terminals** - consistently worse performance

❌ **Don't ignore font configuration** - broken Unicode rendering compounds issues

---

## When Nothing Works

If you've tried everything and still have severe issues:

### Alternative 1: VS Code Extension (Beta)
- Bypasses terminal rendering entirely
- Different UI paradigm
- Still in beta as of October 2025

### Alternative 2: Different Terminal Emulator
- For Emacs users: Try `eat` instead of `vterm` (pure elisp, slower but may have fewer Ink issues)
- For others: Try different native terminal apps

### Alternative 3: Manage Expectations
- The issue stems from Ink's fundamental architecture
- No official fix exists or is planned
- Working within limitations (frequent `/clear`, native terminals) is current best practice

---

## Resources

**Full Documentation:**
- `claude-code-scrolling-bug-overview.md` - Technical details and root cause
- `claude-code-emacs-vterm-guide.md` - Comprehensive Emacs configuration

**Official Links:**
- Claude Code: https://www.anthropic.com/claude-code
- Ink GitHub: https://github.com/vadimdemedes/ink
- Issue #826: https://github.com/anthropics/claude-code/issues/826

**Community Packages (Emacs):**
- claude-code.el: https://github.com/stevemolitor/claude-code.el
- claude-code-ide.el: https://github.com/manzaltu/claude-code-ide.el

---

## Summary

The most effective strategy combines:

1. **Frequent `/clear` commands** (every 5-10 interactions)
2. **Native terminal usage** (not IDE integrated terminals)
3. **Terminal-specific configurations** (based on your environment)
4. **For Emacs users:** Comprehensive vterm optimization

This approach provides the best experience possible given the fundamental limitations of Ink's architecture.
