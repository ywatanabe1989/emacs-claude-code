<!-- Bug Report: ecc-mode Auto-Activation -->
<!-- Author: ywatanabe -->
<!-- Date: 2025-05-26 -->
<!-- Status: Solved -->

# Bug Report: ecc-mode Automatically Activated on Package Load

## Summary
The `ecc-mode` minor mode is automatically enabled when the package is loaded, which violates the principle that modes should only be activated by explicit user action.

## Location
- File: `emacs-claude-code.el:73`
- Code: `(ecc-mode 1)`

## Description
When `emacs-claude-code` is loaded (via `require` or package initialization), it automatically enables `ecc-mode` globally. This forces the mode's keybindings on users without their explicit consent.

## Current Behavior
```elisp
;; In emacs-claude-code.el
(require 'ecc-convenience-commands)
;; Enable minor mode by default
(ecc-mode 1)  ; Line 73
```

## Expected Behavior
The mode should not be activated automatically. Users should explicitly enable it through:
- Calling `(ecc-mode 1)` in their init file
- Using `M-x ecc-mode`
- Using a provided setup function

## Impact
- **Forced Keybindings**: All `C-c c` keybindings are activated without user consent
- **Namespace Pollution**: The mode takes over keybinding space the user may want for other purposes
- **Emacs Convention Violation**: Goes against the Emacs convention that modes should be opt-in
- **Configuration Conflicts**: May conflict with user's existing keybindings

## Root Cause
The auto-activation was likely added for convenience but violates Emacs packaging best practices.

## Proposed Solution

### 1. Remove Auto-Activation
Simply remove the auto-activation line:

```elisp
;; In emacs-claude-code.el
(require 'ecc-convenience-commands)
;; Remove this line:
;; (ecc-mode 1)
```

### 2. Provide Setup Function (Optional)
Add a convenience function for users who want easy setup:

```elisp
;;;###autoload
(defun ecc-setup ()
  "Setup emacs-claude-code with recommended settings."
  (interactive)
  (ecc-mode 1)
  (message "Emacs Claude Code mode enabled. Use C-c c h for help."))
```

### 3. Update Documentation
Update README and documentation to show how to enable the mode:

```elisp
;; In user's init file:
(require 'emacs-claude-code)
(ecc-mode 1)  ; Enable keybindings

;; Or using use-package:
(use-package emacs-claude-code
  :config
  (ecc-mode 1))
```

## Test Case
```elisp
(ert-deftest test-no-auto-activation ()
  "Test that loading the package doesn't auto-enable ecc-mode."
  (let ((ecc-mode-enabled-before ecc-mode))
    (unwind-protect
        (progn
          ;; Disable mode if it's on
          (when ecc-mode (ecc-mode -1))
          ;; Reload the package
          (load "emacs-claude-code")
          ;; Mode should still be off
          (should-not ecc-mode))
      ;; Restore previous state
      (when ecc-mode-enabled-before (ecc-mode 1)))))
```

## Related Issues
- Violates Emacs packaging conventions
- May cause unexpected behavior for users
- Makes it harder to use the package libraries without the keybindings

## Priority
High - Violates core Emacs conventions and user expectations

## Recommendation
1. Remove the auto-activation immediately
2. Add clear documentation on how to enable the mode
3. Consider adding a setup function for convenience
4. Follow the pattern of other well-behaved Emacs packages

## Examples of Proper Behavior
Well-behaved packages that don't auto-enable their modes:
- `magit` - Requires explicit `(global-set-key (kbd "C-x g") 'magit-status)`
- `projectile` - Requires `(projectile-mode +1)`
- `company` - Requires `(global-company-mode)`
- `which-key` - Requires `(which-key-mode)`

These packages provide functionality without forcing their keybindings on users.

## Solution Implemented
✅ **Fixed in commit 6323adf** - `feature/remove-auto-activation`

Removed automatic mode activation:
- Removed `(ecc-mode 1)` from `emacs-claude-code.el:73`
- Added `ecc-setup()` convenience function for easy activation
- Added documentation comment explaining manual activation required
- Package now follows Emacs conventions that modes are opt-in

Users now explicitly enable the mode via:
- `(ecc-mode 1)` in init file
- `M-x ecc-mode` interactively
- `M-x ecc-setup` convenience function