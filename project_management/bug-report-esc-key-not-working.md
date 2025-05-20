# Bug Report: ESC Key Not Working to Interrupt Claude

## Description
The ESC key does not work to interrupt Claude in vterm mode, even though vterm-send-escape function works when called directly.

## Steps to Reproduce
1. Start a Claude session in vterm mode
2. Press ESC key to attempt to interrupt Claude
3. Observe that nothing happens

## Expected Behavior
Pressing ESC should interrupt Claude and disable auto-response mode.

## Current Behavior
Pressing ESC has no effect. However, running `M-x vterm-send-escape` does work to send the escape character.

## Possible Causes
1. The ESC key binding is not properly set up in the vterm mode map
2. There might be conflicts with other key bindings for ESC
3. The ecc-esc-disable-auto-response function might not be properly implemented or connected

## Related Code
In `ecc-auto-response-fix.el`, there's a function designed to handle this:

```elisp
(defun ecc-esc-disable-auto-response ()
  "Disable auto-response when ESC is pressed."
  (interactive)
  (when (and ecc-auto-response-esc-disables
             (boundp 'ecc-buffer-auto-response-enabled)
             ecc-buffer-auto-response-enabled)
    (vterm-send-escape)
    (ecc-stop-auto-response)
    (message "Auto-response disabled by ESC"))
  ;; Call the regular ESC function
  (call-interactively 'keyboard-escape-quit))

;; Setup ESC key to disable auto-response in vterm mode
(defun ecc-setup-esc-key-in-vterm ()
  "Setup ESC key to disable auto-response in vterm mode."
  (when (boundp 'vterm-mode-map)
    (define-key vterm-mode-map [escape] 'ecc-esc-disable-auto-response)))

(with-eval-after-load 'vterm
  (ecc-setup-esc-key-in-vterm))
```

## Impact
Users are unable to easily interrupt Claude using the ESC key, which is a standard way to cancel operations or escape from the current context in many interfaces. This reduces the usability of the Claude interface.

## Priority and Severity
**Priority: Medium**
The issue affects usability but doesn't prevent core functionality from working, as users can still use other methods to interrupt Claude or disable auto-response.

**Reasoning:**
1. There's a workaround (using M-x vterm-send-escape)
2. The issue doesn't completely prevent using Claude
3. It's likely a simple fix involving keybinding setup