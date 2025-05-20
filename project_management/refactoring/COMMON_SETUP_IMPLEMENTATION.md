# Common Setup Logic Extraction

## Current Implementation

The current implementation in `ecc-term-claude-mode.el` duplicates setup logic in multiple places:

1. In the `ecc-term-claude-mode` definition (lines 115-159):

```elisp
(define-derived-mode ecc-term-claude-mode vterm-mode "Claude-VTerm"
  "Major mode for optimized Claude interaction in vterm.
This mode is optimized for high-performance streaming output.
You can also use `ecc-term-claude` in an existing vterm buffer
to apply Claude features without changing the major mode.

Key bindings:
\\{ecc-term-claude-mode-map}"
  ;; Disable line numbers for performance
  (when (and (boundp 'display-line-numbers-mode)
             (not ecc-term-claude-line-numbers))
    (display-line-numbers-mode -1))
  
  ;; Performance optimizations
  (setq-local scroll-conservatively ecc-term-claude-scroll-conservatively
              scroll-margin 0
              scroll-step 1
              fast-but-imprecise-scrolling t
              truncate-lines ecc-term-claude-truncate-lines)
  
  ;; Register buffer
  (ecc-register-buffer (current-buffer))
  
  ;; Set up visual indicators
  (ecc-term-claude-setup-mode-line)
  
  ;; Set up state detection timer
  (when ecc-term-claude-state-timer
    (cancel-timer ecc-term-claude-state-timer))
  
  (setq ecc-term-claude-state-timer
        (run-with-timer 0 ecc-term-claude-state-update-interval
                        'ecc-term-claude-check-state))
  
  ;; Connect to vterm hooks
  (add-hook 'vterm-update-functions
            (lambda (&rest _)
              (run-hooks 'ecc-term-claude-update-functions)))
  
  ;; Enable follow bottom by default
  (add-hook 'ecc-term-claude-update-functions
            'ecc-term-claude-follow-bottom-after-output)
  
  ;; Add hook to clean up when buffer is killed
  (add-hook 'kill-buffer-hook 'ecc-term-claude-cleanup-buffer nil t))
```

2. In the `ecc-term-claude-setup-existing-buffer` function (lines 357-391):

```elisp
(defun ecc-term-claude-setup-existing-buffer ()
  "Setup current vterm buffer with Claude enhancements without changing major mode."
  (unless (eq major-mode 'vterm-mode)
    (user-error "Current buffer is not in vterm-mode"))
  
  ;; Register buffer as a Claude buffer
  (ecc-register-buffer)
  
  ;; Set up visual indicators
  (ecc-term-claude-setup-mode-line)
  
  ;; Set up state detection timer
  (when ecc-term-claude-state-timer
    (cancel-timer ecc-term-claude-state-timer))
  (setq ecc-term-claude-state-timer
        (run-with-timer 0 ecc-term-claude-state-update-interval
                        'ecc-term-claude-check-state))
  
  ;; Connect to vterm hooks
  (add-hook 'vterm-update-functions
            (lambda (&rest _)
              (run-hooks 'ecc-term-claude-update-functions))
            nil t)
  
  ;; Enable follow bottom by default
  (add-hook 'ecc-term-claude-update-functions
            'ecc-term-claude-follow-bottom-after-output)
  
  ;; Add hook to clean up when buffer is killed
  (add-hook 'kill-buffer-hook 'ecc-term-claude-cleanup-buffer nil t)
  
  ;; Setup local keybindings
  (ecc-term-claude-setup-keys)
  
  (message "Claude features applied to current vterm buffer"))
```

## Problems with Current Implementation

1. **Duplication**: The same setup steps appear in both the mode definition and the setup function
2. **Inconsistent Parameters**: Some hooks use different parameters in different places
3. **No Clear Separation**: Mode-specific setup is mixed with general Claude features setup
4. **Maintenance Challenges**: Changes need to be made in multiple places
5. **Lack of Reusability**: Common setup can't be easily applied to other contexts

## Proposed Refactored Implementation

```elisp
;;;; Setup and initialization functions

(defun ecc-term-claude-setup-performance (buffer)
  "Set up performance optimizations for Claude in BUFFER."
  (with-current-buffer buffer
    ;; Disable line numbers for performance if appropriate
    (when (and (boundp 'display-line-numbers-mode)
               (not ecc-term-claude-line-numbers))
      (display-line-numbers-mode -1))
    
    ;; Apply performance-oriented settings
    (setq-local scroll-conservatively ecc-term-claude-scroll-conservatively
                scroll-margin 0
                scroll-step 1
                fast-but-imprecise-scrolling t
                truncate-lines ecc-term-claude-truncate-lines)))

(defun ecc-term-claude-setup-timer (&optional buffer)
  "Set up or reset the state detection timer for BUFFER or current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (when ecc-term-claude-state-timer
      (cancel-timer ecc-term-claude-state-timer))
    
    (setq ecc-term-claude-state-timer
          (run-with-timer 0 ecc-term-claude-state-update-interval
                         'ecc-term-claude-check-state))))

(defun ecc-term-claude-setup-hooks (&optional buffer)
  "Set up hooks for Claude term mode in BUFFER or current buffer."
  (with-current-buffer (or buffer (current-buffer))
    ;; Connect to vterm hooks
    (add-hook 'vterm-update-functions
              (lambda (&rest _)
                (run-hooks 'ecc-term-claude-update-functions))
              nil t)
    
    ;; Enable follow bottom by default
    (add-hook 'ecc-term-claude-update-functions
              'ecc-term-claude-follow-bottom-after-output)
    
    ;; Add hook to clean up when buffer is killed
    (add-hook 'kill-buffer-hook 'ecc-term-claude-cleanup-buffer nil t)))

(defun ecc-term-claude-setup-common (buffer)
  "Set up common Claude features in BUFFER.
This function applies the essential Claude features to a vterm buffer,
setting up the core infrastructure needed for Claude-specific functionality."
  (with-current-buffer buffer
    ;; Register buffer as a Claude buffer
    (ecc-register-buffer)
    
    ;; Set up visual indicators
    (ecc-term-claude-setup-mode-line)
    
    ;; Set up state detection timer
    (ecc-term-claude-setup-timer)
    
    ;; Connect to vterm hooks
    (ecc-term-claude-setup-hooks)))

;; Mode definition using extracted functions
(define-derived-mode ecc-term-claude-mode vterm-mode "Claude-VTerm"
  "Major mode for optimized Claude interaction in vterm.
This mode is optimized for high-performance streaming output.
You can also use `ecc-term-claude` in an existing vterm buffer
to apply Claude features without changing the major mode.

Key bindings:
\\{ecc-term-claude-mode-map}"
  ;; Apply performance optimizations
  (ecc-term-claude-setup-performance (current-buffer))
  
  ;; Apply common Claude features
  (ecc-term-claude-setup-common (current-buffer)))

;; Existing buffer setup using extracted functions
(defun ecc-term-claude-setup-existing-buffer ()
  "Setup current vterm buffer with Claude enhancements without changing major mode."
  (unless (eq major-mode 'vterm-mode)
    (user-error "Current buffer is not in vterm-mode"))
  
  ;; Apply common Claude features
  (ecc-term-claude-setup-common (current-buffer))
  
  ;; Setup local keybindings
  (ecc-term-claude-setup-keys)
  
  (message "Claude features applied to current vterm buffer"))
```

## Benefits of Refactored Approach

1. **Eliminated Duplication**: Common setup logic is defined in dedicated functions
2. **Consistent Parameters**: Each setup function takes an optional buffer parameter
3. **Clear Separation**: Each function has a specific responsibility
4. **Improved Maintenance**: Changes can be made in a single place
5. **Enhanced Reusability**: Setup functions can be used in different contexts
6. **Better Documentation**: Each function has a clear purpose and documentation

## Additional Notes on Implementation

The refactored approach uses these principles:

1. **Single Responsibility**: Each function does one thing
2. **Parameterization**: Functions accept buffer to support different contexts
3. **Composition**: Smaller functions are composed into larger ones
4. **Consistent Naming**: All setup functions follow `ecc-term-claude-setup-*` pattern

## Backward Compatibility Considerations

The refactored approach maintains backward compatibility:
- External-facing functions like `ecc-term-claude-mode` and `ecc-term-claude-setup-existing-buffer` keep the same interface
- Internal implementation changes are contained within the module
- No changes to the behavior or API

## Testing Approach

The refactored implementation should be tested to ensure:

1. Buffer registration works properly
2. Mode line indicators update correctly
3. Timer setup functions as expected
4. Hooks are added correctly
5. Performance settings are applied
6. The mode behaves identically to the original implementation