;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 08:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-vterm/test-ecc-vterm-yank-integration.el

;;; Commentary:
;;; Tests for the integration of vterm yank-as-file with Claude mode

(require 'ert)

;; Mock vterm if not available
(unless (featurep 'vterm)
  (defvar vterm-timer-delay 0.01)
  (defvar vterm-max-scrollback 1000)
  (defvar display-line-numbers-mode nil)
  (defvar vterm-disable-bold-font nil)
  (defvar vterm-disable-underline nil)
  (defun vterm-send-string (_string) nil)
  (defun vterm-send-return () nil)
  (defvar font-lock-mode nil)
  (defun font-lock-mode (_) nil)
  (defun display-line-numbers-mode (_) nil)
  (defvar vterm-mode-map (make-sparse-keymap))
  (define-derived-mode vterm-mode nil "VTerm")
  (provide 'vterm))

(require 'ecc-variables)

;; Load the files under test
(when (file-exists-p "/home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-vterm-yank-as-file.el")
  (load-file "/home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-vterm-yank-as-file.el"))

(when (file-exists-p "/home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-term-claude-mode.el")
  (load-file "/home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-term-claude-mode.el"))

;; Mock test data
(defvar ecc-test-vterm-content 
  "Claude: Here's an example Python function that calculates Fibonacci numbers:

```python
def fibonacci(n):
    if n <= 0:
        return 0
    elif n == 1:
        return 1
    else:
        return fibonacci(n-1) + fibonacci(n-2)

# Example usage
for i in range(10):
    print(f\"Fibonacci({i}) = {fibonacci(i)}\")
```

Would you like me to explain how this works?")

;; Mock buffer functions
(defun ecc-test-buffer-substring (start end)
  "Mock `buffer-substring-no-properties' to return test content."
  ecc-test-vterm-content)

(defun ecc-test-region-active-p ()
  "Mock function for `region-active-p' returning true."
  t)

(defun ecc-test-region-beginning ()
  "Mock function for `region-beginning' returning 1."
  1)

(defun ecc-test-region-end ()
  "Mock function for `region-end' returning length of test content."
  (1+ (length ecc-test-vterm-content)))

;; Tests for the integration of yank-as-file with Claude mode
(ert-deftest test-ecc-vterm-claude-mode-integration ()
  "Test that yank-as-file commands are properly integrated with Claude mode."
  ;; Skip if required functions don't exist
  (skip-unless (and (fboundp 'ecc-term-claude-mode)
                     (fboundp 'ecc-vterm-yank-as-file)))
  
  ;; Check keybindings in the Claude mode map
  (let ((map ecc-term-claude-mode-map))
    (should (eq (lookup-key map (kbd "C-c C-f")) 'ecc-vterm-yank-as-file))
    (should (eq (lookup-key map (kbd "C-c C-b")) 'ecc-vterm-yank-buffer-as-file))
    (should (eq (lookup-key map (kbd "C-c C-q")) 'ecc-vterm-quick-yank-region))))

;; Test the menu integration
(ert-deftest test-ecc-vterm-claude-menu-integration ()
  "Test that yank-as-file commands are in the Claude vterm menu."
  ;; Skip if variable doesn't exist
  (skip-unless (boundp 'ecc-term-claude-menu))
  
  (let ((menu ecc-term-claude-menu))
    ;; Check that menu items exist for yank functions
    (should (get-buffer-window-list (get-buffer-create " *Minibuf-0*")))
    (should (lookup-key menu [ecc-vterm-yank-as-file]))
    (should (lookup-key menu [ecc-vterm-yank-buffer-as-file]))
    (should (lookup-key menu [ecc-vterm-quick-yank-region]))))

;; Test the setup-keys function
(ert-deftest test-ecc-term-claude-setup-keys-yank ()
  "Test that the setup-keys function adds yank-as-file keybindings."
  ;; Skip if function doesn't exist
  (skip-unless (fboundp 'ecc-term-claude-setup-keys))
  
  ;; Create a vterm-mode buffer to test local keybindings
  (with-temp-buffer
    ;; Make this buffer appear to be in vterm-mode
    (setq major-mode 'vterm-mode)
    
    ;; Call the function to setup keys
    (ecc-term-claude-setup-keys)
    
    ;; Check that local keybindings were set correctly
    (should (eq (local-key-binding (kbd "C-c C-f")) 'ecc-vterm-yank-as-file))
    (should (eq (local-key-binding (kbd "C-c C-b")) 'ecc-vterm-yank-buffer-as-file))
    (should (eq (local-key-binding (kbd "C-c C-q")) 'ecc-vterm-quick-yank-region))))

;; Test the actual file generation when using the mode
(ert-deftest test-ecc-vterm-claude-mode-yank-file-generation ()
  "Test that files can be generated from Claude mode buffer."
  ;; Skip if required functions don't exist
  (skip-unless (and (fboundp 'ecc-term-claude-mode)
                     (fboundp 'ecc-vterm-yank-as-file)
                     (fboundp 'ecc-vterm-detect-file-type)))
  
  ;; Create a temp directory for test output
  (let* ((temp-dir (make-temp-file "ecc-test-" t))
         (default-directory temp-dir)
         (test-filename "test_output.py")
         (full-path (expand-file-name test-filename temp-dir)))
    
    ;; Mock functions
    (cl-letf (((symbol-function 'buffer-substring-no-properties) 
               #'ecc-test-buffer-substring)
              ((symbol-function 'region-active-p) 
               #'ecc-test-region-active-p)
              ((symbol-function 'region-beginning) 
               #'ecc-test-region-beginning)
              ((symbol-function 'region-end) 
               #'ecc-test-region-end)
              ((symbol-function 'read-file-name) 
               (lambda (&rest _) test-filename))
              ((symbol-function 'y-or-n-p) 
               (lambda (&rest _) nil)))
      
      ;; Test file type detection - should detect Python from test content
      (should (string= "py" (ecc-vterm-detect-file-type ecc-test-vterm-content)))
      
      ;; Test file generation
      (should (stringp (ecc-vterm-generate-file-from-region 
                        (ecc-test-region-beginning)
                        (ecc-test-region-end)
                        test-filename)))
      
      ;; Verify file was created
      (should (file-exists-p full-path))
      
      ;; Verify content
      (with-temp-buffer
        (insert-file-contents full-path)
        (should (string= ecc-test-vterm-content (buffer-string)))))
    
    ;; Cleanup
    (when (file-exists-p full-path)
      (delete-file full-path))
    (when (file-exists-p temp-dir)
      (delete-directory temp-dir t))))

(provide 'test-ecc-vterm-yank-integration)

;;; test-ecc-vterm-yank-integration.el ends here