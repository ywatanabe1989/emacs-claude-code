;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 19:30:30>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-term/ecc-term-run.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;; Required dependencies
(require 'ecc-variables)
(require 'ecc-term-variables)

;; Check if vterm mode is available
(defvar ecc-term-run--vterm-available
  (condition-case nil 
      (progn (require 'ecc-term-vterm-mode) t)
    (error nil))
  "Whether the Claude vterm mode is available.")

;;;###autoload
(defun ecc-term-run-claude ()
  "Run Claude in a VTerm buffer with optimized settings.
This function creates a new VTerm buffer configured specifically
for Claude interaction, with performance optimizations and
Claude-specific features enabled.

Falls back to a simpler mode if VTerm is not available."
  (interactive)
  (if (not ecc-term-run--vterm-available)
      (ecc-term-run-fallback)
    (let ((buffer (ecc-term-vterm)))
      (with-current-buffer buffer
        ;; Display welcome message in the buffer
        (when (fboundp 'vterm-send-string)
          (vterm-send-string "echo \"Welcome to Claude via VTerm Mode!\"\n")
          (vterm-send-string "echo \"Use C-c C-a to toggle auto-response mode\"\n")
          (vterm-send-string "echo \"Use C-c C-y for Yes, C-c C-n for No\"\n")
          (vterm-send-string "echo \"Use C-c C-c to interrupt Claude\"\n")
          (vterm-send-string "claude\n")))
      ;; Return the buffer
      buffer)))

(defun ecc-term-run-fallback ()
  "Fallback function when VTerm is not available.
Creates a buffer with instructions for using Claude without VTerm."
  (interactive)
  (let ((buffer (get-buffer-create "*Claude Help*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Claude VTerm Mode is not available\n")
      (insert "============================\n\n")
      (insert "VTerm integration requires:\n")
      (insert "1. The vterm package installed in Emacs\n")
      (insert "2. The Claude CLI tool installed on your system\n\n")
      (insert "To install vterm:\n")
      (insert "M-x package-install RET vterm RET\n\n")
      (insert "To install the Claude CLI:\n")
      (insert "Follow instructions at https://github.com/anthropics/claude-cli\n\n")
      (insert "For more information, see the docs/vterm-mode.md file in the emacs-claude-code repository.\n")
      (insert "\n")
      (insert "Press 'q' to close this buffer.\n")
      
      ;; Set up mode for this buffer
      (special-mode)
      (setq buffer-read-only t))
    (switch-to-buffer buffer)
    buffer))

;;;###autoload
(defun ecc-term-run-help ()
  "Display help information for Claude VTerm mode.
Shows relevant information based on whether VTerm mode is available."
  (interactive)
  (with-help-window "*Claude VTerm Help*"
    (princ "Claude VTerm Mode Help\n")
    (princ "====================\n\n")
    
    (if (not ecc-term-run--vterm-available)
        (progn
          (princ "VTerm integration is not currently available.\n\n")
          (princ "Requirements:\n")
          (princ "  1. The vterm package installed in Emacs\n")
          (princ "  2. The Claude CLI tool installed on your system\n\n")
          (princ "To install vterm:\n")
          (princ "  M-x package-install RET vterm RET\n\n")
          (princ "To install the Claude CLI:\n")
          (princ "  Follow instructions at https://github.com/anthropics/claude-cli\n\n")
          (princ "For more information, see the docs/vterm-mode.md file in the emacs-claude-code repository.\n"))
      
      ;; VTerm is available - show regular help
      (progn
        (princ "Key Bindings:\n")
        (princ "  C-c C-k or C-c C-c : Interrupt Claude\n")
        (princ "  C-c C-y            : Send 'y' (yes) response\n")
        (princ "  C-c C-n            : Send 'n' (no) response\n")
        (princ "  C-c C-r            : Send 'r' (retry) response\n")
        (princ "  C-c C-l            : Clear buffer\n")
        (princ "  C-c C-a            : Toggle auto-response mode\n")
        (princ "  C-c C-p or C-c C-b : Previous Claude buffer\n")
        (princ "  C-c C-f            : Next Claude buffer\n")
        (princ "  C-c C-t            : Create new Claude buffer\n\n")
        
        (princ "Mode Line Indicators:\n")
        (princ "  [Waiting]   : Claude is waiting for continuation\n")
        (princ "  [Y/N]       : Claude is prompting for yes/no\n")
        (princ "  [Y/Y/N]     : Claude is prompting for complex choice\n")
        (princ "  [Running]   : Claude is processing\n")
        (princ "  [Continue?] : Claude is asking to continue\n\n")
        
        (princ "Auto-Response Mode:\n")
        (princ "  When enabled, automatically responds to prompts:\n")
        (princ "  - Sends 'continue' to continuation prompts\n")
        (princ "  - Sends 'y' to yes/no prompts\n")
        (princ "  Toggle with C-c C-a\n\n")
        
        (princ "Buffer Management:\n")
        (princ "  - Create multiple Claude sessions with C-c C-t\n")
        (princ "  - Navigate between them with C-c C-p and C-c C-f\n")))))

(provide 'ecc-term-run)

(when
    (not load-file-name)
  (message "ecc-term-run.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))