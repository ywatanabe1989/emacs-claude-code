;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 11:00:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-term-claude-mode-v2.el

;;; Commentary:
;;; Optimized vterm mode for Claude interaction (Version 2).
;;;
;;; This is a complete rewrite of the term-claude mode that leverages the
;;; refactored components for state detection, auto-response, buffer management,
;;; interaction, and setup logic. It provides a cleaner, more maintainable 
;;; implementation while enhancing the original functionality.
;;;
;;; Key features include:
;;; - Optimized terminal interaction with Claude AI
;;; - State detection for different Claude prompts
;;; - Auto-response for common prompts
;;; - Visual indicators and aids
;;; - Content extraction tools

(require 'vterm)
(require 'ecc-variables)
(require 'ecc-vterm-yank-as-file)
(require 'ecc-term-claude-state)
(require 'ecc-term-claude-auto)
(require 'ecc-term-claude-setup)
(require 'ecc-term-claude-buffer)
(require 'ecc-term-claude-interaction)

;;; Code:

;;;; Customization options

(defgroup ecc-term-claude nil
  "Optimized vterm mode for Claude interaction."
  :group 'ecc
  :prefix "ecc-term-claude-")

(defcustom ecc-term-claude-line-numbers nil
  "Whether to display line numbers in Claude vterm buffers.
Enabling line numbers can be helpful for referencing specific parts
of the conversation, but may impact performance with very large outputs."
  :type 'boolean
  :group 'ecc-term-claude)

(defcustom ecc-term-claude-scroll-conservatively 10000
  "Value for scroll-conservatively in Claude vterm buffers.
Higher values make scrolling smoother but may use more CPU.
The default value of 10000 provides a good balance between
smoothness and performance."
  :type 'integer
  :group 'ecc-term-claude)

(defcustom ecc-term-claude-truncate-lines t
  "Whether to truncate lines in Claude vterm buffers.
When enabled, long lines will not wrap to the next line, which can
improve performance and readability for Claude's responses."
  :type 'boolean
  :group 'ecc-term-claude)

(defcustom ecc-term-claude-buffer-name "*CLAUDE-VTERM*"
  "Default buffer name for new Claude vterm buffers.
This name is used when creating a new Claude vterm buffer."
  :type 'string
  :group 'ecc-term-claude)

;;;; Mode menu definition

(defvar ecc-term-claude-menu
  (let ((menu (make-sparse-keymap "Claude-VTerm")))
    ;; Basic commands
    (define-key menu [ecc-term-claude-clear-buffer]
      '(menu-item "Clear buffer" ecc-term-claude-clear-buffer
                  :help "Clear the vterm buffer"))
    (define-key menu [ecc-term-claude-separator-1]
      '(menu-item "--"))
    
    ;; Yank-as-file commands
    (define-key menu [ecc-vterm-yank-buffer-as-file]
      '(menu-item "Yank Buffer to File" ecc-vterm-yank-buffer-as-file
                  :help "Save entire buffer content to a file"))
    (define-key menu [ecc-vterm-yank-as-file]
      '(menu-item "Yank Region to File" ecc-vterm-yank-as-file
                  :help "Save selected region to a file"))
    (define-key menu [ecc-vterm-quick-yank-region]
      '(menu-item "Quick Yank Region" ecc-vterm-quick-yank-region
                  :help "Quickly save selected region with auto-generated filename"))
    (define-key menu [ecc-term-claude-separator-2]
      '(menu-item "--"))
    
    ;; Auto mode & responses
    (define-key menu [ecc-term-claude-toggle-auto-mode]
      '(menu-item "Toggle Auto-mode" ecc-term-claude-toggle-auto-mode
                  :help "Toggle automatic response to Claude prompts"
                  :button (:toggle . ecc-term-claude-auto-mode)))
    (define-key menu [ecc-term-claude-separator-3]
      '(menu-item "--"))
    (define-key menu [ecc-term-claude-send-yes]
      '(menu-item "Yes (y)" ecc-term-claude-send-yes
                  :help "Send 'y' to respond affirmatively"))
    (define-key menu [ecc-term-claude-send-no]
      '(menu-item "No (n)" ecc-term-claude-send-no
                  :help "Send 'n' to respond negatively"))
    (define-key menu [ecc-term-claude-separator-4]
      '(menu-item "--"))
    
    ;; Buffer management
    (define-key menu [ecc-term-claude-switch-to-buffer]
      '(menu-item "Switch to Claude Buffer" ecc-term-claude-switch-to-buffer
                  :help "Switch to another registered Claude buffer"))
    menu)
  "Menu for Claude-VTerm mode.")

;;;; Keybindings

(defvar ecc-term-claude-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map vterm-mode-map)
    
    ;; Claude-specific keybindings
    (define-key map (kbd "C-c C-y") 'ecc-term-claude-send-yes)
    (define-key map (kbd "C-c C-n") 'ecc-term-claude-send-no)
    (define-key map (kbd "C-c C-l") 'ecc-term-claude-clear-buffer)
    (define-key map (kbd "C-c C-a") 'ecc-term-claude-toggle-auto-mode)
    (define-key map (kbd "C-c C-v") 'ecc-term-claude-toggle-follow-bottom)
    (define-key map (kbd "C-c C-s") 'ecc-term-claude-switch-to-buffer)
    
    ;; Yank-as-file keybindings
    (define-key map (kbd "C-c C-f") 'ecc-vterm-yank-as-file)
    (define-key map (kbd "C-c C-b") 'ecc-vterm-yank-buffer-as-file)
    (define-key map (kbd "C-c C-q") 'ecc-vterm-quick-yank-region)
    
    ;; Add menu
    (define-key map [menu-bar claude-vterm] (cons "Claude" ecc-term-claude-menu))
    
    map)
  "Keymap for `ecc-term-claude-mode'.")

;;;; Hook variables

(defvar ecc-term-claude-update-functions nil
  "Functions to run after vterm output is updated.
Each function should take no arguments and will be called
whenever vterm produces new output.")

(defvar ecc-term-claude-mode-hook nil
  "Hook run after `ecc-term-claude-mode' is initialized.
This hook runs at the end of the mode setup, allowing
customization of the Claude vterm environment.")

;;;; Mode definition

(define-derived-mode ecc-term-claude-mode vterm-mode "Claude-VTerm"
  "Major mode for optimized Claude interaction in vterm.
This mode enhances the standard vterm with specialized features for
interacting with Claude AI, including state detection, automatic
responses to prompts, and better visual feedback.

Features include:
- State detection for different Claude prompts (Y/N, waiting, etc.)
- Optional automatic responses to prompts
- Visual indicators of Claude state
- Content extraction tools for saving Claude output
- Performance optimizations for large outputs

You can also use `ecc-term-claude-enable` in an existing vterm buffer
to apply Claude features without changing the major mode.

Key bindings:
\\{ecc-term-claude-mode-map}"
  ;; Apply performance optimizations
  (ecc-term-claude-setup-performance (current-buffer))
  
  ;; Apply common Claude features
  (ecc-term-claude-setup-common (current-buffer)))

;;;; Entry point functions

;;;###autoload
(defun ecc-term-claude-enable ()
  "Enable Claude features in the current vterm buffer.
This applies Claude state detection and auto-response functionality
without changing the major mode, making an existing vterm buffer
Claude-aware.

Errors:
  Signals an error if current buffer is not in vterm-mode."
  (interactive)
  (if (eq major-mode 'vterm-mode)
      (ecc-term-claude-setup-existing-buffer)
    (user-error "This command can only be used in vterm buffers")))

;;;###autoload
(defun ecc-term-claude ()
  "Create a new Claude vterm buffer or enhance an existing one.
If called from an existing vterm buffer, applies Claude settings 
to the current buffer without changing the major mode.
Otherwise, creates a new buffer with Claude vterm mode.

Returns:
  The new or enhanced buffer."
  (interactive)
  (cond
   ;; If we're already in a vterm buffer, apply Claude settings without changing the mode
   ((and (eq major-mode 'vterm-mode) 
         (not (eq major-mode 'ecc-term-claude-mode)))
    (ecc-term-claude-setup-existing-buffer)
    (current-buffer))
   
   ;; Default case: create a new Claude vterm buffer
   (t
    (let* ((buffer-name ecc-term-claude-buffer-name)
           (existing-buffer (get-buffer buffer-name))
           (new-buffer (or existing-buffer (get-buffer-create buffer-name))))
      (with-current-buffer new-buffer
        (unless (eq major-mode 'ecc-term-claude-mode)
          (ecc-term-claude-mode)))
      (switch-to-buffer new-buffer)
      new-buffer))))

;;;; Backward compatibility

;; Backward compatibility aliases for legacy function names
(defalias 'ecc-term-claude-auto-mode-toggle 'ecc-term-claude-toggle-auto-mode)
(defalias 'ecc-term-claude-yes 'ecc-term-claude-send-yes)
(defalias 'ecc-term-claude-no 'ecc-term-claude-send-no)
(defalias 'ecc-term-claude-clear 'ecc-term-claude-clear-buffer)

(provide 'ecc-term-claude-mode-v2)

;;; ecc-term-claude-mode-v2.el ends here