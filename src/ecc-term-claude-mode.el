;;; -*- coding: utf-8; lexical-binding: t -*-
;;; ecc-term-claude-mode.el --- Optimized vterm mode for Claude interaction
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 16:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-term-claude-mode.el

;;; Commentary:
;;; Optimized vterm mode for Claude interaction.
;;;
;;; This module is the standard implementation for Claude terminal interaction,
;;; providing optimized performance and comprehensive functionality.
;;;
;;; It provides a specialized major mode derived from vterm-mode that is
;;; tailored for interacting with the Claude AI assistant, offering:
;;; - Performance optimizations for high-volume streaming output
;;; - Auto-response functionality for Claude prompts (Y/N, continue, etc.)
;;; - Visual indicators for Claude state in the mode line
;;; - Follow-bottom scrolling for real-time interaction
;;; - Integration with state detection and auto-response systems
;;; - Yank-as-file functionality for saving Claude output
;;;
;;; Both a dedicated major mode and lightweight enhancement functions for
;;; existing vterm buffers are provided.

;;; Code:

(require 'vterm)
(require 'ecc-variables)
(require 'ecc-state-detection)
(require 'ecc-auto-response)
(require 'ecc-vterm-yank-as-file)

;;;; Customization

(defgroup ecc-term-claude nil
  "Optimized vterm mode for Claude interaction."
  :group 'ecc
  :prefix "ecc-term-claude-")

(defcustom ecc-term-claude-line-numbers nil
  "Whether to display line numbers in Claude vterm buffers."
  :type 'boolean
  :group 'ecc-term-claude)

(defcustom ecc-term-claude-scroll-conservatively 10000
  "Value for scroll-conservatively in Claude vterm buffers.
Higher values make scrolling smoother but may use more CPU."
  :type 'integer
  :group 'ecc-term-claude)

(defcustom ecc-term-claude-truncate-lines t
  "Whether to truncate lines in Claude vterm buffers.
When enabled, long lines will not wrap to the next line."
  :type 'boolean
  :group 'ecc-term-claude)

(defcustom ecc-term-claude-state-update-interval 1.0
  "Interval in seconds for updating Claude state.
Lower values make the UI more responsive but use more CPU."
  :type 'number
  :group 'ecc-term-claude)

(defcustom ecc-term-claude-auto-mode nil
  "When non-nil, automatically respond to Claude prompts.
This enables the auto-response system to handle yes/no prompts
and continuation prompts without manual intervention."
  :type 'boolean
  :group 'ecc-term-claude)

(defcustom ecc-term-claude-buffer-name "*CLAUDE-VTERM*"
  "Default buffer name for Claude vterm buffers."
  :type 'string
  :group 'ecc-term-claude)

(defcustom ecc-term-claude-show-state-in-mode-line t
  "Whether to show Claude state in the mode line.
When enabled, the current state (Y/N, Waiting, etc.) is shown
in the mode line to provide immediate feedback."
  :type 'boolean
  :group 'ecc-term-claude)

;;;; Internal Variables

(defvar ecc-term-claude--update-functions nil
  "Hook functions to run after vterm output is updated.
Each function should take no arguments and will be run
whenever vterm produces new output.")

(defvar ecc-term-claude--state-timer nil
  "Timer for updating the Claude state.
This is used to periodically check for Claude prompts.")

;;;; Mode Menu

(defvar ecc-term-claude--menu
  (let ((menu (make-sparse-keymap "Claude-VTerm")))
    ;; Basic commands
    (define-key menu [ecc-term-claude-clear]
      '(menu-item "Clear buffer" ecc-term-claude-clear
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
    (define-key menu [ecc-term-claude-auto-mode-toggle]
      '(menu-item "Toggle Auto-mode" ecc-term-claude-auto-mode-toggle
                  :help "Toggle automatic response to Claude prompts"
                  :button (:toggle . ecc-term-claude-auto-mode)))
    (define-key menu [ecc-term-claude-separator-3]
      '(menu-item "--"))
    (define-key menu [ecc-term-claude-yes]
      '(menu-item "Yes (y)" ecc-term-claude-yes
                  :help "Send 'y' to respond affirmatively"))
    (define-key menu [ecc-term-claude-no]
      '(menu-item "No (n)" ecc-term-claude-no
                  :help "Send 'n' to respond negatively"))
    menu)
  "Menu for Claude-VTerm mode.")

;;;; Keybindings

(defvar ecc-term-claude-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map vterm-mode-map)
    
    ;; Claude-specific keybindings
    (define-key map (kbd "C-c C-y") 'ecc-term-claude-yes)
    (define-key map (kbd "C-c C-n") 'ecc-term-claude-no)
    (define-key map (kbd "C-c C-l") 'ecc-term-claude-clear)
    (define-key map (kbd "C-c C-a") 'ecc-term-claude-auto-mode-toggle)
    (define-key map (kbd "C-c C-v") 'ecc-term-claude-toggle-follow-bottom)
    
    ;; Yank-as-file keybindings
    (define-key map (kbd "C-c C-f") 'ecc-vterm-yank-as-file)
    (define-key map (kbd "C-c C-b") 'ecc-vterm-yank-buffer-as-file)
    (define-key map (kbd "C-c C-q") 'ecc-vterm-quick-yank-region)
    
    ;; Add menu
    (define-key map [menu-bar claude-vterm] (cons "Claude" ecc-term-claude--menu))
    
    map)
  "Keymap for `ecc-term-claude-mode'.")

;;;; Mode Definition

(define-derived-mode ecc-term-claude-mode vterm-mode "Claude-VTerm"
  "Major mode for optimized Claude interaction in vterm.
This mode is optimized for high-performance streaming output.
You can also use `ecc-term-claude-enable` in an existing vterm buffer
to apply Claude features without changing the major mode.

Key bindings:
\\{ecc-term-claude-mode-map}"
  ;; Apply the mode setup
  (ecc-term-claude--setup-buffer))

;;;; Buffer Setup Functions

(defun ecc-term-claude--setup-buffer ()
  "Set up the current buffer for Claude vterm mode."
  ;; Disable line numbers for performance
  (ecc-term-claude--setup-display-options)
  
  ;; Apply performance optimizations
  (ecc-term-claude--setup-performance-options)
  
  ;; Register buffer
  (ecc-term-claude--register-buffer (current-buffer))
  
  ;; Set up visual indicators
  (ecc-term-claude--setup-mode-line)
  
  ;; Set up state detection timer
  (ecc-term-claude--setup-state-timer)
  
  ;; Connect to vterm hooks
  (ecc-term-claude--setup-hooks)
  
  ;; Add hook to clean up when buffer is killed
  (add-hook 'kill-buffer-hook 'ecc-term-claude--cleanup-buffer nil t))

(defun ecc-term-claude--setup-display-options ()
  "Set up display options for Claude vterm buffer."
  (when (and (boundp 'display-line-numbers-mode)
             (not ecc-term-claude-line-numbers))
    (display-line-numbers-mode -1)))

(defun ecc-term-claude--setup-performance-options ()
  "Set up performance-related options for Claude vterm buffer."
  (setq-local scroll-conservatively ecc-term-claude-scroll-conservatively
              scroll-margin 0
              scroll-step 1
              fast-but-imprecise-scrolling t
              truncate-lines ecc-term-claude-truncate-lines))

(defun ecc-term-claude--setup-mode-line ()
  "Set up mode line indicator for Claude state."
  (when ecc-term-claude-show-state-in-mode-line
    (setq mode-line-process
          '(:eval (ecc-term-claude--mode-line-state-indicator)))))

(defun ecc-term-claude--setup-state-timer ()
  "Set up the timer for checking Claude state."
  (when ecc-term-claude--state-timer
    (cancel-timer ecc-term-claude--state-timer))
  
  (setq ecc-term-claude--state-timer
        (run-with-timer 0 ecc-term-claude-state-update-interval
                      'ecc-term-claude-check-state)))

(defun ecc-term-claude--setup-hooks ()
  "Set up hooks for vterm output and other events."
  ;; Connect to vterm update functions
  (add-hook 'vterm-update-functions
            (lambda (&rest _)
              (run-hooks 'ecc-term-claude--update-functions))
            nil t)
  
  ;; Enable follow bottom by default
  (add-hook 'ecc-term-claude--update-functions
            'ecc-term-claude-follow-bottom-after-output))

(defun ecc-term-claude--setup-keys ()
  "Set up Claude-specific keybindings for a vterm buffer."
  (when (eq major-mode 'vterm-mode)
    ;; Basic functions
    (local-set-key (kbd "C-c C-y") 'ecc-term-claude-yes)
    (local-set-key (kbd "C-c C-n") 'ecc-term-claude-no)
    (local-set-key (kbd "C-c C-l") 'ecc-term-claude-clear)
    (local-set-key (kbd "C-c C-a") 'ecc-term-claude-auto-mode-toggle)
    (local-set-key (kbd "C-c C-v") 'ecc-term-claude-toggle-follow-bottom)
    
    ;; Yank-as-file functions
    (local-set-key (kbd "C-c C-f") 'ecc-vterm-yank-as-file)
    (local-set-key (kbd "C-c C-b") 'ecc-vterm-yank-buffer-as-file)
    (local-set-key (kbd "C-c C-q") 'ecc-vterm-quick-yank-region)))

;;;; State Detection

(defun ecc-term-claude--mode-line-state-indicator ()
  "Return mode line indicator for current Claude state."
  (let ((state (ecc-detect-state)))
    (cond
     ((eq state :waiting) " [Waiting]")
     ((eq state :y/n) " [Y/N]")
     ((eq state :y/y/n) " [Y/Y/N]")
     ((eq state :initial-waiting) " [Init]")
     (t ""))))

(defun ecc-term-claude-check-state ()
  "Check and update the state of the Claude VTERM buffer.
This is called periodically by the state timer."
  (interactive)
  (when (memq major-mode '(ecc-term-claude-mode vterm-mode))
    (let ((state (ecc-detect-state)))
      (force-mode-line-update)
      state)))

;;;; Claude Interaction Commands

(defun ecc-term-claude-yes ()
  "Send 'y' response to Claude prompt."
  (interactive)
  (vterm-send-string "y")
  (vterm-send-return))

(defun ecc-term-claude-no ()
  "Send 'n' response to Claude prompt."
  (interactive)
  (vterm-send-string "n")
  (vterm-send-return))

(defun ecc-term-claude-clear ()
  "Clear the vterm buffer."
  (interactive)
  (vterm-clear))

;;;; Auto-Response Functions

(defun ecc-term-claude-auto-mode-toggle ()
  "Toggle automatic response to Claude prompts."
  (interactive)
  (setq ecc-term-claude-auto-mode (not ecc-term-claude-auto-mode))
  (message "Claude auto-mode %s"
           (if ecc-term-claude-auto-mode "enabled" "disabled"))
  
  ;; Set up hooks for auto-responses
  (if ecc-term-claude-auto-mode
      (add-to-list 'ecc-term-claude--update-functions
                   'ecc-term-claude--auto-send-respond)
    (setq ecc-term-claude--update-functions
          (remove 'ecc-term-claude--auto-send-respond
                  ecc-term-claude--update-functions))))

(defun ecc-term-claude--auto-send-respond ()
  "Automatically respond to Claude prompts in vterm mode."
  (when ecc-term-claude-auto-mode
    (let ((state (ecc-detect-state)))
      (cond
       ((eq state :y/y/n)
        (ecc-term-claude--auto-send-y/y/n))
       ((eq state :y/n)
        (ecc-term-claude--auto-send-y/n))
       ((eq state :initial-waiting)
        (ecc-term-claude--auto-send-initial-waiting))
       ((eq state :waiting)
        (ecc-term-claude--auto-send-continue))))))

(defun ecc-term-claude--auto-send-y/n ()
  "Automatically respond with 'y' to Y/N prompts."
  (vterm-send-string ecc-auto-response-yes)
  (vterm-send-return)
  (message "Auto-responded: %s" ecc-auto-response-yes))

(defun ecc-term-claude--auto-send-y/y/n ()
  "Automatically respond with 'y' to Y/Y/N prompts."
  (vterm-send-string ecc-auto-response-yes-plus)
  (vterm-send-return)
  (message "Auto-responded: %s" ecc-auto-response-yes-plus))

(defun ecc-term-claude--auto-send-continue ()
  "Automatically respond to continue prompts."
  (vterm-send-string ecc-auto-response-continue)
  (vterm-send-return)
  (message "Auto-responded: %s" ecc-auto-response-continue))

(defun ecc-term-claude--auto-send-initial-waiting ()
  "Automatically respond to initial waiting prompts."
  (vterm-send-string ecc-auto-response-initial-waiting)
  (vterm-send-return)
  (message "Auto-responded: %s" ecc-auto-response-initial-waiting))

;;;; Buffer Management

(defun ecc-term-claude--cleanup-buffer ()
  "Clean up when buffer is killed."
  (when (memq major-mode '(ecc-term-claude-mode vterm-mode))
    ;; Cancel any timers
    (when ecc-term-claude--state-timer
      (cancel-timer ecc-term-claude--state-timer)
      (setq ecc-term-claude--state-timer nil))))

(defun ecc-term-claude--register-buffer (&optional buffer)
  "Register BUFFER as a Claude buffer.
If BUFFER is nil, use the current buffer."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (unless (buffer-live-p buf)
      (user-error "Buffer is not alive"))
    
    ;; Register the buffer with auto-response system
    (ecc-auto-response-register-buffer buf)
    
    ;; Return the buffer
    buf))

;;;; Follow Bottom Functionality

(defun ecc-term-claude-follow-bottom-after-output ()
  "Scroll to bottom after vterm produces new output.
Only has effect if `ecc-vterm-always-follow-bottom` is non-nil."
  (when ecc-vterm-always-follow-bottom
    (ecc-term-claude-scroll-to-bottom)))

(defun ecc-term-claude-scroll-to-bottom ()
  "Scroll to show the bottom of the buffer."
  (when (and ecc-vterm-always-follow-bottom
             (buffer-live-p (current-buffer)))
    (let ((window (get-buffer-window (current-buffer))))
      (when window
        (with-selected-window window
          (recenter (- (window-height) 
                     ecc-vterm-follow-bottom-margin
                     1)))))))

(defun ecc-term-claude-toggle-follow-bottom ()
  "Toggle the always-follow-bottom feature."
  (interactive)
  (setq ecc-vterm-always-follow-bottom 
        (not ecc-vterm-always-follow-bottom))
  (message "Always follow bottom %s"
           (if ecc-vterm-always-follow-bottom "enabled" "disabled"))
  (when ecc-vterm-always-follow-bottom
    (ecc-term-claude-scroll-to-bottom)))

;;;; Public APIs

(defun ecc-term-claude-enable ()
  "Enable Claude features in the current vterm buffer.
This applies Claude state detection and auto-response functionality
without changing the major mode."
  (interactive)
  (if (eq major-mode 'vterm-mode)
      (ecc-term-claude--setup-existing-buffer)
    (user-error "This command can only be used in vterm buffers")))

(defun ecc-term-claude--setup-existing-buffer ()
  "Set up current vterm buffer with Claude enhancements without changing major mode."
  (unless (eq major-mode 'vterm-mode)
    (user-error "Current buffer is not in vterm-mode"))
  
  ;; Apply the individual setup functions
  (ecc-term-claude--register-buffer)
  (ecc-term-claude--setup-mode-line)
  (ecc-term-claude--setup-state-timer)
  (ecc-term-claude--setup-hooks)
  (ecc-term-claude--setup-keys)
  (add-hook 'kill-buffer-hook 'ecc-term-claude--cleanup-buffer nil t)
  
  (message "Claude features applied to current vterm buffer"))

(defun ecc-term-claude ()
  "Create a new Claude vterm buffer with optimized settings.
If called from an existing vterm buffer, applies Claude settings to current buffer.
Otherwise, creates a new buffer with Claude vterm mode."
  (interactive)
  (cond
   ;; If we're already in a vterm buffer, apply Claude settings without changing the mode
   ((and (eq major-mode 'vterm-mode) 
         (not (eq major-mode 'ecc-term-claude-mode)))
    (ecc-term-claude--setup-existing-buffer)
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

;;;; Backward Compatibility

;; Function aliases for backward compatibility
(defalias 'ecc-register-buffer 'ecc-term-claude--register-buffer)
(defalias 'ecc-term-claude-auto-send-accept 'ecc-term-claude--auto-send-respond)
(defalias 'ecc-term-claude-auto-send-y/n 'ecc-term-claude--auto-send-y/n)
(defalias 'ecc-term-claude-auto-send-y/y/n 'ecc-term-claude--auto-send-y/y/n)
(defalias 'ecc-term-claude-auto-send-continue 'ecc-term-claude--auto-send-continue)
(defalias 'ecc-term-claude-auto-send-initial-waiting 'ecc-term-claude--auto-send-initial-waiting)
(defalias 'ecc-term-claude-setup-existing-buffer 'ecc-term-claude--setup-existing-buffer)
(defalias 'ecc-term-claude-setup-mode-line 'ecc-term-claude--setup-mode-line)
(defalias 'ecc-term-claude-mode-line-state-indicator 'ecc-term-claude--mode-line-state-indicator)
(defalias 'ecc-term-claude-setup-keys 'ecc-term-claude--setup-keys)
(defalias 'ecc-term-claude-cleanup-buffer 'ecc-term-claude--cleanup-buffer)

;; Compatibility with old variables
(defvaralias 'ecc-term-claude-update-functions 'ecc-term-claude--update-functions)
(defvaralias 'ecc-term-claude-state-timer 'ecc-term-claude--state-timer)
(defvaralias 'ecc-term-claude-menu 'ecc-term-claude--menu)

;; Provide the standard name and compatibility aliases
(provide 'ecc-term-claude-mode)
(provide 'ecc-term-claude-mode-consolidated) ;; For backward compatibility

;;; ecc-term-claude-mode.el ends here