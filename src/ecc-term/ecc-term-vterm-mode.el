;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 19:25:30>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-term/ecc-term-vterm-mode.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;; Required dependencies
(require 'ecc-variables)
(require 'ecc-state-variables)
(require 'ecc-buffer-variables)
(require 'ecc-term-variables)
(require 'ecc-state)
(require 'ecc-buffer-registry)

;; Mode definition
(define-derived-mode ecc-term-vterm-mode 
  (if ecc-term--vterm-available 'vterm-mode 'special-mode)
  "Claude-VTerm"
  "Major mode for optimized Claude interaction in vterm.
This mode is optimized for high-performance streaming output.

Key bindings:
\\{ecc-term-vterm-mode-map}"
  ;; Disable line numbers
  (when (and (boundp 'display-line-numbers-mode)
             (not ecc-term-vterm-line-numbers))
    (display-line-numbers-mode -1))
  
  ;; Performance optimizations
  (setq-local scroll-conservatively ecc-term-vterm-scroll-conservatively
              scroll-margin 0
              scroll-step 1
              fast-but-imprecise-scrolling t
              truncate-lines ecc-term-vterm-truncate-lines)
  
  ;; Update prompt detection patterns
  (ecc-term-vterm-update-prompt-patterns)
  
  ;; Visual indicators for Claude state
  (ecc-term-vterm-setup-mode-line)
  
  ;; Register in buffer registry
  (ecc-buffer-register-buffer (current-buffer))
  
  ;; Make this buffer the active Claude buffer
  (setq ecc-buffer--active-buffer (current-buffer))
  (setq ecc-buffer-current-buffer (current-buffer))
  
  ;; Set up state detection timer
  (when ecc-term--state-timer
    (cancel-timer ecc-term--state-timer))
  
  (setq ecc-term--state-timer
        (run-with-timer 0 ecc-term-state-update-interval
                        'ecc-term-vterm-check-state))
  
  ;; Set up auto-mode if enabled
  (when ecc-term-vterm-auto-mode
    (add-to-list 'ecc-term--vterm-update-functions
                'ecc-term-vterm-auto-send-accept))
  
  ;; Connect to vterm hooks if available
  (when ecc-term--vterm-available
    (add-hook 'vterm-update-functions
              (lambda (&rest _)
                (run-hooks 'ecc-term--vterm-update-functions))))
  
  ;; Add hook to unregister buffer when killed
  (add-hook 'kill-buffer-hook 'ecc-term-vterm-cleanup-buffer nil t))

;; Mode menu
(defvar ecc-term-vterm-menu
  (let ((menu (make-sparse-keymap "Claude-VTerm")))
    ;; Buffer navigation submenu
    (let ((submenu (make-sparse-keymap "Buffer Navigation")))
      (define-key submenu [ecc-term-vterm]
        '(menu-item "New Buffer" ecc-term-vterm
                    :help "Create a new Claude VTERM buffer"))
      (define-key submenu [ecc-term-vterm-next-buffer]
        '(menu-item "Next Buffer" ecc-term-vterm-next-buffer
                    :help "Switch to the next Claude buffer"))
      (define-key submenu [ecc-term-vterm-prev-buffer]
        '(menu-item "Previous Buffer" ecc-term-vterm-prev-buffer
                    :help "Switch to the previous Claude buffer"))
      (define-key menu [ecc-buffer-navigation]
        (cons "Buffer Navigation" submenu)))
    
    (define-key menu [ecc-term-vterm-separator-0]
      '(menu-item "--"))
    
    (define-key menu [ecc-term-vterm-clear]
      '(menu-item "Clear buffer" ecc-term-vterm-clear
                  :help "Clear the vterm buffer"))
    (define-key menu [ecc-term-vterm-separator-1]
      '(menu-item "--"))
    (define-key menu [ecc-term-vterm-auto-mode-toggle]
      '(menu-item "Toggle Auto-mode" ecc-term-vterm-auto-mode-toggle
                  :help "Toggle automatic response to Claude prompts"
                  :button (:toggle . ecc-term-vterm-auto-mode)))
    (define-key menu [ecc-term-vterm-separator-2]
      '(menu-item "--"))
    (define-key menu [ecc-term-vterm-retry]
      '(menu-item "Retry (r)" ecc-term-vterm-retry
                  :help "Send 'r' to retry current operation"))
    (define-key menu [ecc-term-vterm-no]
      '(menu-item "No (n)" ecc-term-vterm-no
                  :help "Send 'n' to respond negatively"))
    (define-key menu [ecc-term-vterm-yes]
      '(menu-item "Yes (y)" ecc-term-vterm-yes
                  :help "Send 'y' to respond affirmatively"))
    (define-key menu [ecc-term-vterm-separator-3]
      '(menu-item "--"))
    (define-key menu [ecc-term-vterm-interrupt]
      '(menu-item "Interrupt" ecc-term-vterm-interrupt
                  :help "Interrupt the current Claude process"))
    menu)
  "Menu for Claude-VTerm mode.")

;; Keybindings for Claude VTERM mode
(defvar ecc-term-vterm-mode-map
  (let ((map (make-sparse-keymap)))
    ;; If vterm is available, inherit its keymap
    (when ecc-term--vterm-available
      (set-keymap-parent map vterm-mode-map))
    
    ;; Claude-specific keybindings
    (define-key map (kbd "C-c C-k") 'ecc-term-vterm-interrupt)
    (define-key map (kbd "C-c C-c") 'ecc-term-vterm-interrupt)
    (define-key map (kbd "C-c C-y") 'ecc-term-vterm-yes)
    (define-key map (kbd "C-c C-n") 'ecc-term-vterm-no)
    (define-key map (kbd "C-c C-r") 'ecc-term-vterm-retry)
    (define-key map (kbd "C-c C-l") 'ecc-term-vterm-clear)
    (define-key map (kbd "C-c C-a") 'ecc-term-vterm-auto-mode-toggle)
    
    ;; Buffer navigation keybindings
    (define-key map (kbd "C-c C-p") 'ecc-term-vterm-prev-buffer)
    (define-key map (kbd "C-c C-f") 'ecc-term-vterm-next-buffer)
    (define-key map (kbd "C-c C-b") 'ecc-term-vterm-prev-buffer)
    (define-key map (kbd "C-c C-t") 'ecc-term-vterm)
    
    ;; Add menu
    (define-key map [menu-bar claude-vterm] (cons "Claude" ecc-term-vterm-menu))
    
    map)
  "Keymap for `ecc-term-vterm-mode'.")

;; Mode-line indicator for Claude state
(defun ecc-term-vterm-setup-mode-line ()
  "Set up mode line indicator for Claude state."
  (setq mode-line-process 
        '(:eval (ecc-term-vterm-mode-line-state-indicator))))

(defun ecc-term-vterm-mode-line-state-indicator ()
  "Return mode line indicator for current Claude state."
  (let ((state (ecc-state-get)))
    (cond
     ((eq state :waiting) " [Waiting]")
     ((eq state :y/n) " [Y/N]")
     ((eq state :y/y/n) " [Y/Y/N]")
     ((eq state :initial-waiting) " [Continue?]")
     ((eq state :running) " [Running]")
     (t ""))))

;; State detection integration
(defun ecc-term-vterm-check-state ()
  "Check and update the state of the Claude VTERM buffer.
This function is meant to be run periodically to update the mode line."
  (interactive)
  (when (eq major-mode 'ecc-term-vterm-mode)
    (let ((state (ecc-state-get)))
      (force-mode-line-update)
      state)))

;; Claude interaction functions
(defun ecc-term-vterm-interrupt ()
  "Interrupt the current Claude process."
  (interactive)
  (if ecc-term--vterm-available
      (progn
        (vterm-send-string "\C-c")
        (sit-for 0.3)
        (vterm-send-string "\C-c"))
    (message "Interrupt not available without vterm")))

(defun ecc-term-vterm-yes ()
  "Send 'y' response to Claude prompt."
  (interactive)
  (if ecc-term--vterm-available
      (progn
        (vterm-send-string "y")
        (vterm-send-return))
    (message "Yes response not available without vterm")))

(defun ecc-term-vterm-no ()
  "Send 'n' response to Claude prompt."
  (interactive)
  (if ecc-term--vterm-available
      (progn
        (vterm-send-string "n")
        (vterm-send-return))
    (message "No response not available without vterm")))

(defun ecc-term-vterm-retry ()
  "Send 'r' response to Claude prompt for retry."
  (interactive)
  (if ecc-term--vterm-available
      (progn
        (vterm-send-string "r")
        (vterm-send-return))
    (message "Retry response not available without vterm")))

(defun ecc-term-vterm-clear ()
  "Clear the vterm buffer."
  (interactive)
  (if ecc-term--vterm-available
      (vterm-clear)
    (erase-buffer)))

;; Auto-response functions
(defun ecc-term-vterm-auto-send-accept ()
  "Automatically respond to Claude prompts in vterm mode."
  (when ecc-term-vterm-auto-mode
    (let ((state (ecc-state-get)))
      (cond
       ((eq state :y/y/n)
        (ecc-term-vterm-auto-send-y/y/n))
       ((eq state :y/n)
        (ecc-term-vterm-auto-send-y/n))
       ((eq state :waiting)
        (ecc-term-vterm-auto-send-continue))
       ((eq state :initial-waiting)
        (ecc-term-vterm-auto-send-continue))))))

(defun ecc-term-vterm-auto-send-y/n ()
  "Automatically respond with 'y' to Y/N prompts."
  (when ecc-term--vterm-available
    (vterm-send-string "y")
    (vterm-send-return)
    (message "Auto-responded: y")))

(defun ecc-term-vterm-auto-send-y/y/n ()
  "Automatically respond with 'y' to Y/Y/N prompts."
  (when ecc-term--vterm-available
    (vterm-send-string "y")
    (vterm-send-return)
    (message "Auto-responded: y")))

(defun ecc-term-vterm-auto-send-continue ()
  "Automatically respond to continue prompts."
  (when ecc-term--vterm-available
    (vterm-send-string "continue")
    (vterm-send-return)
    (message "Auto-responded: continue")))

;; Toggle auto-mode
(defun ecc-term-vterm-auto-mode-toggle ()
  "Toggle automatic response to Claude prompts."
  (interactive)
  (setq ecc-term-vterm-auto-mode (not ecc-term-vterm-auto-mode))
  (message "Claude auto-mode %s"
           (if ecc-term-vterm-auto-mode "enabled" "disabled"))
  
  ;; Set up hooks for auto-responses
  (if ecc-term-vterm-auto-mode
      (add-to-list 'ecc-term--vterm-update-functions
                  'ecc-term-vterm-auto-send-accept)
    (setq ecc-term--vterm-update-functions
          (remove 'ecc-term-vterm-auto-send-accept
                  ecc-term--vterm-update-functions))))

;; Buffer management functions
(defun ecc-term-vterm-cleanup-buffer ()
  "Unregister buffer from Claude buffer registry when killed."
  (when (eq major-mode 'ecc-term-vterm-mode)
    ;; Cancel any timers
    (when ecc-term--state-timer
      (cancel-timer ecc-term--state-timer)
      (setq ecc-term--state-timer nil))
    
    ;; Unregister from buffer registry
    (when (fboundp 'ecc-buffer-unregister-buffer)
      (ecc-buffer-unregister-buffer (current-buffer)))
    
    ;; If this was the active buffer, clear it
    (when (eq ecc-buffer--active-buffer (current-buffer))
      (setq ecc-buffer--active-buffer nil))
    
    (when (eq ecc-buffer-current-buffer (current-buffer))
      (setq ecc-buffer-current-buffer nil))))

;; Function to create new Claude vterm buffer
(defun ecc-term-vterm ()
  "Create a new Claude vterm buffer with optimized settings."
  (interactive)
  (unless ecc-term--vterm-available
    (error "Cannot create Claude vterm buffer: vterm package not available"))
  
  (let* ((buffer-name (format "*CLAUDE-VTERM-%02d*"
                              (+ (length
                                  (ecc-buffer-get-registered-buffers))
                                 1)))
         (new-buffer (get-buffer-create buffer-name)))
    (with-current-buffer new-buffer
      (ecc-term-vterm-mode)
      (set (make-local-variable 'ecc-original-name) buffer-name))
    (switch-to-buffer new-buffer)
    new-buffer))

;; Buffer navigation functions
(defun ecc-term-vterm-next-buffer ()
  "Switch to the next Claude VTERM buffer."
  (interactive)
  (when (fboundp 'ecc-buffer-next)
    (let ((next-buffer (ecc-buffer-next)))
      (when next-buffer
        (switch-to-buffer next-buffer)))))

(defun ecc-term-vterm-prev-buffer ()
  "Switch to the previous Claude VTERM buffer."
  (interactive)
  (when (fboundp 'ecc-buffer-prev)
    (let ((prev-buffer (ecc-buffer-prev)))
      (when prev-buffer
        (switch-to-buffer prev-buffer)))))

;; Function to update prompt patterns with VTERM-specific patterns
(defun ecc-term-vterm-update-prompt-patterns ()
  "Update state prompt patterns with VTERM-specific patterns.
This function is meant to be called when initializing VTERM mode."
  (when (boundp 'ecc-state-prompt-waiting)
    (setq-default ecc-state-prompt-waiting ecc-term-vterm-prompt-waiting))
  
  (when (boundp 'ecc-state-prompt-y/n)
    (setq-default ecc-state-prompt-y/n ecc-term-vterm-prompt-y/n))
  
  (when (boundp 'ecc-state-prompt-y/y/n)
    (setq-default ecc-state-prompt-y/y/n ecc-term-vterm-prompt-y/y/n))
  
  (when (boundp 'ecc-state-prompt-initial-waiting)
    (setq-default ecc-state-prompt-initial-waiting ecc-term-vterm-prompt-initial-waiting))
  
  ;; Also update regex patterns
  (when (boundp 'ecc-state-prompt-pattern-waiting)
    (setq-default ecc-state-prompt-pattern-waiting ecc-term-vterm-prompt-waiting))
  
  (when (boundp 'ecc-state-prompt-pattern-y/n)
    (setq-default ecc-state-prompt-pattern-y/n ecc-term-vterm-prompt-y/n))
  
  (when (boundp 'ecc-state-prompt-pattern-y/y/n)
    (setq-default ecc-state-prompt-pattern-y/y/n ecc-term-vterm-prompt-y/y/n))
  
  (when (boundp 'ecc-state-prompt-pattern-initial-waiting)
    (setq-default ecc-state-prompt-pattern-initial-waiting ecc-term-vterm-prompt-initial-waiting)))

;; Provide backward compatibility for existing code that uses vterm
(unless ecc-term--vterm-available
  ;; Stub functions for vterm when not available
  (defvar vterm-max-scrollback 1000
    "Stub variable for vterm-max-scrollback.")
  
  (defvar vterm-buffer-name "*vterm*"
    "Stub variable for vterm-buffer-name.")
  
  (defun vterm--internal (arg)
    "Stub function for vterm--internal."
    (error "vterm not available")))

(provide 'ecc-term-vterm-mode)

(when (not load-file-name)
  (message "ecc-term-vterm-mode.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))