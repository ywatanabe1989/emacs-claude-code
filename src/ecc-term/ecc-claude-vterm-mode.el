;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 02:15:24>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-term/ecc-claude-vterm-mode.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;; Required dependencies
(require 'ecc-variables)
(require 'ecc-state)
(require 'ecc-state-detect)
(require 'ecc-buffer-registry)

;; Attempt to load vterm if available
(defvar ecc-claude-vterm--vterm-available
  (condition-case nil
      (progn (require 'vterm) t)
    (error nil))
  "Whether the vterm package is available.")

;; Customization group
(defgroup ecc-claude-vterm nil
  "Optimized vterm mode for Claude interaction."
  :group 'ecc
  :prefix "ecc-claude-vterm-")

;; Custom variables
(defcustom ecc-claude-vterm-line-numbers nil
  "Whether to display line numbers in Claude vterm buffers.
Disabling line numbers can improve performance for large outputs."
  :type 'boolean
  :group 'ecc-claude-vterm)

(defcustom ecc-claude-vterm-scroll-conservatively 10000
  "Value for scroll-conservatively in Claude vterm buffers.
Higher values prevent recentering during fast output."
  :type 'integer
  :group 'ecc-claude-vterm)

(defcustom ecc-claude-vterm-truncate-lines t
  "Whether to truncate lines in Claude vterm buffers.
Enabling truncation can improve performance for long lines."
  :type 'boolean
  :group 'ecc-claude-vterm)

;; Define a parent mode to fallback to if vterm isn't available
(defvar ecc-claude-vterm-parent-mode
  (if ecc-claude-vterm--vterm-available
      'vterm-mode
    'special-mode)
  "Parent mode for ecc-claude-vterm-mode.")

;; Mode menu
(defvar ecc-claude-vterm-menu
  (let ((menu (make-sparse-keymap "Claude-VTerm")))
    ;; Buffer navigation submenu
    (let ((submenu (make-sparse-keymap "Buffer Navigation")))
      (define-key submenu [ecc-claude-vterm]
        '(menu-item "New Buffer" ecc-claude-vterm
                    :help "Create a new Claude VTERM buffer"))
      (define-key submenu [ecc-claude-vterm-next-buffer]
        '(menu-item "Next Buffer" ecc-claude-vterm-next-buffer
                    :help "Switch to the next Claude buffer"))
      (define-key submenu [ecc-claude-vterm-prev-buffer]
        '(menu-item "Previous Buffer" ecc-claude-vterm-prev-buffer
                    :help "Switch to the previous Claude buffer"))
      (define-key menu [ecc-buffer-navigation]
        (cons "Buffer Navigation" submenu)))
    
    (define-key menu [ecc-claude-vterm-separator-0]
      '(menu-item "--"))
    
    (define-key menu [ecc-claude-vterm-clear]
      '(menu-item "Clear buffer" ecc-claude-vterm-clear
                  :help "Clear the vterm buffer"))
    (define-key menu [ecc-claude-vterm-separator-1]
      '(menu-item "--"))
    (define-key menu [ecc-claude-vterm-auto-mode-toggle]
      '(menu-item "Toggle Auto-mode" ecc-claude-vterm-auto-mode-toggle
                  :help "Toggle automatic response to Claude prompts"
                  :button (:toggle . ecc-claude-vterm-auto-mode)))
    (define-key menu [ecc-claude-vterm-separator-2]
      '(menu-item "--"))
    (define-key menu [ecc-claude-vterm-retry]
      '(menu-item "Retry (r)" ecc-claude-vterm-retry
                  :help "Send 'r' to retry current operation"))
    (define-key menu [ecc-claude-vterm-no]
      '(menu-item "No (n)" ecc-claude-vterm-no
                  :help "Send 'n' to respond negatively"))
    (define-key menu [ecc-claude-vterm-yes]
      '(menu-item "Yes (y)" ecc-claude-vterm-yes
                  :help "Send 'y' to respond affirmatively"))
    (define-key menu [ecc-claude-vterm-separator-3]
      '(menu-item "--"))
    (define-key menu [ecc-claude-vterm-interrupt]
      '(menu-item "Interrupt" ecc-claude-vterm-interrupt
                  :help "Interrupt the current Claude process"))
    menu)
  "Menu for Claude-VTerm mode.")

;; Keybindings for Claude VTERM mode
(defvar ecc-claude-vterm-mode-map
  (let ((map (make-sparse-keymap)))
    ;; If vterm is available, inherit its keymap
    (when ecc-claude-vterm--vterm-available
      (set-keymap-parent map vterm-mode-map))
    
    ;; Claude-specific keybindings
    (define-key map (kbd "C-c C-k") 'ecc-claude-vterm-interrupt)
    (define-key map (kbd "C-c C-c") 'ecc-claude-vterm-interrupt)
    (define-key map (kbd "C-c C-y") 'ecc-claude-vterm-yes)
    (define-key map (kbd "C-c C-n") 'ecc-claude-vterm-no)
    (define-key map (kbd "C-c C-r") 'ecc-claude-vterm-retry)
    (define-key map (kbd "C-c C-l") 'ecc-claude-vterm-clear)
    (define-key map (kbd "C-c C-a") 'ecc-claude-vterm-auto-mode-toggle)
    
    ;; Buffer navigation keybindings
    (define-key map (kbd "C-c C-p") 'ecc-claude-vterm-prev-buffer)
    (define-key map (kbd "C-c C-f") 'ecc-claude-vterm-next-buffer)
    (define-key map (kbd "C-c C-b") 'ecc-claude-vterm-prev-buffer)
    (define-key map (kbd "C-c C-t") 'ecc-claude-vterm)
    
    ;; Add menu
    (define-key map [menu-bar claude-vterm] (cons "Claude" ecc-claude-vterm-menu))
    
    map)
  "Keymap for `ecc-claude-vterm-mode'.")

;; Timer for state detection
(defvar ecc-claude-vterm-state-timer nil
  "Timer for updating the Claude state in VTERM mode.")

;; Customization for state detection interval
(defcustom ecc-claude-vterm-state-update-interval 1.0
  "Interval in seconds for updating Claude state in VTERM mode."
  :type 'number
  :group 'ecc-claude-vterm)

;; Mode definition
(define-derived-mode ecc-claude-vterm-mode ecc-claude-vterm-parent-mode "Claude-VTerm"
  "Major mode for optimized Claude interaction in vterm.
This mode is optimized for high-performance streaming output.

Key bindings:
\\{ecc-claude-vterm-mode-map}"
  ;; Disable line numbers
  (when (and (boundp 'display-line-numbers-mode)
             (not ecc-claude-vterm-line-numbers))
    (display-line-numbers-mode -1))
  
  ;; Performance optimizations
  (setq-local scroll-conservatively ecc-claude-vterm-scroll-conservatively
              scroll-margin 0
              scroll-step 1
              fast-but-imprecise-scrolling t
              truncate-lines ecc-claude-vterm-truncate-lines)
  
  ;; Update prompt detection patterns
  (ecc-claude-vterm-update-prompt-patterns)
  
  ;; Visual indicators for Claude state
  (ecc-claude-vterm-setup-mode-line)
  
  ;; Register in buffer registry
  (ecc-buffer-register-buffer (current-buffer))
  
  ;; Make this buffer the active Claude buffer
  (when (boundp 'ecc-active-buffer)
    (setq ecc-active-buffer (current-buffer)))
  
  (when (boundp 'ecc-buffer-current-buffer)
    (setq ecc-buffer-current-buffer (current-buffer)))
  
  ;; Set up state detection timer
  (when ecc-claude-vterm-state-timer
    (cancel-timer ecc-claude-vterm-state-timer))
  
  (setq ecc-claude-vterm-state-timer
        (run-with-timer 0 ecc-claude-vterm-state-update-interval
                        'ecc-claude-vterm-check-state))
  
  ;; Set up auto-mode if enabled
  (when ecc-claude-vterm-auto-mode
    (add-to-list 'ecc-claude-vterm-update-functions
                'ecc-claude-vterm-auto-send-accept))
  
  ;; Connect to vterm hooks if available
  (when ecc-claude-vterm--vterm-available
    (add-hook 'vterm-update-functions
              (lambda (&rest _)
                (run-hooks 'ecc-claude-vterm-update-functions))))
  
  ;; Add hook to unregister buffer when killed
  (add-hook 'kill-buffer-hook 'ecc-claude-vterm-cleanup-buffer nil t))

;; Mode-line indicator for Claude state
(defun ecc-claude-vterm-setup-mode-line ()
  "Set up mode line indicator for Claude state."
  (setq mode-line-process 
        '(:eval (ecc-claude-vterm-mode-line-state-indicator))))

(defun ecc-claude-vterm-mode-line-state-indicator ()
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
(defun ecc-claude-vterm-check-state ()
  "Check and update the state of the Claude VTERM buffer.
This function is meant to be run periodically to update the mode line."
  (interactive)
  (when (eq major-mode 'ecc-claude-vterm-mode)
    (let ((state (ecc-state-get)))
      (force-mode-line-update)
      state)))

;; Auto-mode configuration
(defcustom ecc-claude-vterm-auto-mode nil
  "When non-nil, automatically respond to Claude prompts in VTERM."
  :type 'boolean
  :group 'ecc-claude-vterm)

;; Hook variables
(defvar ecc-claude-vterm-update-functions nil
  "Functions to run after vterm output is updated.")

;; Claude interaction functions
(defun ecc-claude-vterm-interrupt ()
  "Interrupt the current Claude process."
  (interactive)
  (if ecc-claude-vterm--vterm-available
      (progn
        (vterm-send-string "\C-c")
        (sit-for 0.3)
        (vterm-send-string "\C-c"))
    (message "Interrupt not available without vterm")))

(defun ecc-claude-vterm-yes ()
  "Send 'y' response to Claude prompt."
  (interactive)
  (if ecc-claude-vterm--vterm-available
      (progn
        (vterm-send-string "y")
        (vterm-send-return))
    (message "Yes response not available without vterm")))

(defun ecc-claude-vterm-no ()
  "Send 'n' response to Claude prompt."
  (interactive)
  (if ecc-claude-vterm--vterm-available
      (progn
        (vterm-send-string "n")
        (vterm-send-return))
    (message "No response not available without vterm")))

(defun ecc-claude-vterm-retry ()
  "Send 'r' response to Claude prompt for retry."
  (interactive)
  (if ecc-claude-vterm--vterm-available
      (progn
        (vterm-send-string "r")
        (vterm-send-return))
    (message "Retry response not available without vterm")))

(defun ecc-claude-vterm-clear ()
  "Clear the vterm buffer."
  (interactive)
  (if ecc-claude-vterm--vterm-available
      (vterm-clear)
    (erase-buffer)))

;; Auto-response functions
(defun ecc-claude-vterm-auto-send-accept ()
  "Automatically respond to Claude prompts in vterm mode."
  (when ecc-claude-vterm-auto-mode
    (let ((state (ecc-state-get)))
      (cond
       ((eq state :y/y/n)
        (ecc-claude-vterm-auto-send-y/y/n))
       ((eq state :y/n)
        (ecc-claude-vterm-auto-send-y/n))
       ((eq state :waiting)
        (ecc-claude-vterm-auto-send-continue))
       ((eq state :initial-waiting)
        (ecc-claude-vterm-auto-send-continue))))))

(defun ecc-claude-vterm-auto-send-y/n ()
  "Automatically respond with 'y' to Y/N prompts."
  (when ecc-claude-vterm--vterm-available
    (vterm-send-string "y")
    (vterm-send-return)
    (message "Auto-responded: y")))

(defun ecc-claude-vterm-auto-send-y/y/n ()
  "Automatically respond with 'y' to Y/Y/N prompts."
  (when ecc-claude-vterm--vterm-available
    (vterm-send-string "y")
    (vterm-send-return)
    (message "Auto-responded: y")))

(defun ecc-claude-vterm-auto-send-continue ()
  "Automatically respond to continue prompts."
  (when ecc-claude-vterm--vterm-available
    (vterm-send-string "continue")
    (vterm-send-return)
    (message "Auto-responded: continue")))

;; Toggle auto-mode
(defun ecc-claude-vterm-auto-mode-toggle ()
  "Toggle automatic response to Claude prompts."
  (interactive)
  (setq ecc-claude-vterm-auto-mode (not ecc-claude-vterm-auto-mode))
  (message "Claude auto-mode %s"
           (if ecc-claude-vterm-auto-mode "enabled" "disabled"))
  
  ;; Set up hooks for auto-responses
  (if ecc-claude-vterm-auto-mode
      (add-to-list 'ecc-claude-vterm-update-functions
                  'ecc-claude-vterm-auto-send-accept)
    (setq ecc-claude-vterm-update-functions
          (remove 'ecc-claude-vterm-auto-send-accept
                  ecc-claude-vterm-update-functions))))

;; Buffer management functions
(defun ecc-claude-vterm-cleanup-buffer ()
  "Unregister buffer from Claude buffer registry when killed."
  (when (eq major-mode 'ecc-claude-vterm-mode)
    ;; Cancel any timers
    (when ecc-claude-vterm-state-timer
      (cancel-timer ecc-claude-vterm-state-timer)
      (setq ecc-claude-vterm-state-timer nil))
    
    ;; Unregister from buffer registry functions
    (when (fboundp 'ecc-buffer-unregister-buffer)
      (ecc-buffer-unregister-buffer (current-buffer)))
    
    ;; If this was the active buffer, clear it
    (when (and (boundp 'ecc-active-buffer) 
               (eq ecc-active-buffer (current-buffer)))
      (setq ecc-active-buffer nil))
    
    (when (and (boundp 'ecc-buffer-current-buffer)
               (eq ecc-buffer-current-buffer (current-buffer)))
      (setq ecc-buffer-current-buffer nil))))

;; Function to create new Claude vterm buffer
(defun ecc-claude-vterm ()
  "Create a new Claude vterm buffer with optimized settings."
  (interactive)
  (unless ecc-claude-vterm--vterm-available
    (error "Cannot create Claude vterm buffer: vterm package not available"))
  
  (let* ((buffer-name (format "*CLAUDE-VTERM-%02d*"
                              (+ (length
                                  (ecc-buffer-get-registered-buffers))
                                 1)))
         (new-buffer (get-buffer-create buffer-name)))
    (with-current-buffer new-buffer
      (ecc-claude-vterm-mode)
      (set (make-local-variable 'ecc-original-name) buffer-name))
    (switch-to-buffer new-buffer)
    new-buffer))

;; Buffer navigation functions
(defun ecc-claude-vterm-next-buffer ()
  "Switch to the next Claude VTERM buffer."
  (interactive)
  (when (fboundp 'ecc-buffer-next)
    (let ((next-buffer (ecc-buffer-next)))
      (when next-buffer
        (switch-to-buffer next-buffer)))))

(defun ecc-claude-vterm-prev-buffer ()
  "Switch to the previous Claude VTERM buffer."
  (interactive)
  (when (fboundp 'ecc-buffer-prev)
    (let ((prev-buffer (ecc-buffer-prev)))
      (when prev-buffer
        (switch-to-buffer prev-buffer)))))

;; Custom prompt detection patterns for VTERM
(defcustom ecc-claude-vterm-prompt-waiting "Continue generati"
  "Text pattern for detecting waiting prompt in VTERM mode."
  :type 'string
  :group 'ecc-claude-vterm)

(defcustom ecc-claude-vterm-prompt-y/n "y/n"
  "Text pattern for detecting yes/no prompt in VTERM mode."
  :type 'string
  :group 'ecc-claude-vterm)

(defcustom ecc-claude-vterm-prompt-y/y/n "Y/y/n"
  "Text pattern for detecting Y/y/n prompt in VTERM mode."
  :type 'string
  :group 'ecc-claude-vterm)

(defcustom ecc-claude-vterm-prompt-initial-waiting "Would you like Claude to continue?"
  "Text pattern for detecting initial waiting prompt in VTERM mode."
  :type 'string
  :group 'ecc-claude-vterm)

;; Function to update prompt patterns with VTERM-specific patterns
(defun ecc-claude-vterm-update-prompt-patterns ()
  "Update global prompt patterns with VTERM-specific patterns.
This function is meant to be called when initializing VTERM mode."
  (when (boundp 'ecc-prompt-waiting)
    (setq-default ecc-prompt-waiting ecc-claude-vterm-prompt-waiting))
  
  (when (boundp 'ecc-prompt-y/n)
    (setq-default ecc-prompt-y/n ecc-claude-vterm-prompt-y/n))
  
  (when (boundp 'ecc-prompt-y/y/n)
    (setq-default ecc-prompt-y/y/n ecc-claude-vterm-prompt-y/y/n))
  
  (when (boundp 'ecc-prompt-initial-waiting)
    (setq-default ecc-prompt-initial-waiting ecc-claude-vterm-prompt-initial-waiting))
  
  ;; Also update regex patterns
  (when (boundp 'ecc-prompt-pattern-waiting)
    (setq-default ecc-prompt-pattern-waiting ecc-claude-vterm-prompt-waiting))
  
  (when (boundp 'ecc-prompt-pattern-y/n)
    (setq-default ecc-prompt-pattern-y/n ecc-claude-vterm-prompt-y/n))
  
  (when (boundp 'ecc-prompt-pattern-y/y/n)
    (setq-default ecc-prompt-pattern-y/y/n ecc-claude-vterm-prompt-y/y/n))
  
  (when (boundp 'ecc-prompt-pattern-initial-waiting)
    (setq-default ecc-prompt-pattern-initial-waiting ecc-claude-vterm-prompt-initial-waiting)))

;; Provide backward compatibility for existing code that uses vterm
(unless ecc-claude-vterm--vterm-available
  ;; Stub functions for vterm when not available
  (defvar vterm-max-scrollback 1000
    "Stub variable for vterm-max-scrollback.")
  
  (defvar vterm-buffer-name "*vterm*"
    "Stub variable for vterm-buffer-name.")
  
  (defun vterm--internal (arg)
    "Stub function for vterm--internal."
    (error "vterm not available")))

(provide 'ecc-claude-vterm-mode)

(when (not load-file-name)
  (message "ecc-claude-vterm-mode.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))