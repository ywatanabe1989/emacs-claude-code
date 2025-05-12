;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 04:49:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-term/ecc-term-claude-mode.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;; Required dependencies
(require 'ecc-variables)
(require 'ecc-state)
(require 'ecc-state-detect)
(require 'ecc-buffer-registry)

;; Attempt to load vterm if available
(defvar ecc-term-claude--vterm-available
  (condition-case nil
      (progn (require 'vterm) t)
    (error nil))
  "Whether the vterm package is available.")

;; Customization group
(defgroup ecc-term-claude nil
  "Optimized vterm mode for Claude interaction."
  :group 'ecc
  :prefix "ecc-term-claude-")

;; Custom variables
(defcustom ecc-term-claude-line-numbers nil
  "Whether to display line numbers in Claude vterm buffers.
Disabling line numbers can improve performance for large outputs."
  :type 'boolean
  :group 'ecc-term-claude)

(defcustom ecc-term-claude-scroll-conservatively 10000
  "Value for scroll-conservatively in Claude vterm buffers.
Higher values prevent recentering during fast output."
  :type 'integer
  :group 'ecc-term-claude)

(defcustom ecc-term-claude-truncate-lines t
  "Whether to truncate lines in Claude vterm buffers.
Enabling truncation can improve performance for long lines."
  :type 'boolean
  :group 'ecc-term-claude)

;; Determine parent mode to fallback to if vterm isn't available
;; We use a direct symbol here, not a variable
(defvar ecc-term-claude-parent-mode-symbol
  (if ecc-term-claude--vterm-available
      'vterm-mode
    'special-mode)
  "Symbol of parent mode for ecc-term-claude-mode.")

;; Mode menu
(defvar ecc-term-claude-menu
  (let ((menu (make-sparse-keymap "Claude-VTerm")))
    ;; Buffer navigation submenu
    (let ((submenu (make-sparse-keymap "Buffer Navigation")))
      (define-key submenu [ecc-term-claude]
        '(menu-item "New Buffer" ecc-term-claude
                    :help "Create a new Claude VTERM buffer"))
      (define-key submenu [ecc-term-claude-next-buffer]
        '(menu-item "Next Buffer" ecc-term-claude-next-buffer
                    :help "Switch to the next Claude buffer"))
      (define-key submenu [ecc-term-claude-prev-buffer]
        '(menu-item "Previous Buffer" ecc-term-claude-prev-buffer
                    :help "Switch to the previous Claude buffer"))
      (define-key menu [ecc-buffer-navigation]
        (cons "Buffer Navigation" submenu)))
    
    (define-key menu [ecc-term-claude-separator-0]
      '(menu-item "--"))
    
    (define-key menu [ecc-term-claude-clear]
      '(menu-item "Clear buffer" ecc-term-claude-clear
                  :help "Clear the vterm buffer"))
    (define-key menu [ecc-term-claude-separator-1]
      '(menu-item "--"))
    (define-key menu [ecc-term-claude-auto-mode-toggle]
      '(menu-item "Toggle Auto-mode" ecc-term-claude-auto-mode-toggle
                  :help "Toggle automatic response to Claude prompts"
                  :button (:toggle . ecc-term-claude-auto-mode)))
    (define-key menu [ecc-term-claude-separator-2]
      '(menu-item "--"))
    (define-key menu [ecc-term-claude-retry]
      '(menu-item "Retry (r)" ecc-term-claude-retry
                  :help "Send 'r' to retry current operation"))
    (define-key menu [ecc-term-claude-no]
      '(menu-item "No (n)" ecc-term-claude-no
                  :help "Send 'n' to respond negatively"))
    (define-key menu [ecc-term-claude-yes]
      '(menu-item "Yes (y)" ecc-term-claude-yes
                  :help "Send 'y' to respond affirmatively"))
    (define-key menu [ecc-term-claude-separator-3]
      '(menu-item "--"))
    (define-key menu [ecc-term-claude-interrupt]
      '(menu-item "Interrupt" ecc-term-claude-interrupt
                  :help "Interrupt the current Claude process"))
    menu)
  "Menu for Claude-VTerm mode.")

;; Keybindings for Claude VTERM mode
(defvar ecc-term-claude-mode-map
  (let ((map (make-sparse-keymap)))
    ;; If vterm is available, inherit its keymap
    (when ecc-term-claude--vterm-available
      (set-keymap-parent map vterm-mode-map))
    
    ;; Claude-specific keybindings
    (define-key map (kbd "C-c C-k") 'ecc-term-claude-interrupt)
    (define-key map (kbd "C-c C-c") 'ecc-term-claude-interrupt)
    (define-key map (kbd "C-c C-y") 'ecc-term-claude-yes)
    (define-key map (kbd "C-c C-n") 'ecc-term-claude-no)
    (define-key map (kbd "C-c C-r") 'ecc-term-claude-retry)
    (define-key map (kbd "C-c C-l") 'ecc-term-claude-clear)
    (define-key map (kbd "C-c C-a") 'ecc-term-claude-auto-mode-toggle)
    
    ;; Buffer navigation keybindings
    (define-key map (kbd "C-c C-p") 'ecc-term-claude-prev-buffer)
    (define-key map (kbd "C-c C-f") 'ecc-term-claude-next-buffer)
    (define-key map (kbd "C-c C-b") 'ecc-term-claude-prev-buffer)
    (define-key map (kbd "C-c C-t") 'ecc-term-claude)
    
    ;; Add menu
    (define-key map [menu-bar claude-vterm] (cons "Claude" ecc-term-claude-menu))
    
    map)
  "Keymap for `ecc-term-claude-mode'.")

;; Timer for state detection
(defvar ecc-term-claude-state-timer nil
  "Timer for updating the Claude state in VTERM mode.")

;; Customization for state detection interval
(defcustom ecc-term-claude-state-update-interval 1.0
  "Interval in seconds for updating Claude state in VTERM mode."
  :type 'number
  :group 'ecc-term-claude)

;; Mode definition
(define-derived-mode ecc-term-claude-mode 
  ;; Use the symbol directly, not the variable
  (if ecc-term-claude--vterm-available 'vterm-mode 'special-mode)
  "Claude-VTerm"
  "Major mode for optimized Claude interaction in vterm.
This mode is optimized for high-performance streaming output.

Key bindings:
\\{ecc-term-claude-mode-map}"
  ;; Disable line numbers
  (when (and (boundp 'display-line-numbers-mode)
             (not ecc-term-claude-line-numbers))
    (display-line-numbers-mode -1))
  
  ;; Performance optimizations
  (setq-local scroll-conservatively ecc-term-claude-scroll-conservatively
              scroll-margin 0
              scroll-step 1
              fast-but-imprecise-scrolling t
              truncate-lines ecc-term-claude-truncate-lines)
  
  ;; Update prompt detection patterns
  (ecc-term-claude-update-prompt-patterns)
  
  ;; Visual indicators for Claude state
  (ecc-term-claude-setup-mode-line)
  
  ;; Register in buffer registry
  (ecc-buffer-register-buffer (current-buffer))
  
  ;; Make this buffer the active Claude buffer
  (when (boundp 'ecc-active-buffer)
    (setq ecc-active-buffer (current-buffer)))
  
  (when (boundp 'ecc-buffer-current-buffer)
    (setq ecc-buffer-current-buffer (current-buffer)))
  
  ;; Set up state detection timer
  (when ecc-term-claude-state-timer
    (cancel-timer ecc-term-claude-state-timer))
  
  (setq ecc-term-claude-state-timer
        (run-with-timer 0 ecc-term-claude-state-update-interval
                        'ecc-term-claude-check-state))
  
  ;; Set up auto-mode if enabled
  (when ecc-term-claude-auto-mode
    (add-to-list 'ecc-term-claude-update-functions
                'ecc-term-claude-auto-send-accept))
  
  ;; Connect to vterm hooks if available
  (when ecc-term-claude--vterm-available
    (add-hook 'vterm-update-functions
              (lambda (&rest _)
                (run-hooks 'ecc-term-claude-update-functions))))
  
  ;; Add hook to unregister buffer when killed
  (add-hook 'kill-buffer-hook 'ecc-term-claude-cleanup-buffer nil t))

;; Mode-line indicator for Claude state
(defun ecc-term-claude-setup-mode-line ()
  "Set up mode line indicator for Claude state."
  (setq mode-line-process 
        '(:eval (ecc-term-claude-mode-line-state-indicator))))

(defun ecc-term-claude-mode-line-state-indicator ()
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
(defun ecc-term-claude-check-state ()
  "Check and update the state of the Claude VTERM buffer.
This function is meant to be run periodically to update the mode line."
  (interactive)
  (when (eq major-mode 'ecc-term-claude-mode)
    (let ((state (ecc-state-get)))
      (force-mode-line-update)
      state)))

;; Auto-mode configuration
(defcustom ecc-term-claude-auto-mode nil
  "When non-nil, automatically respond to Claude prompts in VTERM."
  :type 'boolean
  :group 'ecc-term-claude)

;; Hook variables
(defvar ecc-term-claude-update-functions nil
  "Functions to run after vterm output is updated.")

;; Claude interaction functions
(defun ecc-term-claude-interrupt ()
  "Interrupt the current Claude process."
  (interactive)
  (if ecc-term-claude--vterm-available
      (progn
        (vterm-send-string "\C-c")
        (sit-for 0.3)
        (vterm-send-string "\C-c"))
    (message "Interrupt not available without vterm")))

(defun ecc-term-claude-yes ()
  "Send 'y' response to Claude prompt."
  (interactive)
  (if ecc-term-claude--vterm-available
      (progn
        (vterm-send-string "y")
        (vterm-send-return))
    (message "Yes response not available without vterm")))

(defun ecc-term-claude-no ()
  "Send 'n' response to Claude prompt."
  (interactive)
  (if ecc-term-claude--vterm-available
      (progn
        (vterm-send-string "n")
        (vterm-send-return))
    (message "No response not available without vterm")))

(defun ecc-term-claude-retry ()
  "Send 'r' response to Claude prompt for retry."
  (interactive)
  (if ecc-term-claude--vterm-available
      (progn
        (vterm-send-string "r")
        (vterm-send-return))
    (message "Retry response not available without vterm")))

(defun ecc-term-claude-clear ()
  "Clear the vterm buffer."
  (interactive)
  (if ecc-term-claude--vterm-available
      (vterm-clear)
    (erase-buffer)))

;; Auto-response functions
(defun ecc-term-claude-auto-send-accept ()
  "Automatically respond to Claude prompts in vterm mode."
  (when ecc-term-claude-auto-mode
    (let ((state (ecc-state-get)))
      (cond
       ((eq state :y/y/n)
        (ecc-term-claude-auto-send-y/y/n))
       ((eq state :y/n)
        (ecc-term-claude-auto-send-y/n))
       ((eq state :waiting)
        (ecc-term-claude-auto-send-continue))
       ((eq state :initial-waiting)
        (ecc-term-claude-auto-send-continue))))))

(defun ecc-term-claude-auto-send-y/n ()
  "Automatically respond with 'y' to Y/N prompts."
  (when ecc-term-claude--vterm-available
    (vterm-send-string "y")
    (vterm-send-return)
    (message "Auto-responded: y")))

(defun ecc-term-claude-auto-send-y/y/n ()
  "Automatically respond with 'y' to Y/Y/N prompts."
  (when ecc-term-claude--vterm-available
    (vterm-send-string "y")
    (vterm-send-return)
    (message "Auto-responded: y")))

(defun ecc-term-claude-auto-send-continue ()
  "Automatically respond to continue prompts."
  (when ecc-term-claude--vterm-available
    (vterm-send-string "continue")
    (vterm-send-return)
    (message "Auto-responded: continue")))

;; Toggle auto-mode
(defun ecc-term-claude-auto-mode-toggle ()
  "Toggle automatic response to Claude prompts."
  (interactive)
  (setq ecc-term-claude-auto-mode (not ecc-term-claude-auto-mode))
  (message "Claude auto-mode %s"
           (if ecc-term-claude-auto-mode "enabled" "disabled"))
  
  ;; Set up hooks for auto-responses
  (if ecc-term-claude-auto-mode
      (add-to-list 'ecc-term-claude-update-functions
                  'ecc-term-claude-auto-send-accept)
    (setq ecc-term-claude-update-functions
          (remove 'ecc-term-claude-auto-send-accept
                  ecc-term-claude-update-functions))))

;; Buffer management functions
(defun ecc-term-claude-cleanup-buffer ()
  "Unregister buffer from Claude buffer registry when killed."
  (when (eq major-mode 'ecc-term-claude-mode)
    ;; Cancel any timers
    (when ecc-term-claude-state-timer
      (cancel-timer ecc-term-claude-state-timer)
      (setq ecc-term-claude-state-timer nil))
    
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
(defun ecc-term-claude ()
  "Create a new Claude vterm buffer with optimized settings."
  (interactive)
  (unless ecc-term-claude--vterm-available
    (error "Cannot create Claude vterm buffer: vterm package not available"))
  
  (let* ((buffer-name (format "*CLAUDE-VTERM-%02d*"
                              (+ (length
                                  (ecc-buffer-get-registered-buffers))
                                 1)))
         (new-buffer (get-buffer-create buffer-name)))
    (with-current-buffer new-buffer
      (ecc-term-claude-mode)
      (set (make-local-variable 'ecc-original-name) buffer-name))
    (switch-to-buffer new-buffer)
    new-buffer))

;; Buffer navigation functions
(defun ecc-term-claude-next-buffer ()
  "Switch to the next Claude VTERM buffer."
  (interactive)
  (when (fboundp 'ecc-buffer-next)
    (let ((next-buffer (ecc-buffer-next)))
      (when next-buffer
        (switch-to-buffer next-buffer)))))

(defun ecc-term-claude-prev-buffer ()
  "Switch to the previous Claude VTERM buffer."
  (interactive)
  (when (fboundp 'ecc-buffer-prev)
    (let ((prev-buffer (ecc-buffer-prev)))
      (when prev-buffer
        (switch-to-buffer prev-buffer)))))

;; Custom prompt detection patterns for VTERM
(defcustom ecc-term-claude-prompt-waiting "Continue generati"
  "Text pattern for detecting waiting prompt in VTERM mode."
  :type 'string
  :group 'ecc-term-claude)

(defcustom ecc-term-claude-prompt-y/n "y/n"
  "Text pattern for detecting yes/no prompt in VTERM mode."
  :type 'string
  :group 'ecc-term-claude)

(defcustom ecc-term-claude-prompt-y/y/n "Y/y/n"
  "Text pattern for detecting Y/y/n prompt in VTERM mode."
  :type 'string
  :group 'ecc-term-claude)

(defcustom ecc-term-claude-prompt-initial-waiting "Would you like Claude to continue?"
  "Text pattern for detecting initial waiting prompt in VTERM mode."
  :type 'string
  :group 'ecc-term-claude)

;; Function to update prompt patterns with VTERM-specific patterns
(defun ecc-term-claude-update-prompt-patterns ()
  "Update global prompt patterns with VTERM-specific patterns.
This function is meant to be called when initializing VTERM mode."
  (when (boundp 'ecc-prompt-waiting)
    (setq-default ecc-prompt-waiting ecc-term-claude-prompt-waiting))
  
  (when (boundp 'ecc-prompt-y/n)
    (setq-default ecc-prompt-y/n ecc-term-claude-prompt-y/n))
  
  (when (boundp 'ecc-prompt-y/y/n)
    (setq-default ecc-prompt-y/y/n ecc-term-claude-prompt-y/y/n))
  
  (when (boundp 'ecc-prompt-initial-waiting)
    (setq-default ecc-prompt-initial-waiting ecc-term-claude-prompt-initial-waiting))
  
  ;; Also update regex patterns
  (when (boundp 'ecc-prompt-pattern-waiting)
    (setq-default ecc-prompt-pattern-waiting ecc-term-claude-prompt-waiting))
  
  (when (boundp 'ecc-prompt-pattern-y/n)
    (setq-default ecc-prompt-pattern-y/n ecc-term-claude-prompt-y/n))
  
  (when (boundp 'ecc-prompt-pattern-y/y/n)
    (setq-default ecc-prompt-pattern-y/y/n ecc-term-claude-prompt-y/y/n))
  
  (when (boundp 'ecc-prompt-pattern-initial-waiting)
    (setq-default ecc-prompt-pattern-initial-waiting ecc-term-claude-prompt-initial-waiting)))

;; Provide backward compatibility for existing code that uses vterm
(unless ecc-term-claude--vterm-available
  ;; Stub functions for vterm when not available
  (defvar vterm-max-scrollback 1000
    "Stub variable for vterm-max-scrollback.")
  
  (defvar vterm-buffer-name "*vterm*"
    "Stub variable for vterm-buffer-name.")
  
  (defun vterm--internal (arg)
    "Stub function for vterm--internal."
    (error "vterm not available")))

;; Backward compatibility aliases
(defalias 'ecc-claude-vterm-mode 'ecc-term-claude-mode)
(defalias 'ecc-claude-vterm--vterm-available 'ecc-term-claude--vterm-available)
(defalias 'ecc-claude-vterm-line-numbers 'ecc-term-claude-line-numbers)
(defalias 'ecc-claude-vterm-scroll-conservatively 'ecc-term-claude-scroll-conservatively)
(defalias 'ecc-claude-vterm-truncate-lines 'ecc-term-claude-truncate-lines)
(defalias 'ecc-claude-vterm-parent-mode-symbol 'ecc-term-claude-parent-mode-symbol)
(defalias 'ecc-claude-vterm-menu 'ecc-term-claude-menu)
(defalias 'ecc-claude-vterm-mode-map 'ecc-term-claude-mode-map)
(defalias 'ecc-claude-vterm-state-timer 'ecc-term-claude-state-timer)
(defalias 'ecc-claude-vterm-state-update-interval 'ecc-term-claude-state-update-interval)
(defalias 'ecc-claude-vterm-setup-mode-line 'ecc-term-claude-setup-mode-line)
(defalias 'ecc-claude-vterm-mode-line-state-indicator 'ecc-term-claude-mode-line-state-indicator)
(defalias 'ecc-claude-vterm-check-state 'ecc-term-claude-check-state)
(defalias 'ecc-claude-vterm-auto-mode 'ecc-term-claude-auto-mode)
(defalias 'ecc-claude-vterm-update-functions 'ecc-term-claude-update-functions)
(defalias 'ecc-claude-vterm-interrupt 'ecc-term-claude-interrupt)
(defalias 'ecc-claude-vterm-yes 'ecc-term-claude-yes)
(defalias 'ecc-claude-vterm-no 'ecc-term-claude-no)
(defalias 'ecc-claude-vterm-retry 'ecc-term-claude-retry)
(defalias 'ecc-claude-vterm-clear 'ecc-term-claude-clear)
(defalias 'ecc-claude-vterm-auto-send-accept 'ecc-term-claude-auto-send-accept)
(defalias 'ecc-claude-vterm-auto-send-y/n 'ecc-term-claude-auto-send-y/n)
(defalias 'ecc-claude-vterm-auto-send-y/y/n 'ecc-term-claude-auto-send-y/y/n)
(defalias 'ecc-claude-vterm-auto-send-continue 'ecc-term-claude-auto-send-continue)
(defalias 'ecc-claude-vterm-auto-mode-toggle 'ecc-term-claude-auto-mode-toggle)
(defalias 'ecc-claude-vterm-cleanup-buffer 'ecc-term-claude-cleanup-buffer)
(defalias 'ecc-claude-vterm 'ecc-term-claude)
(defalias 'ecc-claude-vterm-next-buffer 'ecc-term-claude-next-buffer)
(defalias 'ecc-claude-vterm-prev-buffer 'ecc-term-claude-prev-buffer)
(defalias 'ecc-claude-vterm-prompt-waiting 'ecc-term-claude-prompt-waiting)
(defalias 'ecc-claude-vterm-prompt-y/n 'ecc-term-claude-prompt-y/n)
(defalias 'ecc-claude-vterm-prompt-y/y/n 'ecc-term-claude-prompt-y/y/n)
(defalias 'ecc-claude-vterm-prompt-initial-waiting 'ecc-term-claude-prompt-initial-waiting)
(defalias 'ecc-claude-vterm-update-prompt-patterns 'ecc-term-claude-update-prompt-patterns)

;; Provide both new and old feature names for backward compatibility
(provide 'ecc-term-claude-mode)
(provide 'ecc-claude-vterm-mode)

(when (not load-file-name)
  (message "ecc-term-claude-mode.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))