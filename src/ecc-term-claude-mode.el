;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 07:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-term/ecc-term-claude-mode.el

;;; Commentary:
;;; Optimized vterm mode for Claude interaction.

(require 'vterm)
(require 'ecc-variables)

;; Customization group
(defgroup ecc-term-claude nil
  "Optimized vterm mode for Claude interaction."
  :group 'ecc
  :prefix "ecc-term-claude-")

;; Custom variables
(defcustom ecc-term-claude-line-numbers nil
  "Whether to display line numbers in Claude vterm buffers."
  :type 'boolean
  :group 'ecc-term-claude)

(defcustom ecc-term-claude-scroll-conservatively 10000
  "Value for scroll-conservatively in Claude vterm buffers."
  :type 'integer
  :group 'ecc-term-claude)

(defcustom ecc-term-claude-truncate-lines t
  "Whether to truncate lines in Claude vterm buffers."
  :type 'boolean
  :group 'ecc-term-claude)

(defcustom ecc-term-claude-state-update-interval 1.0
  "Interval in seconds for updating Claude state."
  :type 'number
  :group 'ecc-term-claude)

;; Mode menu
(defvar ecc-term-claude-menu
  (let ((menu (make-sparse-keymap "Claude-VTerm")))
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
    (define-key menu [ecc-term-claude-yes]
      '(menu-item "Yes (y)" ecc-term-claude-yes
                  :help "Send 'y' to respond affirmatively"))
    (define-key menu [ecc-term-claude-no]
      '(menu-item "No (n)" ecc-term-claude-no
                  :help "Send 'n' to respond negatively"))
    menu)
  "Menu for Claude-VTerm mode.")

;; Keybindings
(defvar ecc-term-claude-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map vterm-mode-map)
    
    ;; Claude-specific keybindings
    (define-key map (kbd "C-c C-y") 'ecc-term-claude-yes)
    (define-key map (kbd "C-c C-n") 'ecc-term-claude-no)
    (define-key map (kbd "C-c C-l") 'ecc-term-claude-clear)
    (define-key map (kbd "C-c C-a") 'ecc-term-claude-auto-mode-toggle)
    (define-key map (kbd "C-c C-v") 'ecc-term-claude-toggle-follow-bottom)
    
    ;; Add menu
    (define-key map [menu-bar claude-vterm] (cons "Claude" ecc-term-claude-menu))
    
    map)
  "Keymap for `ecc-term-claude-mode'.")

;; Auto-mode setting
(defvar ecc-term-claude-auto-mode nil
  "When non-nil, automatically respond to Claude prompts.")

;; Hook variables
(defvar ecc-term-claude-update-functions nil
  "Functions to run after vterm output is updated.")

;; State timer
(defvar ecc-term-claude-state-timer nil
  "Timer for updating the Claude state.")

;; Mode definition
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

;; Mode-line setup
(defun ecc-term-claude-setup-mode-line ()
  "Set up mode line indicator for Claude state."
  (setq mode-line-process 
        '(:eval (ecc-term-claude-mode-line-state-indicator))))

(defun ecc-term-claude-mode-line-state-indicator ()
  "Return mode line indicator for current Claude state."
  (let ((state (ecc-detect-simple-state)))
    (cond
     ((eq state :waiting) " [Waiting]")
     ((eq state :y/n) " [Y/N]")
     ((eq state :y/y/n) " [Y/Y/N]")
     (t ""))))

;; State detection and checking
(defun ecc-detect-simple-state ()
  "Detect the current state of Claude prompt.
Returns :y/y/n, :y/n, :waiting, :initial-waiting, or nil."
  (if (featurep 'ecc-state-detect-prompt)
      ;; Use the enhanced detection if available
      (ecc-detect-prompt-in-last-lines 20)
    ;; Fallback to basic detection
    (let ((buffer-text (buffer-substring-no-properties 
                       (max (- (point-max) 1000) (point-min))
                       (point-max))))
      (cond
       ;; Look for y/y/n prompts
       ((string-match "\\[Y/y/n\\]" buffer-text) :y/y/n)
       
       ;; Look for y/n prompts  
       ((string-match "\\[y/n\\]" buffer-text) :y/n)
       ((string-match "\\[Y/n\\]" buffer-text) :y/n)
       
       ;; Look for waiting prompts
       ((string-match "continue>" buffer-text) :waiting)
       ((string-match "Continue>" buffer-text) :waiting)
       
       ;; Try to use custom patterns if defined
       ((and (boundp 'ecc-state-prompt-y/y/n)
             ecc-state-prompt-y/y/n
             (string-match-p (regexp-quote ecc-state-prompt-y/y/n) buffer-text))
        :y/y/n)
       
       ((and (boundp 'ecc-state-prompt-y/n)
             ecc-state-prompt-y/n
             (string-match-p (regexp-quote ecc-state-prompt-y/n) buffer-text))
        :y/n)
       
       ((and (boundp 'ecc-state-prompt-waiting)
             ecc-state-prompt-waiting
             (string-match-p (regexp-quote ecc-state-prompt-waiting) buffer-text))
        :waiting)
       
       ((and (boundp 'ecc-state-prompt-initial-waiting)
             ecc-state-prompt-initial-waiting
             (string-match-p (regexp-quote ecc-state-prompt-initial-waiting) buffer-text))
        :initial-waiting)
       
       (t nil)))))

(defun ecc-term-claude-check-state ()
  "Check and update the state of the Claude VTERM buffer."
  (interactive)
  (when (eq major-mode 'ecc-term-claude-mode)
    (let ((state (ecc-detect-simple-state)))
      (force-mode-line-update)
      state)))

;; Claude interaction commands
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

;; Auto-response functions
(defun ecc-term-claude-auto-send-accept ()
  "Automatically respond to Claude prompts in vterm mode."
  (when ecc-term-claude-auto-mode
    (let ((state (ecc-detect-simple-state)))
      (cond
       ((eq state :y/y/n)
        (ecc-term-claude-auto-send-y/y/n))
       ((eq state :y/n)
        (ecc-term-claude-auto-send-y/n))
       ((eq state :initial-waiting)
        (ecc-term-claude-auto-send-initial-waiting))
       ((eq state :waiting)
        (ecc-term-claude-auto-send-continue))))))

(defun ecc-term-claude-auto-send-y/n ()
  "Automatically respond with 'y' to Y/N prompts."
  (vterm-send-string ecc-auto-response-y/n)
  (vterm-send-return)
  (message "Auto-responded: %s" ecc-auto-response-y/n))

(defun ecc-term-claude-auto-send-y/y/n ()
  "Automatically respond with 'y' to Y/Y/N prompts."
  (vterm-send-string ecc-auto-response-y/y/n)
  (vterm-send-return)
  (message "Auto-responded: %s" ecc-auto-response-y/y/n))

(defun ecc-term-claude-auto-send-continue ()
  "Automatically respond to continue prompts."
  (vterm-send-string ecc-auto-response-waiting)
  (vterm-send-return)
  (message "Auto-responded: %s" ecc-auto-response-waiting))

(defun ecc-term-claude-auto-send-initial-waiting ()
  "Automatically respond to initial waiting prompts."
  (vterm-send-string ecc-auto-response-initial-waiting)
  (vterm-send-return)
  (message "Auto-responded: %s" ecc-auto-response-initial-waiting))

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

;; Buffer management
(defun ecc-term-claude-cleanup-buffer ()
  "Clean up when buffer is killed."
  (when (eq major-mode 'ecc-term-claude-mode)
    ;; Cancel any timers
    (when ecc-term-claude-state-timer
      (cancel-timer ecc-term-claude-state-timer)
      (setq ecc-term-claude-state-timer nil))))

;; Register buffer function
(defun ecc-register-buffer (&optional buffer)
  "Register BUFFER as a Claude buffer."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (unless (buffer-live-p buf)
      (user-error "Buffer is not alive"))
    
    ;; Register the buffer
    (unless (assoc buf ecc-buffer-registered-buffers-alist)
      (push (cons buf nil) ecc-buffer-registered-buffers-alist)
      (message "Buffer '%s' registered as Claude buffer" (buffer-name buf)))
    
    ;; Set as current Claude buffer
    (setq ecc-buffer-current-buffer buf)
    buf))

;; Follow bottom functionality
(defun ecc-term-claude-follow-bottom-after-output ()
  "Scroll to bottom after vterm produces new output."
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

;; Setup functions for existing vterm buffers
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

(defun ecc-term-claude-setup-keys ()
  "Setup Claude-specific keybindings for vterm buffer."
  (when (eq major-mode 'vterm-mode)
    ;; Setup local key bindings similar to ecc-term-claude-mode
    (local-set-key (kbd "C-c C-y") 'ecc-term-claude-yes)
    (local-set-key (kbd "C-c C-n") 'ecc-term-claude-no)
    (local-set-key (kbd "C-c C-l") 'ecc-term-claude-clear)
    (local-set-key (kbd "C-c C-a") 'ecc-term-claude-auto-mode-toggle)
    (local-set-key (kbd "C-c C-v") 'ecc-term-claude-toggle-follow-bottom)))

;; Create a new Claude buffer function
(defun ecc-term-claude-enable ()
  "Enable Claude features in the current vterm buffer.
This applies Claude state detection and auto-response functionality
without changing the major mode."
  (interactive)
  (if (eq major-mode 'vterm-mode)
      (ecc-term-claude-setup-existing-buffer)
    (user-error "This command can only be used in vterm buffers")))

(defun ecc-term-claude ()
  "Create a new Claude vterm buffer with optimized settings.
If called from an existing vterm buffer, applies Claude settings to current buffer.
Otherwise, creates a new buffer with Claude vterm mode."
  (interactive)
  (cond
   ;; If we're already in a vterm buffer, apply Claude settings without changing the mode
   ((and (eq major-mode 'vterm-mode) 
         (not (eq major-mode 'ecc-term-claude-mode)))
    (ecc-term-claude-setup-existing-buffer)
    (current-buffer))
   
   ;; Default case: create a new Claude vterm buffer
   (t
    (let* ((buffer-name "*CLAUDE-VTERM*")
           (existing-buffer (get-buffer buffer-name))
           (new-buffer (or existing-buffer (get-buffer-create buffer-name))))
      (with-current-buffer new-buffer
        (unless (eq major-mode 'ecc-term-claude-mode)
          (ecc-term-claude-mode)))
      (switch-to-buffer new-buffer)
      new-buffer))))

(provide 'ecc-term-claude-mode)

;;; ecc-term-claude-mode.el ends here