;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 23:57:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-term-claude-setup.el

;;; Commentary:
;;; Common setup logic for Claude terminal mode.
;;; This module consolidates duplicated setup code from multiple functions
;;; into a reusable system of modular, well-documented functions.

(require 'ecc-variables)
(require 'ecc-term-claude-state)
(require 'ecc-term-claude-auto)
(require 'cl-lib)  ;; For cl-some

;; Forward declarations to prevent free variable warnings
(defvar ecc-term-claude-line-numbers nil
  "When non-nil, display line numbers in Claude buffers.")
(defvar ecc-term-claude-scroll-conservatively 101
  "Value for scroll-conservatively in Claude buffers.")
(defvar ecc-term-claude-truncate-lines t
  "When non-nil, truncate lines in Claude buffers.")
(defvar ecc-term-claude-state-timer nil
  "Timer used to periodically check state in Claude buffers.")
(defvar ecc-term-claude-state-update-interval 0.5
  "Interval in seconds to check state in Claude buffers.")
(defvar ecc-term-claude-update-functions nil
  "Hook functions to run after each update in Claude buffers.")
(defvar ecc-buffer-registered-buffers-alist nil
  "Alist of registered Claude buffers.")
(defvar ecc-buffer-current-buffer nil
  "Currently active Claude buffer.")

;;; Code:

;;;; Setup validation utilities

(defun ecc-term-claude-validate-buffer (buffer-or-name &optional require-mode)
  "Validate that BUFFER-OR-NAME is a live buffer and optionally check its mode.
BUFFER-OR-NAME can be a buffer or a buffer name.
REQUIRE-MODE can be a mode symbol or list of allowed modes.

Returns the buffer object if valid, otherwise signals an error."
  (let ((buf (if (bufferp buffer-or-name)
                buffer-or-name
              (get-buffer buffer-or-name))))
    ;; Check buffer exists
    (unless (buffer-live-p buf)
      (user-error "Buffer %s does not exist or has been killed"
                 (if (stringp buffer-or-name)
                     buffer-or-name
                   "specified")))
    
    ;; Check buffer mode if specified
    (when require-mode
      (with-current-buffer buf
        (let ((modes (if (listp require-mode) require-mode (list require-mode))))
          (unless (cl-some (lambda (mode) (eq major-mode mode)) modes)
            (user-error "Buffer %s is not in %s mode (current: %s)"
                       (buffer-name buf)
                       (if (listp require-mode)
                           (format "one of %s" require-mode)
                         require-mode)
                       major-mode)))))
    buf))

;;;; Core setup functions

(defun ecc-term-claude-setup-common (buffer)
  "Set up common Claude features in BUFFER.
Configures essential Claude features including state detection,
visual indicators, hooks, and buffer registration.

This function is used by both the major mode and the setup
function for existing buffers to avoid code duplication."
  (let ((buf (ecc-term-claude-validate-buffer buffer '(vterm-mode ecc-term-claude-mode))))
    (with-current-buffer buf
      ;; Register buffer
      (ecc-term-claude-register-buffer)
      
      ;; Set up visual indicators
      (ecc-term-claude-setup-mode-line)
      
      ;; Set up timers and hooks
      (ecc-term-claude-setup-timer)
      (ecc-term-claude-setup-hooks)
      
      ;; Set up follow bottom behavior
      (ecc-term-claude-setup-follow-bottom)
      
      buf)))

(defun ecc-term-claude-setup-performance (buffer)
  "Set up performance optimizations for Claude in BUFFER."
  (with-current-buffer buffer
    ;; Disable line numbers for performance
    (when (and (boundp 'display-line-numbers-mode)
               (not ecc-term-claude-line-numbers))
      (display-line-numbers-mode -1))
    
    ;; Apply performance-oriented settings
    (setq-local scroll-conservatively ecc-term-claude-scroll-conservatively
                scroll-margin 0
                scroll-step 1
                fast-but-imprecise-scrolling t
                truncate-lines ecc-term-claude-truncate-lines)))

(defun ecc-term-claude-register-buffer (&optional buffer)
  "Register BUFFER or current buffer as a Claude buffer.
Adds the buffer to the tracking system and sets it as the current Claude buffer."
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

;;;; Timer and hook setup

(defun ecc-term-claude-setup-timer (&optional buffer)
  "Set up or reset the state detection timer for BUFFER or current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (when ecc-term-claude-state-timer
      (cancel-timer ecc-term-claude-state-timer))
    
    (setq-local ecc-term-claude-state-timer
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
    
    ;; Add hook to clean up when buffer is killed
    (add-hook 'kill-buffer-hook 'ecc-term-claude-cleanup-buffer nil t)))

(defun ecc-term-claude-setup-follow-bottom (&optional buffer)
  "Set up follow-bottom behavior for Claude in BUFFER or current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (add-hook 'ecc-term-claude-update-functions
              'ecc-term-claude-follow-bottom-after-output
              nil t)))

;;;; Visual indicators

(defun ecc-term-claude-setup-mode-line (&optional buffer)
  "Set up mode line indicator for Claude state in BUFFER or current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (setq-local mode-line-process 
                '(:eval (ecc-term-claude-mode-line-state-indicator)))))

(defun ecc-term-claude-mode-line-state-indicator ()
  "Return mode line indicator for current Claude state."
  (let ((state (ecc-term-claude-get-state)))
    (cond
     ((eq state :waiting) " [Waiting]")
     ((eq state :y/n) " [Y/N]")
     ((eq state :y/y/n) " [Y/Y/N]")
     ((eq state :initial-waiting) " [Initial]")
     (t ""))))

;;;; Cleanup functions

(defun ecc-term-claude-cleanup-buffer ()
  "Clean up when buffer is killed.
Cancels any active timers and cleans up registered resources."
  (when (buffer-live-p (current-buffer))
    ;; Cancel any timers
    (when (bound-and-true-p ecc-term-claude-state-timer)
      (cancel-timer ecc-term-claude-state-timer)
      (setq ecc-term-claude-state-timer nil))
    
    ;; Clean up visual aids if available
    (when (featurep 'ecc-term-visual-aid)
      (when (fboundp 'ecc-term-visual-aid-clear-all)
        (ecc-term-visual-aid-clear-all)))
    
    ;; Unregister from buffer list
    (when (and (boundp 'ecc-buffer-registered-buffers-alist)
               (assoc (current-buffer) ecc-buffer-registered-buffers-alist))
      (setq ecc-buffer-registered-buffers-alist
            (assq-delete-all (current-buffer) ecc-buffer-registered-buffers-alist)))))

;;;; Keybinding setup

(defun ecc-term-claude-setup-keys (&optional buffer)
  "Setup Claude-specific keybindings for BUFFER or current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (when (eq major-mode 'vterm-mode)
      ;; Setup local key bindings similar to ecc-term-claude-mode
      ;; Basic functions
      (local-set-key (kbd "C-c C-y") 'ecc-term-claude-send-yes)
      (local-set-key (kbd "C-c C-n") 'ecc-term-claude-send-no)
      (local-set-key (kbd "C-c C-l") 'ecc-term-claude-clear)
      (local-set-key (kbd "C-c C-a") 'ecc-term-claude-toggle-auto-mode)
      (local-set-key (kbd "C-c C-v") 'ecc-term-claude-toggle-follow-bottom)
      
      ;; Yank-as-file functions
      (local-set-key (kbd "C-c C-f") 'ecc-vterm-yank-as-file)
      (local-set-key (kbd "C-c C-b") 'ecc-vterm-yank-buffer-as-file)
      (local-set-key (kbd "C-c C-q") 'ecc-vterm-quick-yank-region))))

(provide 'ecc-term-claude-setup)

;;; ecc-term-claude-setup.el ends here