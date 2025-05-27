;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-28 05:49:48>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-vterm-mode.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;; 1. Dependencies
;; ----------------------------------------
(require 'ecc-debug)
(require 'ecc-state-detection)
(require 'ecc-auto-response)
(require 'ecc-notification)
(require 'ecc-vterm-utils)
(require 'vterm nil t)


;; 2. Configuration
;; ----------------------------------------
(defcustom --ecc-vterm-mode-buffer-name "*CLAUDE-VTERM*"
  "Default buffer name for Claude vterm buffers."
  :type 'string
  :group 'ecc)

(defcustom --ecc-vterm-mode-show-state-in-mode-line t
  "Whether to show Claude state in the mode line."
  :type 'boolean
  :group 'ecc)

(defcustom --ecc-vterm-mode-auto-mode-color "#4a5d23"
  "Background color for modeline when auto-mode is enabled."
  :type 'color
  :group 'ecc)


;; 3. Variables
;; ----------------------------------------
(defvar-local --ecc-vterm-mode--auto-mode-active nil
  "Whether auto-mode is active for this buffer.")

(defvar-local --ecc-vterm-mode--state-timer nil
  "Timer for updating Claude state.")

(defvar --ecc-vterm-mode-map
  (let ((map (make-sparse-keymap)))
    (when (boundp 'vterm-mode-map)
      (set-keymap-parent map vterm-mode-map))
    (define-key map (kbd "C-c C-y") '--ecc-vterm-mode-yes)
    (define-key map (kbd "C-c C-n") '--ecc-vterm-mode-no)
    (define-key map (kbd "C-c C-a") '--ecc-vterm-mode-toggle-auto)
    (define-key map (kbd "C-c C-f")
                '--ecc-vterm-utils-yank-region-to-file)
    (define-key map (kbd "C-c C-b")
                '--ecc-vterm-utils-yank-buffer-to-file)
    (define-key map (kbd "C-c C-q")
                '--ecc-vterm-utils-quick-yank-region)
    map)
  "Keymap for Claude vterm mode.")


;; 4. Main Entry Points
;; ----------------------------------------
(when (featurep 'vterm)
  (define-derived-mode --ecc-vterm-mode vterm-mode
    "Claude-VTerm"
    "Major mode for optimized Claude interaction in vterm."
    (--ecc-vterm-mode--setup-buffer)))

(defun --ecc-vterm-mode-create (&optional buffer)
  "Create a new Claude vterm buffer or convert BUFFER to ecc-vterm-mode."
  (interactive)
  (let ((target-buffer (or buffer (current-buffer))))
    (cond
     ((eq (buffer-local-value 'major-mode target-buffer) 'vterm-mode)
      (--ecc-debug-message
       "Converting vterm buffer to ecc-vterm-mode: %s"
       (buffer-name target-buffer))
      (with-current-buffer target-buffer
        (--ecc-vterm-mode))
      target-buffer)
     (t
      (--ecc-debug-message "Creating new Claude vterm buffer: %s"
                           --ecc-vterm-mode-buffer-name)
      (let
          ((new-buffer
            (get-buffer-create --ecc-vterm-mode-buffer-name)))
        (with-current-buffer new-buffer
          (unless (eq major-mode '--ecc-vterm-mode)
            (--ecc-debug-message "Initializing --ecc-vterm-mode")
            (--ecc-vterm-mode)))
        (switch-to-buffer new-buffer)
        new-buffer)))))

(defun --ecc-vterm-mode-toggle-auto ()
  "Toggle auto-response mode."
  (interactive)
  (setq-local --ecc-vterm-mode--auto-mode-active
              (not --ecc-vterm-mode--auto-mode-active))
  (if --ecc-vterm-mode--auto-mode-active
      (--ecc-auto-response-enable-buffer)
    (--ecc-auto-response-disable-buffer))
  (--ecc-debug-message "Auto-mode %s"
                       (if --ecc-vterm-mode--auto-mode-active
                           "enabled"
                         "disabled"))
  (force-mode-line-update))


;; 5. Core Functions
;; ----------------------------------------
(defun --ecc-vterm-mode--setup-buffer ()
  "Set up current buffer for Claude vterm mode."
  (--ecc-debug-message "Setting up Claude vterm buffer: %s"
                       (buffer-name))
  (--ecc-vterm-mode--optimize-buffer)
  (--ecc-auto-response-enable-buffer)
  (when --ecc-vterm-mode-show-state-in-mode-line
    (setq mode-line-process
          '(:eval (--ecc-vterm-mode--mode-line-indicator))))
  (--ecc-vterm-mode--setup-state-timer)
  (add-hook 'kill-buffer-hook '--ecc-vterm-mode--cleanup-buffer nil t))

(defun --ecc-vterm-mode--optimize-buffer ()
  "Apply performance optimizations to current buffer."
  (--ecc-debug-message "Optimizing buffer performance settings")
  (setq-local scroll-conservatively 101
              scroll-margin 0
              scroll-step 1
              fast-but-imprecise-scrolling t
              auto-window-vscroll nil
              truncate-lines t
              line-move-visual t
              inhibit-field-text-motion t
              jit-lock-defer-time 0.05
              redisplay-skip-fontification-on-input t
              redisplay-dont-pause t)
  (when (and (boundp 'display-line-numbers-mode)
             display-line-numbers-mode)
    (--ecc-debug-message "Disabling line numbers for performance")
    (display-line-numbers-mode -1))
  (buffer-disable-undo)
  (--ecc-debug-message "Buffer optimization complete"))

(defun --ecc-vterm-mode--setup-state-timer ()
  "Set up timer for checking Claude state."
  (--ecc-debug-message "Setting up state timer for buffer: %s"
                       (buffer-name))
  (when --ecc-vterm-mode--state-timer
    (cancel-timer --ecc-vterm-mode--state-timer)
    (--ecc-debug-message "Cancelled existing state timer"))
  (let ((buffer (current-buffer)))
    (setq --ecc-vterm-mode--state-timer
          (run-with-timer 0 1.0
                          (lambda ()
                            (when (buffer-live-p buffer)
                              (with-current-buffer buffer
                                (force-mode-line-update)))))))
  (--ecc-debug-message "State timer started"))

(defun --ecc-vterm-mode--cleanup-buffer ()
  "Clean up when buffer is killed."
  (--ecc-debug-message "Cleaning up buffer: %s" (buffer-name))
  (when --ecc-vterm-mode--state-timer
    (cancel-timer --ecc-vterm-mode--state-timer)
    (setq --ecc-vterm-mode--state-timer nil)
    (--ecc-debug-message "State timer cancelled")))


;; 6. Helper/Utility Functions
;; ----------------------------------------
(defun --ecc-vterm-mode--mode-line-indicator ()
  "Return mode line indicator for current Claude state."
  (let ((state (--ecc-state-detection-detect))
        (auto-indicator (if --ecc-vterm-mode--auto-mode-active
                            (propertize " [AUTO] "
                                        'face `(:background
                                                ,--ecc-vterm-mode-auto-mode-color
                                                :foreground "#ffffff"
                                                :weight bold))
                          "")))
    (concat auto-indicator
            (cond
             ((eq state :waiting) " [Waiting]")
             ((eq state :y/n) " [Y/N]")
             ((eq state :y/y/n) " [Y/Y/N]")
             ((eq state :initial-waiting) " [Init]")
             ((eq state :running) " [Running]")
             (t "")))))


;; Note: The following functions are commented out in the original file
;; (defun --ecc-vterm-mode-yes ()
;;   "Send 'y' response to Claude prompt."
;;   (--ecc-debug-message "Sending 'y' response")
;;   (when (fboundp 'vterm-send-string)
;;     (vterm-send-string "y")
;;     (vterm-send-return)
;;     (--ecc-debug-message "'y' response sent")))

;; (defun --ecc-vterm-mode-no ()
;;   "Send 'n' response to Claude prompt."
;;   (--ecc-debug-message "Sending 'n' response")
;;   (when (fboundp 'vterm-send-string)
;;     (vterm-send-string "n")
;;     (vterm-send-return)
;;     (--ecc-debug-message "'n' response sent")))

;; (defun --ecc-vterm-mode-create (&optional buffer)
;;   "Create a new Claude vterm buffer."
;;   (interactive)
;;   (--ecc-debug-message "Creating new Claude vterm buffer: %s"
;;                        --ecc-vterm-mode-buffer-name)
;;   (let ((buffer (get-buffer-create --ecc-vterm-mode-buffer-name)))
;;     (with-current-buffer buffer
;;       (unless (eq major-mode '--ecc-vterm-mode)
;;         (--ecc-debug-message "Initializing --ecc-vterm-mode")
;;         (--ecc-vterm-mode)))
;;     (switch-to-buffer buffer)
;;     (--ecc-debug-message "Switched to buffer: %s" (buffer-name buffer))
;;     buffer))


(provide 'ecc-vterm-mode)

(when
    (not load-file-name)
  (message "ecc-vterm-mode.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))