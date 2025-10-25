;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-10-24 18:45:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-anti-flicker.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:
;; Anti-flicker optimizations for Claude Code in vterm.
;; Addresses the Ink rendering issues that cause screen flashing,
;; uncontrollable scrolling, and other visual glitches.
;;
;; Based on community research and workarounds documented in:
;; - claude-code-scrolling-bug-overview.md
;; - claude-code-quick-solutions.md

;; 1. Dependencies
;; ----------------------------------------
(require 'ecc-debug)
(require 'ecc-vterm-utils)

;; 2. Configuration
;; ----------------------------------------

(defgroup ecc-anti-flicker nil
  "Anti-flicker optimizations for Claude Code in vterm."
  :prefix "--ecc-anti-flicker-"
  :group 'ecc)

(defcustom --ecc-anti-flicker-scrollback-size 100000
  "Maximum scrollback buffer size for vterm.
Set to 100,000 to preserve long Claude Code conversations.
This is the maximum supported by vterm's hardcoded limit."
  :type 'integer
  :group 'ecc-anti-flicker)

(defcustom --ecc-anti-flicker-context-threshold 0.5
  "Buffer size threshold (as fraction of max) to warn about.
When buffer exceeds this ratio, suggest using /clear command.
Default: 0.5 (50% of scrollback limit)."
  :type 'float
  :group 'ecc-anti-flicker)

(defcustom --ecc-anti-flicker-check-interval 30.0
  "Interval in seconds to check buffer size and warn if needed.
Default: 30 seconds."
  :type 'float
  :group 'ecc-anti-flicker)

(defcustom --ecc-anti-flicker-auto-warn t
  "Whether to automatically warn when buffer size approaches threshold."
  :type 'boolean
  :group 'ecc-anti-flicker)

(defcustom --ecc-anti-flicker-font-family "JuliaMono"
  "Font family for Unicode rendering of Ink borders.
Good alternatives: DejaVu Sans Mono, Fira Code, Iosevka, Cascadia Code."
  :type 'string
  :group 'ecc-anti-flicker)

;; 3. Variables
;; ----------------------------------------

(defvar-local --ecc-anti-flicker--enabled nil
  "Whether anti-flicker mode is enabled in this buffer.")

(defvar-local --ecc-anti-flicker--check-timer nil
  "Timer for periodic buffer size checks.")

(defvar-local --ecc-anti-flicker--last-warning-time 0
  "Timestamp of last warning to avoid spam.")

(defvar-local --ecc-anti-flicker--warning-cooldown 300
  "Minimum seconds between warnings (5 minutes).")

(defvar-local --ecc-anti-flicker--original-scrollback nil
  "Original vterm-max-scrollback value before anti-flicker mode.")

;; 4. Core Functions
;; ----------------------------------------

;;;###autoload
(defun --ecc-anti-flicker-enable (&optional buffer)
  "Enable anti-flicker optimizations for BUFFER."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (unless (derived-mode-p 'vterm-mode)
      (user-error "Anti-flicker mode only works in vterm buffers"))

    ;; Store original values
    (setq-local --ecc-anti-flicker--original-scrollback
                (if (boundp 'vterm-max-scrollback)
                    vterm-max-scrollback
                  1000))

    ;; Apply vterm optimizations
    (when (boundp 'vterm-max-scrollback)
      (setq-local vterm-max-scrollback --ecc-anti-flicker-scrollback-size))

    (when (boundp 'vterm-clear-scrollback-when-clearing)
      (setq-local vterm-clear-scrollback-when-clearing nil))

    (when (boundp 'vterm-term-environment-variable)
      (setq-local vterm-term-environment-variable "xterm-256color"))

    (when (boundp 'vterm-timer-delay)
      (setq-local vterm-timer-delay 0.1))

    ;; Start monitoring if auto-warn is enabled
    (when --ecc-anti-flicker-auto-warn
      (--ecc-anti-flicker--start-monitoring))

    (setq-local --ecc-anti-flicker--enabled t)
    (--ecc-debug-message "Anti-flicker mode enabled in %s" (buffer-name))
    (message "Anti-flicker mode enabled (scrollback: %d lines)"
             --ecc-anti-flicker-scrollback-size)))

;;;###autoload
(defun --ecc-anti-flicker-disable (&optional buffer)
  "Disable anti-flicker optimizations for BUFFER."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    ;; Restore original values
    (when --ecc-anti-flicker--original-scrollback
      (when (boundp 'vterm-max-scrollback)
        (setq-local vterm-max-scrollback
                    --ecc-anti-flicker--original-scrollback)))

    ;; Stop monitoring
    (--ecc-anti-flicker--stop-monitoring)

    (setq-local --ecc-anti-flicker--enabled nil)
    (--ecc-debug-message "Anti-flicker mode disabled in %s" (buffer-name))
    (message "Anti-flicker mode disabled")))

;;;###autoload
(defun --ecc-anti-flicker-toggle (&optional buffer)
  "Toggle anti-flicker mode for BUFFER."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (if --ecc-anti-flicker--enabled
        (--ecc-anti-flicker-disable buffer)
      (--ecc-anti-flicker-enable buffer))))

;; 5. Buffer Size Monitoring
;; ----------------------------------------

(defun --ecc-anti-flicker--start-monitoring ()
  "Start periodic buffer size monitoring."
  (when --ecc-anti-flicker--check-timer
    (cancel-timer --ecc-anti-flicker--check-timer))
  (setq-local --ecc-anti-flicker--check-timer
              (run-with-timer --ecc-anti-flicker-check-interval
                              --ecc-anti-flicker-check-interval
                              #'--ecc-anti-flicker--check-buffer-size
                              (current-buffer))))

(defun --ecc-anti-flicker--stop-monitoring ()
  "Stop periodic buffer size monitoring."
  (when --ecc-anti-flicker--check-timer
    (cancel-timer --ecc-anti-flicker--check-timer)
    (setq-local --ecc-anti-flicker--check-timer nil)))

(defun --ecc-anti-flicker--check-buffer-size (buffer)
  "Check BUFFER size and warn if approaching threshold."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when --ecc-anti-flicker--enabled
        (let* ((buffer-size (- (point-max) (point-min)))
               (max-size (* --ecc-anti-flicker-scrollback-size 80)) ; Assume ~80 chars/line
               (ratio (/ (float buffer-size) max-size))
               (current-time (float-time))
               (time-since-warning (- current-time
                                     --ecc-anti-flicker--last-warning-time)))
          (when (and (>= ratio --ecc-anti-flicker-context-threshold)
                     (>= time-since-warning --ecc-anti-flicker--warning-cooldown))
            (--ecc-anti-flicker--warn-buffer-size ratio)
            (setq-local --ecc-anti-flicker--last-warning-time current-time)))))))

(defun --ecc-anti-flicker--warn-buffer-size (ratio)
  "Warn user that buffer size is at RATIO of threshold."
  (let ((percentage (round (* ratio 100))))
    (message
     "[ECC Anti-Flicker] Buffer is %d%% full. Consider using /clear to prevent flicker issues."
     percentage)
    (--ecc-debug-message "Buffer size warning: %d%% of threshold" percentage)))

;; 6. Font Configuration
;; ----------------------------------------

;;;###autoload
(defun --ecc-anti-flicker-configure-fonts ()
  "Configure fonts for optimal Ink border rendering."
  (interactive)
  (when (display-graphic-p)
    ;; Ensure proper Unicode rendering
    (setq use-default-font-for-symbols nil)
    (when (font-info --ecc-anti-flicker-font-family)
      (set-fontset-font t 'unicode
                        (font-spec :family --ecc-anti-flicker-font-family))
      (message "Configured Unicode fonts for Ink borders: %s"
               --ecc-anti-flicker-font-family))
    (unless (font-info --ecc-anti-flicker-font-family)
      (message "Font '%s' not found. Please install or change --ecc-anti-flicker-font-family"
               --ecc-anti-flicker-font-family))))

;; 7. Diagnostic Functions
;; ----------------------------------------

;;;###autoload
(defun --ecc-anti-flicker-status ()
  "Show current anti-flicker status and buffer statistics."
  (interactive)
  (let* ((enabled --ecc-anti-flicker--enabled)
         (buffer-size (- (point-max) (point-min)))
         (max-size (* --ecc-anti-flicker-scrollback-size 80))
         (ratio (/ (float buffer-size) max-size))
         (percentage (round (* ratio 100))))
    (message "=== ECC Anti-Flicker Status ===")
    (message "Enabled: %s" (if enabled "yes" "no"))
    (message "Buffer size: %d chars" buffer-size)
    (message "Capacity used: %d%%" percentage)
    (message "Scrollback limit: %d lines"
             (if (boundp 'vterm-max-scrollback)
                 vterm-max-scrollback
               "N/A"))
    (message "Auto-warn: %s" (if --ecc-anti-flicker-auto-warn "yes" "no"))
    (message "Monitoring: %s"
             (if --ecc-anti-flicker--check-timer "active" "inactive"))
    (message "=============================")
    (when (>= ratio --ecc-anti-flicker-context-threshold)
      (message "⚠ Buffer approaching threshold! Consider /clear command"))))

;; 8. Integration Helpers
;; ----------------------------------------

;;;###autoload
(defun --ecc-anti-flicker-setup-vterm-hook ()
  "Add anti-flicker setup to vterm-mode-hook."
  (interactive)
  (add-hook 'vterm-mode-hook #'--ecc-anti-flicker-enable)
  (message "Anti-flicker will auto-enable for new vterm buffers"))

;;;###autoload
(defun --ecc-anti-flicker-remove-vterm-hook ()
  "Remove anti-flicker setup from vterm-mode-hook."
  (interactive)
  (remove-hook 'vterm-mode-hook #'--ecc-anti-flicker-enable)
  (message "Anti-flicker auto-enable removed from vterm buffers"))

(when (not load-file-name)
  (--ecc-debug-message "ecc-anti-flicker.el loaded."))

(provide 'ecc-anti-flicker)

(when (not load-file-name)
  (message "ecc-anti-flicker.el loaded."))
