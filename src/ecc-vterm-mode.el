;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-27 08:53:30>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-vterm-mode.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;; Optional dependency - vterm must be installed separately
(require 'vterm nil t)

(defgroup ecc-vterm nil
  "Customization group for Claude Code vterm mode."
  :group 'ecc
  :prefix "ecc-vterm-")

(defcustom ecc-vterm-hide-line-numbers t
  "Whether to hide line numbers in ecc-vterm-mode.
When non-nil, line numbers will be disabled in vterm buffers with ecc-vterm-mode."
  :type 'boolean
  :group 'ecc-vterm)

(defcustom ecc-vterm-default-font-size 0
  "Default font size adjustment for ecc-vterm buffers.
This is the text-scale-mode adjustment, not an absolute size.
Positive numbers increase size, negative numbers decrease size."
  :type 'integer
  :group 'ecc-vterm)

(defcustom ecc-vterm-font-size-step 1
  "Step size for font size adjustments in ecc-vterm buffers."
  :type 'integer
  :group 'ecc-vterm)

(defcustom ecc-vterm-min-font-size -5
  "Minimum font size adjustment allowed for ecc-vterm buffers."
  :type 'integer
  :group 'ecc-vterm)

(defcustom ecc-vterm-max-font-size 10
  "Maximum font size adjustment allowed for ecc-vterm buffers."
  :type 'integer
  :group 'ecc-vterm)

(define-minor-mode ecc-vterm-mode
  "Minor mode for optimized vterm buffers used with Claude Code.
This mode provides performance optimizations and enhanced features
for handling high-throughput interactions with Claude."
  :lighter " ECC-VT"
  :keymap (let ((map (make-sparse-keymap)))
            ;; Buffer management
            (define-key map (kbd "C-c C-t") 'ecc-vterm-truncate-buffer)
            (define-key map (kbd "C-c C-o")
                        'ecc-vterm-toggle-optimizations)

            ;; Visual settings
            (define-key map (kbd "C-c C-l")
                        'ecc-vterm-toggle-line-numbers)

            ;; Font size adjustment
            (define-key map (kbd "C-c C-+")
                        'ecc-vterm-increase-font-size)
            (define-key map (kbd "C-c C-=")
                        'ecc-vterm-increase-font-size)
                                        ; Alternative binding
            (define-key map (kbd "C-c C--")
                        'ecc-vterm-decrease-font-size)
            (define-key map (kbd "C-c C-0") 'ecc-vterm-reset-font-size)

            ;; Mouse wheel bindings for font size
            (define-key map (vector (list 'control 'mouse-4))
                        'ecc-vterm-increase-font-size)
            (define-key map (vector (list 'control 'mouse-5))
                        'ecc-vterm-decrease-font-size)

            map)
  (if ecc-vterm-mode
      (ecc-vterm--enable)
    (ecc-vterm--disable)))

(defvar-local ecc-vterm--saved-config nil
  "Saved configuration before enabling optimizations.")

(defun ecc-vterm--enable ()
  "Enable ecc-vterm-mode optimizations."
  ;; Save current configuration
  (setq ecc-vterm--saved-config
        (list :read-process-output-max read-process-output-max
              :vterm-timer-delay vterm-timer-delay
              :vterm-max-scrollback vterm-max-scrollback
              :font-lock-mode font-lock-mode
              :line-numbers display-line-numbers-mode
              :text-scale (if (boundp 'text-scale-mode-amount)
                              text-scale-mode-amount
                            0)))

  ;; Apply optimizations
  (setq read-process-output-max (* 1 1024 1024))  ; 1MB
  (setq vterm-timer-delay 0.1)
  (setq vterm-max-scrollback 4096)
  (setq vterm-disable-bold-font t)
  (setq vterm-disable-underline t)

  ;; Local optimizations
  (setq-local bidi-display-reordering nil)
  (setq-local bidi-paragraph-direction 'left-to-right)
  (setq-local inhibit-field-text-motion t)
  (setq-local line-move-visual nil)
  (setq-local font-lock-mode nil)

  ;; UI optimizations
  (when ecc-vterm-hide-line-numbers
    (display-line-numbers-mode -1))

  ;; Apply default font size for ecc-vterm
  (text-scale-set ecc-vterm-default-font-size)

  ;; Add GC optimization advice
  (advice-add 'vterm--flush-timer-callback :around
              #'ecc-vterm--gc-optimized-flush)

  ;; Color to Grayscale
  (ignore-errors (vterm-mode))

  (ecc-debug-message "ECC vterm mode enabled"))

(defun ecc-vterm--disable ()
  "Disable ecc-vterm-mode and restore settings."
  ;; Restore saved configuration
  (when ecc-vterm--saved-config
    (setq read-process-output-max
          (plist-get ecc-vterm--saved-config :read-process-output-max))
    (setq vterm-timer-delay
          (plist-get ecc-vterm--saved-config :vterm-timer-delay))
    (setq vterm-max-scrollback
          (plist-get ecc-vterm--saved-config :vterm-max-scrollback))
    (font-lock-mode
     (if (plist-get ecc-vterm--saved-config :font-lock-mode) 1 -1))

    ;; Only restore line numbers if they were enabled before
    (when (and (not ecc-vterm-hide-line-numbers)
               (plist-get ecc-vterm--saved-config :line-numbers))
      (display-line-numbers-mode 1))

    ;; Restore original text scale
    (let
        ((saved-scale (plist-get ecc-vterm--saved-config :text-scale)))
      (when saved-scale
        (text-scale-set saved-scale))))

  ;; Remove advice
  (advice-remove 'vterm--flush-timer-callback
                 #'ecc-vterm--gc-optimized-flush)

  (ecc-debug-message "ECC vterm mode disabled"))

(defun ecc-vterm--gc-optimized-flush (orig-fun &rest args)
  "Run vterm flush with optimized garbage collection."
  (let ((gc-cons-threshold (* 100 1024 1024)))
                                        ; 100MB during flush
    (apply orig-fun args)))

(defun ecc-vterm-truncate-buffer ()
  "Truncate vterm buffer to recent lines for performance."
  (interactive)
  (when (eq major-mode 'vterm-mode)
    (let ((inhibit-read-only t)
          (max-lines (or vterm-max-scrollback 4096)))
      (save-excursion
        (goto-char (point-max))
        (forward-line (- max-lines))
        (beginning-of-line)
        (delete-region (point-min) (point)))
      (ecc-debug-message "Buffer truncated to %d lines" max-lines))))

(defun ecc-vterm-toggle-optimizations ()
  "Toggle vterm optimizations on/off."
  (interactive)
  (if ecc-vterm-mode
      (ecc-vterm-mode -1)
    (ecc-vterm-mode 1)))

(defun ecc-vterm-toggle-line-numbers ()
  "Toggle line numbers display in the current vterm buffer."
  (interactive)
  (if (bound-and-true-p display-line-numbers-mode)
      (progn
        (display-line-numbers-mode -1)
        (ecc-debug-message "Line numbers disabled"))
    (progn
      (display-line-numbers-mode 1)
      (ecc-debug-message "Line numbers enabled")))
  ;; Update preference if in ecc-vterm-mode
  (when ecc-vterm-mode
    (setq-local ecc-vterm-hide-line-numbers
                (not (bound-and-true-p display-line-numbers-mode)))))

(defun ecc-vterm-increase-font-size (&optional n)
  "Increase the font size in current vterm buffer by N steps.
If N is nil, uses `ecc-vterm-font-size-step' as the step size."
  (interactive "P")
  (when (derived-mode-p 'vterm-mode)
    (let* ((step (or n ecc-vterm-font-size-step))
           (new-size (min ecc-vterm-max-font-size
                          (+ (or (and (boundp 'text-scale-mode-amount)
                                      text-scale-mode-amount)
                                 0)
                             step))))
      (text-scale-set new-size)
      (ecc-debug-message "Font size: %+d" new-size))))

(defun ecc-vterm-decrease-font-size (&optional n)
  "Decrease the font size in current vterm buffer by N steps.
If N is nil, uses `ecc-vterm-font-size-step' as the step size."
  (interactive "P")
  (when (derived-mode-p 'vterm-mode)
    (let* ((step (or n ecc-vterm-font-size-step))
           (new-size (max ecc-vterm-min-font-size
                          (- (or (and (boundp 'text-scale-mode-amount)
                                      text-scale-mode-amount)
                                 0)
                             step))))
      (text-scale-set new-size)
      (ecc-debug-message "Font size: %+d" new-size))))

(defun ecc-vterm-reset-font-size ()
  "Reset the font size to default in current vterm buffer."
  (interactive)
  (when (derived-mode-p 'vterm-mode)
    (text-scale-set ecc-vterm-default-font-size)
    (ecc-debug-message "Font size reset to default: %+d"
                       ecc-vterm-default-font-size)))

;; Auto-enable for Claude buffers

(defun ecc-vterm-auto-enable ()
  "Automatically enable ecc-vterm-mode for Claude buffers."
  (when (and (derived-mode-p 'vterm-mode)
             (string-match-p "\\*CLAUDE.*\\*" (buffer-name)))
    (ecc-vterm-mode 1)))

;; Default hook for all vterm buffers that look like Claude buffers
(add-hook 'vterm-mode-hook #'ecc-vterm-auto-enable)

(when
    (not load-file-name)
  (ecc-debug-message "ecc-vterm-mode.el loaded."
                     (file-name-nondirectory
                      (or load-file-name buffer-file-name))))


(provide 'ecc-vterm-mode)

(when
    (not load-file-name)
  (message "ecc-vterm-mode.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))