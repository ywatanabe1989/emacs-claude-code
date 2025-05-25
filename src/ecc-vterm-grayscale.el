;;; -*- coding: utf-8; lexical-binding: t -*-
;;; ecc-vterm-grayscale.el --- Grayscale mode for vterm buffers
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 15:30:00>

;;; Commentary:
;;; This module provides functionality to toggle between color and grayscale
;;; modes in vterm buffers, particularly useful for reducing visual noise
;;; in Claude terminal sessions.

;;; Code:

(require 'ecc-variables)

;;;; Customization

(defgroup ecc-vterm-grayscale nil
  "Settings for vterm grayscale mode."
  :group 'ecc
  :prefix "ecc-vterm-grayscale-")

(defcustom ecc-vterm-grayscale-default t
  "Whether grayscale mode is enabled by default in new vterm buffers."
  :type 'boolean
  :group 'ecc-vterm-grayscale)

(defcustom ecc-vterm-grayscale-auto-enable t
  "Automatically enable grayscale mode for Claude vterm buffers.
When set to t, grayscale mode will be enabled automatically when
opening a Claude vterm buffer."
  :type 'boolean
  :group 'ecc-vterm-grayscale)

(defcustom ecc-vterm-grayscale-indicator t
  "Whether to display a mode indicator when grayscale mode is active."
  :type 'boolean
  :group 'ecc-vterm-grayscale)

(defcustom ecc-vterm-grayscale-indicator-text "🔘 GRAYSCALE"
  "Text to display in the mode line when grayscale mode is active."
  :type 'string
  :group 'ecc-vterm-grayscale)

;;;; Core functionality

(defvar-local ecc-vterm-grayscale-mode t
  "Whether grayscale mode is enabled in the current buffer.")

;;;###autoload
(defun ecc-vterm-grayscale-toggle ()
  "Toggle between color and grayscale modes in vterm.
This affects both vterm's internal color handling and ANSI color processing."
  (interactive)
  (if (derived-mode-p 'vterm-mode)
      (let ((grayscale-on (not ecc-vterm-grayscale-mode)))
        ;; Set vterm's color inhibit variable
        (when (boundp 'vterm-colors-inhibit)
          (setq-local vterm-colors-inhibit grayscale-on))
        
        ;; Also set ansi-color variables for compatibility
        (when (boundp 'ansi-colors-disabled)
          (setq-local ansi-colors-disabled grayscale-on))
        
        ;; Set our tracking variable
        (setq-local ecc-vterm-grayscale-mode grayscale-on)
        
        ;; Force terminal refresh if possible
        (when (fboundp 'vterm-reset-cursor-point)
          (vterm-reset-cursor-point))
        
        ;; Update mode line indicator
        (when ecc-vterm-grayscale-indicator
          (ecc-vterm-grayscale-update-mode-line))
        
        (ecc-debug-message "Vterm grayscale mode: %s" 
                 (if grayscale-on "enabled" "disabled")))
    (ecc-debug-message "Grayscale mode is only available in vterm buffers")))

;;;###autoload
(define-minor-mode ecc-vterm-grayscale-minor-mode
  "Minor mode to enable grayscale rendering in vterm buffers.
When enabled, ANSI colors will be rendered in grayscale rather than color."
  :lighter (when ecc-vterm-grayscale-indicator
             (concat " " ecc-vterm-grayscale-indicator-text))
  :init-value nil
  (if (derived-mode-p 'vterm-mode)
      (let ((enable ecc-vterm-grayscale-minor-mode))
        ;; Set vterm's color inhibit variable
        (when (boundp 'vterm-colors-inhibit)
          (setq-local vterm-colors-inhibit enable))
        
        ;; Also set ansi-color variables for compatibility
        (when (boundp 'ansi-colors-disabled)
          (setq-local ansi-colors-disabled enable))
        
        ;; Set our tracking variable
        (setq-local ecc-vterm-grayscale-mode enable)
        
        ;; Force terminal refresh
        (when (fboundp 'vterm-reset-cursor-point)
          (vterm-reset-cursor-point)))
    
    ;; Not in vterm mode, disable the mode
    (setq ecc-vterm-grayscale-minor-mode nil)
    (ecc-debug-message "Grayscale mode is only available in vterm buffers")))

(defun ecc-vterm-grayscale-update-mode-line ()
  "Update the mode line indicator for grayscale mode."
  (let ((lighter (if ecc-vterm-grayscale-mode
                     (concat " " ecc-vterm-grayscale-indicator-text)
                   "")))
    (setq mode-line-format
          (mapcar (lambda (item)
                    (if (and (listp item)
                             (eq (car item) 'ecc-vterm-grayscale-minor-mode))
                        `(ecc-vterm-grayscale-minor-mode ,lighter)
                      item))
                  mode-line-format))
    (force-mode-line-update)))

;;;; Auto-setup

(defun ecc-vterm-grayscale-maybe-enable ()
  "Enable grayscale mode if appropriate for this buffer.
Based on user configuration in `ecc-vterm-grayscale-auto-enable`."
  (when (and (derived-mode-p 'vterm-mode)
             (or ecc-vterm-grayscale-default
                 (and ecc-vterm-grayscale-auto-enable
                      (string-match-p "\\*CLAUDE.*\\*" (buffer-name)))))
    (ecc-vterm-grayscale-minor-mode 1)))

;; Add to vterm hooks
(add-hook 'vterm-mode-hook 'ecc-vterm-grayscale-maybe-enable)
(add-hook 'ecc-term-claude-mode-hook 'ecc-vterm-grayscale-maybe-enable)

;;;; Integration with other modules

;; Integrate with color themes if available
(with-eval-after-load 'ecc-color-themes
  (defun ecc-vterm-grayscale-after-theme-change ()
    "Update grayscale mode after theme changes."
    (when (and (derived-mode-p 'vterm-mode) 
               ecc-vterm-grayscale-mode)
      (vterm-reset-cursor-point)))
  
  (add-hook 'ecc-colors-theme-changed-hook 
            'ecc-vterm-grayscale-after-theme-change))

(provide 'ecc-vterm-grayscale)
;;; ecc-vterm-grayscale.el ends here
