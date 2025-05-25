;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 09:20:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-color-themes.el

;;; Commentary:
;;; Color theme management for Claude vterm buffers.

(require 'ecc-variables)

;; Customization group
(defgroup ecc-colors nil
  "Color theme settings for Claude vterm buffers."
  :group 'ecc
  :prefix "ecc-colors-")

(defcustom ecc-colors-theme 'dark
  "Current theme for Claude vterm buffers.
Possible values are 'dark, 'light, and 'gray."
  :type '(choice (const :tag "Dark Theme" dark)
                (const :tag "Light Theme" light)
                (const :tag "Gray Theme" gray))
  :group 'ecc-colors)

(defcustom ecc-colors-dark-background "#1e1e1e"
  "Background color for dark theme."
  :type 'string
  :group 'ecc-colors)

(defcustom ecc-colors-dark-foreground "#d4d4d4"
  "Foreground color for dark theme."
  :type 'string
  :group 'ecc-colors)

(defcustom ecc-colors-light-background "#ffffff"
  "Background color for light theme."
  :type 'string
  :group 'ecc-colors)

(defcustom ecc-colors-light-foreground "#000000"
  "Foreground color for light theme."
  :type 'string
  :group 'ecc-colors)

(defcustom ecc-colors-gray-background "#2d2d2d"
  "Background color for gray theme."
  :type 'string
  :group 'ecc-colors)

(defcustom ecc-colors-gray-foreground "#c0c0c0"
  "Foreground color for gray theme."
  :type 'string
  :group 'ecc-colors)

(defvar-local ecc-colors--original-colors nil
  "Original colors before applying color theme.")

(defun ecc-colors-apply-theme (theme buffer)
  "Apply color THEME to the specified BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      ;; Save original colors if not saved yet
      (unless ecc-colors--original-colors
        (setq ecc-colors--original-colors
              (list :background (face-background 'default nil t)
                    :foreground (face-foreground 'default nil t))))
      
      ;; Apply selected theme
      (pcase theme
        ('dark
         (face-remap-add-relative 'default
                                  :background ecc-colors-dark-background
                                  :foreground ecc-colors-dark-foreground))
        ('light
         (face-remap-add-relative 'default
                                  :background ecc-colors-light-background
                                  :foreground ecc-colors-light-foreground))
        ('gray
         (face-remap-add-relative 'default
                                  :background ecc-colors-gray-background
                                  :foreground ecc-colors-gray-foreground))
        (_ nil))))) ; Do nothing for unknown themes

;;;###autoload
(defun ecc-colors-toggle-theme ()
  "Toggle between dark, light, and gray themes for the current Claude buffer."
  (interactive)
  (when (and (or (derived-mode-p 'vterm-mode)
                 (derived-mode-p 'ecc-term-claude-mode))
             (string-match-p "\\*CLAUDE.*\\*" (buffer-name)))
    ;; Cycle through themes
    (setq ecc-colors-theme
          (pcase ecc-colors-theme
            ('dark 'light)
            ('light 'gray)
            ('gray 'dark)
            (_ 'dark)))
    
    ;; Remove existing face remappings
    (face-remap-reset-base 'default)
    
    ;; Apply new theme
    (ecc-colors-apply-theme ecc-colors-theme (current-buffer))
    
    (ecc-debug-message "Claude theme set to %s" ecc-colors-theme)))

;;;###autoload
(defun ecc-colors-init-buffer ()
  "Initialize the current buffer with the chosen color theme."
  (when (and (or (derived-mode-p 'vterm-mode)
                 (derived-mode-p 'ecc-term-claude-mode))
             (string-match-p "\\*CLAUDE.*\\*" (buffer-name)))
    (ecc-colors-apply-theme ecc-colors-theme (current-buffer))))

;; Hook into Claude vterm buffers
(add-hook 'ecc-term-claude-mode-hook 'ecc-colors-init-buffer)
(add-hook 'vterm-mode-hook
          (lambda ()
            (when (string-match-p "\\*CLAUDE.*\\*" (buffer-name))
              (ecc-colors-init-buffer))))

(provide 'ecc-color-themes)

;;; ecc-color-themes.el ends here