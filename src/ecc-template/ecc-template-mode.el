;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 13:50:42>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/ecc-template/ecc-template-mode.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ecc-template/ecc-template)

;; Define markdown-mode fallback for testing
(unless (require 'markdown-mode nil t)
  (define-derived-mode markdown-mode text-mode "Markdown"
    "Stub markdown mode for testing."))

;;;###autoload
(define-derived-mode ecc-template-mode markdown-mode "Claude Template"
  "Major mode for editing Claude prompt templates.
Provides syntax highlighting and special commands for managing Claude templates."
  (setq-local comment-start "<!-- ")
  (setq-local comment-end " -->")
  
  ;; Highlight template variables like {{variable}}
  (font-lock-add-keywords
   nil
   '(("{{\\([^}]+\\)}}" 
      (0 font-lock-variable-name-face t))))
  
  ;; Template-specific key bindings
  (let ((map ecc-template-mode-map))
    (define-key map (kbd "C-c C-c") 'ecc-template-save)
    (define-key map (kbd "C-c C-p") 'ecc-template-preview)
    (define-key map (kbd "C-c C-v") 'ecc-template-insert-variable)))

;; Commands for template mode

(defun ecc-template-save ()
  "Save the current template."
  (interactive)
  (save-buffer)
  (message "Template saved"))

(defun ecc-template-preview ()
  "Preview the current template in a Claude buffer."
  (interactive)
  (let ((template-content (buffer-string))
        (template-name (file-name-base (buffer-file-name))))
    (with-temp-buffer
      (insert template-content)
      (let ((preview-text (buffer-string)))
        (pop-to-buffer (get-buffer-create (format "*Preview: %s*" template-name)))
        (erase-buffer)
        (insert preview-text)
        (markdown-mode)
        (goto-char (point-min))
        (message "Template preview generated")))))

(defun ecc-template-insert-variable ()
  "Insert a template variable at point."
  (interactive)
  (let ((var-name (completing-read "Variable name: " 
                                   '("input" "context" "date" "time" "user" "project"))))
    (insert (format "{{%s}}" var-name))))

;; Auto-mode-alist entry for template files
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.claude-template\\'" . ecc-template-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("/templates/claude/.*\\.md\\'" . ecc-template-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("/templates/genai/.*\\.md\\'" . ecc-template-mode))

(provide 'ecc-template/ecc-template-mode)

(when
    (not load-file-name)
  (message "ecc-template-mode.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))