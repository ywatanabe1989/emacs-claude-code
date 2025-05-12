;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 10:14:05>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/examples/template/template-usage.el

;;; Commentary:
;; Examples of template usage with emacs-claude-code.
;; This file demonstrates how to:
;; 1. Use built-in templates
;; 2. Create custom templates
;; 3. Manage template caching
;; 4. Integrate templates into your workflow

;;; Code:

(require 'emacs-claude-code)
(require 'ecc-template)
(require 'ecc-template-cache)
(require 'ecc-template-mode)

;; Example of using a built-in template
(defun example-use-programming-template ()
  "Use the built-in Programming template with Claude."
  (interactive)
  
  ;; Get the current Claude buffer
  (let ((buffer (ecc-buffer-get-or-create-active-buffer)))
    
    ;; Switch to the buffer
    (switch-to-buffer buffer)
    
    ;; Load the Programming template
    (ecc-template-load "Programming")
    
    ;; Add a specific programming question
    (goto-char (point-max))
    (insert "\nWrite a recursive function in Python to calculate the Fibonacci sequence.")
    
    ;; Send the prompt to Claude
    (ecc-send-buffer-content)
    
    (message "Sent Programming template prompt to Claude.")))

;; Create a custom template
(defun example-create-custom-template ()
  "Create a custom template for Claude."
  (interactive)
  
  ;; Define the template content
  (let ((template-name "CustomExample")
        (template-content "# Custom Example Template

## Instructions
You are an assistant specialized in providing step-by-step explanations.
When responding to questions, please follow these guidelines:

1. Break down the solution into clear, numbered steps
2. Explain each step with simple, concise language
3. Provide a summary at the end
4. If code is involved, add comments to explain key parts

## Query
"))
    
    ;; Create the template file
    (let ((template-dir (expand-file-name "templates/claude" 
                                         (file-name-directory
                                          (locate-library "emacs-claude-code")))))
      
      ;; Create the template file
      (with-temp-buffer
        (insert template-content)
        (write-file (expand-file-name (concat template-name ".md") template-dir)))
      
      ;; Reset the template cache to include the new template
      (ecc-template-reset-cache)
      
      (message "Created custom template '%s'." template-name))))

;; Example of using template cache
(defun example-display-template-cache-info ()
  "Display information about the current template cache."
  (interactive)
  
  ;; Get the template cache information
  (let* ((cache (ecc-template-get-cache))
         (cache-size (hash-table-count cache))
         (templates (hash-table-keys cache)))
    
    ;; Create a buffer to display the information
    (with-current-buffer (get-buffer-create "*TEMPLATE-CACHE-INFO*")
      (erase-buffer)
      (insert "Claude Template Cache Information\n")
      (insert "===============================\n\n")
      
      (insert (format "Cache Size: %d templates\n\n" cache-size))
      
      (insert "Available Templates:\n")
      (insert "-------------------\n")
      (dolist (template (sort templates 'string<))
        (insert (format "- %s\n" template)))
      
      (insert "\nUse (ecc-template-load \"TemplateName\") to load a template.\n")
      (insert "Use (ecc-template-reset-cache) to refresh the template cache.\n")
      
      (special-mode)
      (switch-to-buffer (current-buffer)))))

;; Example of creating a template selection interface
(defun example-template-selector ()
  "Create an interactive template selector for Claude."
  (interactive)
  
  ;; Get all available templates
  (let* ((cache (ecc-template-get-cache))
         (templates (sort (hash-table-keys cache) 'string<))
         (selected-template nil))
    
    ;; Create the selection buffer
    (with-current-buffer (get-buffer-create "*TEMPLATE-SELECTOR*")
      (erase-buffer)
      (insert "Claude Template Selector\n")
      (insert "======================\n\n")
      (insert "Press the key shown to select a template:\n\n")
      
      ;; Display templates with keybindings
      (let ((key ?a))
        (dolist (template templates)
          (insert (format "[%c] %s\n" key template))
          (setq key (1+ key))))
      
      ;; Set up a keymap for template selection
      (let ((keymap (make-sparse-keymap)))
        (let ((key ?a))
          (dolist (template templates)
            (define-key keymap (char-to-string key)
              (lambda ()
                (interactive)
                (setq selected-template template)
                (kill-buffer)
                (example-use-selected-template selected-template)))
            (setq key (1+ key))))
        
        ;; Also bind ESC to cancel
        (define-key keymap (kbd "ESC")
          (lambda ()
            (interactive)
            (kill-buffer)
            (message "Template selection canceled.")))
        
        ;; Activate the keymap
        (use-local-map keymap))
      
      (special-mode)
      (switch-to-buffer (current-buffer)))))

;; Helper function for the template selector
(defun example-use-selected-template (template-name)
  "Use the selected TEMPLATE-NAME with Claude."
  (let ((buffer (ecc-buffer-get-or-create-active-buffer)))
    (switch-to-buffer buffer)
    (ecc-template-load template-name)
    (message "Loaded template: %s" template-name)))

;; Example of creating a template editing interface
(defun example-edit-template (template-name)
  "Edit the Claude template with TEMPLATE-NAME."
  (interactive
   (list (completing-read "Edit template: " 
                          (hash-table-keys (ecc-template-get-cache)))))
  
  ;; Get the template file path
  (let* ((template-dir (expand-file-name "templates/claude" 
                                        (file-name-directory
                                         (locate-library "emacs-claude-code"))))
         (template-file (expand-file-name (concat template-name ".md") template-dir)))
    
    ;; Check if the file exists
    (if (file-exists-p template-file)
        (progn
          ;; Open the template file for editing
          (find-file template-file)
          
          ;; Enable template-mode if available
          (when (fboundp 'ecc-template-mode)
            (ecc-template-mode))
          
          (message "Editing template: %s" template-name))
      
      (message "Template file not found: %s" template-file))))

(provide 'example-template-usage)
;;; template-usage.el ends here