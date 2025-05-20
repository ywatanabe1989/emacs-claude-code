;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-12 07:09:30>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/examples/repository/repository-usage.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; Commentary:
;; Repository usage examples for emacs-claude-code.
;; This example demonstrates how to:
;; 1. Copy repository contents to clipboard
;; 2. Configure repository settings
;; 3. Work with repositories in different ways
;; 4. Customize repository filters

;;; Code:

(require 'emacs-claude-code)
(require 'ecc-repository)

;; Basic example of copying a repository
(defun example-repository-copy-basic ()
  "Copy the contents of the current project to the clipboard.
Uses the default settings for file filtering and sizing."
  (interactive)
  (let* ((project-dir (expand-file-name default-directory))
         (output-file (expand-file-name "repository-export.md" project-dir)))
    ;; Set custom output file for this example
    (let ((ecc-repository-output-file output-file))
      ;; Copy the repository contents
      (ecc-repository-copy-contents project-dir)
      (message "Repository copied to clipboard and saved to %s" output-file))))

;; Example with custom filters
(defun example-repository-copy-filtered ()
  "Copy repository contents with custom filters.
Only includes elisp files and limited size."
  (interactive)
  (let* ((project-dir (read-directory-name "Project directory: " 
                                          default-directory))
         (output-file (expand-file-name "elisp-only-export.md" project-dir)))
    ;; Set custom blacklist to exclude everything except elisp
    (let ((ecc-repository-output-file output-file)
          (ecc-repository-file-blacklist
           '("\\.git" "^[^.]*\\.(?!el$).*$" ; Only include .el files
             "\\.png$" "\\.jpg$" "\\.jpeg$" "\\.gif$" "\\.pdf$"
             "\\.zip$" "\\.tar$" "\\.gz$"))
          ;; Limit to small files only
          (ecc-repository-max-file-size 50000))
      ;; Copy the repository contents
      (ecc-repository-copy-contents project-dir)
      (message "Filtered repository copied to clipboard and saved to %s" 
               output-file))))

;; Example integration with Claude
(defun example-repository-analyze-with-claude ()
  "Copy repository contents and send to Claude for analysis."
  (interactive)
  (let* ((project-dir (read-directory-name "Project directory: " 
                                          default-directory))
         (output-file (expand-file-name "repository-for-claude.md" project-dir))
         (buffer-name "*Claude Repository Analysis*"))
    
    ;; Set custom output file for this example
    (let ((ecc-repository-output-file output-file))
      ;; Copy the repository contents 
      (ecc-repository-copy-contents project-dir)
      
      ;; Create a new Claude buffer or use existing one
      (let ((buffer (get-buffer-create buffer-name)))
        (with-current-buffer buffer
          ;; Add prompt for Claude
          (goto-char (point-max))
          (insert "\n\n# Repository Analysis Request\n\n")
          (insert "I'm sharing a codebase with you for analysis. Please provide:\n\n")
          (insert "1. An overview of the project structure\n")
          (insert "2. Key components and their relationships\n")
          (insert "3. Any architectural patterns you observe\n")
          (insert "4. Suggestions for potential improvements\n\n")
          
          ;; Insert the repository content from clipboard
          (insert "Here's the codebase:\n\n")
          (yank)
          
          ;; Switch to the buffer
          (switch-to-buffer buffer)
          
          ;; Message to the user
          (message "Repository content ready for sending to Claude."))))))

;; Example customization of repository settings
(defun example-repository-customize-settings ()
  "Customize repository extraction settings interactively."
  (interactive)
  (let ((settings-buffer (get-buffer-create "*Repository Settings*")))
    (with-current-buffer settings-buffer
      (erase-buffer)
      (insert "# Repository Extraction Settings\n\n")
      
      ;; Display current settings
      (insert "## Current Settings\n\n")
      (insert (format "- Output file: %s\n" ecc-repository-output-file))
      (insert (format "- Max file size: %d bytes\n" ecc-repository-max-file-size))
      (insert "- Blacklisted patterns:\n")
      (dolist (pattern ecc-repository-file-blacklist)
        (insert (format "  - %s\n" pattern)))
      
      ;; Provide example customizations
      (insert "\n## Example Customizations\n\n")
      (insert "```elisp\n")
      (insert ";; Set custom output file\n")
      (insert "(setq ecc-repository-output-file \"./docs/MY_REPO.md\")\n\n")
      (insert ";; Increase maximum file size (500KB)\n")
      (insert "(setq ecc-repository-max-file-size 500000)\n\n")
      (insert ";; Custom blacklist\n")
      (insert "(setq ecc-repository-file-blacklist\n")
      (insert "      '(\"\\\\.git\" \"\\\\.DS_Store$\" \"node_modules\"\n")
      (insert "        \"\\\\.png$\" \"\\\\.jpg$\" \"\\\\.pdf$\"))\n")
      (insert "```\n\n")
      
      ;; Provide example usage code
      (insert "## Example Usage\n\n")
      (insert "```elisp\n")
      (insert ";; Basic usage\n")
      (insert "(ecc-repository-copy-contents \"/path/to/repository\")\n\n")
      (insert ";; With let-binding for temporary settings\n")
      (insert "(let ((ecc-repository-output-file \"./custom-output.md\")\n")
      (insert "      (ecc-repository-max-file-size 1000000))\n")
      (insert "  (ecc-repository-copy-contents \"/path/to/repository\"))\n")
      (insert "```\n")
      
      ;; Switch to the buffer
      (switch-to-buffer settings-buffer)
      (help-mode))))

;; Example repository selection with magit support
(defun example-repository-select-with-magit ()
  "Select repository using magit if available, otherwise prompt."
  (interactive)
  (let (repo-dir)
    ;; Try to use magit to get the repository root if available
    (if (and (fboundp 'magit-toplevel)
             (magit-toplevel))
        (setq repo-dir (magit-toplevel))
      ;; Otherwise prompt the user
      (setq repo-dir (read-directory-name "Repository root: " 
                                         default-directory)))
    
    ;; Ask for confirmation
    (when (y-or-n-p (format "Copy repository from %s? " repo-dir))
      (ecc-repository-copy-contents repo-dir)
      (message "Repository copied from %s" repo-dir))))

(provide 'repository-usage)

(when (not load-file-name)
  (message "repository-usage.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

;;; repository-usage.el ends here