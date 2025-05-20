;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-19 20:40:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/examples/vterm-yank-file-usage.el

;;; Commentary:
;;; Example usage of yank-as-file functionality in vterm.

(require 'ecc-vterm-yank-as-file)

;; Custom configuration
(setq ecc-vterm-yank-threshold 500)  ; Smaller threshold for demo
(setq ecc-vterm-yank-directory (expand-file-name "~/tmp/claude-yanks/"))

;; Example: Paste large code snippet as file
(defun my-paste-code-as-file ()
  "Paste code from clipboard as a file in vterm."
  (interactive)
  (let ((code (gui-get-selection 'CLIPBOARD 'STRING)))
    (if code
        (progn
          ;; Force save as file regardless of size
          (let ((ecc-vterm-yank-threshold 0))
            (ecc-vterm-yank-as-file code))
          ;; Optionally add a comment
          (vterm-send-string " # Pasted code from clipboard")
          (vterm-send-return))
      (message "No code in clipboard"))))

;; Example: Smart paste with size detection
(defun my-smart-claude-paste ()
  "Intelligently paste content for Claude - as text or file based on size."
  (interactive)
  (let* ((content (or (car kill-ring)
                      (gui-get-selection 'CLIPBOARD 'STRING)))
         (size (length content))
         (lines (count-lines (point-min) (point-max) content)))
    
    (cond
     ;; Very large content - always save as file
     ((> size 10000)
      (let ((ecc-vterm-yank-threshold 0))
        (ecc-vterm-yank-as-file content))
      (message "Large content (%d chars, %d lines) saved as file" size lines))
     
     ;; Medium content - ask user
     ((> size 1000)
      (if (y-or-n-p (format "Content is %d chars. Save as file? " size))
          (let ((ecc-vterm-yank-threshold 0))
            (ecc-vterm-yank-as-file content))
        (ecc-vterm-yank-as-file content)))
     
     ;; Small content - paste directly
     (t
      (ecc-vterm-yank-as-file content)
      (message "Pasted %d chars directly" size)))))

;; Example: Create a file with specific content for Claude
(defun my-send-prompt-file-to-claude (prompt)
  "Send a PROMPT to Claude by creating a temporary file."
  (interactive "sPrompt for Claude: ")
  (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (filename (format "claude-prompt-%s.txt" timestamp))
         (filepath (expand-file-name filename ecc-vterm-yank-directory)))
    
    ;; Ensure directory exists
    (unless (file-exists-p ecc-vterm-yank-directory)
      (make-directory ecc-vterm-yank-directory t))
    
    ;; Write prompt to file
    (with-temp-file filepath
      (insert prompt))
    
    ;; Send cat command to vterm
    (when (derived-mode-p 'vterm-mode)
      (vterm-send-string (format "cat '%s'" filepath))
      (vterm-send-return)
      (message "Prompt sent from file: %s" filepath))))

;; Example: Paste with automatic cleanup
(defun my-paste-with-cleanup ()
  "Paste content and clean old files."
  (interactive)
  ;; Clean old files first
  (ecc-vterm-clean-yank-files)
  
  ;; Then paste
  (call-interactively 'ecc-vterm-smart-yank))

;; Key bindings
(define-key vterm-mode-map (kbd "C-c y") 'my-smart-claude-paste)
(define-key vterm-mode-map (kbd "C-c Y") 'my-paste-code-as-file)
(define-key vterm-mode-map (kbd "C-c p") 'my-send-prompt-file-to-claude)
(define-key vterm-mode-map (kbd "C-c C") 'my-paste-with-cleanup)

;; Auto-cleanup timer - clean old files every hour
(run-with-timer 3600 3600 'ecc-vterm-clean-yank-files)

;; Helper function to show yank statistics
(defun my-vterm-yank-stats ()
  "Show statistics about yanked files."
  (interactive)
  (if (file-exists-p ecc-vterm-yank-directory)
      (let* ((files (directory-files ecc-vterm-yank-directory t 
                                     (format "^%s.*\\.txt$" ecc-vterm-yank-file-prefix)))
             (total-size (apply '+ (mapcar (lambda (f) 
                                             (nth 7 (file-attributes f))) 
                                           files))))
        (message "Yank files: %d files, %s total" 
                 (length files)
                 (file-size-human-readable total-size)))
    (message "No yank directory found")))

(provide 'vterm-yank-file-usage)

;;; vterm-yank-file-usage.el ends here