;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 10:35:25>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/examples/command-integration.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:
;; Examples of command system and integration features in emacs-claude-code.
;; This file demonstrates how to:
;; 1. Create custom commands for Claude
;; 2. Integrate Claude with your workflow
;; 3. Create UI components for Claude
;; 4. Connect Claude to external tools

;;; Code:

(require 'emacs-claude-code)
(require 'ecc-command)
(require 'ecc-integration)

;; ---- Custom Command Examples ----

;; Define a simple command that explains selected code
(defun example-command-explain-region ()
  "Send the selected region to Claude for explanation."
  (interactive)
  (if (use-region-p)
      (let* ((code (buffer-substring-no-properties (region-beginning) (region-end)))
             (lang (or (ecc-command--detect-language) "unknown"))
             (prompt (format "Explain the following %s code in detail:\n\n```%s\n%s\n```\n\nProvide a clear explanation of what this code does, step by step."
                            lang lang code))
             (buffer (ecc-buffer-buffer 
                     (ecc-buffer-manager-create "*CLAUDE-CODE-EXPLAIN*"))))
        
        ;; Send the prompt to Claude
        (with-current-buffer buffer
          (erase-buffer)
          (insert prompt)
          (ecc-send-buffer-content))
        
        ;; Display the buffer
        (switch-to-buffer buffer))
    
    (message "No region selected. Please select some code first.")))

;; Command that summarizes a file
(defun example-command-summarize-file (file-path)
  "Send a file to Claude for summarization."
  (interactive "fFile to summarize: ")
  
  (if (not (file-exists-p file-path))
      (message "File not found: %s" file-path)
    
    (let ((file-content (with-temp-buffer
                          (insert-file-contents file-path)
                          (buffer-string)))
          (file-name (file-name-nondirectory file-path))
          (prompt-template "Summarize the following file in detail:\n\n```\n%s\n```\n\nProvide a comprehensive summary that covers:\n\n1. Overall purpose of the file\n2. Main functions/components and what they do\n3. Key algorithms or data structures\n4. Important dependencies or connections to other components\n5. Any potential issues or complexities"))
      
      ;; Create a new Claude buffer
      (let ((buffer (ecc-buffer-buffer 
                    (ecc-buffer-manager-create 
                     (format "*CLAUDE-SUMMARY-%s*" file-name)))))
        
        ;; Send the prompt to Claude
        (with-current-buffer buffer
          (erase-buffer)
          (insert (format prompt-template file-content))
          (ecc-send-buffer-content))
        
        ;; Display the buffer
        (switch-to-buffer buffer)))))

;; ---- Integration Examples ----

;; Enhance a function with Claude integration
(defun example-integration-git-commit-message (file-path)
  "Generate a descriptive git commit message using Claude."
  (interactive "fFile with changes: ")
  
  ;; Get the git diff for the file
  (let ((diff-output (shell-command-to-string 
                      (format "git diff %s" (shell-quote-argument file-path)))))
    
    (if (string-empty-p diff-output)
        (message "No changes detected in %s" file-path)
      
      ;; Create a prompt for Claude
      (let ((prompt (format "Based on the following git diff, generate a concise, descriptive commit message. The message should be in the imperative mood (e.g., 'Add feature' not 'Added feature') and not exceed 50 characters for the first line. After the first line, add a blank line and then a more detailed explanation if necessary.\n\nDiff:\n```\n%s\n```\n\nCommit message:"
                           diff-output))
            (buffer (ecc-buffer-buffer 
                    (ecc-buffer-manager-create "*CLAUDE-COMMIT-MESSAGE*"))))
        
        ;; Send the prompt to Claude
        (with-current-buffer buffer
          (erase-buffer)
          (insert prompt)
          (ecc-send-buffer-content))
        
        ;; Display the buffer
        (switch-to-buffer buffer)))))

;; Integrate Claude into error handling
(defun example-integration-explain-error (error-message)
  "Use Claude to explain an error message."
  (interactive "sError message: ")
  
  (let ((prompt (format "Explain the following error message in detail and suggest possible solutions:\n\n```\n%s\n```\n\nPlease include:\n1. What the error means\n2. Common causes for this error\n3. Step-by-step solutions to fix it\n4. How to prevent it in the future"
                       error-message))
        (buffer (ecc-buffer-buffer 
                (ecc-buffer-manager-create "*CLAUDE-ERROR-EXPLAIN*"))))
    
    ;; Send the prompt to Claude
    (with-current-buffer buffer
      (erase-buffer)
      (insert prompt)
      (ecc-send-buffer-content))
    
    ;; Display the buffer
    (switch-to-buffer buffer)))

;; ---- UI Component Examples ----

;; Create a dashboard for Claude
(defun example-ui-claude-dashboard ()
  "Create a dashboard for Claude interaction."
  (interactive)
  
  ;; Create the dashboard buffer
  (let ((buffer (get-buffer-create "*CLAUDE-DASHBOARD*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Claude AI Dashboard\n")
      (insert "=================\n\n")
      
      ;; Current buffers section
      (insert "Current Claude Buffers:\n")
      (insert "------------------\n")
      (let* ((claude-buffers (ecc-buffer-manager-get-all))
             (live-buffers (seq-filter (lambda (cb)
                                         (buffer-live-p (ecc-buffer-buffer cb)))
                                       claude-buffers)))
        (if (null live-buffers)
            (insert "  No active Claude buffers.\n")
          (dolist (cb live-buffers)
            (let ((buf (ecc-buffer-buffer cb)))
              (insert (format "  • %s (%s)\n"
                              (buffer-name buf)
                              (or (symbol-name (ecc-buffer-state cb))
                                  "unknown")))))))
      
      ;; Quick actions section
      (insert "\nQuick Actions:\n")
      (insert "------------\n")
      (insert "  [n] New Claude buffer\n")
      (insert "  [e] Explain code\n")
      (insert "  [s] Summarize file\n")
      (insert "  [g] Generate git commit message\n")
      (insert "  [h] Help with error\n")
      (insert "  [q] Quit dashboard\n")
      
      ;; Templates section
      (insert "\nAvailable Templates:\n")
      (insert "------------------\n")
      (let ((templates (hash-table-keys (ecc-template-get-cache))))
        (if (null templates)
            (insert "  No templates found.\n")
          (dolist (template (sort templates #'string<))
            (insert (format "  • %s\n" template)))))
      
      ;; Set up a keymap for the dashboard
      (use-local-map (make-sparse-keymap))
      (local-set-key (kbd "n") 
                     (lambda () 
                       (interactive)
                       (let ((cb (ecc-buffer-manager-create "*CLAUDE-NEW*")))
                         (switch-to-buffer (ecc-buffer-buffer cb))
                         (kill-buffer "*CLAUDE-DASHBOARD*"))))
      (local-set-key (kbd "e") 
                     (lambda () 
                       (interactive)
                       (kill-buffer "*CLAUDE-DASHBOARD*")
                       (call-interactively #'example-command-explain-region)))
      (local-set-key (kbd "s") 
                     (lambda () 
                       (interactive)
                       (kill-buffer "*CLAUDE-DASHBOARD*")
                       (call-interactively #'example-command-summarize-file)))
      (local-set-key (kbd "g") 
                     (lambda () 
                       (interactive)
                       (kill-buffer "*CLAUDE-DASHBOARD*")
                       (call-interactively #'example-integration-git-commit-message)))
      (local-set-key (kbd "h") 
                     (lambda () 
                       (interactive)
                       (kill-buffer "*CLAUDE-DASHBOARD*")
                       (call-interactively #'example-integration-explain-error)))
      (local-set-key (kbd "q") 
                     (lambda () 
                       (interactive)
                       (kill-buffer "*CLAUDE-DASHBOARD*")))
      
      ;; Set up the mode
      (special-mode)
      (switch-to-buffer buffer))))

;; ---- Command Registration Examples ----

;; Register custom commands with keybindings
(defun example-register-commands ()
  "Register custom Claude commands with keybindings."
  (interactive)
  
  ;; Define the keymap if it doesn't exist
  (unless (boundp 'example-claude-command-map)
    (defvar example-claude-command-map (make-sparse-keymap)
      "Keymap for custom Claude commands."))
  
  ;; Register commands
  (define-key example-claude-command-map (kbd "e") #'example-command-explain-region)
  (define-key example-claude-command-map (kbd "s") #'example-command-summarize-file)
  (define-key example-claude-command-map (kbd "g") #'example-integration-git-commit-message)
  (define-key example-claude-command-map (kbd "h") #'example-integration-explain-error)
  (define-key example-claude-command-map (kbd "d") #'example-ui-claude-dashboard)
  
  ;; Define the prefix key in global map (C-c C-l)
  (global-set-key (kbd "C-c C-l") example-claude-command-map)
  
  (message "Claude commands registered. Access with C-c C-l prefix."))

;; ---- Helper Functions ----

;; Detect programming language in current buffer
(defun ecc-command--detect-language ()
  "Detect the programming language of the current buffer."
  (let ((mode-name (format-mode-line mode-name nil nil (current-buffer))))
    (cond
     ((string-match-p "Python" mode-name) "python")
     ((string-match-p "Lisp\\|Emacs-Lisp\\|Elisp" mode-name) "elisp")
     ((string-match-p "JavaScript\\|JS" mode-name) "javascript")
     ((string-match-p "TypeScript\\|TS" mode-name) "typescript")
     ((string-match-p "Ruby" mode-name) "ruby")
     ((string-match-p "Java" mode-name) "java")
     ((string-match-p "C\\+\\+" mode-name) "cpp")
     ((string-match-p "C " mode-name) "c")
     ((string-match-p "Go" mode-name) "go")
     ((string-match-p "Rust" mode-name) "rust")
     ((string-match-p "PHP" mode-name) "php")
     ((string-match-p "SQL" mode-name) "sql")
     ((string-match-p "HTML" mode-name) "html")
     ((string-match-p "CSS" mode-name) "css")
     ((string-match-p "Shell\\|Bash" mode-name) "bash")
     (t nil))))

(provide 'example-command-integration)

(when (not load-file-name)
  (message "command-integration.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

;;; command-integration.el ends here