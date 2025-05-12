;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 09:25:30>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/examples/term/term-claude-automation.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:
;; Example demonstrating how to activate and use ecc-term-claude-mode
;; for automated Claude interaction in a vterm session.
;; 
;; This example shows:
;; 1. Starting a Claude session in a vterm buffer
;; 2. Automatically configuring buffer with ecc-term-claude-mode
;; 3. Setting up automated responses to Claude prompts
;; 4. Customizing the terminal integration
;; 5. Integrating with your Emacs workflow

;;; Code:

(require 'emacs-claude-code)

;;
;; 1. Starting a Claude session with the new terminal modules
;;

(defun example-start-claude-term-session ()
  "Start a Claude session in a vterm buffer with term-claude-mode."
  (interactive)
  
  ;; Ensure vterm is available
  (unless (featurep 'vterm)
    (error "This example requires vterm to be installed"))
  
  ;; Start Claude using the new terminal module (renamed from ecc-run-vterm)
  (require 'ecc-term-run)
  (require 'ecc-term-claude-mode)
  
  ;; Create a new Claude terminal session with auto-mode enabled
  (let ((buffer (ecc-term-run-claude)))
    (with-current-buffer buffer
      ;; Enable auto-responses to Claude prompts
      (ecc-term-claude-auto-mode-toggle)
      
      ;; Customize terminal appearance
      (setq-local ecc-term-claude-truncate-lines nil) ; disable line truncation
      
      ;; Set up a status message
      (message "Claude terminal session started with auto-mode enabled"))))

;;
;; 2. Setting up prompt detection and automated responses
;;

(defun example-setup-automated-claude-responses ()
  "Configure the current buffer for automated Claude interaction."
  (interactive)
  
  ;; Ensure we're in a Claude terminal buffer
  (unless (eq major-mode 'ecc-term-claude-mode)
    (error "This function must be run in an ecc-term-claude-mode buffer"))
  
  ;; Customize prompt detection patterns (these are buffer-local)
  (setq-local ecc-term-claude-prompt-waiting "Continue generati")
  (setq-local ecc-term-claude-prompt-y/n "y/n")
  (setq-local ecc-term-claude-prompt-y/y/n "Y/y/n")
  (setq-local ecc-term-claude-prompt-initial-waiting "Would you like Claude to continue?")
  
  ;; Update prompt patterns with our custom settings
  (ecc-term-claude-update-prompt-patterns)
  
  ;; Define custom auto-response function
  (defun my-custom-claude-auto-responder ()
    "Custom function to automatically respond to Claude prompts."
    (let ((state (ecc-state-get)))
      (cond
       ;; Custom handling for Y/N prompts - always say yes
       ((eq state :y/n)
        (ecc-term-claude-yes)
        (message "Auto-responded YES to y/n prompt"))
       
       ;; Custom handling for Y/Y/N prompts - special case
       ((eq state :y/y/n)
        (ecc-term-claude-yes)
        (message "Auto-responded YES to Y/y/n prompt"))
       
       ;; Custom handling for continue prompts - always continue
       ((or (eq state :waiting) (eq state :initial-waiting))
        (vterm-send-string "continue")
        (vterm-send-return)
        (message "Auto-responded CONTINUE to continuation prompt")))))
  
  ;; Add our custom responder to the update functions
  (add-to-list 'ecc-term-claude-update-functions 'my-custom-claude-auto-responder)
  
  ;; Enable auto-mode if not already enabled
  (unless ecc-term-claude-auto-mode
    (ecc-term-claude-auto-mode-toggle))
  
  (message "Automated Claude responses configured for this buffer"))

;;
;; 3. Sending content to Claude terminal with progress tracking
;;

(defun example-send-file-to-claude-term (file-path)
  "Send a file to Claude terminal with progress tracking."
  (interactive "fSelect file to send to Claude: ")
  
  ;; Ensure the file exists
  (unless (file-exists-p file-path)
    (error "File not found: %s" file-path))
  
  ;; Create a progress buffer
  (let ((progress-buffer (get-buffer-create "*Claude Send Progress*")))
    (with-current-buffer progress-buffer
      (erase-buffer)
      (insert "# Sending File to Claude Terminal\n\n")
      (insert (format "File: %s\n\n" file-path))
      (special-mode))
    
    (display-buffer progress-buffer)
    
    ;; Read the file content
    (let* ((file-content (with-temp-buffer
                           (insert-file-contents file-path)
                           (buffer-string)))
           (file-size (length file-content)))
      
      ;; Update progress buffer
      (with-current-buffer progress-buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (format "File size: %d characters\n" file-size))))
      
      ;; Get the current Claude terminal buffer
      (let ((claude-buffer (car (seq-filter 
                                 (lambda (buf)
                                   (with-current-buffer buf
                                     (eq major-mode 'ecc-term-claude-mode)))
                                 (buffer-list)))))
        
        (unless claude-buffer
          (error "No active Claude terminal buffer found"))
        
        ;; Use the large buffer module to split content if needed
        (require 'ecc-buffer-large) ;; New module name (previously ecc-large-buffer)
        
        (let* ((chunk-size (ecc-buffer-large-get-optimal-chunk-size))
               (chunks (ecc-buffer-large-chunk-string file-content chunk-size)))
          
          ;; Update progress buffer
          (with-current-buffer progress-buffer
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert (format "Split into %d chunks\n\n" (length chunks)))))
          
          ;; Send chunks with the new term-send module (previously ecc-send)
          (require 'ecc-term-send)
          
          (let ((i 0))
            (dolist (chunk chunks)
              (setq i (1+ i))
              
              ;; Update progress
              (with-current-buffer progress-buffer
                (let ((inhibit-read-only t))
                  (goto-char (point-max))
                  (insert (format "Sending chunk %d/%d... " i (length chunks)))))
              
              ;; Send the chunk
              (with-current-buffer claude-buffer
                (if (= i 1)
                    ;; First chunk - include optional prompt
                    (let ((prompt (read-string "Enter prompt for Claude (optional): ")))
                      (ecc-term-send-string (if (string-empty-p prompt)
                                               chunk
                                             (concat prompt "\n\n" chunk))
                                           t 0.5))
                  ;; Subsequent chunks - add continuation note
                  (ecc-term-send-string
                   (concat "Continued from previous chunk...\n\n" chunk)
                   t 0.5)))
              
              ;; Mark as completed in progress buffer
              (with-current-buffer progress-buffer
                (let ((inhibit-read-only t))
                  (goto-char (point-max))
                  (insert "done\n")))))
          
          ;; Final status
          (with-current-buffer progress-buffer
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert "\nFile sent successfully to Claude terminal buffer.\n"))))))))

;;
;; 4. Creating a Claude terminal dashboard with automatic setup
;;

(defun example-claude-term-dashboard ()
  "Create a dashboard for Claude terminal interaction."
  (interactive)
  
  (let ((buffer (get-buffer-create "*Claude Terminal Dashboard*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "# Claude Terminal Dashboard\n\n")
      
      ;; Session management section
      (insert "## Session Management\n\n")
      
      ;; New session button
      (insert-button "[Start New Session]"
                    'action (lambda (_) 
                             (example-start-claude-term-session))
                    'help-echo "Start a new Claude terminal session with auto-mode")
      (insert "  ")
      
      ;; Auto-setup button
      (insert-button "[Configure Auto-Responses]"
                    'action (lambda (_)
                             (let ((buf (get-buffer "*CLAUDE-VTERM*")))
                               (if buf
                                   (progn
                                     (switch-to-buffer buf)
                                     (example-setup-automated-claude-responses))
                                 (message "No Claude terminal buffer found"))))
                    'help-echo "Configure automated responses in current Claude terminal")
      (insert "\n\n")
      
      ;; File sending section
      (insert "## Send Content to Claude\n\n")
      
      (insert-button "[Send File to Claude]"
                    'action (lambda (_)
                             (call-interactively #'example-send-file-to-claude-term))
                    'help-echo "Send a file to Claude with progress tracking")
      (insert "  ")
      
      (insert-button "[Send Buffer Content]"
                    'action (lambda (_)
                             (let ((content (save-excursion
                                             (set-buffer (other-buffer (current-buffer) t))
                                             (buffer-string))))
                               (if (fboundp 'ecc-term-send-string)
                                   (ecc-term-send-string content)
                                 (message "ecc-term-send-string not available"))))
                    'help-echo "Send content from other buffer to Claude")
      (insert "\n\n")
      
      ;; Quick responses section
      (insert "## Quick Responses\n\n")
      
      (insert-button "[Yes (y)]"
                    'action (lambda (_)
                             (when (fboundp 'ecc-term-claude-yes)
                               (ecc-term-claude-yes)))
                    'help-echo "Send 'y' response to Claude")
      (insert "  ")
      
      (insert-button "[No (n)]"
                    'action (lambda (_)
                             (when (fboundp 'ecc-term-claude-no)
                               (ecc-term-claude-no)))
                    'help-echo "Send 'n' response to Claude")
      (insert "  ")
      
      (insert-button "[Continue]"
                    'action (lambda (_)
                             (when (fboundp 'ecc-term-send-string)
                               (ecc-term-send-string "continue")))
                    'help-echo "Send 'continue' response to Claude")
      (insert "  ")
      
      (insert-button "[Retry (r)]"
                    'action (lambda (_)
                             (when (fboundp 'ecc-term-claude-retry)
                               (ecc-term-claude-retry)))
                    'help-echo "Send 'r' response to Claude")
      (insert "\n\n")
      
      ;; Buffer management section
      (insert "## Buffer Management\n\n")
      
      (insert-button "[Next Claude Buffer]"
                    'action (lambda (_)
                             (when (fboundp 'ecc-term-claude-next-buffer)
                               (ecc-term-claude-next-buffer)))
                    'help-echo "Switch to the next Claude buffer")
      (insert "  ")
      
      (insert-button "[Previous Claude Buffer]"
                    'action (lambda (_)
                             (when (fboundp 'ecc-term-claude-prev-buffer)
                               (ecc-term-claude-prev-buffer)))
                    'help-echo "Switch to the previous Claude buffer")
      (insert "  ")
      
      (insert-button "[Clear Buffer]"
                    'action (lambda (_)
                             (when (fboundp 'ecc-term-claude-clear)
                               (ecc-term-claude-clear)))
                    'help-echo "Clear the current Claude buffer")
      (insert "\n\n")
      
      ;; Mode info section
      (insert "## Mode Information\n\n")
      (insert "The `ecc-term-claude-mode` (previously `ecc-claude-vterm-mode`) provides:\n\n")
      (insert "- Optimized settings for Claude terminal interaction\n")
      (insert "- Automatic response to prompts with auto-mode\n")
      (insert "- State detection to identify various Claude prompts\n")
      (insert "- Integration with buffer management system\n")
      (insert "- Keyboard shortcuts for common Claude operations\n\n")
      
      (insert "### Key Bindings\n\n")
      (insert "| Shortcut | Action |\n")
      (insert "|----------|--------|\n")
      (insert "| C-c C-k  | Interrupt Claude |\n")
      (insert "| C-c C-y  | Send 'y' response |\n")
      (insert "| C-c C-n  | Send 'n' response |\n") 
      (insert "| C-c C-r  | Send 'r' (retry) response |\n")
      (insert "| C-c C-l  | Clear buffer |\n")
      (insert "| C-c C-a  | Toggle auto-mode |\n")
      (insert "| C-c C-p  | Previous Claude buffer |\n")
      (insert "| C-c C-f  | Next Claude buffer |\n")
      (insert "| C-c C-t  | New Claude buffer |\n\n")
      
      ;; Exit button
      (insert-button "[Close Dashboard]"
                    'action (lambda (_)
                             (kill-buffer buffer))
                    'help-echo "Close this dashboard")
      
      ;; Set up the mode
      (special-mode)
      (switch-to-buffer buffer))))

;;
;; 5. Global mode for automatic setup of Claude terminal buffers
;;

(define-minor-mode example-global-claude-term-mode
  "Global minor mode to automatically set up Claude terminal buffers."
  :global t
  :init-value nil
  :lighter " Claude-Term"
  :group 'ecc-term-claude
  
  (if example-global-claude-term-mode
      (progn
        ;; Set up hook to detect and configure Claude terminal buffers
        (add-hook 'vterm-mode-hook #'example-detect-and-setup-claude-term)
        (message "Global Claude terminal mode enabled"))
    (remove-hook 'vterm-mode-hook #'example-detect-and-setup-claude-term)
    (message "Global Claude terminal mode disabled")))

(defun example-detect-and-setup-claude-term ()
  "Detect if this is a Claude terminal and set up ecc-term-claude-mode."
  (when (and (eq major-mode 'vterm-mode)
             (string-match-p "CLAUDE-\\|claude" (buffer-name)))
    ;; This appears to be a Claude terminal - set up our mode
    (ecc-term-claude-mode)
    (ecc-term-claude-auto-mode-toggle)
    (message "Claude terminal detected and configured automatically")))

(provide 'term-claude-automation)

;;; term-claude-automation.el ends here