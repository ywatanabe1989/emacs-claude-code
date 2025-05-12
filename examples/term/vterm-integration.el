;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 10:16:25>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/examples/term/vterm-integration.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:
;; Examples of vterm integration with emacs-claude-code.
;; This file demonstrates how to:
;; 1. Use Claude in vterm-mode
;; 2. Set up vterm-specific configurations
;; 3. Create automated integrations with vterm
;; 4. Handle state detection in vterm

;;; Code:

(require 'emacs-claude-code)
(require 'ecc-claude-vterm-mode)

;; Create a new Claude vterm buffer
(defun example-create-claude-vterm ()
  "Create a new Claude vterm buffer."
  (interactive)
  
  ;; Check if vterm is available
  (if (not (featurep 'vterm))
      (message "vterm is not available. Please install it first.")
    
    ;; Create a new Claude vterm buffer
    (let ((buffer (ecc-claude-vterm)))
      (message "Created new Claude vterm buffer: %s" (buffer-name buffer))
      buffer)))

;; Configure vterm settings for Claude
(defun example-configure-claude-vterm ()
  "Configure settings for Claude vterm integration."
  (interactive)
  
  ;; Set custom vterm-related settings
  (setq ecc-claude-vterm-line-numbers nil)        ; Disable line numbers for better performance
  (setq ecc-claude-vterm-scroll-conservatively 10000) ; Prevent recentering during fast output
  (setq ecc-claude-vterm-truncate-lines t)        ; Truncate long lines for better performance
  
  ;; Show current settings
  (with-current-buffer (get-buffer-create "*CLAUDE-VTERM-SETTINGS*")
    (erase-buffer)
    (insert "Claude VTerm Settings\n")
    (insert "====================\n\n")
    
    (insert (format "Line Numbers: %s\n" 
                    (if ecc-claude-vterm-line-numbers "Enabled" "Disabled")))
    (insert (format "Scroll Conservatively: %d\n" ecc-claude-vterm-scroll-conservatively))
    (insert (format "Truncate Lines: %s\n" 
                    (if ecc-claude-vterm-truncate-lines "Enabled" "Disabled")))
    
    (insert "\nTo change these settings, use:\n")
    (insert "(setq ecc-claude-vterm-line-numbers VALUE)\n")
    (insert "(setq ecc-claude-vterm-scroll-conservatively VALUE)\n")
    (insert "(setq ecc-claude-vterm-truncate-lines VALUE)\n")
    
    (special-mode)
    (switch-to-buffer (current-buffer))))

;; Setup auto-response for Claude in vterm
(defun example-setup-vterm-auto-response ()
  "Set up automatic responses to Claude prompts in vterm mode."
  (interactive)
  
  ;; Enable auto-mode for Claude vterm
  (setq ecc-claude-vterm-auto-mode t)
  
  ;; Create a new vterm buffer with auto-mode enabled
  (let ((buffer (example-create-claude-vterm)))
    (with-current-buffer buffer
      ;; Ensure auto-mode hooks are connected
      (add-hook 'ecc-claude-vterm-update-functions
                'ecc-claude-vterm-auto-send-accept nil t)
      
      ;; Show message about auto-response
      (message "Auto-response mode enabled for %s" (buffer-name buffer))
      (insert "\nClaude auto-response mode is enabled. Claude will automatically:")
      (insert "\n - Respond 'y' to continue generation prompts")
      (insert "\n - Respond 'y' to yes/no prompts")
      (insert "\n - Respond 'continue' to initial prompts")
      (insert "\n\nYou can toggle this with C-c C-a or M-x ecc-claude-vterm-auto-mode-toggle\n"))))

;; Example function for state detection in vterm
(defun example-vterm-detect-state ()
  "Demonstrate state detection in a Claude vterm buffer."
  (interactive)
  
  ;; Create or get a Claude vterm buffer
  (let ((buffer (or (and (eq major-mode 'ecc-claude-vterm-mode)
                         (current-buffer))
                    (example-create-claude-vterm))))
    
    (with-current-buffer buffer
      ;; Run state detection
      (let ((state (ecc-claude-vterm-check-state)))
        (message "Current Claude vterm state: %s" state)
        
        ;; Provide examples of what to do based on the state
        (pcase state
          (:waiting
           (insert "\n\n# State detection example")
           (insert "\nDetected: WAITING state")
           (insert "\nYou can respond with 'y' to continue generation"))
          
          (:y/n
           (insert "\n\n# State detection example")
           (insert "\nDetected: Y/N state")
           (insert "\nYou can respond with 'y' or 'n'"))
          
          (:y/y/n
           (insert "\n\n# State detection example")
           (insert "\nDetected: Y/Y/N state")
           (insert "\nYou can respond with 'Y', 'y', or 'n'"))
          
          (:initial-waiting
           (insert "\n\n# State detection example")
           (insert "\nDetected: INITIAL WAITING state")
           (insert "\nYou can respond with 'continue' to begin"))
          
          (_
           (insert "\n\n# State detection example")
           (insert "\nNo special state detected.")))))))

;; Demo script to showcase Claude vterm functionality
(defun example-claude-vterm-demo ()
  "Run a complete demo of Claude vterm integration."
  (interactive)
  
  ;; Create a new Claude vterm buffer
  (let ((buffer (example-create-claude-vterm)))
    (with-current-buffer buffer
      ;; Show introduction message
      (insert "\n\n# Claude VTerm Integration Demo")
      (insert "\n\nThis demo shows how to interact with Claude in vterm mode.")
      (insert "\n\nKey features:")
      (insert "\n - Optimized for high-speed streaming output")
      (insert "\n - State detection for automatic response handling")
      (insert "\n - Buffer navigation through multiple Claude conversations")
      (insert "\n - Customizable settings for performance")
      
      ;; Show available commands
      (insert "\n\nAvailable commands:")
      (insert "\n - C-c C-y: Send 'y' response")
      (insert "\n - C-c C-n: Send 'n' response")
      (insert "\n - C-c C-r: Send 'r' to retry")
      (insert "\n - C-c C-c: Interrupt Claude")
      (insert "\n - C-c C-a: Toggle auto-mode")
      (insert "\n - C-c C-l: Clear buffer")
      (insert "\n - C-c C-p: Previous Claude buffer")
      (insert "\n - C-c C-f: Next Claude buffer")
      (insert "\n - C-c C-t: Create new Claude buffer")
      
      (insert "\n\nPress Enter to send this information to Claude...\n")
      
      ;; Focus on the buffer
      (switch-to-buffer buffer))))

;; Custom command for sending shell commands to Claude for explanation
(defun example-explain-shell-command (command)
  "Send a shell COMMAND to Claude for explanation."
  (interactive "sEnter shell command to explain: ")
  
  ;; Create or get a Claude vterm buffer
  (let ((buffer (example-create-claude-vterm)))
    (with-current-buffer buffer
      ;; Prepare the prompt for Claude
      (insert (format "\nExplain this shell command in detail: `%s`\n" command))
      (insert "\nPlease include:")
      (insert "\n- What each part of the command does")
      (insert "\n- Any flags and their meanings")
      (insert "\n- Potential pitfalls or alternatives")
      (insert "\n- Examples of common use cases\n")
      
      ;; Send to Claude
      (ecc-send-buffer-content)
      
      ;; Switch to the buffer
      (switch-to-buffer buffer))))

(provide 'example-vterm-integration)

(when (not load-file-name)
  (message "vterm-integration.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

;;; vterm-integration.el ends here