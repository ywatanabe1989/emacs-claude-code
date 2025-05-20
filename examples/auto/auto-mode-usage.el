;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-12 07:42:30>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/examples/auto/auto-mode-usage.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; Commentary:
;; Auto-mode usage examples for emacs-claude-code.
;; This example demonstrates how to:
;; 1. Enable and configure auto-accept mode for Claude
;; 2. Customize auto-accept behavior
;; 3. Create automatic workflows with Claude
;; 4. Handle auto-response for different Claude prompts

;;; Code:

(require 'emacs-claude-code)
(require 'ecc-auto)

;; Basic auto-mode setup and toggling
(defun example-auto-mode-toggle ()
  "Toggle auto-accept mode for Claude.
This demonstrates the basic auto-mode toggle functionality."
  (interactive)
  ;; Toggle auto-mode
  (let ((new-state (ecc-auto-toggle)))
    (message "Claude auto-accept mode is now %s"
             (if new-state "enabled" "disabled"))
    ;; Display help on using auto-mode
    (when new-state
      (with-help-window "*Auto-Mode Help*"
        (princ "Claude Auto-Accept Mode\n")
        (princ "======================\n\n")
        (princ "Auto-accept mode enables Claude to automatically respond to prompts:\n\n")
        (princ "- Automatically continues when Claude pauses\n")
        (princ "- Automatically answers 'y' to yes/no questions\n")
        (princ "- Automatically selects default options\n\n")
        (princ "This allows for uninterrupted interaction with Claude.\n\n")
        (princ "To disable auto-accept mode, run M-x ecc-auto-toggle again.")))))

;; Setup auto-mode with custom configuration
(defun example-auto-mode-configure ()
  "Configure and enable auto-accept mode with custom settings."
  (interactive)
  ;; Configure auto-mode settings
  (let ((orig-interval ecc-auto-interval-sec))
    ;; Set a custom interval (e.g., check every 2 seconds)
    (setq ecc-auto-interval-sec 2.0)
    
    ;; Enable auto-mode
    (ecc-auto-enable)
    
    ;; Show confirmation of settings
    (message "Auto-accept enabled with interval: %.1f seconds (was %.1f)"
             ecc-auto-interval-sec orig-interval)
    
    ;; Create a temporary buffer with instructions
    (with-current-buffer (get-buffer-create "*Auto-Mode Configuration*")
      (erase-buffer)
      (insert "# Auto-Accept Mode Configuration\n\n")
      (insert "Auto-accept mode has been enabled with the following settings:\n\n")
      (insert (format "- Check interval: %.1f seconds\n" ecc-auto-interval-sec))
      (insert "- Behavior: Automatically respond to Claude prompts\n")
      (insert "- Status: Enabled\n\n")
      
      (insert "## Customization Options\n\n")
      (insert "```elisp\n")
      (insert ";; Change the auto-mode check interval\n")
      (insert "(setq ecc-auto-interval-sec 1.0)  ; Check every 1 second\n\n")
      (insert ";; Force auto-mode to enable on startup\n")
      (insert "(setq ecc-auto-enable t)\n")
      (insert "```\n\n")
      
      (insert "## Using Auto-Mode\n\n")
      (insert "Auto-mode will remain active until you disable it with:\n")
      (insert "- M-x ecc-auto-toggle\n")
      (insert "- M-x ecc-auto-disable\n\n")
      
      (insert "While auto-mode is active, the mode line indicates [Auto] status.\n")
      
      ;; Set up buffer for display
      (special-mode)
      (switch-to-buffer (current-buffer)))))

;; Example of specialized automated workflow
(defun example-auto-mode-multi-query-workflow ()
  "Set up an automated multi-query workflow with Claude.
This demonstrates using auto-mode for complex automated interactions."
  (interactive)
  ;; Define the sequence of queries
  (let ((queries '("What are the main design patterns used in software development?"
                   "Explain the Model-View-Controller pattern in detail."
                   "How is the Observer pattern different from MVC?"
                   "Show an example of the Factory pattern in Python."
                   "What are best practices for implementing design patterns?")))
    
    ;; Create or get a Claude buffer
    (unless (fboundp 'ecc-claude-vterm)
      (error "Claude vterm mode is not available"))
    
    ;; Create a new Claude buffer
    (let ((claude-buffer (ecc-claude-vterm)))
      (with-current-buffer claude-buffer
        ;; Enable auto-mode
        (ecc-auto-enable)
        
        ;; Setup instructions and begin workflow
        (insert "\n\n# Automated Design Patterns Tutorial\n\n")
        (insert "I'll be sending a series of queries about design patterns.\n")
        (insert "Auto-mode is enabled, so Claude will handle continuation prompts automatically.\n\n")
        (insert "Press C-c n to move to the next query at any time.\n\n")
        
        ;; Set up a counter for the current query
        (set (make-local-variable 'example-query-index) 0)
        
        ;; Send the first query
        (vterm-send-string (nth 0 queries))
        (vterm-send-return)
        
        ;; Set up a custom keymap for advancing queries
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "C-c n")
                     (lambda ()
                       (interactive)
                       (let ((next-index (1+ example-query-index)))
                         (if (< next-index (length queries))
                             (progn
                               ;; Send the next query
                               (vterm-send-string (nth next-index queries))
                               (vterm-send-return)
                               ;; Update the counter
                               (setq example-query-index next-index)
                               (message "Sending query %d/%d: %s"
                                       (1+ next-index) (length queries)
                                       (nth next-index queries)))
                           (message "All queries have been sent.")))))
          
          ;; Install the temporary keymap
          (use-local-map (make-composed-keymap map (current-local-map))))
        
        ;; Display help message
        (message "Automated workflow started. Press C-c n for next query.")))))

;; Example of customized auto-response behavior
(defun example-auto-custom-responses ()
  "Configure Claude with custom auto-responses for different prompts.
This demonstrates how to customize auto-response behavior."
  (interactive)
  ;; Create a settings buffer to customize responses
  (let ((settings-buffer (get-buffer-create "*Custom Auto-Responses*")))
    (with-current-buffer settings-buffer
      (erase-buffer)
      (insert "# Custom Auto-Response Configuration\n\n")
      
      ;; Show current settings
      (insert "## Current Settings\n\n")
      (insert (format "- Yes/No response: %s\n" 
                     (if (boundp 'ecc-auto-yn-response) 
                         ecc-auto-yn-response "y")))
      (insert (format "- Continuation response: %s\n" 
                     ecc-prompt-to-send-on-waiting))
      (insert (format "- Check interval: %.1f seconds\n\n" 
                     ecc-auto-interval-sec))
      
      ;; Add customization options
      (insert "## Customize Responses\n\n")
      
      ;; Yes/No response options
      (insert "### Yes/No Response\n\n")
      (let ((options '("y" "n")))
        (dolist (option options)
          (insert-button (format "[%s] " option)
                        'action (lambda (_)
                                 (defvar ecc-auto-yn-response option)
                                 (example-auto-custom-responses))
                        'help-echo (format "Always respond '%s' to Yes/No prompts" option))))
      (insert "\n\n")
      
      ;; Continuation response options
      (insert "### Continuation Response\n\n")
      (let ((options '("continue" "go on" "proceed" "next")))
        (dolist (option options)
          (insert-button (format "[%s] " option)
                        'action (lambda (_)
                                 (setq ecc-prompt-to-send-on-waiting option)
                                 (example-auto-custom-responses))
                        'help-echo (format "Always respond '%s' to continuation prompts" option))))
      (insert "\n\n")
      
      ;; Custom response option
      (insert-button "[Set Custom Response]"
                    'action (lambda (_)
                             (let ((custom-response (read-string 
                                                   "Enter custom continuation response: " 
                                                   ecc-prompt-to-send-on-waiting)))
                               (setq ecc-prompt-to-send-on-waiting custom-response)
                               (example-auto-custom-responses)))
                    'help-echo "Set a custom response for continuation prompts")
      (insert "\n\n")
      
      ;; Check interval options
      (insert "### Check Interval\n\n")
      (let ((intervals '(0.5 1.0 1.5 2.0 3.0)))
        (dolist (interval intervals)
          (insert-button (format "[%.1fs] " interval)
                        'action (lambda (_)
                                 (setq ecc-auto-interval-sec interval)
                                 (example-auto-custom-responses))
                        'help-echo (format "Check every %.1f seconds" interval))))
      (insert "\n\n")
      
      ;; Apply settings section
      (insert "## Apply Settings\n\n")
      (insert-button "[Enable Auto-Mode with These Settings]"
                    'action (lambda (_)
                             (ecc-auto-enable)
                             (message "Auto-mode enabled with custom settings"))
                    'help-echo "Enable auto-mode with the current settings")
      (insert "  ")
      (insert-button "[Disable Auto-Mode]"
                    'action (lambda (_)
                             (ecc-auto-disable)
                             (message "Auto-mode disabled"))
                    'help-echo "Disable auto-mode")
      (insert "\n\n")
      
      ;; Implementation notes
      (insert "## Implementation Notes\n\n")
      (insert "These settings configure how auto-mode responds to Claude prompts:\n\n")
      (insert "- Yes/No: Used when Claude asks a yes/no question\n")
      (insert "- Continuation: Used when Claude is waiting for you to continue\n")
      (insert "- Check interval: How often to check Claude's state\n\n")
      (insert "These settings take effect immediately when auto-mode is enabled.\n")
      
      ;; Show the buffer
      (special-mode)
      (switch-to-buffer settings-buffer))))

;; Example of notification integration with auto-mode
(defun example-auto-mode-with-notifications ()
  "Enable auto-mode with desktop notifications for Claude interactions.
This demonstrates how to integrate notifications with auto-mode."
  (interactive)
  ;; Define custom notification function
  (defun example-auto-notify (title message)
    "Send a desktop notification with TITLE and MESSAGE."
    (when (fboundp 'notifications-notify)
      (notifications-notify
       :title title
       :body message
       :timeout 5000)))
  
  ;; Enable auto-mode
  (ecc-auto-enable)
  
  ;; Notify user that auto-mode is enabled
  (example-auto-notify "Claude Auto-Mode" "Auto-mode is now enabled")
  
  ;; Add advice to send-accept functions to show notifications
  (advice-add 'ecc-auto-send-1 :after
             (lambda (&rest _)
               (example-auto-notify 
                "Claude Auto-Response" 
                "Automatically responded 'Y' to yes/no prompt")))
  
  (advice-add 'ecc-auto-send-2 :after
             (lambda (&rest _)
               (example-auto-notify 
                "Claude Auto-Response" 
                "Automatically responded to multi-option prompt")))
  
  (advice-add 'ecc-auto-send-continue :after
             (lambda (&rest _)
               (example-auto-notify 
                "Claude Auto-Response" 
                "Automatically continued generation")))
  
  (message "Auto-mode enabled with desktop notifications"))

;; Example of automated repeated prompting
(defun example-auto-mode-repeated-queries (topic count)
  "Set up an automated workflow to ask COUNT related questions about TOPIC.
Demonstrates using auto-mode for continuous interactions."
  (interactive "sTopic to explore: \nnNumber of questions to generate (3-10): ")
  ;; Validate count
  (setq count (max 3 (min 10 count)))
  
  ;; Create or get a Claude buffer
  (unless (fboundp 'ecc-claude-vterm)
    (error "Claude vterm mode is not available"))
  
  ;; Create a new Claude buffer
  (let ((claude-buffer (ecc-claude-vterm)))
    (with-current-buffer claude-buffer
      ;; Enable auto-mode
      (ecc-auto-enable)
      
      ;; Setup prompt for generating questions
      (let ((prompt (format "I want to explore the topic of %s thoroughly. 
Please generate %d interesting questions about this topic. 
After generating the questions, I would like you to answer each question in depth, one by one.
For each question and answer, use clear formatting with headers to make it easy to read.

Generate the questions first, then proceed with answering them in sequence."
                           topic count)))
        
        ;; Send the prompt
        (vterm-send-string prompt)
        (vterm-send-return)
        
        ;; Display help message
        (message "Auto-mode will handle Claude's prompts automatically.")))))

(provide 'example-auto-mode-usage)

(when (not load-file-name)
  (message "auto-mode-usage.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

;;; auto-mode-usage.el ends here