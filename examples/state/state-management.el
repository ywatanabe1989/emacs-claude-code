;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 10:11:45>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/examples/state/state-management.el

;;; Commentary:
;; Examples of state management with emacs-claude-code.
;; This file demonstrates how to:
;; 1. Detect and handle different Claude states
;; 2. Use the state engine for automated interactions
;; 3. Create custom state detection and handling

;;; Code:

(require 'emacs-claude-code)
(require 'ecc-state)
(require 'ecc-state-detect)
(require 'ecc-state-engine)

;; Example function to demonstrate state detection
(defun example-detect-claude-state ()
  "Detect and report the current Claude state."
  (interactive)
  
  ;; Get the current Claude buffer
  (let ((buffer (ecc-buffer-get-or-create-active-buffer)))
    
    ;; Switch to the buffer
    (switch-to-buffer buffer)
    
    ;; Detect the state
    (let ((state (ecc-state-get)))
      (message "Current Claude state: %s" state)
      
      ;; Show what to do based on the state
      (pcase state
        (:waiting
         (message "Claude is waiting for continued generation. Press 'y' to continue."))
        
        (:y/n
         (message "Claude is asking for a yes/no response. Press 'y' or 'n'."))
        
        (:y/y/n
         (message "Claude is asking for a Y/y/n response. Press 'Y', 'y', or 'n'."))
        
        (:initial-waiting
         (message "Claude is waiting for initial confirmation. Type 'continue' to proceed."))
        
        (:thinking
         (message "Claude is thinking. Please wait..."))
        
        (:loading
         (message "Claude is loading. Please wait..."))
        
        (:running
         (message "Claude is generating a response. You can interrupt with C-c C-c."))
        
        (_
         (message "No special state detected. Claude is ready for input."))))))

;; Example automatic state handler
(defun example-auto-handle-claude-state ()
  "Automatically handle Claude state changes."
  (interactive)
  
  ;; Set up state handlers
  (let ((handlers
         `((:waiting . ,(lambda () 
                          (message "Auto-continuing generation...")
                          (ecc-send-key "y")))
           
           (:y/n . ,(lambda () 
                      (message "Auto-responding 'y' to yes/no prompt...")
                      (ecc-send-key "y")))
           
           (:y/y/n . ,(lambda () 
                        (message "Auto-responding 'y' to Y/y/n prompt...")
                        (ecc-send-key "y")))
           
           (:initial-waiting . ,(lambda () 
                                  (message "Auto-continuing initial prompt...")
                                  (ecc-send-string "continue"))))))
    
    ;; Get the current state and handle it
    (let* ((state (ecc-state-get))
           (handler (alist-get state handlers)))
      
      (if handler
          (progn
            (funcall handler)
            (message "Automatically handled Claude state: %s" state))
        
        (message "No handler for current state: %s" state)))))

;; Example of custom state detection pattern
(defun example-add-custom-state-pattern ()
  "Add a custom state detection pattern for Claude."
  (interactive)
  
  ;; Define a custom pattern for detecting when Claude needs code
  (defvar example-custom-code-request-pattern
    "Please provide the code for"
    "Pattern to detect when Claude is requesting code input.")
  
  ;; Add a custom state detector function
  (defun example-detect-code-request-p ()
    "Detect if Claude is requesting code input."
    (save-excursion
      (goto-char (point-max))
      (re-search-backward example-custom-code-request-pattern nil t)))
  
  ;; Update the state engine with the custom pattern
  (setq ecc-state-detection-functions
        (cons 'example-detect-code-request-p ecc-state-detection-functions))
  
  (message "Added custom code request state detection pattern."))

;; Example function to display state change indicators in mode line
(defun example-setup-mode-line-state-indicator ()
  "Set up a mode line indicator for Claude state."
  (interactive)
  
  (defvar example-claude-state-mode-line-string ""
    "String displayed in the mode line indicating Claude state.")
  
  (defun example-update-claude-state-mode-line ()
    "Update the mode line state indicator based on Claude's state."
    (let ((state (ecc-state-get)))
      (setq example-claude-state-mode-line-string
            (pcase state
              (:waiting " [Claude: Waiting]")
              (:y/n " [Claude: Y/N]")
              (:y/y/n " [Claude: Y/Y/N]")
              (:initial-waiting " [Claude: Continue?]")
              (:thinking " [Claude: Thinking...]")
              (:loading " [Claude: Loading...]")
              (:running " [Claude: Running]")
              (_ " [Claude: Ready]")))
      
      ;; Force the mode line to update
      (force-mode-line-update)))
  
  ;; Set up a timer to periodically update the mode line
  (run-with-timer 0 1 'example-update-claude-state-mode-line)
  
  ;; Add the indicator to the mode line
  (unless (member '(:eval example-claude-state-mode-line-string) mode-line-format)
    (setq-default mode-line-format
                  (append mode-line-format
                          '((:eval example-claude-state-mode-line-string)))))
  
  (message "Claude state indicator added to mode line."))

(provide 'example-state-management)
;;; state-management.el ends here