;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 10:40:15>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/examples/modern-api-usage.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:
;; Modern API usage examples for emacs-claude-code.
;; This example demonstrates how to use the modern buffer manager API,
;; state engine, and other advanced features of the package.

;;; Code:

(require 'emacs-claude-code)
(require 'ecc-buffer-manager)
(require 'ecc-state-engine)
(require 'ecc-command)

;; ===== Buffer Manager Examples =====

(defun example-modern-create-claude-buffer ()
  "Create a new Claude buffer using the modern buffer manager API."
  (interactive)
  
  ;; Create a Claude buffer with a name
  (let* ((buffer-name (format "*CLAUDE-MODERN-%02d*" 
                             (+ (length (ecc-buffer-manager-get-all)) 1)))
         (claude-buffer (ecc-buffer-manager-create buffer-name))
         (buffer (ecc-buffer-buffer claude-buffer)))
    
    ;; Switch to the buffer
    (switch-to-buffer buffer)
    
    ;; Set metadata
    (ecc-buffer-manager-set-metadata claude-buffer 'created-by "Example function")
    (ecc-buffer-manager-set-metadata claude-buffer 'purpose "Demonstration")
    
    ;; Set state
    (ecc-buffer-manager-set-state claude-buffer 'ready)
    
    ;; Display information
    (message "Created buffer: %s (ID: %s)" 
             buffer-name
             (ecc-buffer-id claude-buffer))
    
    ;; Return the claude-buffer structure
    claude-buffer))

(defun example-modern-buffer-info ()
  "Display info about the current Claude buffer using modern API."
  (interactive)
  
  ;; Check if in a Claude buffer
  (let ((claude-buffer (ecc-buffer-manager-get-by-buffer (current-buffer))))
    (if claude-buffer
        (progn
          ;; Get buffer info
          (let ((id (ecc-buffer-id claude-buffer))
                (name (ecc-buffer-name claude-buffer))
                (state (ecc-buffer-state claude-buffer))
                (created-time (ecc-buffer-created-time claude-buffer))
                (last-used-time (ecc-buffer-last-used-time claude-buffer))
                (created-by (ecc-buffer-manager-get-metadata claude-buffer 'created-by))
                (purpose (ecc-buffer-manager-get-metadata claude-buffer 'purpose)))
            
            ;; Display in a new buffer
            (with-current-buffer (get-buffer-create "*CLAUDE-BUFFER-INFO*")
              (erase-buffer)
              (insert "Claude Buffer Information\n")
              (insert "=======================\n\n")
              
              (insert (format "Buffer ID: %s\n" id))
              (insert (format "Buffer Name: %s\n" name))
              (insert (format "State: %s\n" state))
              (insert (format "Created: %s\n" 
                             (format-time-string "%Y-%m-%d %H:%M:%S" created-time)))
              (insert (format "Last Used: %s\n" 
                             (format-time-string "%Y-%m-%d %H:%M:%S" last-used-time)))
              (insert "\nMetadata:\n")
              (insert (format "  Created By: %s\n" (or created-by "Unknown")))
              (insert (format "  Purpose: %s\n" (or purpose "Unknown")))
              
              (special-mode)
              (switch-to-buffer (current-buffer)))))
      
      (message "Current buffer is not a Claude buffer."))))

(defun example-modern-list-claude-buffers ()
  "List all Claude buffers using the modern buffer manager API."
  (interactive)
  
  (let ((claude-buffers (ecc-buffer-manager-get-all)))
    (if (null claude-buffers)
        (message "No Claude buffers exist.")
      
      ;; Create a report buffer
      (with-current-buffer (get-buffer-create "*CLAUDE-BUFFERS*")
        (erase-buffer)
        (insert "Claude Buffers\n")
        (insert "=============\n\n")
        
        (insert (format "%-20s %-15s %-12s %s\n" 
                        "Buffer Name" "State" "Current" "Created"))
        (insert (make-string 70 ?-) "\n")
        
        ;; Sort buffers by creation time (newest first)
        (let* ((sorted-buffers 
                (seq-sort-by (lambda (cb) 
                               (float-time (ecc-buffer-created-time cb)))
                             #'>
                             claude-buffers))
               (current (ecc-buffer-manager-get-current)))
          
          ;; Display each buffer
          (dolist (cb sorted-buffers)
            (let ((buffer-name (ecc-buffer-name cb))
                  (state (ecc-buffer-state cb))
                  (is-current (eq cb current))
                  (created-time (ecc-buffer-created-time cb)))
              
              (insert (format "%-20s %-15s %-12s %s\n"
                             buffer-name
                             (format "%s" state)
                             (if is-current "Yes" "No")
                             (format-time-string "%Y-%m-%d %H:%M" created-time))))))
        
        (special-mode)
        (switch-to-buffer (current-buffer))))))

;; ===== State Engine Examples =====

(defun example-modern-state-machine ()
  "Demonstrate the use of the state engine with a simple state machine."
  (interactive)
  
  ;; Define a simple state machine
  (let ((states '((:idle . "Ready to process")
                  (:loading . "Loading data")
                  (:processing . "Processing data")
                  (:complete . "Processing complete")
                  (:error . "Error occurred"))))
    
    ;; Create a buffer to display the state machine
    (with-current-buffer (get-buffer-create "*STATE-MACHINE-DEMO*")
      (erase-buffer)
      (insert "State Machine Demonstration\n")
      (insert "=========================\n\n")
      
      ;; Display all possible states
      (insert "Available States:\n")
      (dolist (state states)
        (insert (format "  %s: %s\n" (car state) (cdr state))))
      
      (insert "\nState Transitions:\n")
      (insert "  idle -> loading -> processing -> complete\n")
      (insert "  (Can go to error from any state)\n\n")
      
      ;; Add interaction instructions
      (insert "Commands:\n")
      (insert "  n - Move to next state\n")
      (insert "  p - Move to previous state\n")
      (insert "  e - Trigger error state\n")
      (insert "  r - Reset to idle state\n\n")
      
      ;; Initialize the state
      (setq-local current-state :idle)
      (setq-local state-history '(:idle))
      
      ;; Display current state
      (insert (propertize "Current State: idle\n" 'state-indicator t))
      
      ;; Define key bindings
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "n") 
          (lambda () 
            (interactive)
            (example-modern-advance-state)))
        
        (define-key map (kbd "p") 
          (lambda () 
            (interactive)
            (example-modern-previous-state)))
        
        (define-key map (kbd "e") 
          (lambda () 
            (interactive)
            (example-modern-error-state)))
        
        (define-key map (kbd "r") 
          (lambda () 
            (interactive)
            (example-modern-reset-state)))
        
        ;; Use the keymap
        (use-local-map map))
      
      (special-mode)
      (switch-to-buffer (current-buffer)))))

(defun example-modern-update-state-display ()
  "Update the state display in the demo buffer."
  (with-current-buffer "*STATE-MACHINE-DEMO*"
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "Current State: .*" nil t)
        (replace-match (format "Current State: %s" current-state))))))

(defun example-modern-advance-state ()
  "Advance to the next state in the demo."
  (interactive)
  (with-current-buffer "*STATE-MACHINE-DEMO*"
    (let ((next-state (pcase current-state
                        (:idle :loading)
                        (:loading :processing)
                        (:processing :complete)
                        (_ current-state))))
      (unless (eq next-state current-state)
        (setq current-state next-state)
        (push current-state state-history)
        (example-modern-update-state-display)
        (message "Advanced to state: %s" current-state)))))

(defun example-modern-previous-state ()
  "Go back to the previous state in the demo."
  (interactive)
  (with-current-buffer "*STATE-MACHINE-DEMO*"
    (when (> (length state-history) 1)
      (pop state-history) ; Remove current state
      (setq current-state (car state-history))
      (example-modern-update-state-display)
      (message "Reverted to state: %s" current-state))))

(defun example-modern-error-state ()
  "Trigger the error state in the demo."
  (interactive)
  (with-current-buffer "*STATE-MACHINE-DEMO*"
    (setq current-state :error)
    (push current-state state-history)
    (example-modern-update-state-display)
    (message "Triggered error state")))

(defun example-modern-reset-state ()
  "Reset to the idle state in the demo."
  (interactive)
  (with-current-buffer "*STATE-MACHINE-DEMO*"
    (setq current-state :idle)
    (setq state-history '(:idle))
    (example-modern-update-state-display)
    (message "Reset to idle state")))

;; ===== Command System Examples =====

(defun example-modern-command-system ()
  "Demonstrate the use of the command system."
  (interactive)
  
  ;; Create a buffer to display the command system
  (with-current-buffer (get-buffer-create "*COMMAND-SYSTEM-DEMO*")
    (erase-buffer)
    (insert "Command System Demonstration\n")
    (insert "==========================\n\n")
    
    ;; List available commands
    (let ((commands (ecc-command-list-all)))
      (insert (format "Available Commands (%d total):\n" (length commands)))
      (insert (make-string 30 ?-) "\n")
      
      (dolist (cmd (sort commands 'string-lessp))
        (let ((doc (documentation (intern cmd))))
          (insert (format "%-25s: %s\n" 
                          cmd 
                          (if doc
                              (car (split-string doc "\n"))
                            "No documentation")))))
      
      ;; Add usage information
      (insert "\nUsage:\n")
      (insert "To execute a command, use M-x ecc-command-execute RET command-name\n")
      (insert "or define your own function that calls ecc-command-execute with the command name.\n\n")
      (insert "Example: (ecc-command-execute \"help\")\n"))
    
    (special-mode)
    (switch-to-buffer (current-buffer))))

;; ===== Combined Examples =====

(defun example-modern-create-and-send ()
  "Create a Claude buffer and send a prompt using modern API."
  (interactive)
  
  ;; Create a Claude buffer
  (let* ((claude-buffer (example-modern-create-claude-buffer))
         (buffer (ecc-buffer-buffer claude-buffer))
         (prompt "Using the modern Claude API in Emacs, explain how buffer management works."))
    
    ;; Set as current buffer
    (ecc-buffer-manager-set-current claude-buffer)
    
    ;; Send the prompt
    (with-current-buffer buffer
      (erase-buffer)
      (insert prompt)
      (ecc-send-buffer-content))
    
    ;; Log the action
    (message "Created buffer and sent prompt using modern API.")))

(provide 'example-modern-api-usage)
;;; modern-api-usage.el ends here