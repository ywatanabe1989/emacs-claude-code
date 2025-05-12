;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 10:24:15>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/examples/buffer/buffer-customization.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:
;; Examples of buffer customization with emacs-claude-code.
;; This file demonstrates how to:
;; 1. Configure buffer creation and naming
;; 2. Customize buffer behavior
;; 3. Set up buffer verification
;; 4. Implement custom buffer handlers

;;; Code:

(require 'emacs-claude-code)
(require 'ecc-buffer)
(require 'ecc-buffer-current)
(require 'ecc-buffer-state)
(require 'ecc-buffer-verification)

;; Example of customizing buffer naming
(defun example-customize-buffer-naming ()
  "Customize how Claude buffers are named."
  (interactive)
  
  ;; Define a custom buffer naming function
  (defun example-custom-buffer-name ()
    "Generate a custom name for a new Claude buffer."
    (let* ((date-str (format-time-string "%Y%m%d"))
           (time-str (format-time-string "%H%M"))
           (count (length (ecc-buffer-get-registered-buffers)))
           (buffer-name (format "*CLAUDE-%s-%s-%02d*" date-str time-str (1+ count))))
      buffer-name))
  
  ;; Set the custom naming function
  (setq ecc-buffer-name-function 'example-custom-buffer-name)
  
  ;; Create a new buffer to demonstrate the naming
  (let ((buffer (ecc-buffer-current-get-or-create)))
    (message "Created buffer with custom name: %s" (buffer-name buffer))
    (switch-to-buffer buffer)))

;; Example of customizing buffer creation
(defun example-customize-buffer-creation ()
  "Customize how Claude buffers are created."
  (interactive)
  
  ;; Define a custom buffer creation function
  (defun example-custom-buffer-create (buffer-name)
    "Create and configure a custom Claude buffer with BUFFER-NAME."
    (let ((buffer (get-buffer-create buffer-name)))
      ;; Set up the buffer with custom configurations
      (with-current-buffer buffer
        ;; Configure buffer-local variables
        (setq-local truncate-lines t)
        (setq-local scroll-conservatively 10000)
        (setq-local scroll-margin 0)
        
        ;; Set up specific modes or hooks
        (visual-line-mode 1)
        (read-only-mode -1)
        
        ;; Add custom header information
        (erase-buffer)
        (insert "# Claude Conversation\n")
        (insert (format "Created: %s\n\n" 
                        (format-time-string "%Y-%m-%d %H:%M:%S")))
        
        ;; Register the buffer
        (ecc-buffer-register-buffer buffer))
      buffer))
  
  ;; Set the custom creation function
  (setq ecc-buffer-create-function 'example-custom-buffer-create)
  
  ;; Create a new buffer to demonstrate the customization
  (let ((buffer (ecc-buffer-current-get-or-create)))
    (message "Created buffer with custom configuration: %s" (buffer-name buffer))
    (switch-to-buffer buffer)))

;; Example of customizing buffer state tracking
(defun example-customize-buffer-state ()
  "Customize how Claude buffer state is tracked."
  (interactive)
  
  ;; Define a function to display buffer state
  (defun example-display-buffer-state (buffer)
    "Display the state of BUFFER."
    (with-current-buffer buffer
      (let ((state (ecc-buffer-get-state buffer))
            (modified (buffer-modified-p))
            (read-only buffer-read-only)
            (size (buffer-size)))
        
        ;; Create a report buffer
        (with-current-buffer (get-buffer-create "*BUFFER-STATE-INFO*")
          (erase-buffer)
          (insert (format "Buffer State Information for %s\n\n" (buffer-name buffer)))
          
          (insert (format "State: %s\n" state))
          (insert (format "Modified: %s\n" (if modified "Yes" "No")))
          (insert (format "Read-only: %s\n" (if read-only "Yes" "No")))
          (insert (format "Size: %d bytes\n" size))
          
          (special-mode)
          (switch-to-buffer (current-buffer))))))
  
  ;; Create a buffer and show its state
  (let ((buffer (ecc-buffer-current-get-or-create)))
    (example-display-buffer-state buffer)))

;; Example of custom buffer verification
(defun example-customize-buffer-verification ()
  "Customize verification of Claude buffers."
  (interactive)
  
  ;; Define a custom buffer verification function
  (defun example-custom-buffer-verify (buffer)
    "Verify if BUFFER is a valid Claude buffer with custom criteria."
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        ;; Check if buffer has the expected local variables or properties
        (and (boundp 'ecc-buffer-type)
             (eq ecc-buffer-type 'claude)
             ;; Additionally check buffer size is reasonable
             (< (buffer-size) 1000000)))))
  
  ;; Set the custom verification function
  (setq ecc-buffer-verification-function 'example-custom-buffer-verify)
  
  ;; Test the verification on current buffers
  (let ((all-buffers (buffer-list))
        (valid-buffers '()))
    
    ;; Check each buffer
    (dolist (buf all-buffers)
      (when (funcall ecc-buffer-verification-function buf)
        (push buf valid-buffers)))
    
    ;; Report results
    (message "Found %d valid Claude buffers out of %d total buffers."
             (length valid-buffers) (length all-buffers))))

;; Example of creating a buffer status dashboard
(defun example-create-buffer-dashboard ()
  "Create a dashboard showing all Claude buffers and their status."
  (interactive)
  
  ;; Get all Claude buffers
  (let ((buffers (ecc-buffer-get-registered-buffers)))
    
    ;; Create the dashboard buffer
    (with-current-buffer (get-buffer-create "*CLAUDE-DASHBOARD*")
      (erase-buffer)
      (insert "Claude Buffer Dashboard\n")
      (insert "======================\n\n")
      
      (if (null buffers)
          (insert "No Claude buffers found.\n")
        
        ;; Show buffer information
        (insert (format "%-30s %-15s %-10s %s\n" 
                        "Buffer Name" "State" "Size (KB)" "Last Modified"))
        (insert (make-string 75 ?-) "\n")
        
        (dolist (buf buffers)
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (let ((state (ecc-buffer-get-state buf))
                    (size (/ (buffer-size) 1024.0))
                    (timestamp (ecc-buffer-get-timestamp buf)))
                
                (insert (format "%-30s %-15s %-10.1f %s\n" 
                                (buffer-name buf)
                                (or (symbol-name state) "unknown")
                                size
                                (format-time-string "%Y-%m-%d %H:%M:%S" timestamp))))))))
      
      ;; Add controls
      (insert "\n\nControls:\n")
      (insert "- Press 'r' to refresh the dashboard\n")
      (insert "- Press 'g' to visit the selected buffer\n")
      (insert "- Press 'd' to delete the selected buffer\n")
      
      ;; Set up a simple keymap for the dashboard
      (use-local-map (make-sparse-keymap))
      (local-set-key (kbd "r") #'example-create-buffer-dashboard)
      (local-set-key (kbd "g") 
                     (lambda () 
                       (interactive)
                       (let ((buf-name (thing-at-point 'line t)))
                         (when (string-match "^\\([*][^*]+[*]\\)" buf-name)
                           (let ((buffer (get-buffer (match-string 1 buf-name))))
                             (when buffer
                               (switch-to-buffer buffer)))))))
      (local-set-key (kbd "d")
                     (lambda ()
                       (interactive)
                       (let ((buf-name (thing-at-point 'line t)))
                         (when (string-match "^\\([*][^*]+[*]\\)" buf-name)
                           (let ((buffer (get-buffer (match-string 1 buf-name))))
                             (when (and buffer
                                        (yes-or-no-p (format "Delete buffer %s? " (buffer-name buffer))))
                               (kill-buffer buffer)
                               (example-create-buffer-dashboard)))))))
      
      (special-mode)
      (switch-to-buffer (current-buffer))
      (goto-char (point-min))
      (forward-line 4))))

(provide 'example-buffer-customization)

(when (not load-file-name)
  (message "buffer-customization.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

;;; buffer-customization.el ends here