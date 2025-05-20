;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 10:09:15>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/examples/buffer/buffer-management.el

;;; Commentary:
;; Examples of buffer management with emacs-claude-code.
;; This file demonstrates how to:
;; 1. Create multiple Claude buffers
;; 2. Navigate between buffers
;; 3. Manage buffer state and timestamps
;; 4. Handle cleanup of stale buffers

;;; Code:

(require 'emacs-claude-code)
(require 'ecc-buffer)
(require 'ecc-buffer-registry)
(require 'ecc-buffer-navigation)
(require 'ecc-buffer-timestamp)
(require 'ecc-buffer-stale)

;; Create multiple Claude buffers using modern buffer manager API
(defun example-create-multiple-claude-buffers (count)
  "Create COUNT number of Claude buffers for demonstration."
  (interactive "nNumber of buffers to create: ")
  (let ((buffers '()))
    (dotimes (i count)
      (let* ((buffer-name (format "*CLAUDE-EXAMPLE-%d*" (1+ i)))
             (claude-buffer (ecc-buffer-manager-create buffer-name))
             (buffer (ecc-buffer-buffer claude-buffer)))
        (push buffer buffers)))
    
    ;; Return the created buffers
    (nreverse buffers)))

;; Navigate through Claude buffers using modern buffer manager API
(defun example-navigate-claude-buffers ()
  "Navigate through all available Claude buffers."
  (interactive)
  
  ;; Get all registered Claude buffers
  (let* ((claude-buffers (ecc-buffer-manager-get-all))
         (buffers (mapcar #'ecc-buffer-buffer
                         (seq-filter (lambda (cb)
                                       (buffer-live-p (ecc-buffer-buffer cb)))
                                     claude-buffers))))
    (if (null buffers)
        (message "No Claude buffers found. Create some first.")
      
      ;; Display the first buffer
      (switch-to-buffer (car buffers))
      (message "Navigating through %d Claude buffers. Use ecc-buffer-manager-next/previous to move between them."
               (length buffers)))))

;; Example of using timestamp features with modern buffer manager API
(defun example-show-buffer-timestamps ()
  "Show timestamps of all Claude buffers."
  (interactive)
  
  (let* ((claude-buffers (ecc-buffer-manager-get-all))
         (result '()))
    
    (dolist (cb claude-buffers)
      (let ((buf (ecc-buffer-buffer cb))
            (timestamp (ecc-buffer-last-used-time cb)))
        (when (buffer-live-p buf)
          (push (cons (buffer-name buf) timestamp) result))))
    
    ;; Create a report buffer to show the timestamps
    (with-current-buffer (get-buffer-create "*CLAUDE-TIMESTAMPS*")
      (erase-buffer)
      (insert "Claude Buffer Timestamps:\n\n")
      (insert (format "%-30s %s\n" "Buffer Name" "Last Used Timestamp"))
      (insert (make-string 50 ?-) "\n")
      
      (dolist (entry (nreverse result))
        (insert (format "%-30s %s\n" 
                        (car entry) 
                        (format-time-string "%Y-%m-%d %H:%M:%S" (cdr entry)))))
      
      (special-mode)
      (switch-to-buffer (current-buffer)))))

;; Example of using stale buffer detection with modern buffer manager API
(defun example-detect-stale-buffers (hours)
  "Detect and report Claude buffers that haven't been used for HOURS."
  (interactive "nHours of inactivity to consider stale: ")
  
  ;; Get all registered Claude buffers
  (let* ((claude-buffers (ecc-buffer-manager-get-all))
         (stale-buffers '())
         (now (current-time))
         (threshold (* hours 60 60))) ; Convert hours to seconds
    
    ;; Check each buffer for staleness
    (dolist (cb claude-buffers)
      (let* ((buf (ecc-buffer-buffer cb))
             (timestamp (ecc-buffer-last-used-time cb))
             (time-diff (float-time (time-subtract now timestamp))))
        (when (and (buffer-live-p buf) (> time-diff threshold))
          (push (cons (buffer-name buf) (/ time-diff 3600.0)) stale-buffers))))
    
    ;; Report the stale buffers
    (if (null stale-buffers)
        (message "No stale Claude buffers found.")
      
      ;; Create a report buffer
      (with-current-buffer (get-buffer-create "*CLAUDE-STALE-BUFFERS*")
        (erase-buffer)
        (insert "Stale Claude Buffers:\n\n")
        (insert (format "%-30s %s\n" "Buffer Name" "Hours Inactive"))
        (insert (make-string 50 ?-) "\n")
        
        (dolist (entry (sort stale-buffers (lambda (a b) (> (cdr a) (cdr b)))))
          (insert (format "%-30s %.2f\n" (car entry) (cdr entry))))
        
        (insert "\nUse the modern buffer manager API to clean up stale buffers.\n")
        (special-mode)
        (switch-to-buffer (current-buffer))))))

;; Example of cleaning up stale buffers with modern buffer manager API
(defun example-cleanup-stale-buffers (hours)
  "Clean up Claude buffers that haven't been used for HOURS."
  (interactive "nHours of inactivity to cleanup: ")
  
  (let* ((threshold (* hours 60 60)) ; Convert hours to seconds
         (now (current-time))
         (claude-buffers (ecc-buffer-manager-get-all))
         (cleaned-count 0))
    
    ;; Find and clean up stale buffers
    (dolist (cb claude-buffers)
      (let* ((timestamp (ecc-buffer-last-used-time cb))
             (time-diff (float-time (time-subtract now timestamp))))
        (when (> time-diff threshold)
          (ecc-buffer-manager-kill cb)
          (cl-incf cleaned-count))))
    
    (message "Cleaned up %d stale Claude buffer(s) older than %d hours." 
             cleaned-count hours)))

(provide 'example-buffer-management)
;;; buffer-management.el ends here