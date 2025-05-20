;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-12 07:14:30>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/examples/history/history-usage.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; Commentary:
;; History usage examples for emacs-claude-code.
;; This example demonstrates how to:
;; 1. Browse Claude prompt history
;; 2. Reuse prompts from history
;; 3. Create custom history visualizations
;; 4. Implement advanced history functionality

;;; Code:

(require 'emacs-claude-code)
(require 'ecc-history)

;; Basic browsing of prompt history
(defun example-history-browse ()
  "Open the history browser for the current Claude buffer.
This demonstrates the basic history browsing functionality."
  (interactive)
  ;; Get the current Claude buffer or create one if needed
  (let ((buffer (or (and (eq major-mode 'ecc-claude-vterm-mode)
                        (current-buffer))
                   (and (fboundp 'ecc-buffer-current-get-buffer)
                        (ecc-buffer-current-get-buffer))
                   (and (fboundp 'ecc-claude-vterm)
                        (call-interactively 'ecc-claude-vterm)))))
    
    ;; Make sure we have a buffer
    (if (not buffer)
        (message "No active Claude buffer found. Create one first.")
      
      ;; Create some history entries for demo purposes if needed
      (when (null (ecc-history-get-entries buffer))
        (with-current-buffer buffer
          (ecc-history-record-entry buffer "Tell me about Emacs Lisp")
          (ecc-history-record-entry buffer "How can I create custom commands in Emacs?")
          (ecc-history-record-entry buffer "What are the key differences between lexical and dynamic binding in Emacs Lisp?")
          (sit-for 1) ; Add a small delay between entries
          (ecc-history-record-entry buffer "Show me an example of using advice-add in Emacs")
          (message "Created example history entries for demonstration")))
      
      ;; Browse the history
      (ecc-history-browse)
      (message "Use RET to re-send a prompt, 'r' to refresh, 'q' to quit"))))

;; Creating a dashboard of prompt history
(defun example-history-dashboard ()
  "Create a dashboard of prompt history across all Claude buffers."
  (interactive)
  ;; Create a dashboard buffer
  (let ((dashboard-buffer (get-buffer-create "*Claude History Dashboard*"))
        (all-claude-buffers (ecc-buffer-get-registered-buffers)))
    
    (with-current-buffer dashboard-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "# Claude Prompt History Dashboard\n\n")
        
        (if (null all-claude-buffers)
            (insert "No Claude buffers found. Create one to start building history.\n")
          
          ;; Display summary statistics
          (let ((total-prompts 0)
                (buffer-count (length all-claude-buffers)))
            
            ;; Count total prompts
            (dolist (buf all-claude-buffers)
              (when (buffer-live-p buf)
                (let ((entries (ecc-history-get-entries buf)))
                  (setq total-prompts (+ total-prompts (length entries))))))
            
            ;; Display stats
            (insert (format "Active Claude Buffers: %d\n" buffer-count))
            (insert (format "Total Prompts: %d\n\n" total-prompts))
            
            ;; Display each buffer's history
            (dolist (buf all-claude-buffers)
              (when (buffer-live-p buf)
                (let ((entries (ecc-history-get-entries buf)))
                  (insert (format "## %s (%d prompts)\n\n" 
                                 (buffer-name buf) 
                                 (length entries)))
                  
                  ;; Display the most recent 3 prompts for this buffer
                  (let ((recent-entries (if (> (length entries) 3)
                                            (seq-subseq entries 0 3)
                                          entries)))
                    (if (null recent-entries)
                        (insert "  No history entries yet.\n\n")
                      (let ((count 0))
                        (dolist (entry recent-entries)
                          (let* ((timestamp (car entry))
                                 (prompt (cdr entry))
                                 (formatted-time (format-time-string 
                                                 "%Y-%m-%d %H:%M:%S" 
                                                 timestamp))
                                 ;; Truncate prompt if too long
                                 (display-prompt 
                                  (if (> (length prompt) 60)
                                      (concat (substring prompt 0 57) "...")
                                    prompt)))
                            
                            (insert (format "  [%d] %s\n      %s\n\n" 
                                           count 
                                           formatted-time
                                           display-prompt))
                            (setq count (1+ count))))))))))))
        
        ;; Set up the mode and display
        (special-mode)
        (switch-to-buffer dashboard-buffer)
        (goto-char (point-min))))))

;; Example of exporting and importing history
(defun example-history-export ()
  "Export prompt history to a file."
  (interactive)
  (let* ((buffer (or (and (eq major-mode 'ecc-claude-vterm-mode)
                         (current-buffer))
                    (and (fboundp 'ecc-buffer-current-get-buffer)
                         (ecc-buffer-current-get-buffer))))
         (export-file (expand-file-name 
                      (format "claude-history-%s.el" 
                              (format-time-string "%Y%m%d-%H%M%S")))))
    
    ;; Make sure we have a buffer
    (if (not buffer)
        (message "No active Claude buffer found.")
      
      ;; Get history entries
      (let ((entries (ecc-history-get-entries buffer)))
        (if (null entries)
            (message "No history entries to export.")
          
          ;; Export to file
          (with-temp-file export-file
            (insert ";; Claude Prompt History Export\n")
            (insert (format ";; Buffer: %s\n" (buffer-name buffer)))
            (insert (format ";; Date: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
            
            (insert "(setq imported-history-entries\n  '(\n")
            
            ;; Insert each entry
            (dolist (entry entries)
              (let* ((timestamp (car entry))
                     (prompt (cdr entry))
                     (timestamp-seconds (float-time timestamp)))
                
                (insert (format "    (%f . %S)\n" timestamp-seconds prompt))))
            
            (insert "  ))\n\n")
            
            ;; Add import helper
            (insert "(defun example-history-import-to-buffer (buffer)\n")
            (insert "  \"Import the history entries to BUFFER.\"\n")
            (insert "  (when (buffer-live-p buffer)\n")
            (insert "    (dolist (entry imported-history-entries)\n")
            (insert "      (let* ((timestamp-seconds (car entry))\n")
            (insert "             (prompt (cdr entry))\n")
            (insert "             (timestamp (seconds-to-time timestamp-seconds)))\n")
            (insert "        (let ((current-entries (gethash buffer ecc-history-entries)))\n")
            (insert "          (puthash buffer (cons (cons timestamp prompt) current-entries) ecc-history-entries))))))\n"))
          
          (message "Exported %d history entries to %s" 
                  (length entries) export-file))))))

;; Advanced feature - create a prompt template from history
(defun example-history-create-template ()
  "Create a template from a prompt in history."
  (interactive)
  ;; Get the current Claude buffer
  (let ((buffer (or (and (eq major-mode 'ecc-claude-vterm-mode)
                        (current-buffer))
                   (and (fboundp 'ecc-buffer-current-get-buffer)
                        (ecc-buffer-current-get-buffer)))))
    
    ;; Make sure we have a buffer
    (if (not buffer)
        (message "No active Claude buffer found.")
      
      ;; Get history entries
      (let ((entries (ecc-history-get-entries buffer)))
        (if (null entries)
            (message "No history entries found.")
          
          ;; Create a selection buffer
          (let ((selection-buffer (get-buffer-create "*Create Template from History*")))
            (with-current-buffer selection-buffer
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert "# Select a prompt to create a template\n\n")
                (insert "Press RET on a prompt to create a template from it.\n\n")
                
                ;; List all prompts
                (let ((count 0))
                  (dolist (entry entries)
                    (let* ((timestamp (car entry))
                           (prompt (cdr entry))
                           (formatted-time (format-time-string 
                                           "%Y-%m-%d %H:%M:%S" 
                                           timestamp))
                           (start-pos (point)))
                      
                      ;; Insert with properties
                      (insert (format "[%d] %s\n%s\n\n" 
                                     count 
                                     formatted-time
                                     prompt))
                      
                      ;; Add text properties for selection
                      (put-text-property start-pos (point) 
                                       'example-history-template-entry 
                                       (cons count prompt))
                      
                      (setq count (1+ count)))))
                
                ;; Set up mode and keybindings
                (let ((map (make-sparse-keymap)))
                  (define-key map (kbd "RET") 
                             'example-history-template-select-at-point)
                  (define-key map (kbd "q") 'kill-this-buffer)
                  (use-local-map map))
                
                (special-mode)
                (switch-to-buffer selection-buffer)
                (goto-char (point-min))))))))))

;; Helper function for template creation
(defun example-history-template-select-at-point ()
  "Create a template from the prompt at point."
  (interactive)
  (let* ((pos (point))
         (props (text-properties-at pos))
         (entry-prop (get-text-property pos 'example-history-template-entry)))
    (if entry-prop
        (let ((prompt (cdr entry-prop)))
          (kill-buffer (current-buffer))
          
          ;; Create template buffer
          (let ((template-buffer (get-buffer-create "*New Claude Template*")))
            (with-current-buffer template-buffer
              (erase-buffer)
              (insert "# New Claude Template\n\n")
              (insert prompt)
              (insert "\n\n# Template Variables\n\n")
              (insert "You can add template variables in the format {{variable_name}} to make this template more reusable.\n\n")
              (insert "Example:\n")
              (insert "Tell me about {{programming_language}} programming language.\n\n")
              
              (switch-to-buffer template-buffer)
              (message "Created new template buffer. Edit and save to templates directory when ready."))))
      
      (message "No history entry at point."))))

(provide 'example-history-usage)

(when (not load-file-name)
  (message "history-usage.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

;;; history-usage.el ends here