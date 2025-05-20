;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-12 07:34:30>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/examples/large-buffer/large-buffer-usage.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; Commentary:
;; Large buffer handling examples for emacs-claude-code.
;; This example demonstrates how to:
;; 1. Process and send large text files to Claude
;; 2. Customize chunking behavior for large inputs
;; 3. Handle large code bases and documentation
;; 4. Implement progressive loading with feedback

;;; Code:

(require 'emacs-claude-code)
(require 'ecc-large-buffer)

;; Basic chunking demonstration
(defun example-large-buffer-demonstration ()
  "Demonstrate large buffer chunking with artificial text.
This function creates a large text and shows how it's split into chunks."
  (interactive)
  ;; Create a demonstration buffer
  (let* ((demo-buffer (get-buffer-create "*Large Buffer Demo*"))
         ;; Artificial content generator
         (generate-paragraphs
          (lambda (n)
            (let ((result ""))
              (dotimes (i n)
                (setq result 
                      (concat result
                              (format "Paragraph %d: This is example text to demonstrate large buffer handling in Claude. " (1+ i))
                              (make-string 60 ?a)
                              (make-string 60 ?b)
                              (make-string 60 ?c)
                              "\n\n")))
              result)))
         ;; Generate ~ 10,000 characters of text
         (large-text (funcall generate-paragraphs 30)))
    
    (with-current-buffer demo-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "# Large Buffer Handling Demonstration\n\n")
        (insert "This demonstration shows how large inputs are automatically split into chunks for Claude processing.\n\n")
        
        ;; Show the large text we're using
        (insert "## Sample Large Text\n\n")
        (insert (format "Total size: %d characters\n\n" (length large-text)))
        (insert "First 200 characters:\n")
        (insert (substring large-text 0 200))
        (insert "...\n\n")
        
        ;; Demonstrate chunking
        (let* ((chunk-size (ecc-large-buffer-get-optimal-chunk-size))
               (chunks (ecc-large-buffer-chunk-string large-text chunk-size)))
          
          (insert "## Chunking Result\n\n")
          (insert (format "Using chunk size: %d characters\n" chunk-size))
          (insert (format "Generated %d chunks\n\n" (length chunks)))
          
          ;; Show a preview of each chunk
          (let ((i 0))
            (dolist (chunk chunks)
              (setq i (1+ i))
              (insert (format "### Chunk %d/%d\n\n" i (length chunks)))
              (insert (format "Size: %d characters\n" (length chunk)))
              (insert "Preview:\n")
              (insert (substring chunk 0 (min 100 (length chunk))))
              (insert "...\n\n"))))
        
        ;; Add usage instructions
        (insert "## Example Usage\n\n")
        (insert "When you have a large file to process with Claude:\n\n")
        (insert "```elisp\n")
        (insert ";; Sending a region\n")
        (insert "(ecc-large-buffer-send-region (point-min) (point-max))\n\n")
        (insert ";; Sending a file\n")
        (insert "(ecc-large-buffer-send-file \"/path/to/large/file.txt\")\n")
        (insert "```\n\n")
        
        ;; Add configuration options
        (insert "## Configuration Options\n\n")
        (insert "You can customize the large buffer handling with:\n\n")
        (insert "```elisp\n")
        (insert ";; Set chunk size (default: 4000 characters)\n")
        (insert "(setq ecc-large-buffer-default-chunk-size 4000)\n\n")
        (insert ";; Change the size threshold for chunking (default: 50000 characters)\n")
        (insert "(setq ecc-large-buffer-size-threshold 50000)\n\n")
        (insert ";; Customize the prompt sent between chunks\n")
        (insert "(setq ecc-large-buffer-prompt-between-chunks \"Continued from previous...\")\n")
        (insert "```\n")
        
        ;; Make buffer readable
        (special-mode)))
    
    ;; Show the buffer
    (switch-to-buffer demo-buffer)))

;; Large file handling example
(defun example-large-buffer-send-file-with-progress ()
  "Send a large file to Claude with interactive progress reporting."
  (interactive)
  ;; Prompt for a file
  (let ((file-path (read-file-name "Select large file to send to Claude: ")))
    (if (file-exists-p file-path)
        (let* ((file-size (file-attribute-size (file-attributes file-path)))
               (proceed (if (> file-size 100000) ; ~100KB
                          (y-or-n-p 
                           (format "This file is %d bytes. Process in chunks? " file-size))
                        t)))
          (when proceed
            ;; Show progress in a dedicated buffer
            (let ((progress-buffer (get-buffer-create "*Claude File Progress*")))
              (with-current-buffer progress-buffer
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert "# Claude File Processing Progress\n\n")
                  (insert (format "File: %s\n" file-path))
                  (insert (format "Size: %d bytes\n\n" file-size))
                  (insert "Processing in chunks...\n\n")
                  (insert "[ ] Preparing file...\n")
                  (insert "[ ] Calculating optimal chunk size...\n")
                  (insert "[ ] Splitting content...\n")
                  (insert "[ ] Sending to Claude...\n")
                  
                  ;; Make buffer readable
                  (special-mode)))
              
              ;; Show progress buffer
              (display-buffer progress-buffer)
              
              ;; Define update function
              (cl-flet ((update-progress 
                         (step)
                         (with-current-buffer progress-buffer
                           (let ((inhibit-read-only t))
                             (goto-char (point-min))
                             (when (re-search-forward (format "\\[ \\] %s" step) nil t)
                               (replace-match (format "[✓] %s" step)))))))
                
                ;; Update progress for preparation
                (update-progress "Preparing file...")
                
                ;; Calculate optimal chunk size
                (let ((chunk-size (ecc-large-buffer-get-optimal-chunk-size)))
                  (update-progress "Calculating optimal chunk size...")
                  
                  ;; Read and split content
                  (let* ((content (with-temp-buffer
                                    (insert-file-contents file-path)
                                    (buffer-string)))
                         (chunks (ecc-large-buffer-chunk-string content chunk-size)))
                    
                    ;; Update progress for splitting
                    (update-progress "Splitting content...")
                    
                    ;; Update progress buffer with chunk information
                    (with-current-buffer progress-buffer
                      (let ((inhibit-read-only t))
                        (goto-char (point-max))
                        (insert "\n## Chunk Information\n\n")
                        (insert (format "Total chunks: %d\n" (length chunks)))
                        (insert (format "Chunk size: ~%d characters\n\n" chunk-size))
                        (insert "## Sending Progress\n\n")))
                    
                    ;; Send chunks with progress
                    (let ((i 0)
                          (total (length chunks)))
                      (dolist (chunk chunks)
                        (setq i (1+ i))
                        
                        ;; Update progress in the buffer
                        (with-current-buffer progress-buffer
                          (let ((inhibit-read-only t))
                            (goto-char (point-max))
                            (insert (format "Sending chunk %d/%d (%d characters)... " 
                                           i total (length chunk)))))
                        
                        ;; Send the chunk
                        (if (= i 1)
                            ;; First chunk - send with optional prompt
                            (let ((prompt (read-string 
                                          "Enter a prompt for Claude (optional): ")))
                              (when (fboundp '--ecc-send-string)
                                (--ecc-send-string 
                                 (if (string-empty-p prompt)
                                     chunk
                                   (concat prompt "\n\n" chunk))
                                 t 0.5)))
                          
                          ;; Subsequent chunks - send with continuation prompt
                          (when (fboundp '--ecc-send-string)
                            (--ecc-send-string 
                             (concat ecc-large-buffer-prompt-between-chunks 
                                    "\n\n" chunk) 
                             t 0.5)))
                        
                        ;; Mark chunk as sent
                        (with-current-buffer progress-buffer
                          (let ((inhibit-read-only t))
                            (goto-char (point-max))
                            (insert "done\n"))))
                      
                      ;; Mark sending as complete
                      (update-progress "Sending to Claude...")
                      
                      ;; Add final status
                      (with-current-buffer progress-buffer
                        (let ((inhibit-read-only t))
                          (goto-char (point-max))
                          (insert "\n## Complete\n\n")
                          (insert "All chunks have been sent to Claude.\n")
                          (insert "Check the Claude buffer for responses.\n"))))))))
          
          ;; If user declined
          (unless proceed
            (message "File processing cancelled.")))
      
      ;; File doesn't exist
      (message "File not found: %s" file-path))))

;; Processing code repositories in chunks
(defun example-large-buffer-process-codebase ()
  "Process and send a code repository to Claude in chunks.
This demonstrates how to handle a multi-file codebase as a large input."
  (interactive)
  ;; Prompt for a directory
  (let ((root-dir (read-directory-name "Select code repository root: ")))
    (if (file-directory-p root-dir)
        (let* ((files (directory-files-recursively root-dir "\\.[a-zA-Z]+$" t))
               (proceed (y-or-n-p 
                        (format "Found %d files. Process and send to Claude? " 
                               (length files)))))
          (when proceed
            ;; Create a buffer to hold the combined content
            (let ((combined-buffer (get-buffer-create "*Combined Codebase*")))
              (with-current-buffer combined-buffer
                (erase-buffer)
                
                ;; Add header
                (insert "# Combined Codebase for Claude Analysis\n\n")
                
                ;; Process each file
                (let ((total-size 0)
                      (file-count 0)
                      (skipped-count 0)
                      (skipped-files '()))
                  
                  ;; Filter to handle only text files of reasonable size
                  (dolist (file files)
                    (if (and (file-readable-p file)
                            (not (string-match-p "/\\.git/" file))
                            (let ((file-size (file-attribute-size 
                                             (file-attributes file))))
                              (and (< file-size 500000) ; 500KB max
                                  (> file-size 0))))
                        
                        ;; Process the file
                        (let ((relative-path (file-relative-name file root-dir)))
                          (insert (format "## File: %s\n\n" relative-path))
                          (insert "```")
                          
                          ;; Add language hint based on extension
                          (let ((ext (file-name-extension file)))
                            (when ext
                              (cond
                               ((member ext '("el" "lisp")) (insert "elisp"))
                               ((member ext '("py")) (insert "python"))
                               ((member ext '("js")) (insert "javascript"))
                               ((member ext '("ts")) (insert "typescript"))
                               ((member ext '("c" "h" "cpp")) (insert "c"))
                               ((member ext '("md")) (insert "markdown")))))
                          
                          (insert "\n")
                          
                          ;; Insert file contents
                          (let ((content (with-temp-buffer
                                          (insert-file-contents file)
                                          (buffer-string))))
                            (insert content)
                            (setq total-size (+ total-size (length content))))
                          
                          (insert "\n```\n\n")
                          (setq file-count (1+ file-count)))
                      
                      ;; Skip this file
                      (push (file-relative-name file root-dir) skipped-files)
                      (setq skipped-count (1+ skipped-count))))
                  
                  ;; Add summary of processed and skipped files
                  (goto-char (point-min))
                  (forward-line 2)
                  (insert (format "Repository: %s\n" root-dir))
                  (insert (format "Files processed: %d\n" file-count))
                  (insert (format "Files skipped: %d\n" skipped-count))
                  (insert (format "Total content size: %d characters\n\n" total-size))
                  
                  (when (> skipped-count 0)
                    (insert "Skipped files:\n")
                    (dolist (file (reverse skipped-files))
                      (insert (format "- %s\n" file)))
                    (insert "\n"))
                  
                  ;; Add instructions for Claude
                  (insert "## Analysis Request\n\n")
                  (insert "Please analyze this codebase, focusing on:\n\n")
                  (insert "1. Overall architecture and design patterns\n")
                  (insert "2. Key components and their relationships\n")
                  (insert "3. Code quality and potential improvements\n")
                  (insert "4. Any issues or bugs you can identify\n\n")
                  
                  ;; Process with large buffer handler
                  (if (fboundp 'ecc-large-buffer-send-buffer)
                      (progn
                        (message "Sending combined codebase to Claude (%d characters)..." 
                                total-size)
                        (ecc-large-buffer-send-buffer combined-buffer))
                    (message "ecc-large-buffer-send-buffer function not available"))))))
        
        ;; Not a directory
        (message "Not a valid directory: %s" root-dir))))

;; Customizing chunk parameters example
(defun example-large-buffer-customize-settings ()
  "Interactively customize large buffer handling settings."
  (interactive)
  (let ((settings-buffer (get-buffer-create "*Large Buffer Settings*")))
    (with-current-buffer settings-buffer
      (erase-buffer)
      
      ;; Add current settings info
      (insert "# Large Buffer Handling Settings\n\n")
      
      (insert "## Current Settings\n\n")
      (insert (format "Chunk size: %d characters\n" 
                     ecc-large-buffer-default-chunk-size))
      (insert (format "Size threshold: %d characters\n" 
                     ecc-large-buffer-size-threshold))
      (insert (format "Token ratio: %.1f characters per token\n" 
                     ecc-large-buffer-token-ratio))
      (insert (format "Between-chunks prompt: \"%s\"\n\n" 
                     ecc-large-buffer-prompt-between-chunks))
      
      ;; Add customization options
      (insert "## Customization Options\n\n")
      
      ;; Chunk size
      (let ((chunk-sizes '(1000 2000 4000 8000)))
        (insert "### Chunk Size\n\n")
        (dolist (size chunk-sizes)
          (insert-button (format "[%d chars] " size)
                        'action (lambda (_) 
                                 (setq ecc-large-buffer-default-chunk-size size)
                                 (example-large-buffer-customize-settings))
                        'help-echo (format "Set chunk size to %d characters" size)))
        (insert "\n\n"))
      
      ;; Size threshold
      (let ((thresholds '(10000 25000 50000 100000)))
        (insert "### Size Threshold\n\n")
        (dolist (threshold thresholds)
          (insert-button (format "[%d chars] " threshold)
                        'action (lambda (_) 
                                 (setq ecc-large-buffer-size-threshold threshold)
                                 (example-large-buffer-customize-settings))
                        'help-echo (format "Set size threshold to %d characters" threshold)))
        (insert "\n\n"))
      
      ;; Token ratio
      (let ((ratios '(3.0 3.5 4.0 4.5 5.0)))
        (insert "### Character/Token Ratio\n\n")
        (dolist (ratio ratios)
          (insert-button (format "[%.1f] " ratio)
                        'action (lambda (_) 
                                 (setq ecc-large-buffer-token-ratio ratio)
                                 (example-large-buffer-customize-settings))
                        'help-echo (format "Set character/token ratio to %.1f" ratio)))
        (insert "\n\n"))
      
      ;; Between chunks prompt
      (insert "### Between-Chunks Prompt\n\n")
      (insert-button "[Set Custom Prompt]"
                    'action (lambda (_)
                             (let ((prompt (read-string 
                                          "Enter prompt to use between chunks: " 
                                          ecc-large-buffer-prompt-between-chunks)))
                               (setq ecc-large-buffer-prompt-between-chunks prompt)
                               (example-large-buffer-customize-settings)))
                    'help-echo "Set a custom prompt to use between chunks")
      (insert "\n\n")
      
      ;; Reset to defaults
      (insert "### Reset to Defaults\n\n")
      (insert-button "[Reset All Settings]"
                    'action (lambda (_)
                             (setq ecc-large-buffer-default-chunk-size 4000)
                             (setq ecc-large-buffer-size-threshold 50000)
                             (setq ecc-large-buffer-token-ratio 4.0)
                             (setq ecc-large-buffer-prompt-between-chunks 
                                  "Continued from previous chunk...")
                             (example-large-buffer-customize-settings))
                    'help-echo "Reset all settings to their defaults")
      (insert "\n\n")
      
      ;; Testing section
      (insert "## Test Your Settings\n\n")
      (insert-button "[Run Demonstration]"
                    'action (lambda (_) (example-large-buffer-demonstration))
                    'help-echo "Run a demonstration of large buffer handling")
      (insert "  ")
      (insert-button "[Process Codebase]"
                    'action (lambda (_) (example-large-buffer-process-codebase))
                    'help-echo "Process a code repository with current settings")
      
      ;; Show buffer
      (special-mode)
      (switch-to-buffer settings-buffer))))

(provide 'example-large-buffer-usage)

(when (not load-file-name)
  (message "large-buffer-usage.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

;;; large-buffer-usage.el ends here