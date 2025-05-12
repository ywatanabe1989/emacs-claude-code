;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-12 07:24:30>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/examples/run/run-usage.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; Commentary:
;; Run usage examples for emacs-claude-code.
;; This example demonstrates how to:
;; 1. Use the core run functions to send prompts to Claude
;; 2. Run Claude in VTerm mode
;; 3. Create custom run functions for specific workflows
;; 4. Process regions and buffers with Claude

;;; Code:

(require 'emacs-claude-code)
(require 'ecc-run)
(require 'ecc-run-vterm)

;; Basic usage - run Claude in VTerm mode
(defun example-run-claude-vterm ()
  "Launch Claude in VTerm mode with optimized settings.
VTerm provides a full terminal environment for Claude, optimal for
streaming responses and real-time feedback."
  (interactive)
  (if (not (featurep 'ecc-claude-vterm-mode))
      (message "Claude VTerm mode is not available. Make sure vterm is installed.")
    (ecc-run-vterm-claude)
    (message "Claude VTerm started. Type your queries directly in the terminal.")))

;; Example of running Claude on a region of text
(defun example-run-claude-on-region (start end)
  "Send selected region from START to END to Claude for processing.
Demonstrates the basic region processing functionality."
  (interactive "r")
  (if (region-active-p)
      (let ((prompt (read-string "Enter prompt for Claude (e.g., 'Please explain this code'): ")))
        (ecc-run-on-region start end prompt)
        (message "Region sent to Claude with prompt: %s" prompt))
    (message "No region selected. Please select a region first.")))

;; Example of running Claude on a whole buffer
(defun example-run-claude-on-buffer ()
  "Send the entire current buffer to Claude for processing.
Useful for analyzing entire files or documents."
  (interactive)
  (let ((prompt (read-string "Enter prompt for Claude (e.g., 'Please analyze this document'): ")))
    (ecc-run-on-buffer prompt)
    (message "Buffer sent to Claude with prompt: %s" prompt)))

;; Example with template processing
(defun example-run-with-template (start end)
  "Send region from START to END to Claude using a template.
Demonstrates how to use templates with Claude prompts."
  (interactive "r")
  (if (region-active-p)
      (let* ((prompt (read-string "What would you like Claude to do with this code? "))
             (template-text "Please analyze the following code and PLACEHOLDER
             
The code:
             
```
CONTENT
```

Provide your analysis in a clear, structured format with:
1. Overview of what the code does
2. Key functions and their purposes
3. Potential issues or improvements
4. Code quality assessment"))
        
        ;; Replace CONTENT placeholder with the actual region content
        (setq template-text (replace-regexp-in-string "CONTENT" 
                                                     (buffer-substring-no-properties start end) 
                                                     template-text))
        
        ;; Send to Claude using the template
        (ecc-run prompt nil template-text)
        (message "Region sent to Claude using code analysis template."))
    (message "No region selected. Please select a region first.")))

;; Example for quick queries without displaying buffer
(defun example-run-quick-background (prompt)
  "Send a quick PROMPT to Claude without switching to the Claude buffer.
This allows you to continue working while Claude processes your request."
  (interactive "sQuick background query for Claude: ")
  (ecc-run prompt t)  ; The 't' argument prevents buffer display
  (message "Query sent to Claude in the background. Switch to Claude buffer to see the response."))

;; Example of specialized code review function
(defun example-run-code-review (start end)
  "Send code from START to END to Claude for a detailed code review."
  (interactive "r")
  (if (region-active-p)
      (let* ((code (buffer-substring-no-properties start end))
             (language (read-string "Programming language: " 
                                   (or (file-name-extension (buffer-file-name)) "unknown")))
             (prompt (format "Please review this %s code carefully and provide feedback on:
1. Correctness: Are there any bugs or logical errors?
2. Performance: Are there any inefficiencies or performance concerns?
3. Readability: Is the code clear, well-commented, and easy to understand?
4. Best practices: Does the code follow %s best practices?
5. Security: Are there any security vulnerabilities or concerns?
6. Specific improvements: How could this code be improved?

Here's the code:

```%s
%s
```"
                            language language language code)))
        
        ;; Send to Claude
        (ecc-run prompt)
        (message "Code sent to Claude for comprehensive review."))
    (message "No region selected. Please select the code to review first.")))

;; Example of a function that runs multiple queries in sequence
(defun example-run-multi-query ()
  "Run multiple related queries to Claude in sequence.
This demonstrates how to perform a more complex interaction
by building on the results of previous queries."
  (interactive)
  
  ;; Define the sequence of prompts
  (let ((prompts '("What are the key principles of functional programming?"
                   "How do these principles apply specifically to Emacs Lisp?"
                   "Can you show practical examples of applying functional programming in Emacs Lisp?"
                   "What are the performance considerations when using functional patterns in Emacs Lisp?")))
    
    ;; Create a new Claude buffer for this sequence
    (let ((claude-buffer (ecc-run-vterm-claude))
          (prompt-count 0))
      
      ;; Display intro message
      (with-current-buffer claude-buffer
        (insert "\n\n# Multi-Query Example: Functional Programming in Emacs Lisp\n\n")
        (insert "This example will send a sequence of related queries to Claude to build a comprehensive understanding.\n\n")
        (insert "Press any key after each response to continue to the next query...\n\n"))
      
      ;; Send the first prompt to start
      (ecc-run (nth 0 prompts))
      (setq prompt-count 1)
      
      ;; Set up function to continue the sequence
      (message "Query 1/%d sent. Press 'C-c n' to send the next query when Claude completes its response."
               (length prompts))
      
      ;; Create a temporary keymap for continuing
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c n")
                   (lambda ()
                     (interactive)
                     (if (< prompt-count (length prompts))
                         (progn
                           (ecc-run (nth prompt-count prompts))
                           (setq prompt-count (1+ prompt-count))
                           (if (< prompt-count (length prompts))
                               (message "Query %d/%d sent. Press 'C-c n' for next query after response."
                                       prompt-count (length prompts))
                             (message "Final query sent. Sequence complete.")))
                       (message "All queries have been sent."))))
        
        ;; Install the temporary keymap
        (with-current-buffer claude-buffer
          (use-local-map (make-composed-keymap map (current-local-map))))))))

;; Example of running Claude with debugging enabled
(defun example-run-with-debugging ()
  "Run Claude with debugging information enabled.
This demonstrates how to enable and use the debugging features."
  (interactive)
  ;; Enable debugging
  (let ((ecc-run-debug t))
    (message "Debugging enabled for this Claude session")
    
    ;; Run a test prompt
    (ecc-run "This is a test prompt with debugging enabled")
    
    ;; Inform the user
    (message "Check the *Messages* buffer to see the debug information")))

(provide 'example-run-usage)

(when (not load-file-name)
  (message "run-usage.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

;;; run-usage.el ends here