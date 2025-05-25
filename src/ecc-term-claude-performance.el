;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 12:00:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-term-claude-performance.el

;;; Commentary:
;;; Performance optimizations for Claude terminal mode.
;;; This module provides specialized functions for optimizing performance
;;; in Claude vterm buffers, particularly for handling large outputs,
;;; efficient state detection, and responsive UI updates.

(require 'ecc-variables)

;; Forward declarations to prevent free variable warnings
(defvar ecc-term-claude-line-numbers nil
  "When non-nil, display line numbers in Claude buffers.")
(defvar ecc-term-claude-scroll-conservatively 101
  "Value for scroll-conservatively in Claude buffers.")
(defvar ecc-term-claude-truncate-lines t
  "When non-nil, truncate lines in Claude buffers.")
(defvar ecc-term-claude-update-functions nil
  "Hook functions to run after each update in Claude buffers.")
(defvar ecc-state-prompt-waiting nil
  "Custom prompt string for waiting state.")
(defvar ecc-state-prompt-y/n nil
  "Custom prompt string for Y/N state.")
(defvar ecc-state-prompt-y/y/n nil
  "Custom prompt string for Y/Y/N state.")
(defvar ecc-state-prompt-initial-waiting nil
  "Custom prompt string for initial waiting state.")

;;; Code:

;;;; Buffer performance settings


(defcustom ecc-term-claude-performance-mode t
  "Whether to apply performance optimizations to Claude buffers.
When enabled, various performance optimizations are applied to
improve the handling of large outputs and streaming content."
  :type 'boolean
  :group 'ecc-term-claude)


(defcustom ecc-term-claude-gc-threshold (* 64 1024 1024)  ; 64MB
  "GC threshold to use during high-output operations.
Higher values reduce garbage collection frequency during periods of
intense output, improving performance at the cost of higher memory usage.
The default value of 64MB provides a good balance for most systems."
  :type 'integer
  :group 'ecc-term-claude)


(defcustom ecc-term-claude-max-search-size 2000
  "Maximum buffer size in characters to search for state detection.
Larger values may improve detection accuracy but can impact performance
in very large buffers. The default of 2000 characters is sufficient
for most prompt detection needs."
  :type 'integer
  :group 'ecc-term-claude)


(defcustom ecc-term-claude-jit-lock-defer-time 0.05
  "Time to defer fontification during rapid updates.
Lower values make text appear formatted more quickly, but may
reduce responsiveness during rapid content streaming."
  :type 'float
  :group 'ecc-term-claude)

;;;; Performance optimization functions


(defun ecc-term-claude-optimize-buffer (&optional buffer)
  "Apply performance optimizations to BUFFER or current buffer.
These optimizations are particularly helpful for handling large
outputs from Claude, improving responsiveness and reducing
resource usage.

Arguments:
  BUFFER: Optional buffer to optimize. If nil, uses current buffer.

Side Effects:
  Modifies various buffer-local settings that affect performance."
  (with-current-buffer (or buffer (current-buffer))
    ;; Scrolling optimizations
    (setq-local scroll-conservatively ecc-term-claude-scroll-conservatively
                scroll-margin 0
                scroll-step 1
                fast-but-imprecise-scrolling t
                auto-window-vscroll nil)
    
    ;; Display optimizations
    (setq-local truncate-lines ecc-term-claude-truncate-lines
                line-move-visual t
                inhibit-field-text-motion t)
    
    ;; Font rendering optimizations
    (setq-local jit-lock-defer-time ecc-term-claude-jit-lock-defer-time
                redisplay-skip-fontification-on-input t
                redisplay-dont-pause t)
    
    ;; Disable potentially slow minor modes
    (when (and (boundp 'display-line-numbers-mode)
               (not ecc-term-claude-line-numbers))
      (display-line-numbers-mode -1))
    
    ;; Disable undo for better performance
    (buffer-disable-undo)))


(defun ecc-term-claude-with-gc-optimization (func &rest args)
  "Execute FUNC with ARGS under optimized garbage collection settings.
Temporarily increases GC threshold during execution of the function
to reduce GC frequency, which can improve performance during
intensive operations like state detection in large buffers.

Arguments:
  FUNC: The function to execute.
  ARGS: Arguments to pass to FUNC.

Returns:
  The result of calling FUNC with ARGS."
  (let ((old-gc-threshold gc-cons-threshold))
    (setq gc-cons-threshold ecc-term-claude-gc-threshold)
    (unwind-protect
        (apply func args)
      (setq gc-cons-threshold old-gc-threshold))))


(defun ecc-term-claude-get-state-optimized (&optional buffer)
  "Get Claude state with performance optimizations.
Uses optimized GC settings and performs targeted search to improve
performance, especially in large buffers.

Arguments:
  BUFFER: Optional buffer to check. If nil, uses current buffer.

Returns:
  The detected Claude prompt state (a symbol) or nil if no state is detected."
  (ecc-term-claude-with-gc-optimization
   (lambda (buf)
     (with-current-buffer buf
       ;; Only examine relevant parts of large buffers
       (let* ((buffer-size (buffer-size))
              (search-size (min buffer-size ecc-term-claude-max-search-size))
              (state nil))
         ;; First try quick detection at the end of buffer
         (save-excursion
           (goto-char (point-max))
           (let ((end-text (buffer-substring-no-properties
                           (max (- (point-max) 500) (point-min))
                           (point-max))))
             (setq state (ecc-term-claude-analyze-text-for-state end-text))))
         
         ;; If no state found, try more thorough search
         (unless state
           (let ((full-text (buffer-substring-no-properties
                            (max (- (point-max) search-size) (point-min))
                            (point-max))))
             (setq state (ecc-term-claude-analyze-text-for-state full-text))))
         
         state)))
   (or buffer (current-buffer))))


(defun ecc-term-claude-analyze-text-for-state (text)
  "Analyze TEXT for Claude prompt states.
A performance-optimized version of state detection for use in large buffers.

Arguments:
  TEXT: The text to analyze for prompt patterns.

Returns:
  A state symbol (:y/y/n, :y/n, :waiting, :initial-waiting) or nil."
  ;; Most common patterns first for early return
  (cond
   ;; Quick check for waiting prompts (most common)
   ((string-match-p "continue>\\|Continue>" text) :waiting)
   
   ;; Next check for yes/no prompts
   ((string-match-p "\\[y/n\\]\\|\\[Y/n\\]" text) :y/n)
   
   ;; Check for yes/yes+/no prompts
   ((string-match-p "\\[Y/y/n\\]" text) :y/y/n)
   
   ;; Only check custom patterns if the common ones aren't found
   ((and (boundp 'ecc-state-prompt-waiting)
         ecc-state-prompt-waiting
         (string-match-p (regexp-quote ecc-state-prompt-waiting) text))
    :waiting)
   
   ((and (boundp 'ecc-state-prompt-y/n)
         ecc-state-prompt-y/n
         (string-match-p (regexp-quote ecc-state-prompt-y/n) text))
    :y/n)
   
   ((and (boundp 'ecc-state-prompt-y/y/n)
         ecc-state-prompt-y/y/n
         (string-match-p (regexp-quote ecc-state-prompt-y/y/n) text))
    :y/y/n)
   
   ((and (boundp 'ecc-state-prompt-initial-waiting)
         ecc-state-prompt-initial-waiting
         (string-match-p (regexp-quote ecc-state-prompt-initial-waiting) text))
    :initial-waiting)
   
   ;; No state detected
   (t nil)))

;;;; Hook optimizations


(defun ecc-term-claude-optimize-hooks ()
  "Optimize Claude vterm hooks for better performance.
Consolidates multiple hook functions into a single function to reduce
the number of function calls and improve performance during rapid updates.

Side Effects:
  Modifies various hook variables."
  (interactive)
  ;; Consolidate update functions into a single function
  (when (> (length ecc-term-claude-update-functions) 1)
    (let ((funcs (seq-copy ecc-term-claude-update-functions)))
      (setq ecc-term-claude-update-functions
            (list (lambda ()
                    (dolist (func funcs)
                      (condition-case err
                          (funcall func)
                        (error
                         (ecc-debug-message "Error in hook function %s: %s"
                                 func (error-message-string err))))))))))
  
  ;; Connect to vterm hooks with optimized function
  (add-hook 'vterm-update-functions
            (lambda (&rest _)
              (run-hooks 'ecc-term-claude-update-functions))
            nil t))

;;;; UI responsiveness improvements


(defun ecc-term-claude-defer-updates (func)
  "Create a debounced version of FUNC for improved UI responsiveness.
Returns a function that delays execution of FUNC until there's a pause
in input, which helps maintain UI responsiveness during rapid updates.

Arguments:
  FUNC: The function to debounce.

Returns:
  A debounced version of the function."
  (let ((timer nil))
    (lambda (&rest args)
      ;; Cancel previous timer if it exists
      (when timer
        (cancel-timer timer)
        (setq timer nil))
      
      ;; Create new timer
      (setq timer (run-with-idle-timer
                  0.1 nil
                  (lambda ()
                    (apply func args)))))))

(provide 'ecc-term-claude-performance)

;;; ecc-term-claude-performance.el ends here