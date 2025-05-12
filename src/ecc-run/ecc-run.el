;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 01:35:50>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/ecc-run.el
;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;; This file provides the core functionality for sending prompts to Claude

;; Required modules
(require 'ecc-variables)

;; External function declarations
(declare-function vterm-mode "ext:vterm")
(declare-function vterm-send-string "ext:vterm")
(declare-function vterm-send-return "ext:vterm")
(declare-function vterm-clear "ext:vterm")
(declare-function ecc-buffer-get-or-create-active-buffer "ecc-buffer")

;; Customization options
(defgroup ecc-run nil
  "Options for running Claude commands."
  :group 'ecc
  :prefix "ecc-run-")

(defcustom ecc-run-template-delay 0.5
  "Delay in seconds between sending text and hitting return."
  :type 'number
  :group 'ecc-run)

(defcustom ecc-run-debug nil
  "When non-nil, print debug messages."
  :type 'boolean
  :group 'ecc-run)

;; Helper functions
(defun ecc-run--debug (format-string &rest args)
  "Print debug message FORMAT-STRING with ARGS when debugging is enabled."
  (when ecc-run-debug
    (apply #'message (concat "[DEBUG] " format-string) args)))

;; 1. Primary user interface functions
;; ----------------------------------------

;;;###autoload
(defun ecc-run-on-region (start end &optional prompt no-display template-text)
  "Send region from START to END with optional PROMPT to Claude buffer.
If PROMPT is provided and non-empty, it will be prepended to the region content.
If NO-DISPLAY is non-nil, don't display the Claude buffer.
If TEMPLATE-TEXT is provided, it will replace 'PLACEHOLDER' with PROMPT."
  (interactive "r\nsPrompt for Claude (leave empty for default): ")
  (let* ((region-content (buffer-substring-no-properties start end))
         (combined-content (if (and prompt (not (string-empty-p prompt)))
                             (concat prompt "\n\n" region-content)
                           region-content)))
    (ecc-run--debug "ecc-run-on-region: region-content length: %d" (length region-content))
    (ecc-run--debug "ecc-run-on-region: prompt: %s" prompt)
    (ecc-run combined-content no-display template-text)))

;;;###autoload
(defun ecc-run-on-buffer (&optional prompt no-display template-text)
  "Send the entire current buffer as input to Claude.
If PROMPT is provided, it is prepended to the buffer contents.
If NO-DISPLAY is non-nil, don't display the Claude buffer.
If TEMPLATE-TEXT is provided, it will replace 'PLACEHOLDER' with the PROMPT."
  (interactive "sPrompt for Claude (leave empty for default): ")
  (ecc-run-on-region (point-min) (point-max) prompt no-display template-text))

;;;###autoload
(defun ecc-run-quick (prompt)
  "Send a quick PROMPT to Claude without template processing or preparation."
  (interactive "sQuick prompt for Claude: ")
  (let ((claude-buffer (ecc-buffer-get-or-create-active-buffer)))
    (with-current-buffer claude-buffer
      (vterm-send-string prompt)
      (vterm-send-return)
      (message "Quick prompt sent to Claude."))
    (display-buffer claude-buffer)))

;; 2. Core implementation
;; ----------------------------------------

;;;###autoload
(defun ecc-run (prompt &optional no-display template-text)
  "Send PROMPT to Claude buffer.
If NO-DISPLAY is non-nil, don't display the Claude buffer.
If TEMPLATE-TEXT is provided, it will replace 'PLACEHOLDER' with PROMPT."
  (interactive "sPrompt for Claude: ")
  (ecc-run--debug "ecc-run called with prompt: %s" prompt)
  (ecc-run--debug "no-display: %s, template-text: %s" no-display template-text)
  
  (let ((claude-buffer (ecc-buffer-get-or-create-active-buffer)))
    (ecc-run--debug "claude-buffer: %s" claude-buffer)
    
    (with-current-buffer claude-buffer
      ;; Clear previous content
      (vterm-clear)
      (ecc-run--debug "After vterm-clear")
      (sit-for ecc-run-template-delay)
      
      ;; Process template if needed
      (let* ((formatted-prompt 
             (if template-text
                 (replace-regexp-in-string "PLACEHOLDER" prompt template-text)
               prompt)))
        (ecc-run--debug "Formatted prompt: %s" formatted-prompt)
        
        ;; Send prompt and return
        (vterm-send-string formatted-prompt)
        (ecc-run--debug "After vterm-send-string")
        (sit-for ecc-run-template-delay)        
        (vterm-send-return)        
        (message "Prompt sent to Claude.")))
    
    ;; Display buffer if requested
    (when (not no-display)
      (ecc-run--debug "Displaying buffer: %s" claude-buffer)
      (display-buffer claude-buffer))))

(provide 'ecc-run)

(when (not load-file-name)
  (message "ecc-run.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))