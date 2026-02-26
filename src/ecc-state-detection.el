;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-02-26 12:22:10>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-state-detection.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)


;; 1. Dependencies
;; ----------------------------------------
(require 'ecc-debug)
(require 'ecc-vterm-utils)

;; 2. Configuration
;; ----------------------------------------

;; Define face for detection highlighting (works in monochrome/no-color mode)

(defface ecc-state-detection-flash-face
  '((t :inverse-video t :weight bold :underline t))
  "Face for flashing detected Claude prompt states.
Uses inverse-video and underline to work in monochrome/no-color vterm modes."
  :group 'ecc)

(defcustom --ecc-state-detection-buffer-size 2048
  "Number of characters to check from end of buffer for prompt detection.
Reduced from 2048 to 512 for better performance with multiple buffers.
Claude prompts typically appear in the last few hundred characters."
  :type 'integer
  :group 'ecc)

(defcustom --ecc-state-detection-flash-duration 3.0
  "Duration in seconds to flash the detected text."
  :type 'number
  :group 'ecc)

(defcustom --ecc-state-detection-flash-face
  'ecc-state-detection-flash-face
  "Face to use for flashing detected text."
  :type 'face
  :group 'ecc)

;; 3. Variables -- Centralized detection tokens -- AGENTS MUST NOT CHANGE THIS SECTION
;; ----------------------------------------

(defcustom --ecc-state-detection-prompt-char "❯ "
  "Primary prompt character used by Claude Code CLI."
  :type 'string :group 'ecc)

(defcustom --ecc-state-detection-waiting-patterns
  '("Crunched for"
	"Sautéed for"
	"Cogitated for"
	"Whipped up"
    "Brewed for"
	"Cooked for"
	"Marinated for"
	"Stewed for"
    "Baked for"
	"Simmered for"
	"Crafted for"
	"Distilled for"
    "❯ "
	"❯ "
	"> "
	"> ")
  "Explicit patterns that indicate Claude is waiting for input.
Includes completion messages and prompt chars with spacing variants."
  :type '(repeat string) :group 'ecc)

(defcustom --ecc-state-detection-permission-patterns
  '(" 2. Yes, and"
	" 2. Yes, allow"
	" 2. Yes, auto-accept edits"
    "2. Yes, and"
	"2. Yes, allow"
	"2. Yes, auto-accept")
  "Patterns indicating a Y/Y/N permission prompt."
  :type '(repeat string) :group 'ecc)

(defcustom --ecc-state-detection-suggestion-patterns
  '("↵ send")
  "Patterns indicating an edit suggestion."
  :type '(repeat string) :group 'ecc)

(defcustom --ecc-state-detection-running-patterns
  '("(esc to interrupt"
	" tokens"
	"ing…"
	"thought for")
  "Patterns indicating Claude is running."
  :type '(repeat string) :group 'ecc)

(defvar --ecc-state-detection-patterns nil
  "Alist mapping state symbols to detection patterns.
Built from centralized variables by `--ecc-state-detection-build-patterns'.")

(defun --ecc-state-detection-build-patterns ()
  "Build detection patterns from centralized variables."
  (let ((p --ecc-state-detection-prompt-char))
    (setq --ecc-state-detection-patterns
          `((:initial-waiting . (,(concat p " Try ")))
            (:waiting . (,(concat p "  ")
                         ,(concat p " ")
                         ,@--ecc-state-detection-completion-messages))
            (:y/n . (,(concat p " 1. Yes")))
            (:y/y/n . ,--ecc-state-detection-permission-patterns)
            (:running . (,(concat p " Press up")
                         ,@--ecc-state-detection-running-patterns))
            (:suggestion . ,--ecc-state-detection-suggestion-patterns)))))

(--ecc-state-detection-build-patterns)

(defvar --ecc-state-detection--flash-overlays nil
  "List of overlays used for flashing detected text.")

;; 4. Main Entry Points
;; ----------------------------------------

(defun --ecc-state-detection--normalize-text (text)
  "Normalize TEXT for reliable pattern matching.
Replaces non-breaking spaces (U+00A0) with regular spaces and
collapses multiple whitespace chars into single spaces for matching."
  (let ((result text))
    ;; Replace non-breaking spaces with regular spaces
    (setq result (replace-regexp-in-string "\u00a0" " " result))
    ;; Replace other Unicode whitespace with regular spaces
    (setq result
	      (replace-regexp-in-string
	       "[\u2000-\u200b\u202f\u205f\u3000]" " " result))
    result))

(defun --ecc-state-detection-detect (&optional buffer)
  "Detect Claude prompt state in BUFFER or current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (--ecc-debug-message "Detecting state in buffer: %s" (buffer-name))
    (let* ((raw-text (buffer-substring-no-properties
                      (max
                       (- (point-max)
                          --ecc-state-detection-buffer-size)
                       (point-min))
                      (point-max)))
           (buffer-text
	        (--ecc-state-detection--normalize-text raw-text)))
      (--ecc-state-detection--analyze-text buffer-text))))

;; 5. Core Functions
;; ----------------------------------------

(defun --ecc-state-detection--analyze-text (text)
  "Analyze TEXT to detect Claude prompt state."
  (catch 'found
    ;; Check for Y/Y/N pattern FIRST (highest priority - permission prompts)
    (let
        ((yyn-patterns
          (cdr (assq :y/y/n --ecc-state-detection-patterns))))
      (when yyn-patterns
        (dolist (pattern yyn-patterns)
          (when (string-match-p (regexp-quote pattern) text)
            (--ecc-debug-message
             "Matched state :y/y/n with pattern: %s" pattern)
            (throw 'found :y/y/n)))))
    ;; Regex fallback for Y/Y/N (handles vterm whitespace variations)
    (when (string-match-p "2\\.\\s-*Yes,\\s-*and" text)
      (--ecc-debug-message "Matched state :y/y/n with regex fallback")
      (throw 'found :y/y/n))

    ;; Check for suggestion pattern (second priority - actionable suggestions)
    (let
        ((suggestion-patterns
          (cdr (assq :suggestion --ecc-state-detection-patterns))))
      (when suggestion-patterns
        (dolist (pattern suggestion-patterns)
          (when (string-match-p (regexp-quote pattern) text)
            (--ecc-debug-message "Matched state :suggestion")
            (throw 'found :suggestion)))))

    ;; Check for Y/N pattern (third priority, must come before running check)
    (let
        ((yn-patterns
          (cdr (assq :y/n --ecc-state-detection-patterns))))
      (when yn-patterns
        (dolist (pattern yn-patterns)
          (when (string-match-p (regexp-quote pattern) text)
            (--ecc-debug-message "Matched state :y/n")
            (throw 'found :y/n)))))

    ;; Check for running pattern (lower priority than permission prompts)
    (let
        ((running-patterns
          (cdr (assq :running --ecc-state-detection-patterns))))
      (when running-patterns
        (dolist (pattern running-patterns)
          (when (string-match-p (regexp-quote pattern) text)
            (--ecc-debug-message "Matched state :running")
            (throw 'found :running)))))

    ;; Check for other exact pattern matches
    (dolist (pattern-pair --ecc-state-detection-patterns)
      (let ((state (car pattern-pair))
            (patterns (cdr pattern-pair)))
        (unless (memq state '(:y/y/n :y/n :running))
          (dolist (pattern patterns)
            (when (string-match-p (regexp-quote pattern) text)
              (--ecc-debug-message "Matched state %s" state)
              (throw 'found state))))))
    ;; Regex fallbacks for waiting (handles vterm char/spacing variations)
    (when (string-match-p "❯[ \t\u00a0]" text)
      (--ecc-debug-message
       "Matched state :waiting with regex (❯ + space)")
      (throw 'found :waiting))
    nil))

;; 6. Helper/Utility Functions
;; ----------------------------------------

(defun --ecc-state-detection--has-previous-messages-p ()
  "Check if buffer has previous messages (not the initial state)."
  ;; Simply check buffer size as a quick heuristic
  (> (- (point-max) (point-min)) 500))

(defun --ecc-state-detection-get-name (state)
  "Convert STATE symbol to human-readable name."
  (cond
   ((eq state :y/y/n) "Y/Y/N")
   ((eq state :y/n) "Y/N")
   ((eq state :waiting) "Waiting")
   ((eq state :initial-waiting) "Initial-Waiting")
   ((eq state :running) "Running")
   ((eq state :suggestion) "Suggestion")
   (t (format "%s" state))))

(defun --ecc-state-detection-flash-pattern (state &optional buffer)
  "Flash the entire line containing the detected pattern for STATE in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (let
        ((patterns (cdr (assq state --ecc-state-detection-patterns))))
      (when patterns
        (--ecc-state-detection--remove-flash-overlays)
        (save-excursion
          (goto-char (point-max))
          (let ((found nil))
            (dolist (pattern patterns)
              (unless found
                (when (search-backward pattern
                                       (max
                                        (- (point-max)
                                           --ecc-state-detection-buffer-size)
                                        (point-min))
                                       t)
                  ;; Highlight the entire line instead of just the pattern
                  (let* ((line-start (line-beginning-position))
                         (line-end (min (point-max)
                                        (1+ (line-end-position))))
                         (overlay (make-overlay line-start line-end)))
                    (overlay-put overlay 'face
                                 --ecc-state-detection-flash-face)
                    (overlay-put overlay 'priority 1000)
                    (push overlay
                          --ecc-state-detection--flash-overlays)
                    (run-with-timer
                     --ecc-state-detection-flash-duration nil
                     '--ecc-state-detection--remove-flash-overlays)
                    (setq found t)))))))))))

(defun --ecc-state-detection-flash-all-patterns (&optional buffer)
  "Flash ALL matching patterns for ALL states in BUFFER.
This highlights every pattern that matches, useful for debugging."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (--ecc-state-detection--remove-flash-overlays)
    (let ((search-start (max (- (point-max)
                                --ecc-state-detection-buffer-size)
                             (point-min))))
      (save-excursion
        (dolist (pattern-pair --ecc-state-detection-patterns)
          (let ((patterns (cdr pattern-pair)))
            (dolist (pattern patterns)
              (goto-char (point-max))
              (while (search-backward pattern search-start t)
                (let* ((match-start (match-beginning 0))
                       (match-end (match-end 0))
                       (overlay (make-overlay match-start match-end)))
                  (overlay-put overlay 'face
                               --ecc-state-detection-flash-face)
                  (overlay-put overlay 'priority 1000)
                  (push overlay --ecc-state-detection--flash-overlays))))))
        ;; Set timer to remove all overlays
        (run-with-timer --ecc-state-detection-flash-duration nil
                        '--ecc-state-detection--remove-flash-overlays)))))

(defun --ecc-state-detection--remove-flash-overlays ()
  "Remove all flash overlays."
  (mapc 'delete-overlay --ecc-state-detection--flash-overlays)
  (setq --ecc-state-detection--flash-overlays nil))

(defun --ecc-state-detection-diagnose (&optional buffer)
  "Diagnose what Claude prompt patterns exist in BUFFER."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (let* ((buffer-text (buffer-substring-no-properties
                         (max
                          (- (point-max)
                             --ecc-state-detection-buffer-size)
                          (point-min))
                         (point-max)))
           (state (--ecc-state-detection-detect))
           (last-100-chars
            (substring buffer-text
                       (max 0 (- (length buffer-text) 100))))
           (last-line (--ecc-vterm-utils-get-last-non-empty-line))
           (session-active
            (--ecc-vterm-utils-is-claude-session-active)))
      (--ecc-debug-message "=== Claude State Detection Diagnosis ===")
      (--ecc-debug-message "Current state: %s" (or state "none"))
      (--ecc-debug-message "Claude session active: %s"
                           (if session-active "yes" "no"))
      (--ecc-debug-message "Last non-empty line: %S" last-line)
      (--ecc-debug-message "Last 100 chars: %S" last-100-chars)
      (--ecc-debug-message "Contains '│': %s"
                           (if (string-match-p "│" buffer-text) "yes"
                             "no"))
      (--ecc-debug-message "Contains '>': %s"
                           (if (string-match-p ">" buffer-text) "yes"
                             "no"))
      (--ecc-debug-message "Contains 'Human:': %s"
                           (if (string-match-p "Human:" buffer-text)
                               "yes"
                             "no"))
      (--ecc-debug-message "Contains 'esc to interrupt': %s"
                           (if
                               (string-match-p "esc to interrupt"
                                               buffer-text)
                               "yes"
                             "no"))
      (--ecc-debug-message "========================================")
      state)))

(when
    (not load-file-name)
  (--ecc-debug-message "ecc-state-detection.el loaded."
                       (file-name-nondirectory
                        (or load-file-name buffer-file-name))))


(provide 'ecc-state-detection)

(when
    (not load-file-name)
  (message "ecc-state-detection.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))