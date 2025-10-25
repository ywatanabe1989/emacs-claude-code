;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-10-24 18:15:17>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-state-detection.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


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

;; 3. Variables
;; ----------------------------------------

(defvar --ecc-state-detection-patterns
  '((:initial-waiting . ("> Try "))
    (:waiting . (">  "))
    (:y/n . ("❯ 1. Yes"))
    (:y/y/n . (" 2. Yes, and" " 2. Yes, allow"))
    (:running . ("(esc to interrupt")))
  "Alist mapping state symbols to detection patterns. Note that space around > are non-breaking space.")

(defvar --ecc-state-detection--flash-overlays nil
  "List of overlays used for flashing detected text.")

;; 4. Main Entry Points
;; ----------------------------------------

(defun --ecc-state-detection-detect (&optional buffer)
  "Detect Claude prompt state in BUFFER or current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (--ecc-debug-message "Detecting state in buffer: %s" (buffer-name))
    (let ((buffer-text (buffer-substring-no-properties
                        (max
                         (- (point-max)
                            --ecc-state-detection-buffer-size)
                         (point-min))
                        (point-max))))
      (--ecc-state-detection--analyze-text buffer-text))))

;; 5. Core Functions
;; ----------------------------------------

(defun --ecc-state-detection--analyze-text (text)
  "Analyze TEXT to detect Claude prompt state."
  (catch 'found
    ;; Check for Y/Y/N pattern FIRST (highest priority - permission prompts are most actionable)
    (let
        ((yyn-patterns
          (cdr (assq :y/y/n --ecc-state-detection-patterns))))
      (when yyn-patterns
        (dolist (pattern yyn-patterns)
          (when (string-match-p (regexp-quote pattern) text)
            (--ecc-debug-message
             "Matched state :y/y/n with pattern: %s" pattern)
            (throw 'found :y/y/n)))))

    ;; Check for Y/N pattern (second priority, must come before running check)
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