;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-28 05:36:45>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-vterm-utils.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;; 1. Dependencies
;; ----------------------------------------
(require 'ecc-debug)


;; 2. Configuration
;; ----------------------------------------
(defcustom --ecc-vterm-yank-extension-patterns
  '(("py" . "^\\(import\\|from\\|def\\|class\\|if __name__ ==\\)")
    ("js"
     . "\\(function\\|const\\|let\\|var\\|=>\\|import\\|export\\)")
    ("html" . "\\(<html\\|<!DOCTYPE\\|<body\\|<div\\|<script\\)")
    ("css" . "\\([.#]?[a-zA-Z0-9_-]+\\s-*{\\)")
    ("el"
     . "\\((defun\\|(defvar\\|(defcustom\\|(require\\|(provide\\)")
    ("sh" . "\\(^#!.*sh\\|function\\s-+[a-zA-Z0-9_-]+\\s-*(\\)")
    ("txt" . ".*"))
  "Alist mapping file extensions to regex patterns for content detection."
  :type '(alist :key-type string :value-type regexp)
  :group 'ecc)


;; 3. Variables
;; ----------------------------------------
(defvar --ecc-vterm-yank-history nil
  "History of filenames used for yanking vterm content.")


;; 4. Main Entry Points
;; ----------------------------------------
(defun --ecc-vterm-utils-yank-region-to-file (start end filename)
  "Yank region between START and END to file named FILENAME."
  (interactive
   (if (region-active-p)
       (let* ((content (buffer-substring-no-properties
                        (region-beginning) (region-end)))
              (file-type (--ecc-vterm-utils-detect-file-type content))
              (default-name (format "claude_output.%s" file-type))
              (filename (read-file-name "Save to file: " nil nil nil
                                        default-name
                                        '--ecc-vterm-yank-history)))
         (list (region-beginning) (region-end) filename))
     (user-error "No active region")))
  (--ecc-debug-message "Yanking region (%d chars) to file"
                       (- end start))
  (let* ((content (buffer-substring-no-properties start end))
         (file-type (--ecc-vterm-utils-detect-file-type content))
         (filename (if (and filename (not (string-empty-p filename)))
                       filename
                     (format "claude_output_%s.%s"
                             (format-time-string "%Y%m%d_%H%M%S")
                             file-type)))
         (full-path (expand-file-name filename)))
    (--ecc-debug-message "Target file: %s" full-path)
    (when (and (file-exists-p full-path)
               (not
                (y-or-n-p
                 (format "File %s exists. Overwrite? " full-path))))
      (--ecc-debug-message "File write aborted by user")
      (user-error "Aborted"))
    (with-temp-file full-path
      (insert content))
    (--ecc-debug-message "Wrote %s (%d bytes)" full-path
                         (length content))
    (when (y-or-n-p "Open file in a new buffer? ")
      (--ecc-debug-message "Opening file in new buffer")
      (find-file full-path))
    full-path))

(defun --ecc-vterm-utils-yank-buffer-to-file (filename)
  "Yank entire vterm buffer content to file named FILENAME."
  (interactive
   (let*
       ((content
         (buffer-substring-no-properties (point-min) (point-max)))
        (file-type (--ecc-vterm-utils-detect-file-type content))
        (default-name (format "claude_buffer.%s" file-type))
        (filename (read-file-name "Save buffer to file: " nil nil
                                  nil
                                  default-name
                                  '--ecc-vterm-yank-history)))
     (list filename)))
  (--ecc-debug-message "Yanking entire buffer to file")
  (--ecc-vterm-utils-yank-region-to-file (point-min) (point-max)
                                         filename))

(defun --ecc-vterm-utils-quick-yank-region ()
  "Quickly yank active region to auto-named file based on content."
  (interactive)
  (if (region-active-p)
      (let* ((content (buffer-substring-no-properties
                       (region-beginning) (region-end)))
             (file-type (--ecc-vterm-utils-detect-file-type content))
             (filename (format "claude_output_%s.%s"
                               (format-time-string "%Y%m%d_%H%M%S")
                               file-type)))
        (--ecc-debug-message
         "Quick yanking region to auto-named file: %s" filename)
        (--ecc-vterm-utils-yank-region-to-file
         (region-beginning) (region-end) filename))
    (--ecc-debug-message "Quick yank failed: no active region")
    (user-error "No active region")))


;; 5. Core Functions
;; ----------------------------------------
(defun --ecc-vterm-utils-detect-file-type (content)
  "Detect file type based on CONTENT."
  (--ecc-debug-message "Detecting file type for content of length %d"
                       (length content))
  (catch 'found
    (dolist (entry --ecc-vterm-yank-extension-patterns)
      (when (string-match-p (cdr entry) content)
        (--ecc-debug-message "Detected file type: %s" (car entry))
        (throw 'found (car entry))))
    (--ecc-debug-message
     "No specific file type detected, defaulting to txt")
    "txt"))


;; 6. Helper/Utility Functions
;; ----------------------------------------
;; No helper functions in this file


(provide 'ecc-vterm-utils)

(when
    (not load-file-name)
  (message "ecc-vterm-utils.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))