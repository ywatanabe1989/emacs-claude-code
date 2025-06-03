;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-06-04 08:47:07>
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

(defcustom --ecc-vterm-yank-as-file-prompt t
  "Whether to prompt before yanking as file."
  :type 'boolean
  :group 'ecc)

(defcustom --ecc-vterm-yank-as-file-message-format "See <%s>"
  "Format string for the message sent to Claude. %s is replaced with the file path."
  :type 'string
  :group 'ecc)

;; 3. Variables
;; ----------------------------------------

(defvar --ecc-vterm-yank-history nil
  "History of filenames used for yanking vterm content.")

(defcustom --ecc-vterm-yank-as-file-enabled nil
  "Whether to enable automatic yank-as-file prompt on vterm-yank."
  :type 'boolean
  :group 'ecc)

(defcustom --ecc-vterm-yank-as-file-threshold 100
  "Minimum character count to trigger yank-as-file behavior.
When yanking content in vterm that exceeds this character count
and is detected as code (not plain text), it will be saved to
a temporary file and a file reference will be sent to Claude
instead of the actual content."
  :type 'integer
  :group 'ecc)

(defcustom --ecc-vterm-yank-as-file-auto-threshold 500
  "Character count threshold for automatic yank-as-file without prompting.
When content length exceeds this threshold, it will always be
yanked as a file reference regardless of content type."
  :type 'integer
  :group 'ecc)

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

(defun --ecc-vterm-utils-cleanup-temp-files ()
  "Clean up old temporary files created by yank-as-file.
Removes temporary files older than 24 hours."
  (interactive)
  (let* ((temp-dir (temporary-file-directory))
         (pattern (concat temp-dir "claude_yank_*"))
         (current-time (float-time))
         (age-limit (* 24 60 60))  ; 24 hours in seconds
         (removed-count 0))
    (dolist (file (file-expand-wildcards pattern))
      (when (and (file-regular-p file)
                 (> (- current-time
                       (float-time (nth 5 (file-attributes file))))
                    age-limit))
        (delete-file file)
        (setq removed-count (1+ removed-count))
        (--ecc-debug-message "Removed old temp file: %s" file)))
    (message "Cleaned up %d old temporary file(s)" removed-count)))

(defun --ecc-vterm-utils-list-temp-files ()
  "List all temporary files created by yank-as-file."
  (interactive)
  (let* ((temp-dir (temporary-file-directory))
         (pattern (concat temp-dir "claude_yank_*"))
         (files (file-expand-wildcards pattern)))
    (if files
        (with-current-buffer (get-buffer-create "*Claude Temp Files*")
          (erase-buffer)
          (insert "Temporary files created by yank-as-file:\n\n")
          (dolist (file files)
            (insert (format "%s (%.1f KB)\n"
                            file
                            (/ (nth 7 (file-attributes file)) 1024.0))))
          (display-buffer (current-buffer)))
      (message "No temporary yank-as-file files found"))))

;; 7. Session Detection Functions
;; ----------------------------------------

(defun --ecc-vterm-utils-get-last-non-empty-line (&optional buffer)
  "Get the last non-empty line from vterm BUFFER.
If BUFFER is nil, use the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-max))
      (skip-chars-backward " \t\n\r")
      (let ((end (point)))
        (beginning-of-line)
        (buffer-substring-no-properties (point) end)))))

(defun --ecc-vterm-utils-is-claude-session-active (&optional buffer)
  "Check if a Claude session is active in vterm BUFFER.
Returns t if Claude prompt is detected, nil otherwise."
  (let
      ((last-line (--ecc-vterm-utils-get-last-non-empty-line buffer)))
    (--ecc-debug-message "Checking Claude session, last line: %S"
                         last-line)
    (cond
     ;; Check for Claude prompts - these patterns indicate an active session
     ((string-match-p "Human:" last-line) t)
     ((string-match-p "│[[:space:]]*>" last-line) t)
     ((string-match-p "continue>" last-line) t)
     ((string-match-p "\\[y/n\\]" last-line) t)
     ((string-match-p "\\[Y/n\\]" last-line) t)
     ((string-match-p "\\[Y/y/n\\]" last-line) t)
     ;; Check for Claude running indicator
     ((string-match-p "esc to interrupt" last-line) t)
     ;; Check for initial Claude prompt
     ((string-match-p "Try claude -h" last-line) t)
     (t nil))))

;; 8. Advice Functions
;; ----------------------------------------

(defun --ecc-vterm-utils-yank-advice (orig-fun &rest args)
  "Advice to save yanked content as file and send reference to Claude.
ORIG-FUN is the original yank function, ARGS are its arguments."
  (if (and --ecc-vterm-yank-as-file-enabled
           (derived-mode-p 'vterm-mode)
           (car kill-ring))
                                        ; Ensure there's something to
      yank
    ;; Our custom yank-as-file behavior
    (let* ((yanked-content (car kill-ring))
           (content-length (length yanked-content))
           (file-type
            (--ecc-vterm-utils-detect-file-type yanked-content)))
      (if (or
           ;; Auto yank as file if content is very long
           (>= content-length
               --ecc-vterm-yank-as-file-auto-threshold)
           ;; Or if it meets threshold and user confirms (or prompting disabled)
           (and
            (>= content-length --ecc-vterm-yank-as-file-threshold)
            (not (string= file-type "txt"))  ; Don't prompt for plain text
            (or (not --ecc-vterm-yank-as-file-prompt)
                (y-or-n-p (format
                           "Yank %s content (%d chars) as file? "
                           file-type content-length)))))
          ;; Yank as file
          (let* (;; Create temporary file
                 (temp-file (make-temp-file
                             (format "claude_yank_%s_"
                                     (format-time-string
                                      "%Y%m%d_%H%M%S"))
                             nil
                             (format ".%s" file-type))))
            ;; Write content to temporary file
            (with-temp-file temp-file
              (insert yanked-content))
            (--ecc-debug-message
             "Saved yanked content to temporary file: %s" temp-file)
            ;; Send message to Claude with file reference
            (let
                ((message
                  (format --ecc-vterm-yank-as-file-message-format
                          temp-file)))
              (if (fboundp 'vterm-send-string)
                  (progn
                    (vterm-send-string message)
                    (message "Sent to Claude: %s" message))
                (insert message))))
        ;; Normal yank
        (apply orig-fun args)))
    ;; Not in vterm or feature disabled, use normal yank
    (apply orig-fun args)))

(defun --ecc-vterm-utils-enable-yank-advice ()
  "Enable yank-as-file advice for vterm-yank."
  (interactive)
  (advice-add 'vterm-yank :around #'--ecc-vterm-utils-yank-advice)
  (setq --ecc-vterm-yank-as-file-enabled t)
  (message "Yank-as-file advice enabled for vterm"))

(defun --ecc-vterm-utils-disable-yank-advice ()
  "Disable yank-as-file advice for vterm-yank."
  (interactive)
  (advice-remove 'vterm-yank #'--ecc-vterm-utils-yank-advice)
  (setq --ecc-vterm-yank-as-file-enabled nil)
  (message "Yank-as-file advice disabled for vterm"))

(defun --ecc-vterm-utils-toggle-yank-advice ()
  "Toggle yank-as-file advice for vterm-yank."
  (interactive)
  (if --ecc-vterm-yank-as-file-enabled
      (--ecc-vterm-utils-disable-yank-advice)
    (--ecc-vterm-utils-enable-yank-advice)))

;; Also add advice for regular yank in vterm buffers

(defun --ecc-vterm-utils-regular-yank-advice (orig-fun &rest args)
  "Advice for regular yank command in vterm buffers.
ORIG-FUN is the original yank function, ARGS are its arguments."
  (if (and (derived-mode-p 'vterm-mode)
           (fboundp 'vterm-yank))
      ;; In vterm mode, use vterm-yank which will trigger our advice
      (call-interactively 'vterm-yank)
    ;; Otherwise, use the original yank
    (apply orig-fun args)))

(defun --ecc-vterm-utils-setup-yank-advice ()
  "Setup yank advice for both regular yank and vterm-yank."
  (interactive)
  ;; Add advice to regular yank to redirect to vterm-yank in vterm buffers
  (advice-add 'yank :around #'--ecc-vterm-utils-regular-yank-advice)
  (--ecc-vterm-utils-enable-yank-advice)
  (message "Yank-as-file advice setup complete"))


(provide 'ecc-vterm-utils)

(when
    (not load-file-name)
  (message "ecc-vterm-utils.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))