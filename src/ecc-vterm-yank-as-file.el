;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 09:45:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-vterm-yank-as-file.el

;;; Commentary:
;;; Functions for yanking vterm content as a file with auto-detection
;;; of file type based on content.

(require 'ecc-variables)

;; Customization group
(defgroup ecc-vterm-yank nil
  "Settings for yanking vterm content to files."
  :group 'ecc
  :prefix "ecc-vterm-yank-")

(defcustom ecc-vterm-yank-default-dir nil
  "Default directory for saving yanked vterm content.
If nil, use the current directory."
  :type '(choice (const :tag "Current directory" nil)
                (directory :tag "Custom directory"))
  :group 'ecc-vterm-yank)

(defcustom ecc-vterm-yank-extension-alist
  '(("py" . "^\\(import\\|from\\|def\\|class\\|if __name__ ==\\)")
    ("js" . "\\(function\\|const\\|let\\|var\\|=>\\|import\\|export\\)")
    ("html" . "\\(<html\\|<!DOCTYPE\\|<body\\|<div\\|<script\\)")
    ("css" . "\\([.#]?[a-zA-Z0-9_-]+\\s-*{\\)")
    ("el" . "\\((defun\\|(defvar\\|(defcustom\\|(require\\|(provide\\)")
    ("sh" . "\\(^#!.*sh\\|function\\s-+[a-zA-Z0-9_-]+\\s-*(\\)")
    ("java" . "\\(public\\s-+\\(class\\|interface\\)\\|import\\s-+java\\)")
    ("c" . "\\(#include\\|int\\s-+main\\)")
    ("cpp" . "\\(#include\\|namespace\\|template\\|class\\s-+[a-zA-Z_][a-zA-Z0-9_]*\\)")
    ("rb" . "\\(require\\|def\\|class\\|module\\|gem\\)")
    ("php" . "\\(<\\?php\\|function\\s-+[a-zA-Z0-9_]+\\s-*(\\)")
    ("ts" . "\\(interface\\|type\\|class\\|enum\\|namespace\\|import\\)")
    ("go" . "\\(package\\|import\\|func\\|type\\s-+[A-Z]\\)")
    ("rs" . "\\(fn\\s-+[a-zA-Z0-9_]+\\|struct\\|impl\\|pub\\|use\\)")
    ("json" . "^\\s-*{\\s-*\"[^\"]+\"\\s-*:")
    ("md" . "\\(^#\\|\\[.*\\](.*)\\)")
    ("yaml" . "^[a-zA-Z0-9_-]+:\\s-")
    ("txt" . ".*"))
  "Alist mapping file extensions to regex patterns for content detection.
The regex patterns are used to detect the file type based on content.
The last entry should be a catch-all for plain text files."
  :type '(alist :key-type string :value-type regexp)
  :group 'ecc-vterm-yank)

(defvar ecc-vterm-yank-history nil
  "History of filenames used for yanking vterm content.")

(defun ecc-vterm-detect-file-type (content)
  "Detect the file type based on CONTENT.
Analyzes the provided content using regex patterns to determine the most
appropriate file type/extension. Iterates through `ecc-vterm-yank-extension-alist`
to find a matching pattern.

CONTENT is the text to analyze for file type detection.

Returns the appropriate file extension as a string (without the dot).
If no specific type is detected, defaults to "txt"."
  (catch 'found
    (dolist (entry ecc-vterm-yank-extension-alist)
      (when (string-match-p (cdr entry) content)
        (throw 'found (car entry))))
    "txt"))

(defun ecc-vterm-generate-file-from-region (start end filename)
  "Generate a file named FILENAME from the region between START and END.
Extracts content from the specified region, detects appropriate file type if needed,
and writes the content to the file. Directory is created if it doesn't exist.

START is the beginning position of the region.
END is the ending position of the region.
FILENAME is the name to use for the file. If nil or empty, auto-generates a name
based on content type and current timestamp.

When the file already exists, prompts for confirmation before overwriting.

Returns the full path to the created file."
  (let* ((content (buffer-substring-no-properties start end))
         (file-type (ecc-vterm-detect-file-type content))
         (dir (or ecc-vterm-yank-default-dir default-directory))
         (filename (if (and filename (not (string-empty-p filename)))
                      filename
                    (format "claude_output_%s.%s" 
                            (format-time-string "%Y%m%d_%H%M%S") 
                            file-type)))
         (full-path (expand-file-name filename dir)))
    
    ;; Create directory if it doesn't exist
    (unless (file-exists-p dir)
      (make-directory dir t))
    
    ;; Confirm overwrite if file exists
    (when (and (file-exists-p full-path)
               (not (y-or-n-p (format "File %s exists. Overwrite? " full-path))))
      (user-error "Aborted"))
    
    ;; Write the file
    (with-temp-file full-path
      (insert content))
    
    ;; Return the full path
    (ecc-debug-message "Wrote %s (%d bytes)" full-path (length content))
    full-path))


(defun ecc-vterm-yank-as-file (start end filename)
  "Yank the region between START and END to a file named FILENAME.
Extracts the selected region from the buffer and saves it to a file,
auto-detecting the appropriate file type based on content analysis.

START is the beginning position of the region.
END is the ending position of the region.
FILENAME is the target filename. When called interactively, prompts for this value.

When called interactively:
- Uses the currently active region
- Prompts for a filename with a default based on detected content type
- Offers to open the saved file in a new buffer

Auto-detects the file type based on content if no extension is provided.
  (interactive 
   (if (region-active-p)
       (let* ((content (buffer-substring-no-properties 
                       (region-beginning) (region-end)))
              (file-type (ecc-vterm-detect-file-type content))
              (dir (or ecc-vterm-yank-default-dir default-directory))
              (default-name (format "claude_output.%s" file-type))
              (filename (read-file-name "Save to file: " dir nil nil default-name
                                       'ecc-vterm-yank-history)))
         (list (region-beginning) (region-end) filename))
     (user-error "No active region")))
  
  ;; Generate the file
  (let ((file-path (ecc-vterm-generate-file-from-region start end filename)))
    
    ;; Optionally open the file in a buffer
    (when (and file-path 
               (y-or-n-p "Open file in a new buffer? "))
      (find-file file-path))))


(defun ecc-vterm-yank-buffer-as-file (filename)
  "Yank the entire vterm buffer content to a file named FILENAME.
Saves the complete buffer content to a file, auto-detecting the
appropriate file type based on content analysis.

FILENAME is the target filename. When called interactively, prompts for this value
with a default based on the detected content type.

When called interactively:
- Uses the entire buffer as content source
- Prompts for a filename with a smart default based on detected content type
- Offers to open the saved file in a new buffer

Auto-detects the file type based on content if no extension is provided.
  (interactive
   (let* ((content (buffer-substring-no-properties 
                    (point-min) (point-max)))
          (file-type (ecc-vterm-detect-file-type content))
          (dir (or ecc-vterm-yank-default-dir default-directory))
          (default-name (format "claude_buffer.%s" file-type))
          (filename (read-file-name "Save buffer to file: " dir nil nil default-name
                                   'ecc-vterm-yank-history)))
     (list filename)))
  
  ;; Generate the file
  (let ((file-path (ecc-vterm-generate-file-from-region 
                   (point-min) (point-max) filename)))
    
    ;; Optionally open the file in a buffer
    (when (and file-path 
               (y-or-n-p "Open file in a new buffer? "))
      (find-file file-path))))


(defun ecc-vterm-quick-yank-region ()
  "Quickly yank the active region to an auto-named file based on content.
Provides a streamlined workflow for rapidly saving selected text to files
with minimal interruption.

This function:
1. Extracts the currently selected region
2. Auto-detects the appropriate file type based on content
3. Generates a timestamped filename with the correct extension
4. Saves the content without prompting (for maximum speed)
5. Reports the saved filename

Uses auto-detection for file type and creates the file in the default directory
defined by `ecc-vterm-yank-default-dir`, or the current directory if not set.

Unlike `ecc-vterm-yank-as-file`, this function never prompts for input,
making it ideal for rapid extraction of multiple code snippets."
  (interactive)
  (if (region-active-p)
      (let* ((content (buffer-substring-no-properties 
                       (region-beginning) (region-end)))
             (file-type (ecc-vterm-detect-file-type content))
             (filename (format "claude_output_%s.%s" 
                              (format-time-string "%Y%m%d_%H%M%S") 
                              file-type)))
        (ecc-vterm-generate-file-from-region 
         (region-beginning) (region-end) filename))
    (user-error "No active region")))

(provide 'ecc-vterm-yank-as-file)

;;; ecc-vterm-yank-as-file.el ends here
