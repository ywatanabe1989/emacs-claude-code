;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 20:55:20>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/ecc-repository-view.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ecc-variables)
(require 'ecc-buffer-registry)
(require 'ecc-send)

;; 1. Configuration and customization
;; ----------------------------------------

(defgroup ecc-repository-view nil
  "Repository viewing and integration with Claude."
  :group 'emacs-claude
  :prefix "ecc-repo-view-")

(defcustom ecc-repo-view-script
  (expand-file-name "from_user/view_repo.sh" 
                    (file-name-directory (locate-library "emacs-claude-code")))
  "Path to the view_repo.sh script."
  :type 'string
  :group 'ecc-repository-view)

(defcustom ecc-repo-view-output-file
  "viewed.md"
  "Default output file for repository view."
  :type 'string
  :group 'ecc-repository-view)

(defcustom ecc-repo-view-depth 3
  "Default maximum depth for repository tree view."
  :type 'integer
  :group 'ecc-repository-view)

(defcustom ecc-repo-view-n-head 50
  "Default number of lines to show per file."
  :type 'integer
  :group 'ecc-repository-view)

;; 2. Core functionality
;; ----------------------------------------

(defun ecc-repo-view-path ()
  "Get the most appropriate path for repository viewing.
Tries to find a git repository root, or falls back to current directory."
  (cond
   ;; Try to use magit if available
   ((fboundp 'magit-toplevel) 
    (or (magit-toplevel) default-directory))
   ;; Check if we're in a git repo manually
   ((locate-dominating-file default-directory ".git"))
   ;; Fall back to current directory
   (t default-directory)))

(defun ecc-repo-view-generate (dir &optional depth n-head output-file)
  "Generate a repository view for DIR using view_repo.sh.
DEPTH is the maximum directory depth to show in tree view.
N-HEAD is the number of lines to show per file.
OUTPUT-FILE is the file to write the result to."
  (interactive
   (list (read-directory-name "Repository directory: " (ecc-repo-view-path))
         (read-number "Maximum depth: " ecc-repo-view-depth)
         (read-number "Number of lines per file: " ecc-repo-view-n-head)
         (read-file-name "Output file: " nil nil nil ecc-repo-view-output-file)))
  
  ;; Set defaults if not specified
  (setq depth (or depth ecc-repo-view-depth))
  (setq n-head (or n-head ecc-repo-view-n-head))
  (setq output-file (or output-file 
                       (expand-file-name ecc-repo-view-output-file dir)))
  
  ;; Ensure script exists and is executable
  (unless (and (file-exists-p ecc-repo-view-script)
               (file-executable-p ecc-repo-view-script))
    (error "Repository view script not found or not executable: %s" 
           ecc-repo-view-script))
  
  ;; Run the script
  (message "Generating repository view for %s..." dir)
  (let ((default-directory (file-name-as-directory dir))
        (command (format "%s --path %s --depth %d --n-head %d --output %s"
                        (shell-quote-argument ecc-repo-view-script)
                        (shell-quote-argument dir)
                        depth
                        n-head
                        (shell-quote-argument output-file))))
    (with-temp-buffer
      (unless (= 0 (call-process-shell-command command nil t))
        (error "Failed to generate repository view: %s" 
               (buffer-string))))
    
    (message "Repository view generated in %s" output-file)
    output-file))

(defun ecc-repo-view-read-file (file)
  "Read the contents of FILE into a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

;; 3. Integration with Claude
;; ----------------------------------------

(defun ecc-repo-view-and-send (dir &optional depth n-head)
  "Generate repository view for DIR and send to Claude.
DEPTH is the maximum directory depth to show in tree view.
N-HEAD is the number of lines to show per file."
  (interactive
   (list (read-directory-name "Repository directory: " (ecc-repo-view-path))
         (read-number "Maximum depth: " ecc-repo-view-depth)
         (read-number "Number of lines per file: " ecc-repo-view-n-head)))
  
  ;; First generate the repository view
  (let* ((temp-output-file (make-temp-file "ecc-repo-view-" nil ".md"))
         (output-file (ecc-repo-view-generate dir depth n-head temp-output-file))
         (content (ecc-repo-view-read-file output-file))
         (claude-buffer (ecc-buffer-current-get-buffer)))
    
    ;; Check if we have an active Claude buffer
    (unless claude-buffer
      (error "No active Claude buffer found"))
    
    ;; Send to Claude
    (with-current-buffer claude-buffer
      (goto-char (point-max))
      (if (> (length content) 100000)
          (progn
            (message "Repository content is too large (%d chars), sending first 100k chars" 
                     (length content))
            (--ecc-send-string (substring content 0 100000) t 0.5))
        (--ecc-send-string content t 0.5)))
    
    ;; Clean up temporary file if we used one
    (when (string-prefix-p (temporary-file-directory) output-file)
      (delete-file output-file))
    
    (message "Repository view sent to Claude")))

(defun ecc-repo-view-section-and-send (dir section-pattern &optional depth n-head)
  "Generate repository view for DIR, extract SECTION-PATTERN and send to Claude.
SECTION-PATTERN is a regex to match section headers.
DEPTH is the maximum directory depth to show in tree view.
N-HEAD is the number of lines to show per file."
  (interactive
   (list (read-directory-name "Repository directory: " (ecc-repo-view-path))
         (read-string "Section pattern (regex): " "## File contents")
         (read-number "Maximum depth: " ecc-repo-view-depth)
         (read-number "Number of lines per file: " ecc-repo-view-n-head)))
  
  ;; First generate the repository view
  (let* ((temp-output-file (make-temp-file "ecc-repo-view-" nil ".md"))
         (output-file (ecc-repo-view-generate dir depth n-head temp-output-file))
         (full-content (ecc-repo-view-read-file output-file))
         (claude-buffer (ecc-buffer-current-get-buffer))
         content)
    
    ;; Extract section
    (with-temp-buffer
      (insert full-content)
      (goto-char (point-min))
      (if (re-search-forward section-pattern nil t)
          (progn
            (beginning-of-line)
            (setq content (buffer-substring (point) (point-max))))
        (setq content full-content)))
    
    ;; Check if we have an active Claude buffer
    (unless claude-buffer
      (error "No active Claude buffer found"))
    
    ;; Send to Claude
    (with-current-buffer claude-buffer
      (goto-char (point-max))
      (if (> (length content) 100000)
          (progn
            (message "Repository content is too large (%d chars), sending first 100k chars" 
                     (length content))
            (--ecc-send-string (substring content 0 100000) t 0.5))
        (--ecc-send-string content t 0.5)))
    
    ;; Clean up temporary file if we used one
    (when (string-prefix-p (temporary-file-directory) output-file)
      (delete-file output-file))
    
    (message "Repository section view sent to Claude")))

;; 4. Dired integration
;; ----------------------------------------

(defun ecc-repo-view-dired-dwim ()
  "Generate repository view based on current Dired context."
  (interactive)
  (let ((dir (if (derived-mode-p 'dired-mode)
                (or (dired-get-marked-files nil nil nil t)
                    (list default-directory))
              (list default-directory))))
    
    ;; If multiple directories are marked, use the first one
    (setq dir (if (listp dir) (car dir) dir))
    
    ;; If it's a file, use its directory
    (when (file-regular-p dir)
      (setq dir (file-name-directory dir)))
    
    ;; Generate the view
    (call-interactively 'ecc-repo-view-and-send)))

(provide 'ecc-repository-view)

(when (not load-file-name)
  (message "ecc-repository-view.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))