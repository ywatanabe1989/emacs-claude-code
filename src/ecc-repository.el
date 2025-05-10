;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-07 15:11:52>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/ecc-repository.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ecc-variables)
(declare-function magit-toplevel "ext:magit-git")

;; 1. Configuration variables
;; ----------------------------------------

(defcustom ecc-repository-dir nil
  "Default directory for repository operations."
  :type 'string
  :group 'emacs-claude)

(defcustom ecc-repository-output-file
  "./docs/REPOSITORY_CONCATENATED.md"
  "Output file path for repository content relative to repo root."
  :type 'string
  :group 'emacs-claude)

(defcustom ecc-repository-file-blacklist
  '("\\.git" "\\.png$" "\\.jpg$" "\\.jpeg$" "\\.gif$" "\\.pdf$"
    "\\.zip$" "\\.tar$" "\\.gz$" "\\.mp4$" "\\.mp3$")
  "Regex patterns for files to exclude when copying repository."
  :type '(repeat string)
  :group 'emacs-claude)

(defcustom ecc-repository-max-file-size 100000
  "Maximum file size in bytes to include when copying repository."
  :type 'integer
  :group 'emacs-claude)

;; 2. Main user interface function
;; ----------------------------------------

(defun ecc-repository-copy-contents (dir)
  "Write repository structure from DIR to output file and clipboard."
  (interactive
   (list (or ecc-repository-dir
             (read-directory-name "Repository directory: "))))
  (let ((file-list (ecc-get-repository-files dir))
        (content "")
        (output-path
         (expand-file-name ecc-repository-output-file dir)))
    (setq ecc-repository-dir dir)
    (
     setq content (concat "# Repository Structure\n\n"))
    (dolist (file file-list)
      (let ((relative-path (file-relative-name file dir)))
        (setq content
              (concat content
                      "\n\n## " relative-path "\n\n```"
                      (ecc-get-file-type file)
                      "\n"
                      (ecc-repository-get-file-content file)
                      "\n```\n"))))
    (unless (file-exists-p (file-name-directory output-path))
      (make-directory (file-name-directory output-path) t))
    (with-temp-file output-path
      (insert content))
    (kill-new content)
    (message
     "Repository written to %s and copied to clipboard: %d files."
     output-path (length file-list))
    output-path))

;; 3. Helper functions
;; ----------------------------------------

(defun ecc-get-repository-files (dir)
  "Get list of files in DIR to include in repository copy.
Filter out blacklisted files and large files."
  (let ((result nil))
    (dolist (file (directory-files-recursively dir ".*" t))
      (when (and (file-regular-p file)
                 (not (ecc-repository-blacklisted-p file))
                 (<= (file-attribute-size (file-attributes file))
                     ecc-repository-max-file-size))
        (push file result)))
    result))

(defun ecc-repository-blacklisted-p (file)
  "Return t if FILE matches any pattern in blacklist."
  (let ((relative-name (file-name-nondirectory file)))
    (cl-some (lambda (pattern)
               (string-match-p pattern file))
             ecc-repository-file-blacklist)))

(defun ecc-repository-get-file-content (file)
  "Get content of FILE as string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun ecc-get-file-type (file)
  "Get file type for syntax highlighting based on file extension."
  (let ((ext (file-name-extension file)))
    (cond
     ((member ext '("el" "lisp")) "elisp")
     ((member ext '("py")) "python")
     ((member ext '("js")) "javascript")
     ((member ext '("ts")) "typescript")
     ((member ext '("sh" "bash")) "bash")
     ((member ext '("c" "h" "cpp" "hpp" "cc")) "c")
     ((member ext '("rb")) "ruby")
     ((member ext '("go")) "go")
     ((member ext '("java")) "java")
     ((member ext '("php")) "php")
     ((member ext '("html" "htm")) "html")
     ((member ext '("css")) "css")
     ((member ext '("json")) "json")
     ((member ext '("md" "markdown")) "markdown")
     ((member ext '("yml" "yaml")) "yaml")
     ((member ext '("xml")) "xml")
     ((member ext '("rs")) "rust")
     ((member ext '("swift")) "swift")
     ((member ext '("kt")) "kotlin")
     ((member ext '("sql")) "sql")
     (t ext))))


(provide 'ecc-repository)

(when
    (not load-file-name)
  (message "ecc-repository.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))