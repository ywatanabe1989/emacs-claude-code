;;; test-load-helpers.el --- Helper functions for test loading -*- lexical-binding: t -*-
;;; Commentary:
;; This file provides helper functions to set up the load path for tests.

;;; Code:

(defun ecc-test-add-all-paths ()
  "Add all necessary paths for testing.
Returns the paths added for debugging purposes."
  (let* ((this-file (or load-file-name buffer-file-name))
         (tests-dir (if this-file
                        (file-name-directory this-file)
                      (expand-file-name "./tests" default-directory)))
         (project-root (if this-file
                           (directory-file-name (file-name-directory (directory-file-name tests-dir)))
                         (expand-file-name "." default-directory)))
         (src-dir (expand-file-name "src" project-root))
         (load-paths nil))
    
    ;; Add tests directory and its subdirectories
    (push tests-dir load-paths)
    (message "Added package root to load-path: %s" tests-dir)
    (add-to-list 'load-path tests-dir)
    
    ;; Add subdirectories of tests
    (dolist (dir (directory-files tests-dir t "\\`[^.]"))
      (when (file-directory-p dir)
        (message "Added subdir to load-path: %s" dir)
        (add-to-list 'load-path dir)
        (push dir load-paths)))
    
    ;; Add project root
    (add-to-list 'load-path project-root)
    (push project-root load-paths)
    
    ;; Add src directory
    (add-to-list 'load-path src-dir)
    (push src-dir load-paths)
    
    ;; Add src subdirectories 
    (when (file-directory-p src-dir)
      (dolist (dir (directory-files src-dir t "\\`[^.]"))
        (when (file-directory-p dir)
          (add-to-list 'load-path dir)
          (push dir load-paths))))
    
    ;; Return the paths for debugging
    (nreverse load-paths)))

;; Run the function to set up load paths
(ecc-test-add-all-paths)

(defun ecc-test-list-paths ()
  "List all load paths for debugging."
  (message "Load paths: %S" load-path))

(defun ecc-test-setup-feature-compatibility ()
  "Setup feature compatibility for tests.
This allows tests to use ecc-buffer/module-name format while source
files use the standard Elisp naming convention."
  
  ;; Define a function to create feature aliases
  (defun --create-feature-alias (standard-name prefixed-name)
    "Create an alias from STANDARD-NAME to PREFIXED-NAME."
    (when (featurep standard-name)
      (provide prefixed-name)))
  
  ;; Define a function to automatically translate module names
  (defun --ensure-feature-aliases (module-list prefix)
    "Create aliases for all modules in MODULE-LIST with PREFIX."
    (dolist (module module-list)
      (let ((prefixed-name (intern (format "%s/%s" prefix module))))
        (--create-feature-alias module prefixed-name))))
  
  ;; Buffer modules
  (--ensure-feature-aliases
   '(ecc-buffer
     ecc-buffer-variables
     ecc-buffer-verification
     ecc-buffer-registry
     ecc-buffer-current
     ecc-buffer-state
     ecc-buffer-stale
     ecc-buffer-navigation
     ecc-buffer-timestamp
     ecc-buffer-auto-switch)
   'ecc-buffer)
  
  ;; State modules
  (--ensure-feature-aliases
   '(ecc-state
     ecc-state-detect)
   'ecc-state)
  
  ;; Template modules
  (--ensure-feature-aliases
   '(ecc-template
     ecc-template-cache
     ecc-template-mode)
   'ecc-template)
  
  ;; Term modules
  (--ensure-feature-aliases
   '(ecc-term
     ecc-claude-vterm-mode)
   'ecc-term))

;; Setup feature compatibility after loading modules
(add-hook 'after-load-functions
          (lambda (&rest _)
            (ecc-test-setup-feature-compatibility)))

(provide 'test-load-helpers)
;;; test-load-helpers.el ends here