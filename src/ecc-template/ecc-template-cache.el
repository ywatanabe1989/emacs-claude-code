;;; -*- coding: utf-8; lexical-binding: t -*-
;;; ecc-template-cache.el --- Template caching for emacs-claude-code

;;; Commentary:
;; This module provides caching for templates to improve performance
;; by avoiding repeated file reads and processing of unchanged templates.
;; Each template is cached with its content, file path, and a hash of
;; the file contents to detect modifications.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar --ecc-template-cache nil
  "Hash table storing the template cache.")

(defvar --ecc-template-cache-hits 0
  "Counter for cache hits.")

(defvar --ecc-template-cache-misses 0
  "Counter for cache misses.")

(defun ecc-template-cache-init ()
  "Initialize or reset the template cache.
Returns the cache hash table."
  (setq --ecc-template-cache (make-hash-table :test 'equal))
  --ecc-template-cache)

(defun ecc-template-cache-file-hash (file-path)
  "Calculate a hash for the contents of FILE-PATH.
Returns a string containing the hash value, or nil if the file
doesn't exist or can't be read."
  (when (and file-path (file-exists-p file-path))
    (with-temp-buffer
      (insert-file-contents file-path)
      (secure-hash 'sha256 (buffer-string)))))

(defun ecc-template-cache-get-file-path (template-name)
  "Get the file path for TEMPLATE-NAME.
Checks for files in both old and new locations for compatibility."
  (let* ((root-dir (if load-file-name
                      (file-name-directory (file-truename load-file-name))
                    default-directory))
         (project-root (expand-file-name "../.." root-dir))
         (paths (list 
                 ;; Original templates path
                 (expand-file-name (format "templates/%s.md" template-name) project-root)
                 ;; Claude subdir path
                 (expand-file-name (format "templates/claude/%s.md" template-name) project-root)
                 ;; Genai subdir path
                 (expand-file-name (format "templates/genai/%s.md" template-name) project-root)
                 ;; New path in ecc-template
                 (expand-file-name (format "src/ecc-template/templates/%s.md" template-name) project-root))))
    (cl-some (lambda (path) (when (file-exists-p path) path)) paths)))

(defun ecc-template-cache-put (template-name file-path content hash)
  "Add a template to the cache.
TEMPLATE-NAME is the template identifier.
FILE-PATH is the path to the template file.
CONTENT is the template content.
HASH is the file content hash for future validation."
  (unless --ecc-template-cache
    (ecc-template-cache-init))
  
  (let ((cache-item (list :file file-path
                          :content content
                          :hash hash
                          :timestamp (float-time))))
    (puthash template-name cache-item --ecc-template-cache)
    cache-item))

(defun ecc-template-cache-get (template-name)
  "Get a template from the cache by TEMPLATE-NAME.
Returns the cached item as a plist or nil if not found."
  (unless --ecc-template-cache
    (ecc-template-cache-init))
  
  (let ((cache-item (gethash template-name --ecc-template-cache)))
    (if cache-item
        (progn
          (cl-incf --ecc-template-cache-hits)
          cache-item)
      (cl-incf --ecc-template-cache-misses)
      nil)))

(defun ecc-template-cache-clear ()
  "Clear all items from the template cache."
  (when --ecc-template-cache
    (clrhash --ecc-template-cache))
  (setq --ecc-template-cache-hits 0
        --ecc-template-cache-misses 0)
  nil)

(defun ecc-template-cache-load (template-name)
  "Load a template from file or from cache.
If the template is in the cache and still valid, returns it directly.
Otherwise, reads the template file and updates the cache."
  (let* ((cache-entry (ecc-template-cache-get template-name))
         (file-path (or (and cache-entry (plist-get cache-entry :file))
                        (ecc-template-cache-get-file-path template-name))))
    
    (if (and file-path (file-exists-p file-path))
        (let ((current-hash (ecc-template-cache-file-hash file-path)))
          (if (and cache-entry 
                   (string= (plist-get cache-entry :hash) current-hash))
              ;; Cache hit with valid hash
              (plist-get cache-entry :content)
            
            ;; Cache miss or outdated hash - load from file
            (with-temp-buffer
              (insert-file-contents file-path)
              (let ((content (buffer-string)))
                (ecc-template-cache-put template-name file-path content current-hash)
                content))))
      
      ;; File not found
      (message "Template %s not found" template-name)
      nil)))

(defun ecc-template-cache-purge-outdated ()
  "Purge cache entries whose file contents have changed.
Returns the number of entries purged."
  (unless --ecc-template-cache
    (ecc-template-cache-init)
    (message "Cache initialized")
    (cl-return-from ecc-template-cache-purge-outdated 0))
  
  (let ((purge-count 0)
        (outdated-keys '()))
    ;; First pass: identify outdated entries
    (maphash
     (lambda (key item)
       (let* ((file-path (plist-get item :file))  
              (cached-hash (plist-get item :hash))
              (current-hash (ecc-template-cache-file-hash file-path)))
         ;; If the file no longer exists or its hash has changed, mark for purging
         (when (or (not current-hash)
                   (not (string= cached-hash current-hash)))
           (push key outdated-keys))))
     --ecc-template-cache)
    
    ;; Second pass: remove outdated entries
    (dolist (key outdated-keys)
      (remhash key --ecc-template-cache)
      (cl-incf purge-count))
    
    purge-count))

(defun ecc-template-cache-stats ()
  "Get statistics about the template cache.
Returns a string with a human-readable summary."
  (unless --ecc-template-cache
    (ecc-template-cache-init))
  
  (let* ((item-count (hash-table-count --ecc-template-cache))
         ;; Estimate memory usage: count chars in all cached content
         (memory-usage 
          (let ((total 0))
            (maphash (lambda (_key item)
                       (when-let ((content (plist-get item :content)))
                         (cl-incf total (length content))))
                     --ecc-template-cache)
            total)))
    
    (format "Template cache: %d items, ~%d bytes, %d hits, %d misses"
            item-count memory-usage 
            --ecc-template-cache-hits --ecc-template-cache-misses)))

(provide 'ecc-template-cache)
(provide 'ecc-template/ecc-template-cache)
;;; ecc-template-cache.el ends here