;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 21:55:10>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-template-cache.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)

;; Declare functions from the module we're going to test
(declare-function ecc-template-cache-init "ecc-template-cache")
(declare-function ecc-template-cache-get "ecc-template-cache")
(declare-function ecc-template-cache-put "ecc-template-cache")
(declare-function ecc-template-cache-clear "ecc-template-cache")
(declare-function ecc-template-cache-purge-outdated "ecc-template-cache")
(declare-function ecc-template-cache-file-hash "ecc-template-cache")
(declare-function ecc-template-cache-get-stats "ecc-template-cache")

;; Test for module loading
(ert-deftest test-ecc-template-cache-loadable ()
  "Test that ecc-template-cache.el can be loaded."
  (let ((file-path "/home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-template/ecc-template-cache.el")
        (was-loaded (featurep 'ecc-template-cache)))
    (should (file-exists-p file-path))
    (condition-case nil
        (require 'ecc-template-cache)
      (error nil))
    (should (featurep 'ecc-template-cache))))

;; Test cache initialization
(ert-deftest test-ecc-template-cache-init ()
  "Test that the template cache can be initialized."
  (require 'ecc-template-cache)
  (let ((cache (ecc-template-cache-init)))
    (should (hash-table-p cache))
    (should (= (hash-table-count cache) 0))))

;; Test file hashing function
(ert-deftest test-ecc-template-cache-file-hash ()
  "Test that file hashing works consistently."
  (require 'ecc-template-cache)
  
  ;; Create a temporary file with known content
  (let ((temp-file (make-temp-file "ecc-test-template-"))
        (content "This is a test template content."))
    
    ;; Write test content to file
    (with-temp-file temp-file
      (insert content))
    
    ;; Test hash consistency
    (let ((hash1 (ecc-template-cache-file-hash temp-file))
          (hash2 (ecc-template-cache-file-hash temp-file)))
      ;; Verify hash is a string
      (should (stringp hash1))
      ;; Verify hash is non-empty
      (should (> (length hash1) 0))
      ;; Verify hash is consistent
      (should (string= hash1 hash2)))
    
    ;; Modify the file and verify hash changes
    (let ((original-hash (ecc-template-cache-file-hash temp-file)))
      (with-temp-file temp-file
        (insert content)
        (insert "\nModified content."))
      
      (let ((modified-hash (ecc-template-cache-file-hash temp-file)))
        (should (stringp modified-hash))
        (should (> (length modified-hash) 0))
        ;; Hash should be different with different content
        (should-not (string= original-hash modified-hash))))
    
    ;; Clean up
    (delete-file temp-file)))

;; Test cache put and get operations
(ert-deftest test-ecc-template-cache-put-get ()
  "Test adding and retrieving items from the template cache."
  (require 'ecc-template-cache)
  
  ;; Create a temporary file
  (let ((temp-file (make-temp-file "ecc-test-template-"))
        (content "Test template content for caching."))
    
    ;; Write content to file
    (with-temp-file temp-file
      (insert content))
    
    ;; Initialize cache
    (ecc-template-cache-init)
    
    ;; Calculate hash
    (let ((file-hash (ecc-template-cache-file-hash temp-file))
          (template-name "TestTemplate"))
      
      ;; Add to cache
      (ecc-template-cache-put template-name temp-file content file-hash)
      
      ;; Verify retrieval
      (let ((cached-item (ecc-template-cache-get template-name)))
        (should cached-item)
        (should (string= (plist-get cached-item :content) content))
        (should (string= (plist-get cached-item :file) temp-file))
        (should (string= (plist-get cached-item :hash) file-hash))
        ;; Timestamp should be a number (time-t)
        (should (numberp (plist-get cached-item :timestamp)))))
    
    ;; Clean up
    (delete-file temp-file)
    (ecc-template-cache-clear)))

;; Test cache clearing
(ert-deftest test-ecc-template-cache-clear ()
  "Test clearing the template cache."
  (require 'ecc-template-cache)
  
  ;; Initialize cache and add an item
  (ecc-template-cache-init)
  (ecc-template-cache-put "TestTemplate" "/tmp/test.tpl" "content" "hash123")
  
  ;; Verify item is in cache
  (should (ecc-template-cache-get "TestTemplate"))
  
  ;; Clear cache
  (ecc-template-cache-clear)
  
  ;; Verify cache is empty
  (should-not (ecc-template-cache-get "TestTemplate")))

;; Test outdated cache purging
(ert-deftest test-ecc-template-cache-purge-outdated ()
  "Test purging outdated items from the template cache."
  (require 'ecc-template-cache)
  
  ;; Initialize cache
  (ecc-template-cache-init)
  
  ;; Create two temporary files
  (let ((temp-file1 (make-temp-file "ecc-test-template-"))
        (temp-file2 (make-temp-file "ecc-test-template-"))
        (content1 "Test content 1")
        (content2 "Test content 2"))
    
    ;; Write to files
    (with-temp-file temp-file1 (insert content1))
    (with-temp-file temp-file2 (insert content2))
    
    ;; Add to cache
    (let ((hash1 (ecc-template-cache-file-hash temp-file1))
          (hash2 (ecc-template-cache-file-hash temp-file2)))
      
      (ecc-template-cache-put "Template1" temp-file1 content1 hash1)
      (ecc-template-cache-put "Template2" temp-file2 content2 hash2)
      
      ;; Modify a file to make hash mismatch
      (with-temp-file temp-file1
        (insert content1)
        (insert "\nModified content."))
      
      ;; Now purge outdated entries
      (let ((purged (ecc-template-cache-purge-outdated)))
        ;; Should purge the modified file
        (should (= purged 1))
        
        ;; Template1 should be gone since it's outdated
        (should-not (ecc-template-cache-get "Template1"))
        
        ;; Template2 should still be there
        (should (ecc-template-cache-get "Template2"))))
    
    ;; Clean up
    (delete-file temp-file1)
    (delete-file temp-file2)
    (ecc-template-cache-clear)))

;; Test cache statistics
(ert-deftest test-ecc-template-cache-stats ()
  "Test getting cache statistics."
  (require 'ecc-template-cache)
  
  ;; Initialize cache
  (ecc-template-cache-init)
  (ecc-template-cache-clear)
  
  ;; Add some items
  (ecc-template-cache-put "Template1" "/tmp/test1.tpl" "content1" "hash1")
  (ecc-template-cache-put "Template2" "/tmp/test2.tpl" "content2" "hash2")
  (ecc-template-cache-put "Template3" "/tmp/test3.tpl" "content3" "hash3")
  
  ;; Get stats
  (let ((stats (ecc-template-cache-get-stats)))
    (should (= (plist-get stats :count) 3))
    (should (numberp (plist-get stats :memory)))
    (should (numberp (plist-get stats :hits)))
    (should (numberp (plist-get stats :misses)))))

(provide 'test-ecc-template-cache)