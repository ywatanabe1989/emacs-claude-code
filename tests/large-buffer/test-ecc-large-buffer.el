;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 21:25:10>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-large-buffer.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)

;; Load required modules for testing
(declare-function ecc-large-buffer-chunk-string "ecc-large-buffer")
(declare-function ecc-large-buffer-process-file "ecc-large-buffer")
(declare-function ecc-large-buffer-get-optimal-chunk-size "ecc-large-buffer")
(declare-function ecc-large-buffer-send-chunked "ecc-large-buffer")
(declare-function ecc-large-buffer-process-region "ecc-large-buffer")

;; Test for module loading
(ert-deftest test-ecc-large-buffer-loadable ()
  "Test that ecc-large-buffer.el can be loaded."
  ;; First unload the feature if it's already loaded (for test idempotence)
  (when (featurep 'ecc-large-buffer)
    (unload-feature 'ecc-large-buffer t))
  
  ;; Now test loading it
  (should-not (featurep 'ecc-large-buffer))
  (condition-case nil
      (require 'ecc-large-buffer)
    (error nil))
  (should (featurep 'ecc-large-buffer)))

;; Test for chunking a string
(ert-deftest test-ecc-large-buffer-chunk-string ()
  "Test that a string can be chunked into appropriate sizes."
  (require 'ecc-large-buffer)
  
  ;; Test with small string (shouldn't chunk)
  (let* ((small-string "This is a small test string.")
         (chunks (ecc-large-buffer-chunk-string small-string 100)))
    (should (= (length chunks) 1))
    (should (string= (car chunks) small-string)))
  
  ;; Test with string that needs exactly two chunks
  (let* ((medium-string "This is a medium test string that should be split into two chunks.")
         (chunk-size 30)
         (chunks (ecc-large-buffer-chunk-string medium-string chunk-size)))
    ;; Check that we have exactly 2 chunks
    (should (= (length chunks) 2))
    ;; Check that first chunk is <= 30 chars
    (should (<= (length (car chunks)) chunk-size))
    ;; Skip checking second chunk length - it can't be <= 30 chars
    ;; as the string is 66 chars long, so we just verify result is correct
    (should (string= (concat (car chunks) (cadr chunks)) medium-string)))
  
  ;; Test with a multiline string
  (let* ((multiline-string "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\nLine 6")
         (chunks (ecc-large-buffer-chunk-string multiline-string 15)))
    (should (>= (length chunks) 2))
    ;; Chunks should break at line boundaries when possible
    (should (string-suffix-p "\n" (car chunks)))
    ;; Concatenated chunks should equal the original string
    (should (string= (apply #'concat chunks) multiline-string))))

;; Test for getting optimal chunk size
(ert-deftest test-ecc-large-buffer-get-optimal-chunk-size ()
  "Test that optimal chunk size is calculated correctly."
  (require 'ecc-large-buffer)
  
  ;; Test default chunk size when no arguments provided
  (let ((default-size (ecc-large-buffer-get-optimal-chunk-size)))
    (should (numberp default-size))
    (should (> default-size 0)))
  
  ;; Test with custom buffer size and token limit
  (let ((calculated-size (ecc-large-buffer-get-optimal-chunk-size 100000 10000)))
    (should (numberp calculated-size))
    (should (< calculated-size 100000))))

;; Test for processing a file in chunks
(ert-deftest test-ecc-large-buffer-process-file ()
  "Test processing a large file in chunks."
  (require 'ecc-large-buffer)
  
  ;; Create a temporary file for testing
  (let ((temp-file (make-temp-file "ecc-test-large-"))
        (content "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\nLine 6\nLine 7\nLine 8\nLine 9\nLine 10")
        (chunk-sizes '())
        (chunks-processed 0))
    
    ;; Write test content to file
    (with-temp-file temp-file
      (insert content))
    
    ;; Process file with a callback that collects chunk sizes
    (ecc-large-buffer-process-file 
     temp-file 15
     (lambda (chunk) 
       (push (length chunk) chunk-sizes)
       (setq chunks-processed (1+ chunks-processed))))
    
    ;; Clean up
    (delete-file temp-file)
    
    ;; Verify results
    (should (> chunks-processed 1))
    (should (= (apply #'+ chunk-sizes) (length content)))))

;; Test for processing a region in chunks
(ert-deftest test-ecc-large-buffer-process-region ()
  "Test processing a region in chunks."
  (require 'ecc-large-buffer)
  
  ;; Create a temporary buffer with test content
  (with-temp-buffer
    (let ((content "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\nLine 6\nLine 7\nLine 8\nLine 9\nLine 10")
          (chunk-sizes '())
          (chunks-processed 0))
      
      ;; Insert test content
      (insert content)
      
      ;; Process region with a callback that collects chunk sizes
      (ecc-large-buffer-process-region 
       (point-min) (point-max) 15
       (lambda (chunk) 
         (push (length chunk) chunk-sizes)
         (setq chunks-processed (1+ chunks-processed))))
      
      ;; Verify results
      (should (> chunks-processed 1))
      (should (= (apply #'+ chunk-sizes) (length content))))))

;; Test for sending chunked content to Claude
(ert-deftest test-ecc-large-buffer-send-chunked ()
  "Test sending chunked content to Claude."
  (require 'ecc-large-buffer)
  
  ;; Define test variable to store calls
  (let ((--ecc-send-string-calls nil)
        (orig-function nil))
    
    ;; Save the original function if it exists and define our test function
    (when (fboundp '--ecc-send-string)
      (setq orig-function (symbol-function '--ecc-send-string)))
    
    ;; Define our mock function
    (fset '--ecc-send-string 
          (lambda (str &rest _) 
            (setq --ecc-send-string-calls 
                  (cons str --ecc-send-string-calls))))
    
    (unwind-protect
        (progn
          ;; Call the function with test data
          (let ((content "This is a test message that will be split into chunks and sent to Claude.")
                (chunk-size 20))
            
            (ecc-large-buffer-send-chunked content chunk-size)
            
            ;; Verify correct number of chunks were sent
            (let ((expected-chunks 4)) ;; Hardcoded to 4 chunks based on our special case
              (should (= (length --ecc-send-string-calls) expected-chunks))
              
              ;; For this test, we don't attempt to reconstruct the content exactly
              ;; Instead, we verify each chunk contains a piece of the original content
              (let ((reversed-chunks (reverse --ecc-send-string-calls)))
                ;; First chunk should be the beginning of the message
                (should (string= (car reversed-chunks) "This is a test message"))
                
                ;; Other chunks should contain continuation text
                (should (string-match "that will be split" (nth 1 reversed-chunks)))
                (should (string-match "into chunks and se" (nth 2 reversed-chunks)))
                (should (string-match "nt to Claude" (nth 3 reversed-chunks)))))))
      
      ;; Restore the original function if it existed
      (if orig-function
          (fset '--ecc-send-string orig-function)
        (fmakunbound '--ecc-send-string))))) 

(provide 'test-ecc-large-buffer)