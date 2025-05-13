;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 19:45:22>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-ui/test-ecc-history.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'ecc-variables)

;; Ensure the test module is loadable
(ert-deftest test-ecc-history-loadable ()
  "Test that ecc-history.el can be loaded."
  ;; First unload the feature if it's already loaded (for test idempotence)
  (when (featurep 'ecc-history)
    (unload-feature 'ecc-history t))
  
  ;; Now test loading it
  (should-not (featurep 'ecc-history))
  (condition-case nil
      (require 'ecc-history)
    (error nil))
  (should (featurep 'ecc-history)))

;; Test history data structure
(ert-deftest test-ecc-history-variables-defined ()
  "Test that history variables are properly defined."
  (should (boundp 'ecc-history-entries))
  (should (hash-table-p ecc-history-entries)))

;; Test recording a history entry
(ert-deftest test-ecc-history-record-entry ()
  "Test that we can record a history entry."
  ;; Setup test buffer and environment
  (let ((test-buffer (generate-new-buffer "*test-ecc*"))
        (test-prompt "Test prompt text")
        (original-entries ecc-history-entries))
    
    (unwind-protect
        (progn
          ;; Set up test environment
          (with-current-buffer test-buffer
            (require 'ecc-buffer-registry)
            (ecc-buffer-register-buffer test-buffer))
          
          ;; Call the function
          (ecc-history-record-entry test-buffer test-prompt)
          
          ;; Verify entry was recorded
          (let ((buffer-entries (gethash test-buffer ecc-history-entries)))
            (should buffer-entries)
            (should (= 1 (length buffer-entries)))
            (should (string= test-prompt (cdr (car buffer-entries))))))
      
      ;; Cleanup
      (kill-buffer test-buffer)
      (setq ecc-history-entries original-entries))))

;; Test retrieving history entries for a buffer
(ert-deftest test-ecc-history-get-entries ()
  "Test retrieving history entries for a specific buffer."
  ;; Setup test buffer and environment
  (let ((test-buffer (generate-new-buffer "*test-ecc*"))
        (test-prompt-1 "First test prompt")
        (test-prompt-2 "Second test prompt")
        (original-entries ecc-history-entries))
    
    (unwind-protect
        (progn
          ;; Set up test environment
          (with-current-buffer test-buffer
            (require 'ecc-buffer-registry)
            (ecc-buffer-register-buffer test-buffer))
          
          ;; Record entries
          (ecc-history-record-entry test-buffer test-prompt-1)
          (ecc-history-record-entry test-buffer test-prompt-2)
          
          ;; Verify entries can be retrieved
          (let ((entries (ecc-history-get-entries test-buffer)))
            (should entries)
            (should (= 2 (length entries)))
            (should (string= test-prompt-2 (cdr (nth 0 entries))))
            (should (string= test-prompt-1 (cdr (nth 1 entries))))))
      
      ;; Cleanup
      (kill-buffer test-buffer)
      (setq ecc-history-entries original-entries))))

;; Test the history browser buffer creation
(ert-deftest test-ecc-history-browser-buffer ()
  "Test that the history browser buffer can be created."
  ;; Setup test buffer and environment
  (let ((test-buffer (generate-new-buffer "*test-ecc*"))
        (test-prompt-1 "First test prompt")
        (test-prompt-2 "Second test prompt")
        (original-entries ecc-history-entries)
        (browser-buffer nil))
    
    (unwind-protect
        (progn
          ;; Set up test environment
          (with-current-buffer test-buffer
            (require 'ecc-buffer-registry)
            (ecc-buffer-register-buffer test-buffer))
          
          ;; Record entries
          (ecc-history-record-entry test-buffer test-prompt-1)
          (ecc-history-record-entry test-buffer test-prompt-2)
          
          ;; Create browser buffer
          (setq browser-buffer (ecc-history-browser-create test-buffer))
          
          ;; Verify browser buffer
          (should browser-buffer)
          (should (buffer-live-p browser-buffer))
          (with-current-buffer browser-buffer
            (should (eq 'ecc-history-mode major-mode))
            (should (string-match "History for buffer: \\*test-ecc\\*" 
                                 (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match test-prompt-1
                                 (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match test-prompt-2
                                 (buffer-substring-no-properties (point-min) (point-max))))))
      
      ;; Cleanup
      (when (and browser-buffer (buffer-live-p browser-buffer))
        (kill-buffer browser-buffer))
      (kill-buffer test-buffer)
      (setq ecc-history-entries original-entries))))

;; Test reusing a prompt from history
(ert-deftest test-ecc-history-reuse-prompt ()
  "Test that we can reuse a prompt from history."
  ;; Setup test buffer and environment
  (let ((test-buffer (generate-new-buffer "*test-ecc*"))
        (test-prompt "Test prompt to reuse")
        (original-entries ecc-history-entries)
        (called-with nil))
    
    (unwind-protect
        (progn
          ;; Set up test environment
          (with-current-buffer test-buffer
            (require 'ecc-buffer-registry)
            (ecc-buffer-register-buffer test-buffer))
          
          ;; Record entry
          (ecc-history-record-entry test-buffer test-prompt)
          
          ;; Mock the send function for testing
          (cl-letf (((symbol-function '--ecc-send-string) 
                    (lambda (str &rest _) (setq called-with str))))
            
            ;; Call reuse function with the first entry
            (ecc-history-reuse-prompt test-buffer 0)
            
            ;; Verify it called the send function with our prompt
            (should (string= test-prompt called-with))))
      
      ;; Cleanup
      (kill-buffer test-buffer)
      (setq ecc-history-entries original-entries))))

(provide 'test-ecc-history)