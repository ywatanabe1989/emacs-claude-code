;;; test-ecc-term-claude-buffer.el --- Tests for term-claude buffer management -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Created: 2025-05-21
;; Version: 1.0.0
;; Keywords: convenience, testing

;;; Commentary:
;; Unit tests for term-claude buffer management functionality.
;; These tests verify that buffer registration, tracking, and management
;; functions work correctly with various scenarios.

;;; Code:

(require 'ert)

;; Load the mock version of ecc-term-claude-buffer.el for testing
(load-file (expand-file-name "mock-ecc-term-claude-buffer.el" 
                             (file-name-directory load-file-name)))

;;;; Test Utilities

(defun ecc-buffer-with-cleanup (fn)
  "Run FN with a clean buffer tracking state and restore afterward."
  (let ((orig-buffers ecc-buffer-registered-buffers-alist)
        (orig-current ecc-buffer-current-buffer))
    (unwind-protect
        (progn
          ;; Start with clean state
          (setq ecc-buffer-registered-buffers-alist nil)
          (setq ecc-buffer-current-buffer nil)
          
          ;; Run the test function
          (funcall fn))
      
      ;; Restore original state
      (setq ecc-buffer-registered-buffers-alist orig-buffers)
      (setq ecc-buffer-current-buffer orig-current))))

;;;; Buffer Registration Tests

(ert-deftest test-ecc-term-claude-register-buffer-vterm ()
  "Test buffer registration functionality."
  (ecc-buffer-with-cleanup
   (lambda ()
     (let ((test-buf (generate-new-buffer "*test-claude*")))
       (unwind-protect
           (progn
             ;; Test registering a buffer
             (should (eq (ecc-term-claude-register-buffer test-buf) test-buf))
             
             ;; Check it was added to the alist
             (should (assoc test-buf ecc-buffer-registered-buffers-alist))
             
             ;; Check it was set as current
             (should (eq ecc-buffer-current-buffer test-buf))
             
             ;; Test registering the same buffer again (should not add duplicate)
             (let ((alist-length (length ecc-buffer-registered-buffers-alist)))
               (ecc-term-claude-register-buffer test-buf)
               (should (= (length ecc-buffer-registered-buffers-alist) alist-length)))
             
             ;; Test error on dead buffer
             (let ((dead-buf (generate-new-buffer "*dead*")))
               (kill-buffer dead-buf)
               (should-error (ecc-term-claude-register-buffer dead-buf)
                            :type 'user-error)))
         
         ;; Clean up
         (kill-buffer test-buf))))))

(ert-deftest test-ecc-term-claude-unregister-buffer-vterm ()
  "Test buffer unregistration functionality."
  (ecc-buffer-with-cleanup
   (lambda ()
     (let ((test-buf1 (generate-new-buffer "*test-claude-1*"))
           (test-buf2 (generate-new-buffer "*test-claude-2*")))
       (unwind-protect
           (progn
             ;; Register two buffers
             (ecc-term-claude-register-buffer test-buf1)
             (ecc-term-claude-register-buffer test-buf2)
             
             ;; Test unregistering - current buffer should change
             (should (ecc-term-claude-unregister-buffer test-buf2))
             (should-not (assoc test-buf2 ecc-buffer-registered-buffers-alist))
             (should (eq ecc-buffer-current-buffer test-buf1))
             
             ;; Test unregistering a non-registered buffer
             (let ((non-reg-buf (generate-new-buffer "*non-reg*")))
               (unwind-protect
                   (should-not (ecc-term-claude-unregister-buffer non-reg-buf))
                 (kill-buffer non-reg-buf))))
         
         ;; Clean up
         (kill-buffer test-buf1)
         (kill-buffer test-buf2))))))

(ert-deftest test-ecc-term-claude-get-all-buffers ()
  "Test getting all registered buffers."
  (ecc-buffer-with-cleanup
   (lambda ()
     (let ((test-buf1 (generate-new-buffer "*test-claude-1*"))
           (test-buf2 (generate-new-buffer "*test-claude-2*")))
       (unwind-protect
           (progn
             ;; Register two buffers
             (ecc-term-claude-register-buffer test-buf1)
             (ecc-term-claude-register-buffer test-buf2)
             
             ;; Test getting all buffers
             (let ((all-bufs (ecc-term-claude-get-all-buffers)))
               (should (= (length all-bufs) 2))
               (should (member test-buf1 all-bufs))
               (should (member test-buf2 all-bufs)))
             
             ;; Test with a killed buffer (should be excluded)
             (kill-buffer test-buf1)
             (let ((all-bufs (ecc-term-claude-get-all-buffers)))
               (should (= (length all-bufs) 1))
               (should-not (member test-buf1 all-bufs))
               (should (member test-buf2 all-bufs))))
         
         ;; Clean up
         (when (buffer-live-p test-buf1) (kill-buffer test-buf1))
         (when (buffer-live-p test-buf2) (kill-buffer test-buf2)))))))

(ert-deftest test-ecc-term-claude-get-current-buffer-vterm ()
  "Test getting the current Claude buffer."
  (ecc-buffer-with-cleanup
   (lambda ()
     (let ((test-buf1 (generate-new-buffer "*test-claude-1*"))
           (test-buf2 (generate-new-buffer "*test-claude-2*")))
       (unwind-protect
           (progn
             ;; Test with no buffers registered
             (should-not (ecc-term-claude-get-current-buffer))
             
             ;; Register one buffer
             (ecc-term-claude-register-buffer test-buf1)
             (should (eq (ecc-term-claude-get-current-buffer) test-buf1))
             
             ;; Register second buffer
             (ecc-term-claude-register-buffer test-buf2)
             (should (eq (ecc-term-claude-get-current-buffer) test-buf2))
             
             ;; Kill current buffer - should fall back to other buffer
             (kill-buffer test-buf2)
             (should (eq (ecc-term-claude-get-current-buffer) test-buf1))
             
             ;; Kill all buffers - should return nil
             (kill-buffer test-buf1)
             (should-not (ecc-term-claude-get-current-buffer)))
         
         ;; Clean up
         (when (buffer-live-p test-buf1) (kill-buffer test-buf1))
         (when (buffer-live-p test-buf2) (kill-buffer test-buf2)))))))

(ert-deftest test-ecc-term-claude-set-current-buffer-vterm ()
  "Test setting the current Claude buffer."
  (ecc-buffer-with-cleanup
   (lambda ()
     (let ((test-buf1 (generate-new-buffer "*test-claude-1*"))
           (test-buf2 (generate-new-buffer "*test-claude-2*")))
       (unwind-protect
           (progn
             ;; Register one buffer
             (ecc-term-claude-register-buffer test-buf1)
             
             ;; Test setting a new buffer (should register it)
             (should (eq (ecc-term-claude-set-current-buffer test-buf2) test-buf2))
             (should (eq ecc-buffer-current-buffer test-buf2))
             (should (assoc test-buf2 ecc-buffer-registered-buffers-alist))
             
             ;; Test error on dead buffer
             (let ((dead-buf (generate-new-buffer "*dead*")))
               (kill-buffer dead-buf)
               (should-error (ecc-term-claude-set-current-buffer dead-buf)
                            :type 'user-error)))
         
         ;; Clean up
         (when (buffer-live-p test-buf1) (kill-buffer test-buf1))
         (when (buffer-live-p test-buf2) (kill-buffer test-buf2)))))))

;; Test for cleanup functionality
(ert-deftest test-ecc-term-claude-cleanup-dead-buffers ()
  "Test cleaning up dead buffers."
  (ecc-buffer-with-cleanup
   (lambda ()
     (let ((test-buf1 (generate-new-buffer "*test-claude-1*"))
           (test-buf2 (generate-new-buffer "*test-claude-2*"))
           (test-buf3 (generate-new-buffer "*test-claude-3*")))
       (unwind-protect
           (progn
             ;; Register three buffers
             (ecc-term-claude-register-buffer test-buf1)
             (ecc-term-claude-register-buffer test-buf2)
             (ecc-term-claude-register-buffer test-buf3)
             
             ;; Kill two buffers
             (kill-buffer test-buf1)
             (kill-buffer test-buf2)
             
             ;; Test cleanup - count should be correct
             (should (= (ecc-term-claude-cleanup-dead-buffers) 2))
             
             ;; Make sure remaining buffer is still registered
             (should (assoc test-buf3 ecc-buffer-registered-buffers-alist))
             (should (eq ecc-buffer-current-buffer test-buf3)))
         
         ;; Clean up
         (when (buffer-live-p test-buf1) (kill-buffer test-buf1))
         (when (buffer-live-p test-buf2) (kill-buffer test-buf2))
         (when (buffer-live-p test-buf3) (kill-buffer test-buf3)))))))

(provide 'test-ecc-term-claude-buffer)

;;; test-ecc-term-claude-buffer.el ends here