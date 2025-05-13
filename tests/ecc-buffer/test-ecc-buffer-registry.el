;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 15:10:25>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-buffer-registry.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ert)
(require 'ecc-buffer-registry)
(require 'ecc-buffer-variables)
(require 'ecc-buffer-timestamp)
(require 'ecc-buffer-stale)
(require 'ecc-ui-mode-line)

(ert-deftest test-ecc-buffer-registry-loadable ()
  "Test that ecc-buffer-registry loads properly."
  (should (featurep 'ecc-buffer-registry)))

(ert-deftest test-ecc-buffer-timestamps-defined ()
  "Test that timestamp-related variables are defined."
  (should (boundp 'ecc-buffer-timestamps))
  (should (hash-table-p ecc-buffer-timestamps)))

(ert-deftest test-ecc-buffer-cleanup-buffer-registry ()
  "Test that cleanup removes dead buffers."
  (let ((original-alist ecc-buffer-registered-buffers-alist)
        (original-active ecc-buffer-current-buffer)
        (test-buffer (generate-new-buffer "*test-claude*"))
        (kill-hook-status nil))
    (unwind-protect
        (progn
          ;; Mock kill-buffer-hook to prevent recursion
          (cl-letf (((symbol-function 'ecc-buffer-cleanup-hook) 
                   (lambda () (setq kill-hook-status 'called))))
            ;; Setup test environment
            (setq ecc-buffer-registered-buffers-alist (list (cons test-buffer nil)))
            (setq ecc-buffer-current-buffer test-buffer)
            
            ;; Kill the buffer directly without invoking hooks
            (let ((kill-buffer-hook nil)) 
              (kill-buffer test-buffer))
            
            ;; Manually run cleanup
            (ecc-buffer-cleanup-buffer-registry)
            
            ;; Verify cleanup worked
            (should (equal ecc-buffer-registered-buffers-alist nil))))
      
      ;; Restore original state
      (setq ecc-buffer-registered-buffers-alist original-alist)
      (setq ecc-buffer-current-buffer original-active))))

(ert-deftest test-ecc-buffer-unregister-buffer ()
  "Test buffer unregistration functionality."
  (let ((original-alist ecc-buffer-registered-buffers-alist)
        (original-active ecc-buffer-current-buffer)
        (test-buffer (generate-new-buffer "*test-claude*")))
    (unwind-protect
        (progn
          ;; Mock kill-buffer-hook to prevent recursion
          (cl-letf (((symbol-function 'ecc-buffer-cleanup-hook) 
                   (lambda () nil))
                  ((symbol-function 'ecc-ui-update-mode-line-all-buffers) 
                   #'ignore))
            
            ;; Setup test environment (direct assignment to avoid hooks)
            (setq ecc-buffer-registered-buffers-alist (list (cons test-buffer nil)))
            
            ;; Run the unregister function
            (ecc-buffer-unregister-buffer test-buffer)
            
            ;; Verify buffer was unregistered
            (should (equal ecc-buffer-registered-buffers-alist nil))))
      
      ;; Restore original state
      (setq ecc-buffer-registered-buffers-alist original-alist)
      (setq ecc-buffer-current-buffer original-active))))

(ert-deftest test-ecc-buffer-register-as-active ()
  "Test buffer registration functionality."
  (let ((original-alist ecc-buffer-registered-buffers-alist)
        (original-active ecc-buffer-current-buffer)
        (test-buffer (generate-new-buffer "*test-claude*")))
    (unwind-protect
        (progn
          ;; Setup test environment
          (setq ecc-buffer-registered-buffers-alist nil)
          (setq ecc-buffer-current-buffer nil)
          
          ;; Mock update function to prevent UI updates
          (cl-letf (((symbol-function 'ecc-ui-update-mode-line-all-buffers) 
                   #'ignore))
            
            ;; Register the test buffer
            (ecc-buffer-register-buffer test-buffer)
            
            ;; Verify buffer was registered
            (should (assoc test-buffer ecc-buffer-registered-buffers-alist))))
      
      ;; Clean up
      (setq ecc-buffer-registered-buffers-alist original-alist)
      (setq ecc-buffer-current-buffer original-active)
      (kill-buffer test-buffer))))

(ert-deftest test-ecc-buffer-list-buffers ()
  "Test listing of registered buffers."
  (let ((original-alist ecc-buffer-registered-buffers-alist)
        (test-buffer1 (generate-new-buffer "*test-claude-1*"))
        (test-buffer2 (generate-new-buffer "*test-claude-2*")))
    (unwind-protect
        (progn
          ;; Setup test environment
          (setq ecc-buffer-registered-buffers-alist 
                (list (cons test-buffer1 nil)
                      (cons test-buffer2 nil)))
          
          ;; Test list-registered-buffers
          (let ((buffers (ecc-buffer-list-registered-buffers)))
            (should (= (length buffers) 2))
            (should (equal buffers ecc-buffer-registered-buffers-alist)))
          
          ;; Test get-registered-buffers
          (let ((buffers (ecc-buffer-get-registered-buffers)))
            (should (= (length buffers) 2))
            (should (member test-buffer1 buffers))
            (should (member test-buffer2 buffers))))
      
      ;; Clean up
      (setq ecc-buffer-registered-buffers-alist original-alist)
      (kill-buffer test-buffer1)
      (kill-buffer test-buffer2))))

(provide 'test-ecc-buffer-registry)