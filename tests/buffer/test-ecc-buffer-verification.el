;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 16:37:15>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-buffer-verification.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ert)
(require 'ecc-variables)
(require 'ecc-buffer/ecc-buffer-variables)
(require 'ecc-buffer/ecc-buffer-verification)

(ert-deftest test-ecc-buffer-verification-loadable ()
  "Test that ecc-buffer-verification loads properly."
  (should (featurep 'ecc-buffer/ecc-buffer-verification)))

(ert-deftest test-ecc-buffer-verify-buffer-exists ()
  "Test that ecc-buffer-verify-buffer function exists."
  (should (fboundp 'ecc-buffer-verify-buffer)))

(ert-deftest test-ecc-buffer-verify-buffer-with-various-inputs ()
  "Test verification with different buffer inputs."
  (let ((original-registry ecc-buffer-registered-buffers-alist)
        (test-buffer (generate-new-buffer "*test-claude-verify*"))
        (test-regular-buffer (generate-new-buffer "*test-regular*")))
    (unwind-protect
        (progn
          ;; Setup test environment
          (setq ecc-buffer-registered-buffers-alist
                (list (cons test-buffer nil)))
          
          ;; Mock vterm-mode for test-buffer
          (cl-letf (((symbol-function 'derived-mode-p) 
                     (lambda (mode)
                       (and (eq mode 'vterm-mode)
                            (eq (current-buffer) test-buffer)))))
            
            ;; Test with valid registered vterm buffer
            (should (ecc-buffer-verify-buffer test-buffer))
            
            ;; Test with non-vterm buffer
            (should-not (ecc-buffer-verify-buffer test-regular-buffer))
            
            ;; Test with unregistered buffer
            (setq ecc-buffer-registered-buffers-alist nil)
            (should-not (ecc-buffer-verify-buffer test-buffer))
            
            ;; Test with nil buffer
            (should-not (ecc-buffer-verify-buffer nil))
            
            ;; Test with non-existent buffer name
            (should-not (ecc-buffer-verify-buffer "*non-existent-buffer*"))
            
            ;; Test with killed buffer
            (kill-buffer test-buffer)
            (should-not (ecc-buffer-verify-buffer test-buffer))))
      
      ;; Cleanup
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer))
      (when (buffer-live-p test-regular-buffer)
        (kill-buffer test-regular-buffer))
      (setq ecc-buffer-registered-buffers-alist original-registry))))

(ert-deftest test-ecc-buffer-verify-buffer-with-buffer-name ()
  "Test verification with buffer name instead of buffer object."
  (let ((original-registry ecc-buffer-registered-buffers-alist)
        (test-buffer (generate-new-buffer "*test-claude-verify-name*")))
    (unwind-protect
        (progn
          ;; Setup test environment
          (setq ecc-buffer-registered-buffers-alist
                (list (cons test-buffer nil)))
          
          ;; Mock vterm-mode
          (cl-letf (((symbol-function 'derived-mode-p) 
                     (lambda (mode) (eq mode 'vterm-mode))))
            
            ;; Test with buffer name
            (should (ecc-buffer-verify-buffer (buffer-name test-buffer)))))
      
      ;; Cleanup
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer))
      (setq ecc-buffer-registered-buffers-alist original-registry))))

(provide 'test-ecc-buffer-verification)

(when
    (not load-file-name)
  (message "test-ecc-buffer-verification.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))