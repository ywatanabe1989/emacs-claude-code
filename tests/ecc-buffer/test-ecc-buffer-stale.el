;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-14 16:32:10>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-buffer/test-ecc-buffer-stale.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'ecc-variables)
(require 'ecc-buffer-variables)
(require 'ecc-buffer-registry)
(require 'ecc-buffer-current)
(require 'ecc-buffer-stale)

(ert-deftest test-ecc-buffer-stale-loadable ()
  "Test that ecc-buffer-stale loads properly."
  (should (featurep 'ecc-buffer-stale)))

(ert-deftest test-ecc-buffer-get-buffer-state-exists ()
  "Test that ecc-buffer-get-buffer-state function exists."
  (should (fboundp 'ecc-buffer-get-buffer-state)))

(ert-deftest test-ecc-buffer-set-buffer-state-exists ()
  "Test that ecc-buffer-set-buffer-state function exists."
  (should (fboundp 'ecc-buffer-set-buffer-state)))

(ert-deftest test-ecc-buffer-unregister-stale-buffer-exists ()
  "Test that ecc-buffer-unregister-stale-buffer function exists."
  (should (fboundp 'ecc-buffer-unregister-stale-buffer)))

(ert-deftest test-ecc-buffer-unregister-stale-buffers-exists ()
  "Test that ecc-buffer-unregister-stale-buffers function exists."
  (should (fboundp 'ecc-buffer-unregister-stale-buffers)))

(ert-deftest test-ecc-buffer-cleanup-buffer-registry-exists ()
  "Test that ecc-buffer-cleanup-buffer-registry function exists."
  (should (fboundp 'ecc-buffer-cleanup-buffer-registry)))

(ert-deftest test-ecc-buffer-unregister-stale-buffers ()
  "Test unregistering multiple stale buffers."
  (let ((original-registry ecc-buffer-registered-buffers-alist)
        (original-current ecc-buffer-current-buffer)
        (live-buffer (generate-new-buffer "*test-claude-live*"))
        (dead-buffer1 (generate-new-buffer "*test-claude-dead1*"))
        (dead-buffer2 (generate-new-buffer "*test-claude-dead2*")))
    (unwind-protect
        (progn
          ;; Setup test environment
          (setq ecc-buffer-registered-buffers-alist 
                (list (cons live-buffer nil)
                      (cons dead-buffer1 nil)
                      (cons dead-buffer2 nil)))
          
          ;; Mock functions
          (cl-letf (((symbol-function 'derived-mode-p) 
                     (lambda (mode) (eq mode 'vterm-mode)))
                    ((symbol-function 'ecc-buffer-unregister-buffer) 
                     (lambda (buf)
                       (setq ecc-buffer-registered-buffers-alist
                             (assoc-delete-all buf ecc-buffer-registered-buffers-alist))
                       t))
                    ((symbol-function 'ecc-buffer-registry-unregister-buffer-with-uid)
                     (lambda (uid) t)))
            
            ;; Kill buffers to make them stale
            (kill-buffer dead-buffer1)
            (kill-buffer dead-buffer2)
            
            ;; Skip UID-based tests by mocking the hash-table check
            (cl-letf (((symbol-function 'hash-table-p) 
                       (lambda (obj) nil)))
              
              ;; Test unregistering all stale buffers
              (should (= 2 (ecc-buffer-unregister-stale-buffers)))
              
              ;; Verify stale buffers were unregistered
              (should (= 1 (length ecc-buffer-registered-buffers-alist)))
              (should (assoc live-buffer ecc-buffer-registered-buffers-alist))
              
              ;; Test cleanup compatibility function
              (should (= 0 (ecc-buffer-cleanup-buffer-registry))))))
      
      ;; Cleanup
      (when (buffer-live-p live-buffer)
        (kill-buffer live-buffer))
      (setq ecc-buffer-registered-buffers-alist original-registry)
      (setq ecc-buffer-current-buffer original-current))))

(provide 'test-ecc-buffer-stale)

;;; test-ecc-buffer-stale.el ends here