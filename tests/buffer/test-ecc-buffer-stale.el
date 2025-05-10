;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 16:22:15>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-buffer-stale.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ert)
(require 'ecc-variables)
(require 'ecc-buffer/ecc-buffer-variables)
(require 'ecc-buffer/ecc-buffer-registry)
(require 'ecc-buffer/ecc-buffer-current)
(require 'ecc-buffer/ecc-buffer-stale)

(ert-deftest test-ecc-buffer-stale-loadable ()
  "Test that ecc-buffer-stale loads properly."
  (should (featurep 'ecc-buffer/ecc-buffer-stale)))

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

(ert-deftest test-ecc-buffer-get-and-set-buffer-state ()
  "Test getting and setting buffer state."
  (let ((original-registry ecc-buffer-registered-buffers-alist)
        (test-buffer (generate-new-buffer "*test-claude-state*")))
    (unwind-protect
        (progn
          ;; Setup test environment
          (setq ecc-buffer-registered-buffers-alist 
                (list (cons test-buffer nil)))
          
          ;; Test get/set with valid state
          (should-not (ecc-buffer-get-buffer-state test-buffer)) ; Initially nil
          (should (ecc-buffer-set-buffer-state test-buffer 'waiting))
          (should (eq 'waiting (ecc-buffer-get-buffer-state test-buffer)))
          
          ;; Test with invalid state - should error
          (should-error (ecc-buffer-set-buffer-state test-buffer :invalid-state))
          
          ;; Test with nil buffer
          (should-not (ecc-buffer-get-buffer-state nil))
          (should-not (ecc-buffer-set-buffer-state nil :active))
          
          ;; Test with non-registered buffer
          (let ((other-buffer (generate-new-buffer "*test-claude-other*")))
            (unwind-protect
                (progn
                  (should-not (ecc-buffer-get-buffer-state other-buffer))
                  (should-not (ecc-buffer-set-buffer-state other-buffer :active)))
              (kill-buffer other-buffer))))
      
      ;; Cleanup
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer))
      (setq ecc-buffer-registered-buffers-alist original-registry))))

(ert-deftest test-ecc-buffer-unregister-stale-buffer ()
  "Test unregistering stale buffers."
  (let ((original-registry ecc-buffer-registered-buffers-alist)
        (original-current ecc-buffer-current-buffer)
        (live-buffer (generate-new-buffer "*test-claude-live*"))
        (dead-buffer (generate-new-buffer "*test-claude-dead*")))
    (unwind-protect
        (progn
          ;; Setup test environment
          (setq ecc-buffer-registered-buffers-alist 
                (list (cons live-buffer nil)
                      (cons dead-buffer nil)))
          (setq ecc-buffer-current-buffer dead-buffer)
          
          ;; Mock vterm-mode
          (cl-letf (((symbol-function 'derived-mode-p) 
                     (lambda (mode) (eq mode 'vterm-mode)))
                    ((symbol-function 'ecc-buffer-unregister-buffer) 
                     (lambda (buf)
                       (setq ecc-buffer-registered-buffers-alist
                             (assoc-delete-all buf ecc-buffer-registered-buffers-alist))
                       t)))
            
            ;; Kill one buffer to make it stale
            (kill-buffer dead-buffer)
            
            ;; Test unregistering stale buffer
            (should (ecc-buffer-unregister-stale-buffer dead-buffer))
            
            ;; Verify buffer was unregistered
            (should-not (assoc dead-buffer ecc-buffer-registered-buffers-alist))
            
            ;; Verify current buffer was cleared
            (should-not ecc-buffer-current-buffer)
            
            ;; Live buffer should not be unregistered
            (should-not (ecc-buffer-unregister-stale-buffer live-buffer))
            (should (assoc live-buffer ecc-buffer-registered-buffers-alist))))
      
      ;; Cleanup
      (when (buffer-live-p live-buffer)
        (kill-buffer live-buffer))
      (setq ecc-buffer-registered-buffers-alist original-registry)
      (setq ecc-buffer-current-buffer original-current))))

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
                       t)))
            
            ;; Kill buffers to make them stale
            (kill-buffer dead-buffer1)
            (kill-buffer dead-buffer2)
            
            ;; Test unregistering all stale buffers
            (should (= 2 (ecc-buffer-unregister-stale-buffers)))
            
            ;; Verify stale buffers were unregistered
            (should (= 1 (length ecc-buffer-registered-buffers-alist)))
            (should (assoc live-buffer ecc-buffer-registered-buffers-alist))
            
            ;; Test cleanup compatibility function
            (should (= 0 (ecc-buffer-cleanup-buffer-registry)))))
      
      ;; Cleanup
      (when (buffer-live-p live-buffer)
        (kill-buffer live-buffer))
      (setq ecc-buffer-registered-buffers-alist original-registry)
      (setq ecc-buffer-current-buffer original-current))))

(provide 'test-ecc-buffer-stale)

(when
    (not load-file-name)
  (message "test-ecc-buffer-stale.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))