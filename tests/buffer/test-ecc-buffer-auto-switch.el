;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 03:27:15>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/buffer/test-ecc-buffer-auto-switch.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ert)
(require 'ecc-buffer/ecc-buffer-variables)
(require 'ecc-buffer/ecc-buffer-registry)
(require 'ecc-buffer/ecc-buffer-current)
(require 'ecc-buffer/ecc-buffer-state)

;; Load our module being tested
(require 'ecc-buffer/ecc-buffer-auto-switch)

(ert-deftest test-ecc-buffer-auto-switch-loadable ()
  "Test that ecc-buffer-auto-switch loads properly."
  (should (featurep 'ecc-buffer/ecc-buffer-auto-switch)))

(ert-deftest test-ecc-buffer-auto-switch-mode-variable-exists ()
  "Test that ecc-buffer-auto-switch-mode variable exists."
  (should (boundp 'ecc-buffer-auto-switch-mode)))

(ert-deftest test-ecc-buffer-auto-switch-mode-function-exists ()
  "Test that ecc-buffer-auto-switch-mode function exists."
  (should (fboundp 'ecc-buffer-auto-switch-mode)))

(ert-deftest test-ecc-buffer-auto-switch-next-buffer-function-exists ()
  "Test that ecc-buffer-auto-switch-next-buffer function exists."
  (should (fboundp 'ecc-buffer-auto-switch-next-buffer)))

(ert-deftest test-ecc-buffer-auto-switch-previous-buffer-function-exists ()
  "Test that ecc-buffer-auto-switch-previous-buffer function exists."
  (should (fboundp 'ecc-buffer-auto-switch-previous-buffer)))

(ert-deftest test-ecc-buffer-auto-switch-toggle-function-exists ()
  "Test that ecc-buffer-auto-switch-toggle function exists."
  (should (fboundp 'ecc-buffer-auto-switch-toggle)))

(ert-deftest test-ecc-buffer-auto-switch-mode-initialization ()
  "Test enabling and disabling auto-switch mode."
  (unwind-protect
      (progn
        ;; Test enabling
        (ecc-buffer-auto-switch-mode 1)
        (should ecc-buffer-auto-switch-mode)
        
        ;; Test disabling
        (ecc-buffer-auto-switch-mode -1)
        (should-not ecc-buffer-auto-switch-mode))
    
    ;; Cleanup
    (when (boundp 'ecc-buffer-auto-switch-mode)
      (setq ecc-buffer-auto-switch-mode nil))))

(ert-deftest test-ecc-buffer-auto-switch-toggle ()
  "Test toggling auto-switch mode."
  (unwind-protect
      (progn
        ;; Start with mode off
        (when (and (boundp 'ecc-buffer-auto-switch-mode) ecc-buffer-auto-switch-mode)
          (ecc-buffer-auto-switch-mode -1))
        
        ;; Test toggling on
        (ecc-buffer-auto-switch-toggle)
        (should ecc-buffer-auto-switch-mode)
        
        ;; Test toggling off
        (ecc-buffer-auto-switch-toggle)
        (should-not ecc-buffer-auto-switch-mode))
    
    ;; Cleanup
    (when (boundp 'ecc-buffer-auto-switch-mode)
      (setq ecc-buffer-auto-switch-mode nil))))

(ert-deftest test-ecc-buffer-auto-switch-next-buffer ()
  "Test switching to next Claude buffer."
  (let ((original-registry ecc-buffer-registered-buffers-alist)
        (original-current ecc-buffer-current-buffer)
        (test-buffer-1 (generate-new-buffer "*test-claude-auto-1*"))
        (test-buffer-2 (generate-new-buffer "*test-claude-auto-2*"))
        (test-buffer-3 (generate-new-buffer "*test-claude-auto-3*")))
    (unwind-protect
        (progn
          ;; Setup clean test environment
          (setq ecc-buffer-registered-buffers-alist nil)
          (setq ecc-buffer-current-buffer nil)
          
          ;; Register test buffers
          (cl-letf (((symbol-function 'ecc-update-mode-line-all-buffers) #'ignore))
            ;; Register buffers in order
            (ecc-buffer-register-buffer test-buffer-1)
            (ecc-buffer-register-buffer test-buffer-2)
            (ecc-buffer-register-buffer test-buffer-3)
            
            ;; Set current buffer to first one
            (ecc-buffer-set-current-buffer test-buffer-1)
            
            ;; Test next buffer - should go to buffer 2
            ;; Use buffer name comparison instead of buffer object comparison
            ;; since the test runs after buffers might be killed
            (let ((next-buffer (ecc-buffer-auto-switch-next-buffer)))
              (should (equal (buffer-name test-buffer-2) 
                             (and next-buffer (buffer-name next-buffer)))))
            (should (equal (buffer-name test-buffer-2)
                           (and (ecc-buffer-get-current-buffer)
                                (buffer-name (ecc-buffer-get-current-buffer)))))
            
            ;; Test next buffer again - should go to buffer 3
            (let ((next-buffer (ecc-buffer-auto-switch-next-buffer)))
              (should (equal (buffer-name test-buffer-3)
                             (and next-buffer (buffer-name next-buffer)))))
            (should (equal (buffer-name test-buffer-3)
                           (and (ecc-buffer-get-current-buffer)
                                (buffer-name (ecc-buffer-get-current-buffer)))))
            
            ;; Test wrap-around - should go to buffer 1
            (let ((next-buffer (ecc-buffer-auto-switch-next-buffer)))
              (should (equal (buffer-name test-buffer-1)
                             (and next-buffer (buffer-name next-buffer)))))
            (should (equal (buffer-name test-buffer-1)
                           (and (ecc-buffer-get-current-buffer)
                                (buffer-name (ecc-buffer-get-current-buffer)))))))
      
      ;; Cleanup
      (when (buffer-live-p test-buffer-1)
        (kill-buffer test-buffer-1))
      (when (buffer-live-p test-buffer-2)
        (kill-buffer test-buffer-2))
      (when (buffer-live-p test-buffer-3)
        (kill-buffer test-buffer-3))
      (setq ecc-buffer-registered-buffers-alist original-registry)
      (setq ecc-buffer-current-buffer original-current))))

(ert-deftest test-ecc-buffer-auto-switch-previous-buffer ()
  "Test switching to previous Claude buffer."
  (let ((original-registry ecc-buffer-registered-buffers-alist)
        (original-current ecc-buffer-current-buffer)
        (test-buffer-1 (generate-new-buffer "*test-claude-auto-1*"))
        (test-buffer-2 (generate-new-buffer "*test-claude-auto-2*"))
        (test-buffer-3 (generate-new-buffer "*test-claude-auto-3*")))
    (unwind-protect
        (progn
          ;; Setup clean test environment
          (setq ecc-buffer-registered-buffers-alist nil)
          (setq ecc-buffer-current-buffer nil)
          
          ;; Register test buffers
          (cl-letf (((symbol-function 'ecc-update-mode-line-all-buffers) #'ignore))
            ;; Register buffers in order
            (ecc-buffer-register-buffer test-buffer-1)
            (ecc-buffer-register-buffer test-buffer-2)
            (ecc-buffer-register-buffer test-buffer-3)
            
            ;; Set current buffer to middle one
            (ecc-buffer-set-current-buffer test-buffer-2)
            
            ;; Test previous buffer - should go to buffer 1
            ;; Use buffer name comparison instead of buffer object comparison
            (let ((prev-buffer (ecc-buffer-auto-switch-previous-buffer)))
              (should (equal (buffer-name test-buffer-1)
                             (and prev-buffer (buffer-name prev-buffer)))))
            (should (equal (buffer-name test-buffer-1)
                           (and (ecc-buffer-get-current-buffer)
                                (buffer-name (ecc-buffer-get-current-buffer)))))
            
            ;; Test wrap-around - should go to buffer 3
            (let ((prev-buffer (ecc-buffer-auto-switch-previous-buffer)))
              (should (equal (buffer-name test-buffer-3)
                             (and prev-buffer (buffer-name prev-buffer)))))
            (should (equal (buffer-name test-buffer-3)
                           (and (ecc-buffer-get-current-buffer)
                                (buffer-name (ecc-buffer-get-current-buffer)))))
            
            ;; Test previous buffer again - should go to buffer 2
            (let ((prev-buffer (ecc-buffer-auto-switch-previous-buffer)))
              (should (equal (buffer-name test-buffer-2)
                             (and prev-buffer (buffer-name prev-buffer)))))
            (should (equal (buffer-name test-buffer-2)
                           (and (ecc-buffer-get-current-buffer)
                                (buffer-name (ecc-buffer-get-current-buffer)))))))
      
      ;; Cleanup
      (when (buffer-live-p test-buffer-1)
        (kill-buffer test-buffer-1))
      (when (buffer-live-p test-buffer-2)
        (kill-buffer test-buffer-2))
      (when (buffer-live-p test-buffer-3)
        (kill-buffer test-buffer-3))
      (setq ecc-buffer-registered-buffers-alist original-registry)
      (setq ecc-buffer-current-buffer original-current))))

(ert-deftest test-ecc-buffer-auto-switch-with-non-existent-buffers ()
  "Test auto-switching with non-existent buffers."
  (let ((original-registry ecc-buffer-registered-buffers-alist)
        (original-current ecc-buffer-current-buffer)
        (test-buffer-1 (generate-new-buffer "*test-claude-auto-1*"))
        (test-buffer-2 (generate-new-buffer "*test-claude-auto-2*")))
    (unwind-protect
        (progn
          ;; Setup clean test environment
          (setq ecc-buffer-registered-buffers-alist nil)
          (setq ecc-buffer-current-buffer nil)
          
          ;; Register test buffers
          (cl-letf (((symbol-function 'ecc-update-mode-line-all-buffers) #'ignore))
            ;; Register buffers in order
            (ecc-buffer-register-buffer test-buffer-1)
            (ecc-buffer-register-buffer test-buffer-2)
            
            ;; Set current buffer to first one
            (ecc-buffer-set-current-buffer test-buffer-1)
            
            ;; Kill buffer 2
            (kill-buffer test-buffer-2)
            
            ;; Test next buffer - should return buffer 1 since it's the only valid one
            (let ((next-buffer (ecc-buffer-auto-switch-next-buffer)))
              (should (equal (buffer-name test-buffer-1)
                             (and next-buffer (buffer-name next-buffer)))))
            
            ;; Test previous buffer - should also return buffer 1
            (let ((prev-buffer (ecc-buffer-auto-switch-previous-buffer)))
              (should (equal (buffer-name test-buffer-1)
                             (and prev-buffer (buffer-name prev-buffer)))))))
      
      ;; Cleanup
      (when (buffer-live-p test-buffer-1)
        (kill-buffer test-buffer-1))
      (when (buffer-live-p test-buffer-2)
        (kill-buffer test-buffer-2))
      (setq ecc-buffer-registered-buffers-alist original-registry)
      (setq ecc-buffer-current-buffer original-current))))

(provide 'test-ecc-buffer-auto-switch)

(when (not load-file-name)
  (message "test-ecc-buffer-auto-switch.el loaded: %s"
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))