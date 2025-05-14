;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 03:27:15>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-buffer/test-ecc-buffer-auto-switch.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ert)
(require 'ecc-buffer-variables)
(require 'ecc-buffer-registry)
(require 'ecc-buffer-current)
;; ecc-buffer-state removed in reorganization
;; (require 'ecc-buffer-state)

;; Load our module being tested
(require 'ecc-buffer-switch)

(ert-deftest test-ecc-buffer-switch-loadable ()
  "Test that ecc-buffer-switch loads properly."
  (should (featurep 'ecc-buffer-switch)))

(ert-deftest test-ecc-buffer-switch-mode-variable-exists ()
  "Test that ecc-buffer-switch-mode variable exists."
  (should (boundp 'ecc-buffer-switch-mode)))

(ert-deftest test-ecc-buffer-switch-mode-function-exists ()
  "Test that ecc-buffer-switch-mode function exists."
  (should (fboundp 'ecc-buffer-switch-mode)))

(ert-deftest test-ecc-buffer-switch-next-buffer-function-exists ()
  "Test that ecc-buffer-switch-next-buffer function exists."
  (should (fboundp 'ecc-buffer-switch-next-buffer)))

(ert-deftest test-ecc-buffer-switch-previous-buffer-function-exists ()
  "Test that ecc-buffer-switch-previous-buffer function exists."
  (should (fboundp 'ecc-buffer-switch-previous-buffer)))

(ert-deftest test-ecc-buffer-switch-toggle-function-exists ()
  "Test that ecc-buffer-switch-toggle function exists."
  (should (fboundp 'ecc-buffer-switch-toggle)))

(ert-deftest test-ecc-buffer-switch-mode-initialization ()
  "Test enabling and disabling switch mode."
  (unwind-protect
      (progn
        ;; Test enabling
        (ecc-buffer-switch-mode 1)
        (should ecc-buffer-switch-mode)
        
        ;; Test disabling
        (ecc-buffer-switch-mode -1)
        (should-not ecc-buffer-switch-mode))
    
    ;; Cleanup
    (when (boundp 'ecc-buffer-switch-mode)
      (setq ecc-buffer-switch-mode nil))))

(ert-deftest test-ecc-buffer-switch-toggle ()
  "Test toggling switch mode."
  (unwind-protect
      (progn
        ;; Start with mode off
        (when (and (boundp 'ecc-buffer-switch-mode) ecc-buffer-switch-mode)
          (ecc-buffer-switch-mode -1))
        
        ;; Test toggling on
        (ecc-buffer-switch-toggle)
        (should ecc-buffer-switch-mode)
        
        ;; Test toggling off
        (ecc-buffer-switch-toggle)
        (should-not ecc-buffer-switch-mode))
    
    ;; Cleanup
    (when (boundp 'ecc-buffer-switch-mode)
      (setq ecc-buffer-switch-mode nil))))

(ert-deftest test-ecc-buffer-switch-next-buffer ()
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
            (let ((next-buffer (ecc-buffer-switch-next-buffer)))
              (should (equal (buffer-name test-buffer-2) 
                             (and next-buffer (buffer-name next-buffer)))))
            (should (equal (buffer-name test-buffer-2)
                           (and (ecc-buffer-get-current-buffer)
                                (buffer-name (ecc-buffer-get-current-buffer)))))
            
            ;; Test next buffer again - should go to buffer 3
            (let ((next-buffer (ecc-buffer-switch-next-buffer)))
              (should (equal (buffer-name test-buffer-3)
                             (and next-buffer (buffer-name next-buffer)))))
            (should (equal (buffer-name test-buffer-3)
                           (and (ecc-buffer-get-current-buffer)
                                (buffer-name (ecc-buffer-get-current-buffer)))))
            
            ;; Test wrap-around - should go to buffer 1
            (let ((next-buffer (ecc-buffer-switch-next-buffer)))
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

(ert-deftest test-ecc-buffer-switch-previous-buffer ()
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
            (let ((prev-buffer (ecc-buffer-switch-previous-buffer)))
              (should (equal (buffer-name test-buffer-1)
                             (and prev-buffer (buffer-name prev-buffer)))))
            (should (equal (buffer-name test-buffer-1)
                           (and (ecc-buffer-get-current-buffer)
                                (buffer-name (ecc-buffer-get-current-buffer)))))
            
            ;; Test wrap-around - should go to buffer 3
            (let ((prev-buffer (ecc-buffer-switch-previous-buffer)))
              (should (equal (buffer-name test-buffer-3)
                             (and prev-buffer (buffer-name prev-buffer)))))
            (should (equal (buffer-name test-buffer-3)
                           (and (ecc-buffer-get-current-buffer)
                                (buffer-name (ecc-buffer-get-current-buffer)))))
            
            ;; Test previous buffer again - should go to buffer 2
            (let ((prev-buffer (ecc-buffer-switch-previous-buffer)))
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

(ert-deftest test-ecc-buffer-switch-with-non-existent-buffers ()
  "Test switching with non-existent buffers."
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
            (let ((next-buffer (ecc-buffer-switch-next-buffer)))
              (should (equal (buffer-name test-buffer-1)
                             (and next-buffer (buffer-name next-buffer)))))
            
            ;; Test previous buffer - should also return buffer 1
            (let ((prev-buffer (ecc-buffer-switch-previous-buffer)))
              (should (equal (buffer-name test-buffer-1)
                             (and prev-buffer (buffer-name prev-buffer)))))))
      
      ;; Cleanup
      (when (buffer-live-p test-buffer-1)
        (kill-buffer test-buffer-1))
      (when (buffer-live-p test-buffer-2)
        (kill-buffer test-buffer-2))
      (setq ecc-buffer-registered-buffers-alist original-registry)
      (setq ecc-buffer-current-buffer original-current))))

(provide 'test-ecc-buffer-switch)

(when (not load-file-name)
  (message "test-ecc-buffer-switch.el loaded: %s"
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))