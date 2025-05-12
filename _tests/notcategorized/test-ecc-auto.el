;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 14:35:42>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-auto.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'ecc-auto)
(require 'ecc-mode)
(require 'ecc-variables)
(require 'ecc-send)

;; Mock external functions and variables for testing
(defvar test-ecc-auto-notifications '()
  "List to store calls to the notification system for verification.")

(defvar test-ecc-original-timer nil
  "Store original timer for restoration.")

(defvar test-ecc-original-registered-buffers nil
  "Store original registered buffers for restoration.")

(defvar test-ecc-original-active-buffer nil
  "Store original active buffer for restoration.")

(cl-defmacro with-mock-notifications (&rest body)
  "Execute BODY with mock notification functions for testing."
  `(let ((old-notify-fn (when (fboundp 'ecc-auto-notify-completion)
                          (symbol-function 'ecc-auto-notify-completion)))
         (old-notify-on-fn (when (fboundp 'ecc-auto-notification-on)
                             (symbol-function 'ecc-auto-notification-on)))
         (old-notify-off-fn (when (fboundp 'ecc-auto-notification-off)
                              (symbol-function 'ecc-auto-notification-off))))
     (unwind-protect
         (progn
           (setq test-ecc-auto-notifications '())
           (fset 'ecc-auto-notify-completion
                 (lambda (prompt-type)
                   (push (cons 'completion prompt-type) test-ecc-auto-notifications)))
           (fset 'ecc-auto-notification-on
                 (lambda ()
                   (push 'on test-ecc-auto-notifications)))
           (fset 'ecc-auto-notification-off
                 (lambda ()
                   (push 'off test-ecc-auto-notifications)))
           ,@body)
       ;; Restore original functions
       (when old-notify-fn
         (fset 'ecc-auto-notify-completion old-notify-fn))
       (when old-notify-on-fn
         (fset 'ecc-auto-notification-on old-notify-on-fn))
       (when old-notify-off-fn
         (fset 'ecc-auto-notification-off old-notify-off-fn)))))

(cl-defmacro with-ecc-auto-test-env (&rest body)
  "Set up a clean environment for ecc-auto tests and execute BODY."
  `(let ((test-buffer (generate-new-buffer "*test-claude*")))
     (unwind-protect
         (progn
           ;; Save original state
           (setq test-ecc-original-timer ecc-auto-timer
                 test-ecc-original-registered-buffers ecc-buffer-registered-buffers-alist
                 test-ecc-original-active-buffer ecc-buffer-current-active-buffer)
           
           ;; Set up test environment
           (setq ecc-auto-timer nil
                 ecc-buffer-registered-buffers-alist nil
                 ecc-buffer-current-active-buffer nil)
           
           ;; Set up mock vterm-mode buffer 
           (with-current-buffer test-buffer
             (let ((vterm-mode-hook nil))
               (setq major-mode 'vterm-mode)
               (setq mode-name "vterm")))
           
           ;; Register test buffer
           (ecc-buffer-register-buffer test-buffer)
           (setq ecc-buffer-current-active-buffer test-buffer)
           
           ;; Run test body
           ,@body)
       
       ;; Clean up
       (when (buffer-live-p test-buffer)
         (kill-buffer test-buffer))
       
       ;; Restore original state
       (setq ecc-auto-timer test-ecc-original-timer
             ecc-buffer-registered-buffers-alist test-ecc-original-registered-buffers
             ecc-buffer-current-active-buffer test-ecc-original-active-buffer))))

(ert-deftest test-ecc-auto-mode-module-loadable ()
  "Test that ecc-auto module loads properly."
  (should (featurep 'ecc-auto)))

(ert-deftest test-ecc-auto-mode-enables-auto-accept ()
  "Test that ecc-auto-mode properly sets ecc-auto-accept."
  (with-mock-notifications
   (let ((ecc-auto-mode nil)
         (ecc-auto-accept nil))
     ;; Enable auto mode
     (ecc-auto-mode 1)
     (should ecc-auto-mode)
     (should ecc-auto-accept)
     
     ;; Disable auto mode
     (ecc-auto-mode -1)
     (should-not ecc-auto-mode)
     (should-not ecc-auto-accept))))

(ert-deftest test-ecc-auto-mode-sends-notifications ()
  "Test that ecc-auto-mode sends notifications when toggled."
  (with-mock-notifications
   (let ((ecc-auto-mode nil))
     ;; Enable auto mode
     (ecc-auto-mode 1)
     (should (member 'on test-ecc-auto-notifications))
     
     ;; Clear notifications
     (setq test-ecc-auto-notifications '())
     
     ;; Disable auto mode
     (ecc-auto-mode -1)
     (should (member 'off test-ecc-auto-notifications)))))

(ert-deftest test-ecc-auto-toggle-function ()
  "Test ecc-auto-toggle function."
  (with-ecc-auto-test-env
   (with-mock-notifications
    ;; Mock vterm-update-functions
    (let ((vterm-update-functions '()))
      ;; First toggle (on)
      (should (ecc-auto-toggle))
      (should (member 'ecc-send-accept vterm-update-functions))
      (should ecc-auto-timer)
      
      ;; Second toggle (off)
      (should-not (ecc-auto-toggle))
      (should-not (member 'ecc-send-accept vterm-update-functions))
      (should-not ecc-auto-timer)))))

(ert-deftest test-ecc-auto-check-and-restart-function ()
  "Test the auto-check function."
  (with-ecc-auto-test-env
   (let ((vterm-update-functions '()))
     ;; Set up test conditions
     (add-hook 'vterm-update-functions 'ecc-send-accept)
     
     ;; Mock vterm-send-string and vterm-send-return
     (cl-letf (((symbol-function 'vterm-send-string) #'ignore)
               ((symbol-function 'vterm-send-return) #'ignore)
               ((symbol-function 'vterm-copy-mode) #'ignore)
               ((symbol-function 'ecc-update-mode-line-all-buffers) #'ignore)
               ((symbol-function '--ecc-state-y/y/n-p) (lambda () t)))
       
       ;; Run the check function - should handle prompt
       (--ecc-auto-check-and-restart)
       
       ;; Verify hook is still active
       (should (member 'ecc-send-accept vterm-update-functions))))))

(ert-deftest test-ecc-auto-send-notification-functions ()
  "Test that auto-send functions send notifications."
  (with-ecc-auto-test-env
   (with-mock-notifications
    ;; Mock state detection and sending functions
    (cl-letf (((symbol-function '--ecc-send-string) #'ignore)
              ((symbol-function 'vterm-send-return) #'ignore)
              ((symbol-function '--ecc-state-y/n-p) (lambda () t))
              ((symbol-function '--ecc-state-y/y/n-p) (lambda () t))
              ((symbol-function '--ecc-state-waiting-p) (lambda () t))
              ((symbol-function '--ecc-state-initial-waiting-p) (lambda () nil))
              ((symbol-function 'ecc-state-get) (lambda () :y/n)))
      
      ;; Test Y/N auto-send with notification
      (--ecc-auto-send-1-y/n)
      (should (assoc 'completion test-ecc-auto-notifications))
      (should (equal (cdr (car test-ecc-auto-notifications)) "Y/N"))
      
      ;; Clear notifications
      (setq test-ecc-auto-notifications '())
      
      ;; Test Y/Y/N auto-send with notification
      (--ecc-auto-send-2-y/y/n)
      (should (assoc 'completion test-ecc-auto-notifications))
      (should (equal (cdr (car test-ecc-auto-notifications)) "Y/Y/N"))
      
      ;; Clear notifications
      (setq test-ecc-auto-notifications '())
      
      ;; Test continue auto-send with notification
      (--ecc-auto-send-continue-on-y/y/n)
      (should (assoc 'completion test-ecc-auto-notifications))
      (should (equal (cdr (car test-ecc-auto-notifications)) "waiting/continue"))))))

(provide 'test-ecc-auto)

(when (not load-file-name)
  (message "test-ecc-auto.el loaded: %s"
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))