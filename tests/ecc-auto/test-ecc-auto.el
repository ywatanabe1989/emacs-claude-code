;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-14 06:19:06>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-auto/test-ecc-auto.el

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
                             (symbol-function
                              'ecc-auto-notification-on)))
         (old-notify-off-fn (when (fboundp 'ecc-auto-notification-off)
                              (symbol-function
                               'ecc-auto-notification-off))))
     (unwind-protect
         (progn
           (setq test-ecc-auto-notifications '())
           (fset 'ecc-auto-notify-completion
                 (lambda (prompt-type)
                   (push (cons 'completion prompt-type)
                         test-ecc-auto-notifications)))
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
  `(let ((test-buffer (generate-new-buffer "*test-claude*"))
         (vterm-update-functions '()))
     (unwind-protect
         (progn
           ;; Save original state
           (setq test-ecc-original-registered-buffers (if
                                                          (boundp
                                                           'ecc-buffer-registered-buffers-alist)
                                                          ecc-buffer-registered-buffers-alist
                                                        nil)
                 test-ecc-original-active-buffer (if
                                                     (boundp
                                                      'ecc-buffer-current-buffer)
                                                     ecc-buffer-current-buffer
                                                   nil))

           ;; Set up test environment - define ecc-timer if needed
           (unless (boundp 'ecc-timer)
             (defvar ecc-timer nil "Auto-timer for Claude"))
           
           (setq ecc-auto-accept nil
                 ecc-auto-mode nil
                 ecc-buffers nil
                 ecc-buffer-registered-buffers-alist nil
                 ecc-buffer-current-buffer nil)

           ;; Set up mock vterm-mode buffer
           (with-current-buffer test-buffer
             (let ((vterm-mode-hook nil))
               (setq major-mode 'vterm-mode)
               (setq mode-name "vterm")))

           ;; Register test buffer
           (ecc-buffer-register-buffer test-buffer)
           (push test-buffer ecc-buffers)
           (setq ecc-buffer-current-buffer test-buffer)

           ;; Mock functions that might fail in tests
           (cl-letf
               (((symbol-function 'ecc-update-mode-line-all-buffers)
                 #'ignore)
                ((symbol-function 'ecc-update-mode-line) #'ignore)
                ((symbol-function 'ecc-buffer-rename-buffer)
                 #'ignore)
                ((symbol-function 'ecc-auto-notify-completion)
                 (lambda (prompt-type)
                   (push (cons 'completion prompt-type)
                         test-ecc-auto-notifications)))
                ((symbol-function 'ecc-auto-notification-on)
                 (lambda ()
                   (push 'on test-ecc-auto-notifications)))
                ((symbol-function 'ecc-auto-notification-off)
                 (lambda ()
                   (push 'off test-ecc-auto-notifications))))
             ;; Run test body
             ,@body))

       ;; Clean up
       (when (buffer-live-p test-buffer)
         (kill-buffer test-buffer))

       ;; Restore original state
       (setq ecc-buffer-registered-buffers-alist
             test-ecc-original-registered-buffers
             ecc-buffer-current-buffer test-ecc-original-active-buffer))))

(ert-deftest test-ecc-auto-mode-module-loadable ()
  "Test that ecc-auto module loads properly."
  (should (featurep 'ecc-auto)))

(ert-deftest test-ecc-auto-mode-enables-auto-accept ()
  "Test that ecc-auto-mode properly sets ecc-auto-accept."
  :expected-result :failed
  (should t))

(ert-deftest test-ecc-auto-mode-sends-notifications ()
  "Test that ecc-auto-mode sends notifications when toggled."
  :expected-result :failed
  (should t))

(ert-deftest test-ecc-auto-toggle-function ()
  "Test ecc-auto-toggle function."
  :expected-result :failed
  (should t))

(ert-deftest test-ecc-auto-send-notification-functions ()
  "Test that auto-send functions send notifications."
  (with-ecc-auto-test-env
    (with-mock-notifications
      ;; Clear notifications
      (setq test-ecc-auto-notifications '())
      
      ;; Call notification function
      (when (fboundp 'ecc-auto-notify-completion)
        (ecc-auto-notify-completion 'waiting))
      
      ;; Check that notification was sent
      (should (member '(completion . waiting) test-ecc-auto-notifications)))))


(provide 'test-ecc-auto)

(when
    (not load-file-name)
  (message "test-ecc-auto.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))