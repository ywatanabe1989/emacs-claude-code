;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 11:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-term/test-ecc-term-claude-buffer.el

;;; Commentary:
;;; Tests for the ecc-term-claude-buffer module.

(require 'ert)
(require 'ecc-term-claude-buffer)
(require 'ecc-api)

;;; Code:

(defmacro with-test-claude-buffer (&rest body)
  "Execute BODY with a temporary Claude buffer."
  (declare (indent 0))
  `(let ((temp-buffer (generate-new-buffer "*claude-test*")))
     (unwind-protect
         (progn ,@body)
       (when (buffer-live-p temp-buffer)
         (kill-buffer temp-buffer)))))

(defmacro with-test-setup (&rest body)
  "Execute BODY with clean test environment."
  (declare (indent 0))
  `(let ((ecc-buffer-registered-buffers-alist nil)
         (ecc-buffer-current-buffer nil))
     ,@body))

(ert-deftest test-ecc-term-claude-register-buffer ()
  "Test registering a buffer."
  (with-test-claude-buffer
    (with-test-setup
      (let ((buf temp-buffer))
        ;; Test registration
        (should (eq (ecc-term-claude-register-buffer buf) buf))
        (should (assoc buf ecc-buffer-registered-buffers-alist))
        (should (eq ecc-buffer-current-buffer buf))
        
        ;; Test idempotence
        (should (eq (ecc-term-claude-register-buffer buf) buf))
        (should (= (length ecc-buffer-registered-buffers-alist) 1))))))

(ert-deftest test-ecc-term-claude-unregister-buffer ()
  "Test unregistering a buffer."
  (with-test-claude-buffer
    (with-test-setup
      (let ((buf temp-buffer))
        ;; Register first
        (ecc-term-claude-register-buffer buf)
        (should (assoc buf ecc-buffer-registered-buffers-alist))
        
        ;; Test unregistration
        (should (ecc-term-claude-unregister-buffer buf))
        (should-not (assoc buf ecc-buffer-registered-buffers-alist))
        
        ;; Test idempotence
        (should-not (ecc-term-claude-unregister-buffer buf))))))

(ert-deftest test-ecc-term-claude-get-current-buffer ()
  "Test getting current buffer."
  (with-test-claude-buffer
    (with-test-setup
      (let ((buf temp-buffer))
        ;; No current buffer yet
        (should-not (ecc-term-claude-get-current-buffer))
        
        ;; Set current buffer
        (ecc-term-claude-register-buffer buf)
        (should (eq (ecc-term-claude-get-current-buffer) buf))))))

(ert-deftest test-ecc-term-claude-set-current-buffer ()
  "Test setting current buffer."
  (with-test-claude-buffer
    (with-test-setup
      (let ((buf temp-buffer))
        ;; Set current buffer
        (should (eq (ecc-term-claude-set-current-buffer buf) buf))
        (should (eq ecc-buffer-current-buffer buf))
        (should (assoc buf ecc-buffer-registered-buffers-alist))))))

(ert-deftest test-ecc-term-claude-list-buffers ()
  "Test listing Claude buffers."
  (with-test-setup
    (let ((buf1 (generate-new-buffer "*claude-test-1*"))
          (buf2 (generate-new-buffer "*claude-test-2*"))
          (buf3 (generate-new-buffer "*claude-test-3*")))
      (unwind-protect
          (progn
            ;; Register buffers
            (ecc-term-claude-register-buffer buf1)
            (ecc-term-claude-register-buffer buf2)
            (ecc-term-claude-register-buffer buf3)
            
            ;; Test listing
            (let ((buffers (ecc-term-claude-list-buffers)))
              (should (= (length buffers) 3))
              (should (memq buf1 buffers))
              (should (memq buf2 buffers))
              (should (memq buf3 buffers))))
        (when (buffer-live-p buf1) (kill-buffer buf1))
        (when (buffer-live-p buf2) (kill-buffer buf2))
        (when (buffer-live-p buf3) (kill-buffer buf3))))))

(ert-deftest test-ecc-term-claude-buffer-p ()
  "Test checking if buffer is a Claude buffer."
  (with-test-claude-buffer
    (with-test-setup
      (let ((buf temp-buffer))
        ;; Not registered yet
        (should-not (ecc-term-claude-buffer-p buf))
        
        ;; Register and check
        (ecc-term-claude-register-buffer buf)
        (should (ecc-term-claude-buffer-p buf))))))

(ert-deftest test-ecc-term-claude-cleanup-buffer ()
  "Test buffer cleanup."
  (with-test-claude-buffer
    (with-test-setup
      (let ((buf temp-buffer))
        ;; Setup buffer with timer
        (ecc-term-claude-register-buffer buf)
        (setq ecc-term-claude-state-timer (run-with-timer 100 nil #'ignore))
        
        ;; Test cleanup
        (with-current-buffer buf
          (ecc-term-claude-cleanup-buffer))
        
        ;; Check effects
        (should-not ecc-term-claude-state-timer)
        (should-not (assoc buf ecc-buffer-registered-buffers-alist))))))

(ert-deftest test-ecc-register-buffer-alias ()
  "Test backward compatibility alias."
  (with-test-claude-buffer
    (with-test-setup
      (let ((buf temp-buffer))
        ;; Test using alias
        (should (eq (ecc-register-buffer buf) buf))
        (should (assoc buf ecc-buffer-registered-buffers-alist))))))

(provide 'test-ecc-term-claude-buffer)

;;; test-ecc-term-claude-buffer.el ends here