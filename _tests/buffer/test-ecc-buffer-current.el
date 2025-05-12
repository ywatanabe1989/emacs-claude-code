;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-09 01:25:42>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-buffer-current.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ert)
(require 'ecc-buffer-variables)
(require 'ecc-buffer-registry)
(require 'ecc-buffer-state)
(require 'ecc-buffer-current)

(ert-deftest test-ecc-buffer-current-loadable ()
  "Test that ecc-buffer-current loads properly."
  (should (featurep 'ecc-buffer-current)))

(ert-deftest test-ecc-buffer-current-buffer-variable-exists ()
  "Test that ecc-buffer-current-buffer variable exists."
  (should (boundp 'ecc-buffer-current-buffer)))

(ert-deftest test-ecc-buffer-set-current-buffer-function-exists ()
  "Test that ecc-buffer-set-current-buffer function exists."
  (should (fboundp 'ecc-buffer-set-current-buffer)))

(ert-deftest test-ecc-buffer-get-current-buffer-function-exists ()
  "Test that ecc-buffer-get-current-buffer function exists."
  (should (fboundp 'ecc-buffer-get-current-buffer)))

(ert-deftest test-ecc-buffer-get-current-buffer-state-function-exists
    ()
  "Test that ecc-buffer-get-current-buffer-state function exists."
  (should (fboundp 'ecc-buffer-get-current-buffer-state)))

(ert-deftest test-ecc-buffer-set-current-buffer-state-function-exists
    ()
  "Test that ecc-buffer-set-current-buffer-state function exists."
  (should (fboundp 'ecc-buffer-set-current-buffer-state)))

(ert-deftest test-ecc-buffer-set-and-get-current-buffer ()
  "Test setting and getting current buffer."
  (let ((original-current ecc-buffer-current-buffer)
        (original-registry ecc-buffer-registered-buffers-alist)
        (test-buffer (generate-new-buffer "*test-claude-current*")))
    (unwind-protect
        (progn
          ;; Setup clean test environment
          (setq ecc-buffer-registered-buffers-alist nil)
          (setq ecc-buffer-current-buffer nil)

          ;; Test with mock functions for registration
          (cl-letf (((symbol-function 'ecc-buffer-register-buffer)
                     (lambda (buf)
                       (add-to-list
                        'ecc-buffer-registered-buffers-alist
                        (cons buf nil))))
                    ((symbol-function
                      'ecc-update-mode-line-all-buffers)
                     #'ignore))

            ;; Set current buffer
            (should
             (eq test-buffer
                 (ecc-buffer-set-current-buffer test-buffer)))

            ;; Verify current buffer was set
            (should (eq ecc-buffer-current-buffer test-buffer))

            ;; Verify get returns the correct buffer
            (should (eq test-buffer (ecc-buffer-get-current-buffer)))

            ;; Test with nil buffer
            (should-not (ecc-buffer-set-current-buffer nil))

            ;; Test with dead buffer
            (kill-buffer test-buffer)
            (should-not (ecc-buffer-get-current-buffer))))

      ;; Cleanup
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer))
      (setq ecc-buffer-registered-buffers-alist original-registry)
      (setq ecc-buffer-current-buffer original-current))))

(ert-deftest test-ecc-buffer-current-buffer-state-functions ()
  "Test getting and setting buffer state through the current buffer."
  (let ((original-current ecc-buffer-current-buffer)
        (original-registry ecc-buffer-registered-buffers-alist)
        (test-buffer
         (generate-new-buffer "*test-claude-current-state*")))
    (unwind-protect
        (progn
          ;; Setup clean test environment
          (setq ecc-buffer-registered-buffers-alist nil)
          (setq ecc-buffer-current-buffer nil)

          ;; Test with mock functions
          (cl-letf (((symbol-function 'ecc-buffer-register-buffer)
                     (lambda (buf)
                       (add-to-list
                        'ecc-buffer-registered-buffers-alist
                        (cons buf nil))))
                    ((symbol-function 'ecc-buffer-get-buffer-state)
                     (lambda (buf)
                       (cdr
                        (assoc buf ecc-buffer-registered-buffers-alist))))
                    ((symbol-function 'ecc-buffer-set-buffer-state)
                     (lambda (buf state)
                       (setq ecc-buffer-registered-buffers-alist
                             (cons (cons buf state)
                                   (assoc-delete-all buf
                                                     ecc-buffer-registered-buffers-alist)))
                       t))
                    ((symbol-function
                      'ecc-update-mode-line-all-buffers)
                     #'ignore))

            ;; Set current buffer
            (ecc-buffer-set-current-buffer test-buffer)

            ;; Test state functions
            (should-not (ecc-buffer-get-current-buffer-state)) ; Initially nil
            (should (ecc-buffer-set-current-buffer-state 'waiting))
            (should
             (eq 'waiting (ecc-buffer-get-current-buffer-state)))

            ;; Test with dead buffer
            (kill-buffer test-buffer)
            (should-not (ecc-buffer-get-current-buffer-state))
            (should-not (ecc-buffer-set-current-buffer-state 'waiting))))

      ;; Cleanup
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer))
      (setq ecc-buffer-registered-buffers-alist original-registry)
      (setq ecc-buffer-current-buffer original-current))))


(provide 'test-ecc-buffer-current)

(when
    (not load-file-name)
  (message "test-ecc-buffer-current.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))