;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 16:30:45>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-buffer-variables.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ert)
(require 'ecc-variables)
(require 'ecc-buffer-variables)

(ert-deftest test-ecc-buffer-variables-loadable ()
  "Test that ecc-buffer-variables loads properly."
  (should (featurep 'ecc-buffer-variables)))

(ert-deftest test-ecc-buffer-registered-buffers-alist-exists ()
  "Test that ecc-buffer-registered-buffers-alist variable exists."
  (should (boundp 'ecc-buffer-registered-buffers-alist)))

(ert-deftest test-ecc-state-available-states-exists ()
  "Test that ecc-state-available-states variable exists."
  (should (boundp 'ecc-state-available-states)))

(ert-deftest test-ecc-buffer-current-buffer-exists ()
  "Test that ecc-buffer-current-buffer variable exists."
  (should (boundp 'ecc-buffer-current-buffer)))

(ert-deftest test-ecc-buffer-property-defaults-exists ()
  "Test that ecc-buffer-property-defaults variable exists."
  (should (boundp 'ecc-buffer-property-defaults)))

(ert-deftest test-ecc-buffer-registered-buffers-alist-initialization ()
  "Test that ecc-buffer-registered-buffers-alist is initialized properly."
  (should (listp ecc-buffer-registered-buffers-alist)))

(ert-deftest test-ecc-state-available-states-values ()
  "Test that ecc-state-available-states contains expected values."
  (should (listp ecc-state-available-states))
  (should (memq nil ecc-state-available-states))
  (should (memq 'ready ecc-state-available-states))
  (should (memq 'waiting ecc-state-available-states))
  (should (memq 'y/n ecc-state-available-states))
  (should (memq 'y/y/n ecc-state-available-states)))

(ert-deftest test-ecc-buffer-current-buffer-initialization ()
  "Test that ecc-buffer-current-buffer is initialized to nil."
  (let ((orig-buffer ecc-buffer-current-buffer))
    (unwind-protect
        (progn
          ;; Explicitly reset the variable for this test
          (setq ecc-buffer-current-buffer nil)
          (should (eq nil ecc-buffer-current-buffer)))
      ;; Restore the original value after the test
      (setq ecc-buffer-current-buffer orig-buffer))))

(ert-deftest test-ecc-buffer-property-defaults-structure ()
  "Test that ecc-buffer-property-defaults has the expected structure."
  (should (listp ecc-buffer-property-defaults))
  (should (assq 'role ecc-buffer-property-defaults))
  (should (assq 'project ecc-buffer-property-defaults))
  (should (string= "assistant" (cdr (assq 'role ecc-buffer-property-defaults))))
  (should (eq nil (cdr (assq 'project ecc-buffer-property-defaults)))))

(provide 'test-ecc-buffer-variables)

(when
    (not load-file-name)
  (message "test-ecc-buffer-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))