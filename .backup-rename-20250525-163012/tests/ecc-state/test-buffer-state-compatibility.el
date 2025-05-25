;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 22:05:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-state/test-buffer-state-compatibility.el

;;; Commentary:
;;; Compatibility tests for buffer state module aliases and legacy functions

(require 'ert)

;;; Code:

;; Add the src directory to the load path
(add-to-list 'load-path 
             (expand-file-name "../../src"
                              (file-name-directory
                               (or load-file-name buffer-file-name))))

(require 'ecc-variables)
(require 'ecc-state-detection)
(require 'ecc-buffer-local)
(require 'ecc-buffer-state)

;; Test backward compatibility aliases
(ert-deftest test-buffer-state-update-alias ()
  "Test that ecc-buffer-state-update alias works correctly."
  (with-temp-buffer
    (ecc-buffer-state-init)
    ;; Use the alias
    (ecc-buffer-state-update :y/n)
    ;; Verify it worked
    (should (eq (ecc-buffer-state-get-prompt) :y/n))))

(ert-deftest test-buffer-get-prompt-state-alias ()
  "Test that ecc-buffer-get-prompt-state alias works correctly."
  (with-temp-buffer
    (ecc-buffer-state-init)
    (ecc-buffer-state-update-prompt :y/n)
    ;; Use the alias
    (should (eq (ecc-buffer-get-prompt-state) :y/n))))

(ert-deftest test-buffer-state-detect-alias ()
  "Test that ecc-buffer-state-detect alias works correctly."
  (with-temp-buffer
    (ecc-buffer-state-init)
    ;; Mock the detection for testing
    (cl-letf (((symbol-function 'ecc-detect-state)
               (lambda () :y/n)))
      ;; Use the alias
      (ecc-buffer-state-detect)
      ;; Verify it worked
      (should (eq (ecc-buffer-state-get-prompt) :y/n)))))

;; Test deprecated legacy functions
(ert-deftest test-ecc-update-buffer-state-legacy ()
  "Test that ecc-update-buffer-state legacy function works correctly."
  (with-temp-buffer
    (ecc-buffer-state-init)
    ;; Use the legacy function with warning suppressed
    (let ((byte-compile-warnings nil))
      (ecc-update-buffer-state :y/n))
    ;; Verify it worked
    (should (eq (ecc-buffer-state-get-prompt) :y/n))))

(ert-deftest test-ecc-get-buffer-state-legacy ()
  "Test that ecc-get-buffer-state legacy function works correctly."
  (with-temp-buffer
    (ecc-buffer-state-init)
    (ecc-buffer-state-update-prompt :y/n)
    ;; Use the legacy function with warning suppressed
    (let ((byte-compile-warnings nil))
      (should (eq (ecc-get-buffer-state) :y/n)))))

(provide 'test-buffer-state-compatibility)

;;; test-buffer-state-compatibility.el ends here