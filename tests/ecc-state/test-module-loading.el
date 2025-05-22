;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 21:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-state/test-module-loading.el

;;; Commentary:
;;; Test file to verify module loading works correctly

(require 'ert)

;;; Code:

;; Add the src directory to the load path
(add-to-list 'load-path 
             (expand-file-name "../src"
                              (file-name-directory
                               (or load-file-name buffer-file-name))))

;; Test that variables can be loaded
(ert-deftest test-ecc-variables-loads ()
  "Test that ecc-variables.el loads successfully."
  (require 'ecc-variables-consolidated)
  (should (boundp 'ecc-state-prompt-waiting)))

;; Test that state detection module loads
(ert-deftest test-ecc-state-detection-loads ()
  "Test that ecc-state-detection.el loads successfully."
  (require 'ecc-state-detection-consolidated)
  (should (fboundp 'ecc-detect-state)))

;; Test that buffer state module loads
(ert-deftest test-ecc-buffer-state-loads ()
  "Test that ecc-buffer-state.el loads successfully."
  (should
   (progn
     (require 'ecc-buffer-state)
     (fboundp 'ecc-buffer-state-get-prompt))))

;; Test that buffer state can access functions from state detection
(ert-deftest test-buffer-state-detects-state ()
  "Test that ecc-buffer-state can use state detection functions."
  (require 'ecc-buffer-state)
  (should (fboundp 'ecc-buffer-state-detect-and-update))
  ;; This function internally uses ecc-detect-state
  (should (fboundp 'ecc-detect-state)))

(provide 'test-module-loading)

;;; test-module-loading.el ends here