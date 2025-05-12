;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-11 00:45:24>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/term/test-ecc-run-vterm.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'ecc-run-vterm)

;; Basic loadability tests
(ert-deftest test-ecc-run-vterm-loadable ()
  "Test that ecc-run-vterm can be loaded."
  (should (featurep 'ecc-run-vterm)))

(ert-deftest test-ecc-run-vterm-functions-defined ()
  "Test that runner functions are defined."
  (should (fboundp 'ecc-run-vterm-claude))
  (should (fboundp 'ecc-run-vterm-help)))

;; Test with mock vterm
(ert-deftest test-ecc-run-vterm-help-displays-help ()
  "Test that help function creates a help buffer."
  (when (get-buffer "*Claude VTerm Help*")
    (kill-buffer "*Claude VTerm Help*"))
  
  (ecc-run-vterm-help)
  (should (get-buffer "*Claude VTerm Help*"))
  
  (with-current-buffer "*Claude VTerm Help*"
    (should (string-match-p "Claude VTerm Mode Help" 
                            (buffer-substring-no-properties (point-min) (point-max))))
    (should (string-match-p "Key Bindings:" 
                            (buffer-substring-no-properties (point-min) (point-max))))
    (should (string-match-p "Mode Line Indicators:" 
                            (buffer-substring-no-properties (point-min) (point-max))))
    (should (string-match-p "Auto-Response Mode:" 
                            (buffer-substring-no-properties (point-min) (point-max)))))
  
  (when (get-buffer "*Claude VTerm Help*")
    (kill-buffer "*Claude VTerm Help*")))

;; Advanced tests that depend on vterm availability
(ert-deftest test-ecc-run-vterm-claude-creates-buffer ()
  "Test that ecc-run-vterm-claude creates a buffer when vterm is available."
  (skip-unless (and (fboundp 'ecc-claude-vterm)
                    (bound-and-true-p ecc-claude-vterm--vterm-available)))
  
  ;; Mock the vterm functions to avoid actually running vterm processes
  (require 'vterm-mock)
  (vterm-mock-reset)
  
  ;; Temporarily define a mock ecc-claude-vterm function
  (cl-letf (((symbol-function 'ecc-claude-vterm)
             (lambda ()
               (let ((buffer (generate-new-buffer "*MOCK-CLAUDE-VTERM*")))
                 (with-current-buffer buffer
                   (setq major-mode 'ecc-claude-vterm-mode))
                 buffer))))
    
    (let ((test-buffer (ecc-run-vterm-claude)))
      (unwind-protect
          (progn
            (should test-buffer)
            (should (buffer-live-p test-buffer))
            ;; Check the welcome message was sent
            (should (member "echo \"Welcome to Claude via VTerm Mode!\"\n" 
                           (reverse vterm-mock-history)))
            ;; Check that claude command was sent
            (should (member "claude\n" (reverse vterm-mock-history))))
        (when (and test-buffer (buffer-live-p test-buffer))
          (kill-buffer test-buffer))))))

(provide 'test-ecc-run-vterm)

(when (not load-file-name)
  (message "test-ecc-run-vterm.el loaded."))