;;; -*- coding: utf-8; lexical-binding: t -*-
;;; test-ecc-vterm-grayscale.el --- Tests for grayscale vterm mode
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 15:45:00>

;;; Commentary:
;;; Tests for the grayscale mode functionality in vterm buffers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ecc-vterm-grayscale)

(ert-deftest ecc-test-grayscale-toggle ()
  "Test grayscale toggle function."
  (skip-unless (featurep 'vterm))
  
  ;; Use cl-letf to mock vterm functions for testing outside of a real vterm buffer
  (cl-letf (((symbol-function 'derived-mode-p)
             (lambda (mode) (eq mode 'vterm-mode)))
            ((symbol-function 'vterm-reset-cursor-point)
             (lambda () t)))
    
    ;; Test initial state
    (should-not ecc-vterm-grayscale-mode)
    
    ;; Test toggle on
    (ecc-vterm-grayscale-toggle)
    (should ecc-vterm-grayscale-mode)
    (should (local-variable-p 'vterm-colors-inhibit))
    (should (local-variable-p 'ansi-colors-disabled))
    
    ;; Test toggle off
    (ecc-vterm-grayscale-toggle)
    (should-not ecc-vterm-grayscale-mode)))

(ert-deftest ecc-test-grayscale-minor-mode ()
  "Test grayscale minor mode."
  (skip-unless (featurep 'vterm))
  
  ;; Mock derived-mode-p and vterm-reset-cursor-point
  (cl-letf (((symbol-function 'derived-mode-p)
             (lambda (mode) (eq mode 'vterm-mode)))
            ((symbol-function 'vterm-reset-cursor-point)
             (lambda () t)))
    
    ;; Test enabling the minor mode
    (ecc-vterm-grayscale-minor-mode 1)
    (should ecc-vterm-grayscale-minor-mode)
    (should ecc-vterm-grayscale-mode)
    (should (local-variable-p 'vterm-colors-inhibit))
    (should (local-variable-p 'ansi-colors-disabled))
    
    ;; Test disabling the minor mode
    (ecc-vterm-grayscale-minor-mode -1)
    (should-not ecc-vterm-grayscale-minor-mode)
    (should-not ecc-vterm-grayscale-mode)))

(ert-deftest ecc-test-grayscale-auto-enable ()
  "Test auto-enable functionality."
  (skip-unless (featurep 'vterm))
  
  ;; Mock functions for testing
  (cl-letf (((symbol-function 'derived-mode-p)
             (lambda (mode) (eq mode 'vterm-mode)))
            ((symbol-function 'buffer-name)
             (lambda () "*CLAUDE-TEST*"))
            ((symbol-function 'vterm-reset-cursor-point)
             (lambda () t)))
    
    ;; Test with auto-enable off
    (let ((ecc-vterm-grayscale-auto-enable nil)
          (ecc-vterm-grayscale-default nil))
      (setq-local ecc-vterm-grayscale-mode nil)
      (ecc-vterm-grayscale-maybe-enable)
      (should-not ecc-vterm-grayscale-mode))
    
    ;; Test with auto-enable on
    (let ((ecc-vterm-grayscale-auto-enable t)
          (ecc-vterm-grayscale-default nil))
      (setq-local ecc-vterm-grayscale-mode nil)
      (ecc-vterm-grayscale-maybe-enable)
      (should ecc-vterm-grayscale-mode))
    
    ;; Test with default on
    (let ((ecc-vterm-grayscale-auto-enable nil)
          (ecc-vterm-grayscale-default t))
      (setq-local ecc-vterm-grayscale-mode nil)
      (ecc-vterm-grayscale-maybe-enable)
      (should ecc-vterm-grayscale-mode))))

(provide 'test-ecc-vterm-grayscale)

;;; test-ecc-vterm-grayscale.el ends here