;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 00:00:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-term/test-ecc-term-claude-auto.el

;;; Commentary:
;;; Tests for the ecc-term-claude-auto module.

(require 'ert)
(require 'ecc-term-claude-auto)
(require 'ecc-term-claude-state)
(require 'ecc-variables)

;;; Code:

;; Mock functions to avoid actual vterm interaction during tests
(defvar ecc-test-last-string-sent nil
  "Last string sent to vterm in tests.")

(defvar ecc-test-return-pressed nil
  "Whether return was pressed in tests.")

(defun ecc-test-setup-mock-vterm ()
  "Set up mock vterm functions for testing."
  (setq ecc-test-last-string-sent nil
        ecc-test-return-pressed nil)
  
  (cl-letf (((symbol-function 'vterm-send-string)
             (lambda (str)
               (setq ecc-test-last-string-sent str)))
            ((symbol-function 'vterm-send-return)
             (lambda ()
               (setq ecc-test-return-pressed t))))
    (yield)))

(defmacro with-mock-vterm (&rest body)
  "Execute BODY with mock vterm functions."
  (declare (indent 0))
  `(progn
     (ecc-test-setup-mock-vterm)
     (cl-letf (((symbol-function 'vterm-send-string)
                (lambda (str)
                  (setq ecc-test-last-string-sent str)))
               ((symbol-function 'vterm-send-return)
                (lambda ()
                  (setq ecc-test-return-pressed t))))
       ,@body)))

;; Test custom response variables
(defvar ecc-test-auto-response-y/n "y"
  "Test response for Y/N prompts.")

(defvar ecc-test-auto-response-y/y/n "y"
  "Test response for Y/Y/N prompts.")

(defvar ecc-test-auto-response-waiting ""
  "Test response for waiting prompts.")

(defvar ecc-test-auto-response-initial-waiting ""
  "Test response for initial waiting prompts.")

(ert-deftest test-ecc-term-claude-auto-send-y-n ()
  "Test sending auto-response for Y/N state."
  (with-mock-vterm
    (let ((ecc-auto-response-y/n "y")) 
      (should (ecc-term-claude-auto-send :y/n))
      (should (string= ecc-test-last-string-sent "y"))
      (should ecc-test-return-pressed))))

(ert-deftest test-ecc-term-claude-auto-send-y-y-n ()
  "Test sending auto-response for Y/Y/N state."
  (with-mock-vterm
    (let ((ecc-auto-response-y/y/n "y+")) 
      (should (ecc-term-claude-auto-send :y/y/n))
      (should (string= ecc-test-last-string-sent "y+"))
      (should ecc-test-return-pressed))))

(ert-deftest test-ecc-term-claude-auto-send-waiting ()
  "Test sending auto-response for waiting state."
  (with-mock-vterm
    (let ((ecc-auto-response-waiting "")) 
      (should (ecc-term-claude-auto-send :waiting))
      (should (string= ecc-test-last-string-sent ""))
      (should ecc-test-return-pressed))))

(ert-deftest test-ecc-term-claude-auto-send-invalid-state ()
  "Test sending auto-response with invalid state."
  (should-error (ecc-term-claude-auto-send :invalid-state)))

(ert-deftest test-ecc-term-claude-auto-send-accept ()
  "Test the auto-response coordinator function."
  (with-mock-vterm
    (let ((ecc-term-claude-auto-mode t)
          (ecc-auto-response-y/n "y"))
      ;; Mock state detection to always return :y/n
      (cl-letf (((symbol-function 'ecc-term-claude-get-state)
                 (lambda (&optional _) :y/n)))
        (ecc-term-claude-auto-send-accept)
        (should (string= ecc-test-last-string-sent "y"))
        (should ecc-test-return-pressed)))))

(ert-deftest test-ecc-term-claude-auto-send-accept-disabled ()
  "Test the auto-response coordinator function with auto-mode disabled."
  (with-mock-vterm
    (let ((ecc-term-claude-auto-mode nil)
          (ecc-auto-response-y/n "y"))
      ;; Mock state detection to always return :y/n
      (cl-letf (((symbol-function 'ecc-term-claude-get-state)
                 (lambda (&optional _) :y/n)))
        (ecc-term-claude-auto-send-accept)
        ;; Should not send anything when auto-mode is disabled
        (should (eq ecc-test-last-string-sent nil))
        (should (eq ecc-test-return-pressed nil))))))

(ert-deftest test-ecc-term-claude-toggle-auto-mode ()
  "Test toggling the auto-mode."
  (let ((ecc-term-claude-auto-mode nil)
        (ecc-term-claude-update-functions nil))
    ;; First toggle should enable
    (ecc-term-claude-toggle-auto-mode)
    (should ecc-term-claude-auto-mode)
    (should (memq 'ecc-term-claude-auto-send-accept ecc-term-claude-update-functions))
    
    ;; Second toggle should disable
    (ecc-term-claude-toggle-auto-mode)
    (should-not ecc-term-claude-auto-mode)
    (should-not (memq 'ecc-term-claude-auto-send-accept ecc-term-claude-update-functions))))

(ert-deftest test-ecc-term-claude-auto-send-y/n-compat ()
  "Test backward compatibility function."
  (with-mock-vterm
    (let ((ecc-auto-response-y/n "y"))
      ;; Use the compatibility function
      (ecc-term-claude-auto-send-y/n)
      (should (string= ecc-test-last-string-sent "y"))
      (should ecc-test-return-pressed))))

(provide 'test-ecc-term-claude-auto)

;;; test-ecc-term-claude-auto.el ends here