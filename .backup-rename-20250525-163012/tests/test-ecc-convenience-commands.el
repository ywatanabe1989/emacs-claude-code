;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-25 03:50:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-convenience-commands.el

;;; Commentary:
;;; Tests for ecc-convenience-commands.el

(require 'ert)

;; Add the src directory to load path
(add-to-list 'load-path
             (expand-file-name "../src"
                              (file-name-directory
                               (or load-file-name buffer-file-name))))

(require 'ecc-convenience-commands)

;; Test fixtures
(defvar test-ecc-messages nil
  "List to capture messages during tests.")

(defvar test-ecc-orig-fboundp (symbol-function 'fboundp)
  "Original fboundp function.")

(defun test-ecc-mock-message (format-string &rest args)
  "Mock message function to capture messages."
  (push (apply #'format format-string args) test-ecc-messages))

(defun test-ecc-reset-messages ()
  "Reset captured messages."
  (setq test-ecc-messages nil))

;; Tests for ecc-vterm

(ert-deftest test-ecc-vterm-with-ecc-term-claude ()
  "Test ecc-vterm when ecc-term-claude is available."
  (test-ecc-reset-messages)
  (let ((ecc-term-claude-called nil))
    ;; Mock ecc-term-claude
    (cl-letf (((symbol-function 'ecc-term-claude)
               (lambda () (setq ecc-term-claude-called t)))
              ((symbol-function 'fboundp)
               (lambda (sym)
                 (if (eq sym 'ecc-term-claude) t
                   (funcall test-ecc-orig-fboundp sym)))))
      
      (ecc-vterm)
      (should ecc-term-claude-called))))

(ert-deftest test-ecc-vterm-fallback-to-vterm ()
  "Test ecc-vterm fallback when only vterm is available."
  (test-ecc-reset-messages)
  (let ((vterm-called nil))
    ;; Mock vterm and fboundp
    (cl-letf (((symbol-function 'vterm)
               (lambda () 
                 (setq vterm-called t)
                 (get-buffer-create "*vterm-test*")))
              ((symbol-function 'message) #'test-ecc-mock-message)
              ((symbol-function 'fboundp)
               (lambda (sym)
                 (cond ((eq sym 'ecc-term-claude) nil)
                       ((eq sym 'vterm) t)
                       (t (funcall test-ecc-orig-fboundp sym))))))
      
      (ecc-vterm)
      (should vterm-called)
      (should (member "Starting basic vterm (Claude optimizations not available)"
                      test-ecc-messages)))))

(ert-deftest test-ecc-vterm-no-vterm-available ()
  "Test ecc-vterm when neither ecc-term-claude nor vterm is available."
  (test-ecc-reset-messages)
  (cl-letf (((symbol-function 'fboundp)
             (lambda (sym) nil))
            ((symbol-function 'message) #'test-ecc-mock-message))
    
    (ecc-vterm)
    (should (member "Claude vterm mode not available" test-ecc-messages))))

;; Tests for ecc-optimize-vterm

(ert-deftest test-ecc-optimize-vterm-success ()
  "Test ecc-optimize-vterm in a vterm buffer."
  (test-ecc-reset-messages)
  (with-temp-buffer
    ;; Mock being in vterm-mode
    (setq major-mode 'vterm-mode)
    (let ((ecc-vterm-mode-called nil))
      (cl-letf (((symbol-function 'ecc-vterm-mode)
                 (lambda (arg) (setq ecc-vterm-mode-called arg)))
                ((symbol-function 'fboundp)
                 (lambda (sym)
                   (if (eq sym 'ecc-vterm-mode) t
                     (funcall test-ecc-orig-fboundp sym))))
                ((symbol-function 'derived-mode-p)
                 (lambda (mode) (eq mode 'vterm-mode)))
                ((symbol-function 'message) #'test-ecc-mock-message))
        
        (ecc-optimize-vterm)
        (should (equal ecc-vterm-mode-called 1))
        (should (member "Vterm buffer optimized for Claude" test-ecc-messages))))))

(ert-deftest test-ecc-optimize-vterm-not-in-vterm ()
  "Test ecc-optimize-vterm when not in a vterm buffer."
  (test-ecc-reset-messages)
  (with-temp-buffer
    ;; Not in vterm-mode
    (setq major-mode 'fundamental-mode)
    (cl-letf (((symbol-function 'fboundp)
               (lambda (sym)
                 (if (eq sym 'ecc-vterm-mode) t
                   (funcall test-ecc-orig-fboundp sym))))
              ((symbol-function 'derived-mode-p)
               (lambda (mode) nil)))
      
      (should-error (ecc-optimize-vterm) :type 'user-error))))

(ert-deftest test-ecc-optimize-vterm-mode-not-available ()
  "Test ecc-optimize-vterm when ecc-vterm-mode is not available."
  (test-ecc-reset-messages)
  (cl-letf (((symbol-function 'fboundp)
             (lambda (sym) nil))
            ((symbol-function 'message) #'test-ecc-mock-message))
    
    (ecc-optimize-vterm)
    (should (member "ecc-vterm-mode not available" test-ecc-messages))))

;; Tests for ecc-auto-respond

(ert-deftest test-ecc-auto-respond-success ()
  "Test ecc-auto-respond when functions are available."
  (test-ecc-reset-messages)
  (let ((register-called nil)
        (start-called nil))
    (cl-letf (((symbol-function 'ecc-register-buffer)
               (lambda () (setq register-called t)))
              ((symbol-function 'ecc-start-auto-response)
               (lambda () (setq start-called t)))
              ((symbol-function 'fboundp)
               (lambda (sym)
                 (if (eq sym 'ecc-start-auto-response) t
                   (funcall test-ecc-orig-fboundp sym))))
              ((symbol-function 'message) #'test-ecc-mock-message))
      
      (ecc-auto-respond)
      (should register-called)
      (should start-called)
      (should (member "Auto-response enabled for this buffer" test-ecc-messages)))))

(ert-deftest test-ecc-auto-respond-not-available ()
  "Test ecc-auto-respond when functions are not available."
  (test-ecc-reset-messages)
  (cl-letf (((symbol-function 'fboundp)
             (lambda (sym) nil))
            ((symbol-function 'message) #'test-ecc-mock-message))
    
    (ecc-auto-respond)
    (should (member "Auto-response functionality not available" test-ecc-messages))))

;; Tests for ecc-quick-auto-response

(ert-deftest test-ecc-quick-auto-response-success ()
  "Test ecc-quick-auto-response when functions are available."
  (test-ecc-reset-messages)
  (let ((auto-response-args nil)
        (register-called nil))
    (cl-letf (((symbol-function 'ecc-register-buffer)
               (lambda () (setq register-called t)))
              ((symbol-function 'ecc-start-auto-response)
               (lambda (&rest args) (setq auto-response-args args)))
              ((symbol-function 'fboundp)
               (lambda (sym)
                 (if (eq sym 'ecc-start-auto-response) t
                   (funcall test-ecc-orig-fboundp sym))))
              ((symbol-function 'message) #'test-ecc-mock-message))
      
      (ecc-quick-auto-response)
      ;; Should register buffer and call with specific args
      (should register-called)
      (should (equal auto-response-args '("1" "2" "/user:auto")))
      (should (member "Quick auto-response enabled with /user:auto continue" test-ecc-messages)))))

;; Tests for ecc-status

;; ecc-status function doesn't exist, removed test

(provide 'test-ecc-convenience-commands)

;;; test-ecc-convenience-commands.el ends here