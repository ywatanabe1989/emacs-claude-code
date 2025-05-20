;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 00:25:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-auto/test-ecc-auto-notify.el

;;; Commentary:
;;; Tests for the notification system (ecc-auto-notify.el).

(require 'ert)
(require 'ecc-variables)
(require 'ecc-auto-detect)
(require 'ecc-auto-notify)

;;; Code:

;; Test variables
(defvar test-ecc-auto-notify--flash-called nil)
(defvar test-ecc-auto-notify--ring-called nil)

;; Mock functions for testing
(defun test-ecc-auto-notify--mock-ring-bell ()
  "Mock bell function for testing."
  (setq test-ecc-auto-notify--ring-called t))

(defun test-ecc-auto-notify--mock-flash-mode-line ()
  "Mock flash function for testing."
  (setq test-ecc-auto-notify--flash-called t))

;; Tests
(ert-deftest test-ecc-auto-notify-prompt ()
  "Test notification for prompt detection."
  ;; Set up mock environment
  (let ((ecc-auto-notify-enabled t)
        (ecc-auto-notify-method 'both)
        (ecc-auto-notify-prompt-types '(:y/n :y/y/n :waiting :initial-waiting)))
    
    ;; Reset state
    (setq test-ecc-auto-notify--flash-called nil)
    (setq test-ecc-auto-notify--ring-called nil)
    
    ;; Temporarily override notification methods
    (cl-letf (((symbol-function 'ecc-auto-notify-ring-bell)
               #'test-ecc-auto-notify--mock-ring-bell)
              ((symbol-function 'ecc-auto-notify-flash-mode-line)
               #'test-ecc-auto-notify--mock-flash-mode-line))
      
      ;; Test Y/N notification
      (ecc-auto-notify-prompt :y/n)
      (should test-ecc-auto-notify--ring-called)
      (should test-ecc-auto-notify--flash-called)
      
      ;; Reset state for next test
      (setq test-ecc-auto-notify--flash-called nil)
      (setq test-ecc-auto-notify--ring-called nil)
      
      ;; Test waiting notification
      (ecc-auto-notify-prompt :waiting)
      (should test-ecc-auto-notify--ring-called)
      (should test-ecc-auto-notify--flash-called)
      
      ;; Test with disabled notifications
      (setq ecc-auto-notify-enabled nil)
      (setq test-ecc-auto-notify--flash-called nil)
      (setq test-ecc-auto-notify--ring-called nil)
      
      (ecc-auto-notify-prompt :y/n)
      (should-not test-ecc-auto-notify--ring-called)
      (should-not test-ecc-auto-notify--flash-called))))

(ert-deftest test-ecc-auto-notify-response ()
  "Test notification for auto-responses."
  ;; Capture message output
  (let ((ecc-auto-notify-enabled t)
        (last-message nil))
    
    ;; Override message function to capture output
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq last-message (apply #'format format-string args)))))
      
      ;; Test response notification
      (ecc-auto-notify-response :y/n "test-response")
      (should (string-match-p "Auto-responded to Y/N" last-message))
      (should (string-match-p "test-response" last-message))
      
      ;; Test with different state
      (setq last-message nil)
      (ecc-auto-notify-response :waiting "test-continue")
      (should (string-match-p "Auto-responded to Continue" last-message))
      (should (string-match-p "test-continue" last-message))
      
      ;; Test with disabled notifications
      (setq ecc-auto-notify-enabled nil)
      (setq last-message nil)
      (ecc-auto-notify-response :y/n "test-response")
      (should-not last-message))))

(ert-deftest test-ecc-auto-notify-methods ()
  "Test different notification methods."
  ;; Set up test environment
  (let ((ecc-auto-notify-enabled t)
        (ecc-auto-notify-prompt-types '(:y/n))
        (original-message-fn (symbol-function 'message)))
    
    ;; Test bell-only method
    (setq ecc-auto-notify-method 'bell)
    (setq test-ecc-auto-notify--ring-called nil)
    (setq test-ecc-auto-notify--flash-called nil)
    
    (cl-letf (((symbol-function 'ecc-auto-notify-ring-bell)
               #'test-ecc-auto-notify--mock-ring-bell)
              ((symbol-function 'ecc-auto-notify-flash-mode-line)
               #'test-ecc-auto-notify--mock-flash-mode-line))
      
      (ecc-auto-notify-prompt :y/n)
      (should test-ecc-auto-notify--ring-called)
      (should-not test-ecc-auto-notify--flash-called))
    
    ;; Test visual-only method
    (setq ecc-auto-notify-method 'visual)
    (setq test-ecc-auto-notify--ring-called nil)
    (setq test-ecc-auto-notify--flash-called nil)
    
    (cl-letf (((symbol-function 'ecc-auto-notify-ring-bell)
               #'test-ecc-auto-notify--mock-ring-bell)
              ((symbol-function 'ecc-auto-notify-flash-mode-line)
               #'test-ecc-auto-notify--mock-flash-mode-line))
      
      (ecc-auto-notify-prompt :y/n)
      (should-not test-ecc-auto-notify--ring-called)
      (should test-ecc-auto-notify--flash-called))
    
    ;; Test none method
    (setq ecc-auto-notify-method 'none)
    (setq test-ecc-auto-notify--ring-called nil)
    (setq test-ecc-auto-notify--flash-called nil)
    
    (cl-letf (((symbol-function 'ecc-auto-notify-ring-bell)
               #'test-ecc-auto-notify--mock-ring-bell)
              ((symbol-function 'ecc-auto-notify-flash-mode-line)
               #'test-ecc-auto-notify--mock-flash-mode-line))
      
      (ecc-auto-notify-prompt :y/n)
      (should-not test-ecc-auto-notify--ring-called)
      (should-not test-ecc-auto-notify--flash-called))))

(ert-deftest test-ecc-auto-notify-bell-methods ()
  "Test different bell notification methods."
  ;; Set up test environment
  (let ((ecc-auto-notify-bell-method 'visible-bell)
        (visible-bell-called nil)
        (ding-called nil))
    
    ;; Test visible-bell method
    (cl-letf (((symbol-function 'ding)
               (lambda (&optional arg)
                 (setq ding-called t)))
              ((symbol-function 'face-remap-add-relative)
               (lambda (&rest _) nil)))
      
      ;; Reset state
      (setq visible-bell-called nil)
      (setq ding-called nil)
      
      ;; Set visible-bell to capture call
      (let ((visible-bell t))
        (ecc-auto-notify-ring-bell)
        (should ding-called))
      
      ;; Test beep method
      (setq ecc-auto-notify-bell-method 'beep)
      (setq ding-called nil)
      
      (let ((visible-bell nil))
        (ecc-auto-notify-ring-bell)
        (should ding-called))
      
      ;; Test flash method
      (setq ecc-auto-notify-bell-method 'flash)
      (setq test-ecc-auto-notify--flash-called nil)
      
      (cl-letf (((symbol-function 'ecc-auto-notify-flash-mode-line)
                 #'test-ecc-auto-notify--mock-flash-mode-line))
        (ecc-auto-notify-ring-bell)
        (should test-ecc-auto-notify--flash-called)))))

(ert-deftest test-ecc-auto-notify-toggle ()
  "Test toggling notification functionality."
  ;; Ensure known starting state
  (setq ecc-auto-notify-enabled t)
  
  ;; Toggle off
  (ecc-auto-notify-toggle)
  (should-not ecc-auto-notify-enabled)
  
  ;; Toggle on
  (ecc-auto-notify-toggle)
  (should ecc-auto-notify-enabled))

(ert-deftest test-ecc-auto-notify-bell-toggle ()
  "Test toggling bell notification."
  ;; Test starting with bell
  (setq ecc-auto-notify-method 'bell)
  (ecc-auto-notify-bell-toggle)
  (should (eq ecc-auto-notify-method 'none))
  
  ;; Test starting with visual
  (setq ecc-auto-notify-method 'visual)
  (ecc-auto-notify-bell-toggle)
  (should (eq ecc-auto-notify-method 'both))
  
  ;; Test starting with both
  (setq ecc-auto-notify-method 'both)
  (ecc-auto-notify-bell-toggle)
  (should (eq ecc-auto-notify-method 'visual)))

(provide 'test-ecc-auto-notify)

;;; test-ecc-auto-notify.el ends here