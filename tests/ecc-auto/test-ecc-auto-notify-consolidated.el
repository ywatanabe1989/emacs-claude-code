;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 01:15:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-auto/test-ecc-auto-notify-consolidated.el

;;; Commentary:
;;; Test suite for the consolidated auto-notify module.
;;; These tests cover the notification functionality for Claude prompts
;;; including both global and buffer-local variants.

(require 'ert)
(require 'ecc-variables-consolidated)
(require 'ecc-state-detection-consolidated)
(require 'ecc-auto-notify-consolidated)

;;; Code:

;;; Test state checking functionality

(ert-deftest test-ecc-auto-notify-consolidated-check-state-basic ()
  "Test basic state checking functionality."
  ;; Set up test environment
  (let ((ecc-auto-notify-on-claude-prompt t)
        (ecc-auto-notify-prompt-types '(:waiting :y/n :y/y/n :initial-waiting))
        (ecc-auto-notify--last-state nil)
        (ecc-auto-notify--last-time 0)
        (ecc-auto-notify-interval 1.0)
        (notification-called nil)
        (notification-state nil))
    
    ;; Mock notification function
    (cl-letf (((symbol-function 'ecc-auto-notify-prompt)
               (lambda (state)
                 (setq notification-called t)
                 (setq notification-state state)))
              ((symbol-function 'ecc-debug-message)
               (lambda (&rest _) nil)))
      
      ;; Call function with valid state
      (ecc-auto-notify-check-state :waiting)
      
      ;; Verify notification was called
      (should notification-called)
      (should (eq notification-state :waiting))
      (should (eq ecc-auto-notify--last-state :waiting))
      (should (> ecc-auto-notify--last-time 0)))))

(ert-deftest test-ecc-auto-notify-consolidated-check-state-notifications-disabled ()
  "Test that notifications are not triggered when disabled."
  ;; Set up test environment
  (let ((ecc-auto-notify-on-claude-prompt nil)
        (ecc-auto-notify-prompt-types '(:waiting :y/n :y/y/n :initial-waiting))
        (ecc-auto-notify--last-state nil)
        (ecc-auto-notify--last-time 0)
        (ecc-auto-notify-interval 1.0)
        (notification-called nil))
    
    ;; Mock notification function
    (cl-letf (((symbol-function 'ecc-auto-notify-prompt)
               (lambda (state)
                 (setq notification-called t)))
              ((symbol-function 'ecc-debug-message)
               (lambda (&rest _) nil)))
      
      ;; Call function with valid state
      (ecc-auto-notify-check-state :waiting)
      
      ;; Verify notification was not called
      (should-not notification-called))))

(ert-deftest test-ecc-auto-notify-consolidated-check-state-not-in-prompt-types ()
  "Test that notifications are not triggered for states not in prompt-types."
  ;; Set up test environment
  (let ((ecc-auto-notify-on-claude-prompt t)
        (ecc-auto-notify-prompt-types '(:y/n :y/y/n)) ;; :waiting not included
        (ecc-auto-notify--last-state nil)
        (ecc-auto-notify--last-time 0)
        (ecc-auto-notify-interval 1.0)
        (notification-called nil))
    
    ;; Mock notification function
    (cl-letf (((symbol-function 'ecc-auto-notify-prompt)
               (lambda (state)
                 (setq notification-called t)))
              ((symbol-function 'ecc-debug-message)
               (lambda (&rest _) nil)))
      
      ;; Call function with state not in prompt-types
      (ecc-auto-notify-check-state :waiting)
      
      ;; Verify notification was not called
      (should-not notification-called))))

(ert-deftest test-ecc-auto-notify-consolidated-check-state-throttling ()
  "Test notification throttling based on time interval."
  ;; Set up test environment
  (let ((ecc-auto-notify-on-claude-prompt t)
        (ecc-auto-notify-prompt-types '(:waiting :y/n))
        (ecc-auto-notify--last-state :waiting)
        (ecc-auto-notify--last-time (float-time))
        (ecc-auto-notify-interval 10.0) ;; Long interval to ensure throttling
        (notification-called nil))
    
    ;; Mock notification function
    (cl-letf (((symbol-function 'ecc-auto-notify-prompt)
               (lambda (state)
                 (setq notification-called t)))
              ((symbol-function 'ecc-debug-message)
               (lambda (&rest _) nil)))
      
      ;; Call function with the same state - should be throttled
      (ecc-auto-notify-check-state :waiting)
      
      ;; Verify notification was not called due to throttling
      (should-not notification-called)
      
      ;; Reset for next test
      (setq notification-called nil)
      
      ;; Call function with different state - should override throttling
      (ecc-auto-notify-check-state :y/n)
      
      ;; Verify notification was called for new state despite interval
      (should notification-called))))

;;; Test prompt notification functionality

(ert-deftest test-ecc-auto-notify-consolidated-prompt-basic ()
  "Test basic prompt notification functionality."
  (let ((ecc-auto-notify-bell nil) ;; Disable bell for testing
        (ecc-auto-notify-flash nil) ;; Disable flash for testing
        (message-output nil))
    
    ;; Mock message function
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq message-output (apply #'format format-string args)))))
      
      ;; Test with various prompt types
      (ecc-auto-notify-prompt :waiting)
      (should (string-match-p "waiting for input" message-output))
      
      (ecc-auto-notify-prompt :y/n)
      (should (string-match-p "yes/no prompt" message-output))
      
      (ecc-auto-notify-prompt :y/y/n)
      (should (string-match-p "multi-choice prompt" message-output))
      
      (ecc-auto-notify-prompt :initial-waiting)
      (should (string-match-p "initial waiting for input" message-output))
      
      (ecc-auto-notify-prompt :unknown-state)
      (should (string-match-p "unknown-state" message-output)))))

(ert-deftest test-ecc-auto-notify-consolidated-prompt-with-bell ()
  "Test prompt notification with bell enabled."
  (let ((ecc-auto-notify-bell t)
        (ecc-auto-notify-flash nil)
        (bell-called nil))
    
    ;; Mock bell function
    (cl-letf (((symbol-function 'ecc-auto-notify-ring-bell)
               (lambda ()
                 (setq bell-called t)))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      
      ;; Test notification
      (ecc-auto-notify-prompt :waiting)
      
      ;; Verify bell was called
      (should bell-called))))

(ert-deftest test-ecc-auto-notify-consolidated-prompt-with-flash ()
  "Test prompt notification with mode line flash enabled."
  (let ((ecc-auto-notify-bell nil)
        (ecc-auto-notify-flash t)
        (flash-called nil))
    
    ;; Mock flash function
    (cl-letf (((symbol-function 'ecc-auto-notify-flash-mode-line)
               (lambda ()
                 (setq flash-called t)))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      
      ;; Test notification
      (ecc-auto-notify-prompt :waiting)
      
      ;; Verify flash was called
      (should flash-called))))

;;; Test bell notification functionality

(ert-deftest test-ecc-auto-notify-consolidated-ring-bell-audible ()
  "Test audible bell notification."
  (let ((ecc-auto-notify-bell-method 'audible)
        (ding-called nil))
    
    ;; Mock bell functions
    (cl-letf (((symbol-function 'ding)
               (lambda (&rest _)
                 (setq ding-called t)))
              ((symbol-function 'play-sound-file)
               (lambda (&rest _) nil)))
      
      ;; Call bell function
      (ecc-auto-notify-ring-bell)
      
      ;; Verify ding was called
      (should ding-called))))

(ert-deftest test-ecc-auto-notify-consolidated-ring-bell-visible ()
  "Test visible bell notification."
  (let ((ecc-auto-notify-bell-method 'visible)
        (ding-called nil)
        (visible-bell-value nil))
    
    ;; Mock bell functions
    (cl-letf (((symbol-function 'ding)
               (lambda (&rest _)
                 (setq ding-called t)
                 (setq visible-bell-value visible-bell))))
      
      ;; Call bell function
      (let ((visible-bell nil))
        (ecc-auto-notify-ring-bell))
      
      ;; Verify ding was called with visible-bell set to t
      (should ding-called)
      (should visible-bell-value))))

(ert-deftest test-ecc-auto-notify-consolidated-ring-bell-external ()
  "Test external command bell notification."
  (let ((ecc-auto-notify-bell-method 'external)
        (ecc-auto-notify-bell-external-command "echo 'bell'")
        (start-process-called nil)
        (shell-file-name-used nil)
        (shell-command-switch-used nil))
    
    ;; Mock process function
    (cl-letf (((symbol-function 'start-process)
               (lambda (name buffer program &rest args)
                 (setq start-process-called t)
                 (setq shell-file-name-used program)
                 (setq shell-command-switch-used (car args)))))
      
      ;; Call bell function
      (ecc-auto-notify-ring-bell)
      
      ;; Verify external command was called
      (should start-process-called)
      (should (string= shell-file-name-used shell-file-name))
      (should (string= shell-command-switch-used shell-command-switch)))))

;;; Test mode line flash functionality

(ert-deftest test-ecc-auto-notify-consolidated-flash-mode-line ()
  "Test mode line flash functionality."
  (let ((ecc-auto-notify--flash-timer nil)
        (ecc-auto-notify-bell-duration 0.3)
        (invert-face-called 0)
        (run-with-timer-called nil)
        (timer-duration nil))
    
    ;; Mock functions
    (cl-letf (((symbol-function 'invert-face)
               (lambda (_)
                 (setq invert-face-called (1+ invert-face-called))))
              ((symbol-function 'run-with-timer)
               (lambda (secs repeat function &rest args)
                 (setq run-with-timer-called t)
                 (setq timer-duration secs)
                 (funcall function))))
      
      ;; Call flash function
      (ecc-auto-notify-flash-mode-line)
      
      ;; Verify behavior
      (should run-with-timer-called)
      (should (= timer-duration 0.3))
      (should (= invert-face-called 2))))) ;; Once directly, once from the timer function

;;; Test notification toggle functionality

(ert-deftest test-ecc-auto-notify-consolidated-toggle ()
  "Test toggling notification functionality."
  (let ((ecc-auto-notify-on-claude-prompt nil)
        (message-output nil))
    
    ;; Mock message function
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq message-output (apply #'format format-string args)))))
      
      ;; Toggle notifications on
      (ecc-auto-notify-toggle)
      
      ;; Verify state changed
      (should ecc-auto-notify-on-claude-prompt)
      (should (string-match-p "enabled" message-output))
      
      ;; Toggle notifications off
      (ecc-auto-notify-toggle)
      
      ;; Verify state changed back
      (should-not ecc-auto-notify-on-claude-prompt)
      (should (string-match-p "disabled" message-output)))))

(ert-deftest test-ecc-auto-notify-consolidated-toggle-bell ()
  "Test toggling bell functionality."
  (let ((ecc-auto-notify-bell nil)
        (message-output nil))
    
    ;; Mock message function
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq message-output (apply #'format format-string args)))))
      
      ;; Toggle bell on
      (ecc-auto-notify-toggle-bell)
      
      ;; Verify state changed
      (should ecc-auto-notify-bell)
      (should (string-match-p "enabled" message-output))
      
      ;; Toggle bell off
      (ecc-auto-notify-toggle-bell)
      
      ;; Verify state changed back
      (should-not ecc-auto-notify-bell)
      (should (string-match-p "disabled" message-output)))))

(ert-deftest test-ecc-auto-notify-consolidated-toggle-flash ()
  "Test toggling flash functionality."
  (let ((ecc-auto-notify-flash nil)
        (message-output nil))
    
    ;; Mock message function
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq message-output (apply #'format format-string args)))))
      
      ;; Toggle flash on
      (ecc-auto-notify-toggle-flash)
      
      ;; Verify state changed
      (should ecc-auto-notify-flash)
      (should (string-match-p "enabled" message-output))
      
      ;; Toggle flash off
      (ecc-auto-notify-toggle-flash)
      
      ;; Verify state changed back
      (should-not ecc-auto-notify-flash)
      (should (string-match-p "disabled" message-output)))))

;;; Test buffer setup functionality

(ert-deftest test-ecc-auto-notify-consolidated-setup-for-buffer ()
  "Test notification setup for buffer."
  (let ((mode-hooks nil)
        (derived-mode 'ecc-term-claude-mode))
    
    ;; Mock functions
    (cl-letf (((symbol-function 'derived-mode-p)
               (lambda (&rest modes)
                 (memq derived-mode modes)))
              ((symbol-function 'add-hook)
               (lambda (hook function &optional _append _local)
                 (push (cons hook function) mode-hooks))))
      
      ;; Call setup function
      (ecc-auto-notify-setup-for-buffer)
      
      ;; Verify hooks were added
      (should (= (length mode-hooks) 1))
      (should (eq (caar mode-hooks) 'ecc-term-claude-update-functions)))))

;;; Test buffer-local functionality

(ert-deftest test-ecc-auto-notify-consolidated-buffer-local-init ()
  "Test buffer-local notification initialization."
  (with-temp-buffer
    (let ((ecc-auto-notify-on-claude-prompt t)
          (ecc-auto-notify-bell t)
          (ecc-auto-notify-flash t))
      
      ;; Initialize buffer-local settings
      (ecc-auto-notify-buffer-local-init)
      
      ;; Verify buffer-local variables were set
      (should (boundp 'ecc-buffer-auto-notify-enabled))
      (should ecc-buffer-auto-notify-enabled)
      (should (boundp 'ecc-buffer-auto-notify-bell))
      (should ecc-buffer-auto-notify-bell)
      (should (boundp 'ecc-buffer-auto-notify-flash))
      (should ecc-buffer-auto-notify-flash))))

(ert-deftest test-ecc-auto-notify-consolidated-buffer-local-toggle ()
  "Test toggling buffer-local notifications."
  (with-temp-buffer
    ;; Initialize buffer-local settings
    (setq-local ecc-buffer-auto-notify-enabled nil)
    
    ;; Mock message function
    (let ((message-output nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq message-output (apply #'format format-string args)))))
        
        ;; Toggle buffer-local notifications on
        (ecc-auto-notify-buffer-local-toggle)
        
        ;; Verify state changed
        (should ecc-buffer-auto-notify-enabled)
        (should (string-match-p "enabled" message-output))
        
        ;; Toggle buffer-local notifications off
        (ecc-auto-notify-buffer-local-toggle)
        
        ;; Verify state changed back
        (should-not ecc-buffer-auto-notify-enabled)
        (should (string-match-p "disabled" message-output))))))

;;; Test unified checking function

(ert-deftest test-ecc-auto-notify-consolidated-check-unified-global ()
  "Test unified checking with global mode."
  (with-temp-buffer
    (let ((ecc-auto-notify-on-claude-prompt t)
          (ecc-auto-notify-buffer-local-default nil)
          (ecc-auto-notify-prompt-types '(:waiting))
          (global-check-called nil))
      
      ;; Mock functions
      (cl-letf (((symbol-function 'ecc-detect-state)
                 (lambda () :waiting))
                ((symbol-function 'ecc-auto-notify-check-state)
                 (lambda (state)
                   (setq global-check-called t)
                   (should (eq state :waiting))
                   t)))
        
        ;; Call unified check function
        (ecc-auto-notify-check-unified (current-buffer))
        
        ;; Verify global check was called
        (should global-check-called)))))

(ert-deftest test-ecc-auto-notify-consolidated-check-unified-buffer-local ()
  "Test unified checking with buffer-local mode."
  (with-temp-buffer
    (let ((ecc-auto-notify-buffer-local-default t)
          (ecc-auto-notify-prompt-types '(:waiting))
          (buffer-local-check-called nil))
      
      ;; Set up buffer-local variables
      (setq-local ecc-buffer-auto-notify-enabled t)
      
      ;; Mock functions
      (cl-letf (((symbol-function 'ecc-detect-state)
                 (lambda () :waiting))
                ((symbol-function 'ecc-auto-notify-prompt-buffer-local)
                 (lambda (state)
                   (setq buffer-local-check-called t)
                   (should (eq state :waiting))
                   t)))
        
        ;; Call unified check function
        (ecc-auto-notify-check-unified (current-buffer))
        
        ;; Verify buffer-local check was called
        (should buffer-local-check-called)))))

;;; Run tests if in batch mode
(when noninteractive
  (ert-run-tests-batch-and-exit))

(provide 'test-ecc-auto-notify-consolidated)

;;; test-ecc-auto-notify-consolidated.el ends here