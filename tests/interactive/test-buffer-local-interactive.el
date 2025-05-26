;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 21:00:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/interactive/test-buffer-local-interactive.el

;;; Commentary:
;;; Interactive tests for manually verifying the buffer-local configuration.
;;; These tests provide a guided experience to verify the functionality works
;;; correctly in a real-world environment.

(require 'ecc-variables)
(require 'ecc-api)
(require 'ecc-buffer-local)
(require 'ecc-api)
(require 'ecc-buffer-api)
(require 'ecc-api)
(require 'ecc-auto-response-buffer-local)
(require 'ecc-api)

;;;###autoload
(defun ecc-interactive-test-setup ()
  "Set up the interactive test environment.
Creates two test buffers and configures them with different settings."
  (interactive)
  ;; Create test buffers
  (let ((buffer-a (get-buffer-create "*ecc-interactive-a*"))
        (buffer-b (get-buffer-create "*ecc-interactive-b*")))
    
    ;; Configure test buffers
    (with-current-buffer buffer-a
      (erase-buffer)
      (insert "=== Test Buffer A ===\n\n")
      (insert "This buffer simulates one Claude instance.\n")
      (insert "Use the test commands to interact with it.\n\n"))
    
    (with-current-buffer buffer-b
      (erase-buffer)
      (insert "=== Test Buffer B ===\n\n")
      (insert "This buffer simulates another Claude instance.\n")
      (insert "It has different configuration from Buffer A.\n\n"))
    
    ;; Register buffers
    (ecc-buffer-register buffer-a)
    (ecc-buffer-register buffer-b)
    
    ;; Configure with different settings
    (with-current-buffer buffer-a
      (setq-local ecc-buffer-auto-response-y/n "buffer-a-yes")
      (setq-local ecc-buffer-auto-response-waiting "/buffer-a-continue")
      (setq-local ecc-buffer-auto-response-initial-waiting "/buffer-a-start")
      (setq-local ecc-buffer-debug-enabled t))
    
    (with-current-buffer buffer-b
      (setq-local ecc-buffer-auto-response-y/n "buffer-b-yes")
      (setq-local ecc-buffer-auto-response-waiting "/buffer-b-continue")
      (setq-local ecc-buffer-auto-response-initial-waiting "/buffer-b-start")
      (setq-local ecc-buffer-debug-enabled nil))
    
    ;; Display buffers
    (display-buffer buffer-a)
    (display-buffer buffer-b)
    
    ;; Display instructions
    (message "Interactive test environment set up. Use ecc-interactive-test-menu to continue.")))

;;;###autoload
(defun ecc-interactive-test-teardown ()
  "Clean up the interactive test environment."
  (interactive)
  (let ((buffer-a (get-buffer "*ecc-interactive-a*"))
        (buffer-b (get-buffer "*ecc-interactive-b*")))
    
    (when (buffer-live-p buffer-a)
      (kill-buffer buffer-a))
    
    (when (buffer-live-p buffer-b)
      (kill-buffer buffer-b))
    
    (message "Interactive test environment cleaned up.")))

;;;###autoload
(defun ecc-interactive-test-info ()
  "Display information about the current test buffer configuration."
  (interactive)
  (let ((buffer (current-buffer))
        (info-buffer (get-buffer-create "*ecc-test-info*")))
    
    (with-current-buffer info-buffer
      (erase-buffer)
      (insert "=== Buffer Configuration ===\n\n")
      (insert (format "Buffer: %s\n" (buffer-name buffer)))
      (insert (format "Auto-response enabled: %s\n" 
                     (if (and (boundp 'ecc-buffer-auto-response-enabled)
                              ecc-buffer-auto-response-enabled)
                         "Yes" "No")))
      (insert (format "Y/N response: %s\n" 
                     (if (boundp 'ecc-buffer-auto-response-y/n)
                         ecc-buffer-auto-response-y/n
                       "not set")))
      (insert (format "Waiting response: %s\n" 
                     (if (boundp 'ecc-buffer-auto-response-waiting)
                         ecc-buffer-auto-response-waiting
                       "not set")))
      (insert (format "Initial waiting response: %s\n" 
                     (if (boundp 'ecc-buffer-auto-response-initial-waiting)
                         ecc-buffer-auto-response-initial-waiting
                       "not set")))
      (insert (format "Debug enabled: %s\n" 
                     (if (and (boundp 'ecc-buffer-debug-enabled)
                              ecc-buffer-debug-enabled)
                         "Yes" "No")))
      
      (insert "\n=== Buffer State ===\n\n")
      (insert (format "Current state: %s\n" 
                     (if (boundp 'ecc-buffer-state)
                         (ecc-state-get-name ecc-buffer-state)
                       "Unknown")))
      (insert (format "Last state time: %s\n" 
                     (if (boundp 'ecc-buffer-last-state-time)
                         (format-time-string "%Y-%m-%d %H:%M:%S" 
                                           (seconds-to-time 
                                            ecc-buffer-last-state-time))
                       "Unknown")))
      
      (insert "\n=== Throttling Information ===\n\n")
      (when (boundp 'ecc-buffer-last-time-alist)
        (insert "Last response times:\n")
        (dolist (entry ecc-buffer-last-time-alist)
          (let ((state (car entry))
                (timestamp (cdr entry)))
            (insert (format "  %s: %s\n" 
                          (ecc-state-get-name state)
                          (if (> timestamp 0.0)
                              (format-time-string "%Y-%m-%d %H:%M:%S" 
                                                (seconds-to-time timestamp))
                            "Never"))))))
      
      (insert "\n=== Detection Tests ===\n\n")
      (insert "Y/N detected: ")
      (insert (if (ecc-buffer-state-y/n-p buffer) "Yes\n" "No\n"))
      (insert "Y/Y/N detected: ")
      (insert (if (ecc-buffer-state-y/y/n-p buffer) "Yes\n" "No\n"))
      (insert "Waiting detected: ")
      (insert (if (ecc-buffer-state-waiting-p buffer) "Yes\n" "No\n"))
      (insert "Initial waiting detected: ")
      (insert (if (ecc-buffer-state-initial-waiting-p buffer) "Yes\n" "No\n")))
    
    ;; Display info buffer
    (display-buffer info-buffer)))

;;;###autoload
(defun ecc-interactive-test-inject-state (state)
  "Inject content to simulate a specific STATE in the current buffer.
STATE should be one of: y/n, y/y/n, waiting, initial-waiting, none."
  (interactive 
   (list (completing-read "State to inject: " 
                         '("y/n" "y/y/n" "waiting" "initial-waiting" "none")
                         nil t)))
  
  (with-current-buffer (current-buffer)
    (let ((content 
           (cond
            ((string= state "y/n")
             "Claude output with [Y/n] prompt")
            ((string= state "y/y/n")
             "Claude output with [Y/y/n] prompt")
            ((string= state "waiting")
             "Claude content with continue> prompt")
            ((string= state "initial-waiting")
             "Claude initial content with │ > Try  prompt")
            ((string= state "none")
             "Regular Claude content without any prompt")
            (t "Unknown state"))))
      
      ;; Insert content
      (goto-char (point-max))
      (insert "\n\n" content)
      
      ;; Detect state
      (let ((detected (ecc-buffer-state-detect)))
        (goto-char (point-max))
        (insert (format "\n\nDetected state: %s\n" 
                      (if detected (ecc-state-get-name detected) "none")))))))

;;;###autoload
(defun ecc-interactive-test-toggle-auto-response ()
  "Toggle auto-response in the current buffer."
  (interactive)
  (with-current-buffer (current-buffer)
    (ecc-buffer-auto-response-toggle)
    (message "Auto-response %s in %s"
             (if (and (boundp 'ecc-buffer-auto-response-enabled)
                      ecc-buffer-auto-response-enabled)
                 "enabled" "disabled")
             (buffer-name))))

;;;###autoload
(defun ecc-interactive-test-force-check ()
  "Force auto-response check in the current buffer."
  (interactive)
  (with-current-buffer (current-buffer)
    (let ((state (ecc-buffer-state-detect)))
      (if state
          (progn
            (ecc-auto-response-buffer-local-check (current-buffer))
            (message "Auto-response check completed for state: %s" 
                     (ecc-state-get-name state)))
        (message "No prompt state detected")))))

;;;###autoload
(defun ecc-interactive-test-reset-throttling ()
  "Reset throttling timestamps in the current buffer."
  (interactive)
  (with-current-buffer (current-buffer)
    (when (boundp 'ecc-buffer-last-time-alist)
      (setq-local ecc-buffer-last-time-alist
                  '((:y/n . 0.0)
                    (:y/y/n . 0.0)
                    (:waiting . 0.0)
                    (:initial-waiting . 0.0)))
      (message "Throttling timestamps reset in %s" (buffer-name)))))

;;;###autoload
(defun ecc-interactive-test-set-custom-response (state response)
  "Set a custom RESPONSE for STATE in the current buffer."
  (interactive
   (list (completing-read "State: " 
                         '("y/n" "y/y/n" "waiting" "initial-waiting")
                         nil t)
         (read-string "Response: ")))
  
  (with-current-buffer (current-buffer)
    (cond
     ((string= state "y/n")
      (setq-local ecc-buffer-auto-response-y/n response))
     ((string= state "y/y/n")
      (setq-local ecc-buffer-auto-response-y/y/n response))
     ((string= state "waiting")
      (setq-local ecc-buffer-auto-response-waiting response))
     ((string= state "initial-waiting")
      (setq-local ecc-buffer-auto-response-initial-waiting response)))
    
    (message "Set %s response to \"%s\" in %s" 
             state response (buffer-name))))

;;;###autoload
(defun ecc-interactive-test-toggle-debug ()
  "Toggle debug logging in the current buffer."
  (interactive)
  (with-current-buffer (current-buffer)
    (ecc-buffer-debug-toggle)))

;;;###autoload
(defun ecc-interactive-test-copy-settings (target-buffer)
  "Copy current buffer's settings to TARGET-BUFFER."
  (interactive
   (list (completing-read "Target buffer: " 
                         (mapcar #'buffer-name (ecc-buffer-list))
                         nil t)))
  
  (let ((source (current-buffer))
        (target (get-buffer target-buffer)))
    (when (buffer-live-p target)
      ;; Copy settings
      (with-current-buffer source
        ;; Copy auto-response settings
        (let ((y/n (and (boundp 'ecc-buffer-auto-response-y/n)
                        ecc-buffer-auto-response-y/n))
              (y/y/n (and (boundp 'ecc-buffer-auto-response-y/y/n)
                         ecc-buffer-auto-response-y/y/n))
              (waiting (and (boundp 'ecc-buffer-auto-response-waiting)
                           ecc-buffer-auto-response-waiting))
              (initial (and (boundp 'ecc-buffer-auto-response-initial-waiting)
                           ecc-buffer-auto-response-initial-waiting))
              (enabled (and (boundp 'ecc-buffer-auto-response-enabled)
                           ecc-buffer-auto-response-enabled))
              (debug (and (boundp 'ecc-buffer-debug-enabled)
                         ecc-buffer-debug-enabled)))
          
          ;; Apply to target
          (with-current-buffer target
            (when y/n (setq-local ecc-buffer-auto-response-y/n y/n))
            (when y/y/n (setq-local ecc-buffer-auto-response-y/y/n y/y/n))
            (when waiting (setq-local ecc-buffer-auto-response-waiting waiting))
            (when initial (setq-local ecc-buffer-auto-response-initial-waiting initial))
            (setq-local ecc-buffer-auto-response-enabled enabled)
            (setq-local ecc-buffer-debug-enabled debug))))
      
      (message "Settings copied from %s to %s" 
               (buffer-name source) (buffer-name target)))))

;;;###autoload
(defun ecc-interactive-test-menu ()
  "Display a menu of interactive test commands."
  (interactive)
  (let ((cmd (completing-read "Test command: " 
                             '("Setup test environment"
                               "Display buffer info"
                               "Inject state"
                               "Toggle auto-response"
                               "Force auto-response check"
                               "Reset throttling"
                               "Set custom response"
                               "Toggle debug logging"
                               "Copy settings to another buffer"
                               "Clean up test environment")
                             nil t)))
    (cond
     ((string= cmd "Setup test environment")
      (call-interactively #'ecc-interactive-test-setup))
     ((string= cmd "Display buffer info")
      (call-interactively #'ecc-interactive-test-info))
     ((string= cmd "Inject state")
      (call-interactively #'ecc-interactive-test-inject-state))
     ((string= cmd "Toggle auto-response")
      (call-interactively #'ecc-interactive-test-toggle-auto-response))
     ((string= cmd "Force auto-response check")
      (call-interactively #'ecc-interactive-test-force-check))
     ((string= cmd "Reset throttling")
      (call-interactively #'ecc-interactive-test-reset-throttling))
     ((string= cmd "Set custom response")
      (call-interactively #'ecc-interactive-test-set-custom-response))
     ((string= cmd "Toggle debug logging")
      (call-interactively #'ecc-interactive-test-toggle-debug))
     ((string= cmd "Copy settings to another buffer")
      (call-interactively #'ecc-interactive-test-copy-settings))
     ((string= cmd "Clean up test environment")
      (call-interactively #'ecc-interactive-test-teardown)))))

;; Interactive test guide
(defun ecc-interactive-test-guide ()
  "Display a guide for running interactive tests."
  (interactive)
  (with-current-buffer (get-buffer-create "*ecc-test-guide*")
    (erase-buffer)
    (insert "=== Buffer-Local Configuration Interactive Test Guide ===\n\n")
    
    (insert "This guide will help you manually verify the buffer-local configuration system.\n")
    (insert "Follow the steps below to test the functionality:\n\n")
    
    (insert "1. Setup the test environment:\n")
    (insert "   M-x ecc-interactive-test-setup\n\n")
    
    (insert "2. Access the test menu:\n")
    (insert "   M-x ecc-interactive-test-menu\n\n")
    
    (insert "3. Verify independent buffer configuration:\n")
    (insert "   a. Switch to Buffer A\n")
    (insert "   b. Display buffer info (from menu)\n")
    (insert "   c. Switch to Buffer B\n")
    (insert "   d. Display buffer info (from menu)\n")
    (insert "   e. Compare the different settings in each buffer\n\n")
    
    (insert "4. Test state detection:\n")
    (insert "   a. Switch to Buffer A\n")
    (insert "   b. Inject various states (from menu)\n")
    (insert "   c. Verify detected states match injected states\n")
    (insert "   d. Repeat for Buffer B\n\n")
    
    (insert "5. Test auto-response:\n")
    (insert "   a. Switch to Buffer A\n")
    (insert "   b. Toggle auto-response (from menu)\n")
    (insert "   c. Inject a state\n")
    (insert "   d. Force auto-response check (from menu)\n")
    (insert "   e. Verify the appropriate response is displayed\n")
    (insert "   f. Repeat for Buffer B with different states\n\n")
    
    (insert "6. Test throttling:\n")
    (insert "   a. Switch to Buffer A\n")
    (insert "   b. Inject a state\n")
    (insert "   c. Force auto-response check twice in quick succession\n")
    (insert "   d. Verify the second check is throttled\n")
    (insert "   e. Reset throttling (from menu)\n")
    (insert "   f. Force auto-response check again and verify it works\n")
    (insert "   g. Verify that Buffer B throttling is independent\n\n")
    
    (insert "7. Test cross-buffer independence:\n")
    (insert "   a. Change settings in Buffer A\n")
    (insert "   b. Verify settings in Buffer B are unchanged\n")
    (insert "   c. Try copying settings between buffers (from menu)\n\n")
    
    (insert "8. Clean up:\n")
    (insert "   a. Clean up test environment (from menu)\n\n")
    
    (insert "Each test step will help verify a different aspect of the buffer-local system.\n")
    (insert "If all steps work as expected, the system is functioning correctly.\n"))
  
  (display-buffer "*ecc-test-guide*"))

(provide 'test-buffer-local-interactive)

;;; test-buffer-local-interactive.el ends here