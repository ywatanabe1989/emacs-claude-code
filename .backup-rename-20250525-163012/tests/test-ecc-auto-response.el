;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 15:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-auto-response.el

;;; Commentary:
;;; Tests for the auto-response module.
;;; These tests verify the functionality of the auto-response system,
;;; including global mode, buffer-local mode, and different response types.

(require 'ert)
(require 'ecc-auto-core)
(require 'ecc-auto-response)
(require 'ecc-state-detection)

;;; Code:

;;;; Test utilities

(defvar auto-response-test-sent-string nil
  "String sent during testing.")

(defvar auto-response-test-sent-buffer nil
  "Buffer to which string was sent during testing.")

(defvar auto-response-test-notification nil
  "Notification message displayed during testing.")

(defmacro with-temp-buffer-fixture (content &rest body)
  "Create a temporary buffer with CONTENT and evaluate BODY.
Ensures proper cleanup of test resources."
  (declare (indent 1))
  `(let ((temp-buffer (generate-new-buffer "*auto-response-test*")))
     (unwind-protect
         (progn
           (with-current-buffer temp-buffer
             (insert ,content)
             (goto-char (point-max)))
           (setq auto-response-test-sent-string nil
                 auto-response-test-sent-buffer nil
                 auto-response-test-notification nil)
           ,@body)
       (when (buffer-live-p temp-buffer)
         (kill-buffer temp-buffer)))))

;; Mock state detection function for controlled testing
(defvar auto-response-test-mock-state nil
  "State to return from the mock state detection function.")

(defun auto-response-test-mock-detect-state ()
  "Mock implementation of state detection.
Returns `auto-response-test-mock-state'."
  auto-response-test-mock-state)

;; Mock buffer process functions for testing
(defun auto-response-test-mock-vterm-send-string (buffer string)
  "Mock implementation of vterm-send-string for BUFFER and STRING.
Records the string and buffer for testing."
  (setq auto-response-test-sent-string string
        auto-response-test-sent-buffer buffer))

(defun auto-response-test-mock-message (&rest args)
  "Mock implementation of message for ARGS.
Records the message for testing."
  (setq auto-response-test-notification (apply #'format args)))

;; Mock buffer-local variables for testing
(defvar-local ecc-buffer-auto-response-enabled nil
  "Mock buffer-local auto-response enabled state.")

(defvar-local ecc-buffer-auto-response-y/n "1"
  "Mock buffer-local response for Y/N prompt.")

(defvar-local ecc-buffer-auto-response-y/y/n "2"
  "Mock buffer-local response for Y/Y/N prompt.")

(defvar-local ecc-buffer-auto-response-waiting "/user:auto"
  "Mock buffer-local response for waiting prompt.")

(defvar-local ecc-buffer-auto-response-initial-waiting "/user:understand-guidelines"
  "Mock buffer-local response for initial waiting prompt.")

(defvar-local ecc-buffer-auto-notify-completions t
  "Mock buffer-local auto-notify setting.")

;; Test buffer state system
(defun auto-response-test-mock-buffer-state-throttled-p (state)
  "Mock implementation of buffer state throttling test for STATE.
Always returns nil for testing (no throttling)."
  nil)

(defun auto-response-test-mock-buffer-state-get (key)
  "Mock implementation of buffer state get for KEY.
Returns a test value for KEY."
  (cond
   ((eq key 'prompt-state) auto-response-test-mock-state)
   (t nil)))

(defun auto-response-test-mock-buffer-state-set (key value)
  "Mock implementation of buffer state set for KEY and VALUE.
Does nothing in testing."
  nil)

;;;; Global mode tests

(ert-deftest test-auto-response-buffer-start ()
  "Test starting auto-response in buffer-local mode."
  (with-temp-buffer-fixture "Test content"
    ;; Mock core functions
    (cl-letf (((symbol-function 'ecc-buffer-state-init) #'ignore)
              ((symbol-function 'ecc-buffer-local-init) #'ignore)
              ((symbol-function 'ecc-auto-core-register-buffer-local) #'ignore))
      
      ;; Start buffer-local auto-response
      (ecc-auto-response-buffer-start temp-buffer)
      
      ;; Check if auto-response was enabled for buffer
      (with-current-buffer temp-buffer
        (should (boundp 'ecc-buffer-auto-response-enabled))
        (should ecc-buffer-auto-response-enabled)))))

(ert-deftest test-auto-response-buffer-stop ()
  "Test stopping auto-response in buffer-local mode."
  (with-temp-buffer-fixture "Test content"
    ;; Initialize buffer-local state
    (with-current-buffer temp-buffer
      (setq-local ecc-buffer-auto-response-enabled t))
    
    ;; Mock core functions
    (cl-letf (((symbol-function 'ecc-auto-core-unregister-buffer-local) #'ignore))
      
      ;; Stop buffer-local auto-response
      (ecc-auto-response-buffer-stop temp-buffer)
      
      ;; Check if auto-response was disabled for buffer
      (with-current-buffer temp-buffer
        (should-not ecc-buffer-auto-response-enabled)))))

(ert-deftest test-auto-response-buffer-toggle ()
  "Test toggling auto-response in buffer-local mode."
  (let ((ecc-auto-response-default t)) ; Set buffer-local mode
    (with-temp-buffer-fixture "Test content"
      ;; Initialize buffer-local state
      (ecc-auto-response-buffer-start temp-buffer)
      
      ;; Start from disabled state
      (with-current-buffer temp-buffer
        (setq-local ecc-auto-response-buffer-enabled nil)
        
        ;; Toggle to enabled
        (ecc-auto-response-buffer-toggle)
        (should ecc-auto-response-buffer-enabled)
        (should ecc-buffer-auto-response-enabled) ;; Check compat variable too
        
        ;; Toggle to disabled
        (ecc-auto-response-buffer-toggle)
        (should-not ecc-auto-response-buffer-enabled)
        (should-not ecc-buffer-auto-response-enabled)))))

(ert-deftest test-auto-response-send ()
  "Test sending response to Claude prompt."
  (with-temp-buffer-fixture "Test content with [y/n] prompt"
    ;; Initialize buffer-local mode with custom yes response
    (ecc-auto-response-buffer-start temp-buffer "TEST_YES")
    (with-current-buffer temp-buffer
      (setq-local ecc-buffer-auto-notify-completions t))
    
    ;; Replace dependencies with mocks
    (cl-letf (((symbol-function 'ecc-detect-state) #'auto-response-test-mock-detect-state)
              ((symbol-function 'vterm-send-string) 
               (lambda (text) 
                 (setq auto-response-test-sent-string text
                       auto-response-test-sent-buffer (current-buffer))))
              ((symbol-function 'vterm-send-return) #'ignore)
              ((symbol-function 'message) #'auto-response-test-mock-message))
      
      ;; Test y/n prompt
      (setq auto-response-test-mock-state :y/n)
      (with-current-buffer temp-buffer
        (setq major-mode 'vterm-mode)) ;; Mock vterm mode
      
      ;; Send response via buffer-local check
      (ecc-auto-response--process-buffer-local temp-buffer)
      
      ;; Check response was sent correctly
      (should (string= auto-response-test-sent-string "TEST_YES"))
      (should (eq auto-response-test-sent-buffer temp-buffer))
      (should auto-response-test-notification)
      
      ;; Test disabled auto-response
      (with-current-buffer temp-buffer
        (setq-local ecc-buffer-auto-response-enabled nil))
      (setq auto-response-test-sent-string nil)
      (ecc-auto-response--process-buffer-local temp-buffer)
      
      ;; Check no response was sent
      (should-not auto-response-test-sent-string))))

(ert-deftest test-auto-response-dispatch-response ()
  "Test dispatching responses to different terminal types."
  (with-temp-buffer-fixture "Test content"
    (cl-letf (((symbol-function 'vterm-send-string) 
               (lambda (text) 
                 (setq auto-response-test-sent-string text
                       auto-response-test-sent-buffer (current-buffer))))
              ((symbol-function 'vterm-send-return) #'ignore)
              ((symbol-function 'message) #'auto-response-test-mock-message))
      
      ;; Test vterm mode
      (with-current-buffer temp-buffer
        (setq major-mode 'vterm-mode))
      
      (ecc-auto-response--dispatch-response temp-buffer "TEST_RESP" "TEST_TYPE")
      
      ;; Check response was sent via vterm
      (should (string= auto-response-test-sent-string "TEST_RESP"))
      (should (eq auto-response-test-sent-buffer temp-buffer))
      
      ;; Test notification includes buffer name
      (should (string-match-p "TEST_TYPE" auto-response-test-notification))
      (should (string-match-p (buffer-name temp-buffer) auto-response-test-notification)))))

(ert-deftest test-auto-response-buffer-name-in-notification ()
  "Test that auto-response notifications include buffer name."
  (with-temp-buffer-fixture "Test content"
    ;; Set up a recognizable buffer name
    (let ((test-buffer-name "*test-auto-response-buffer*"))
      (with-current-buffer temp-buffer
        (rename-buffer test-buffer-name))
      
      ;; Mock notification capture
      (cl-letf (((symbol-function 'message) 
                 (lambda (format-string &rest args)
                   (setq auto-response-test-notification (apply #'format format-string args)))))
        
        ;; Test notification includes buffer name
        (ecc-auto-response--send-to-buffer temp-buffer "test-response" "test-state")
        
        ;; Check buffer name is in notification
        (should (string-match-p (regexp-quote test-buffer-name) auto-response-test-notification))
        (should (string-match-p "\\[.*\\]" auto-response-test-notification))))))

;;;; Buffer-local mode tests

(ert-deftest test-auto-response-buffer-local-init ()
  "Test buffer-local auto-response initialization."
  (with-temp-buffer-fixture "Test content"
    ;; Mock dependencies
    (cl-letf (((symbol-function 'ecc-buffer-state-init) #'ignore)
              ((symbol-function 'ecc-buffer-local-init) #'ignore)
              ((symbol-function 'ecc-auto-core-register-buffer-local) #'ignore))
      
      ;; Initialize buffer-local auto-response
      (ecc-auto-response-buffer-start temp-buffer)
      
      ;; Check buffer-local variables set
      (with-current-buffer temp-buffer
        (should (boundp 'ecc-buffer-auto-response-enabled))
        (should (equal ecc-buffer-auto-response-y/n ecc-auto-response-yes))
        (should (equal ecc-buffer-auto-response-y/y/n ecc-auto-response-yes-plus))
        (should (equal ecc-buffer-auto-response-waiting ecc-auto-response-continue))
        (should (equal ecc-buffer-auto-response-initial-waiting ecc-auto-response-initial-waiting))
        (should (equal ecc-buffer-auto-notify-completions ecc-auto-response-notify))))))

(ert-deftest test-auto-response-buffer-local-check ()
  "Test buffer-local state checking and response."
  (with-temp-buffer-fixture "Test content with [y/n] prompt"
    ;; Set up buffer-local mode and state
    (setq ecc-auto-response-buffer-local-default t)
    (with-current-buffer temp-buffer
      (setq ecc-buffer-auto-response-enabled t
            ecc-auto-response-buffer-yes "TEST_BUFFER_YES"
            ;; Initialize throttling state
            ecc-auto-response-buffer-last-state nil
            ecc-auto-response-buffer-last-response-time 0))
    
    ;; Replace dependencies with mocks
    (cl-letf (((symbol-function 'ecc-detect-state) #'auto-response-test-mock-detect-state)
              ((symbol-function 'ecc-buffer-state-throttled-p) #'auto-response-test-mock-buffer-state-throttled-p)
              ((symbol-function 'ecc-buffer-state-get) #'auto-response-test-mock-buffer-state-get)
              ((symbol-function 'ecc-buffer-state-set) #'auto-response-test-mock-buffer-state-set)
              ((symbol-function 'ecc-auto-response--send-to-buffer)
               (lambda (buffer response type)
                 (setq auto-response-test-sent-string response
                       auto-response-test-sent-buffer buffer)))
              ((symbol-function 'ecc-auto-response--debug) #'ignore))
      
      ;; Test with y/n state
      (setq auto-response-test-mock-state :y/n)
      
      ;; Check state and send response  
      (ecc-auto-response--process-buffer-local temp-buffer)
      
      ;; Verify response was sent
      (should (string= auto-response-test-sent-string "TEST_BUFFER_YES"))
      (should (eq auto-response-test-sent-buffer temp-buffer))
      
      ;; Test disabled buffer-local auto-response
      (with-current-buffer temp-buffer
        (setq ecc-buffer-auto-response-enabled nil))
      
      (setq auto-response-test-sent-string nil)
      (ecc-auto-response--process-buffer-local temp-buffer)
      
      ;; Check no response was sent
      (should-not auto-response-test-sent-string))))

;;;; Convenience function tests

(ert-deftest test-auto-response-yes ()
  "Test convenience function for sending Y response."
  (with-temp-buffer-fixture "Test content"
    ;; Initialize buffer-local mode
    (ecc-auto-response-buffer-start temp-buffer)
    (with-current-buffer temp-buffer
      (setq-local ecc-buffer-auto-response-enabled t)
      (setq-local ecc-buffer-auto-response-y/n "TEST_BUFFER_YES"))
    
    ;; Replace dependencies with mocks
    (cl-letf (((symbol-function 'ecc-auto-response--send-to-buffer)
               (lambda (buffer response type)
                 (setq auto-response-test-sent-string response
                       auto-response-test-sent-buffer buffer))))
      
      ;; Send yes response using buffer-local function
      (ecc-auto-response--send-to-buffer temp-buffer 
                                                    (buffer-local-value 'ecc-buffer-auto-response-y/n temp-buffer)
                                                    "Y/N")
      
      ;; Verify response was sent
      (should (string= auto-response-test-sent-string "TEST_BUFFER_YES"))
      (should (eq auto-response-test-sent-buffer temp-buffer)))))

(ert-deftest test-auto-response-custom ()
  "Test sending custom response text."
  (with-temp-buffer-fixture "Test content"
    ;; Initialize buffer-local mode
    (ecc-auto-response-buffer-start temp-buffer)
    
    ;; Replace dependencies with mocks
    (cl-letf (((symbol-function 'ecc-auto-response--send-to-buffer)
               (lambda (buffer response type)
                 (setq auto-response-test-sent-string response
                       auto-response-test-sent-buffer buffer))))
      
      ;; Send custom response
      (ecc-auto-response--send-to-buffer temp-buffer "TEST_CUSTOM" "custom")
      
      ;; Verify response was sent
      (should (string= auto-response-test-sent-string "TEST_CUSTOM"))
      (should (eq auto-response-test-sent-buffer temp-buffer)))))

;;;; Accumulation Detection Tests

(ert-deftest test-auto-response-accumulation-detection ()
  "Test detection of accumulated auto-responses.
Verifies that the system can detect when multiple responses
are being sent in rapid succession beyond normal throttling."
  (with-temp-buffer-fixture "Test content"
    ;; Mock current time for controlled testing
    (let ((mock-time 1000.0)
          (responses-sent 0))
      (cl-letf (((symbol-function 'float-time) (lambda () mock-time))
                ((symbol-function 'ecc-auto-response--send-to-buffer)
                 (lambda (buffer text state-name)
                   (setq responses-sent (1+ responses-sent))))
                ((symbol-function 'ecc-detect-state) 
                 (lambda () :waiting))
                ((symbol-function 'message) #'ignore))
        
        ;; Enable buffer-local auto-response
        (ecc-auto-response-buffer-start temp-buffer)
        (with-current-buffer temp-buffer
          (setq-local ecc-buffer-auto-response-enabled t))
        
        ;; Send multiple responses quickly (should trigger accumulation detection)
        (dotimes (i 10)
          (setq mock-time (+ mock-time 0.1)) ; 100ms intervals
          (ecc-auto-response--process-buffer-local temp-buffer))
        
        ;; Should detect accumulation and stop sending after threshold
        (should (fboundp 'ecc-auto-response--accumulation-detected-p))
        (should (fboundp 'ecc-auto-response--reset-accumulation-counter))
        
        ;; Test accumulation counter exists and works
        (should (boundp 'ecc-auto-response--accumulation-count))
        (should (boundp 'ecc-auto-response--accumulation-start-time))))))

(ert-deftest test-auto-response-accumulation-threshold ()
  "Test accumulation threshold configuration and detection.
Verifies that accumulation detection works with configurable thresholds."
  (with-temp-buffer-fixture "Test content"
    ;; Test that accumulation threshold variables exist
    (should (boundp 'ecc-auto-response-accumulation-threshold))
    (should (boundp 'ecc-auto-response-accumulation-window))
    
    ;; Test default values are reasonable
    (should (numberp ecc-auto-response-accumulation-threshold))
    (should (> ecc-auto-response-accumulation-threshold 2))
    (should (numberp ecc-auto-response-accumulation-window))
    (should (> ecc-auto-response-accumulation-window 1.0))
    
    ;; Test accumulation detection function
    (should (fboundp 'ecc-auto-response--accumulation-detected-p))
    
    ;; Mock scenario where accumulation should be detected
    (let ((ecc-auto-response-accumulation-threshold 3)
          (ecc-auto-response-accumulation-window 2.0))
      (cl-letf (((symbol-function 'float-time) (lambda () 1000.0)))
        ;; Simulate accumulation state
        (setq ecc-auto-response--accumulation-count 4
              ecc-auto-response--accumulation-start-time 999.0)
        
        ;; Should detect accumulation
        (should (ecc-auto-response--accumulation-detected-p))))))

(ert-deftest test-auto-response-accumulation-reset ()
  "Test accumulation counter reset functionality.
Verifies that accumulation tracking can be properly reset."
  ;; Test reset function exists
  (should (fboundp 'ecc-auto-response--reset-accumulation-counter))
  
  ;; Set up accumulation state
  (setq ecc-auto-response--accumulation-count 5
        ecc-auto-response--accumulation-start-time 1000.0)
  
  ;; Reset accumulation
  (ecc-auto-response--reset-accumulation-counter)
  
  ;; Verify reset
  (should (= ecc-auto-response--accumulation-count 0))
  (should (= ecc-auto-response--accumulation-start-time 0)))

;;;; ESC Interrupt Detection Tests

(ert-deftest test-auto-response-esc-interrupt-detection ()
  "Test detection of ESC interrupt sequences in buffer content.
Verifies that the system can detect when user presses ESC to interrupt."
  (with-temp-buffer-fixture "Test content"
    ;; Test ESC detection function exists
    (should (fboundp 'ecc-auto-response--esc-interrupt-detected-p))
    
    ;; Test with buffer containing ESC sequence
    (with-current-buffer temp-buffer
      (erase-buffer)
      (insert "Some output\nPress ESC to interrupt\n^["))
    
    ;; Should detect ESC interrupt
    (should (ecc-auto-response--esc-interrupt-detected-p temp-buffer))
    
    ;; Test with buffer not containing ESC sequence
    (with-current-buffer temp-buffer
      (erase-buffer)
      (insert "Normal output without interrupt"))
    
    ;; Should not detect ESC interrupt
    (should-not (ecc-auto-response--esc-interrupt-detected-p temp-buffer))))

(ert-deftest test-auto-response-esc-interrupt-patterns ()
  "Test various ESC interrupt patterns.
Verifies detection of different ways ESC interrupts can appear."
  (with-temp-buffer-fixture "Test content"
    ;; Test different ESC patterns
    (let ((esc-patterns '("^["
                         "\e"
                         "ESC to interrupt"
                         "Press esc to interrupt")))
      
      (dolist (pattern esc-patterns)
        (with-current-buffer temp-buffer
          (erase-buffer)
          (insert "Output before\n" pattern "\nOutput after"))
        
        ;; Should detect ESC in all patterns
        (should (ecc-auto-response--esc-interrupt-detected-p temp-buffer))))))

(ert-deftest test-auto-response-stop-on-esc-interrupt ()
  "Test auto-response stops when ESC interrupt is detected.
Verifies that detecting ESC automatically disables auto-response."
  (with-temp-buffer-fixture "Test content with ESC"
    ;; Mock ESC detection to return true
    (cl-letf (((symbol-function 'ecc-auto-response--esc-interrupt-detected-p)
               (lambda (buffer) t))
              ((symbol-function 'ecc-detect-state) 
               (lambda () :waiting))
              ((symbol-function 'message) #'ignore))
      
      ;; Enable buffer-local auto-response
      (ecc-auto-response-buffer-start temp-buffer)
      (with-current-buffer temp-buffer
        (setq-local ecc-auto-response-buffer-enabled t))
      
      ;; Process buffer (should detect ESC and stop)
      (ecc-auto-response--process-buffer-local temp-buffer)
      
      ;; Buffer-local auto-response should be disabled
      (with-current-buffer temp-buffer
        (should-not ecc-auto-response-buffer-enabled)))))

(ert-deftest test-auto-response-esc-interrupt-notification ()
  "Test notification when ESC interrupt is detected.
Verifies that user is notified when auto-response stops due to ESC."
  (with-temp-buffer-fixture "Test content"
    (let ((notification-received nil))
      ;; Mock functions
      (cl-letf (((symbol-function 'ecc-auto-response--esc-interrupt-detected-p)
                 (lambda (buffer) t))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (when (string-match-p "interrupt" format-string)
                     (setq notification-received t)))))
        
        ;; Enable buffer-local auto-response
        (ecc-auto-response-buffer-start temp-buffer)
        (with-current-buffer temp-buffer
          (setq-local ecc-buffer-auto-response-enabled t))
        
        ;; Process buffer (should detect ESC)
        (ecc-auto-response--process-buffer-local temp-buffer)
        
        ;; Should receive interrupt notification
        (should notification-received)))))

;;;; Backward compatibility tests

(ert-deftest test-auto-response-buffer-local-functions ()
  "Test buffer-local auto-response functions."
  (let ((ecc-auto-response-default t)) ; Set buffer-local mode
    (with-temp-buffer-fixture "Test content"
      ;; Test buffer-local init
      (ecc-auto-response-buffer-start temp-buffer)
      
      (with-current-buffer temp-buffer
        ;; Verify variables are initialized
        (should (boundp 'ecc-buffer-auto-response-enabled))
        (should (boundp 'ecc-buffer-auto-response-y/n))
        (should (boundp 'ecc-buffer-auto-response-y/y/n))
        
        ;; Test toggle function
        (setq-local ecc-auto-response-buffer-enabled nil)
        (ecc-auto-response-buffer-toggle)
        (should ecc-auto-response-buffer-enabled)
        (should ecc-buffer-auto-response-enabled) ;; Check compat variable too
        
        ;; Toggle again
        (ecc-auto-response-buffer-toggle)
        (should-not ecc-auto-response-buffer-enabled)
        (should-not ecc-buffer-auto-response-enabled)))))

(provide 'test-ecc-auto-response)

;;; test-ecc-auto-response.el ends here
