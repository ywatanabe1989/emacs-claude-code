;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 20:45:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-vterm/test-buffer-local-vterm.el

;;; Commentary:
;;; Tests for buffer-local functionality with real vterm buffers.
;;; NOTE: These tests require a running Emacs with vterm support.
;;; They are intended to be run manually or in a CI environment with vterm.

(require 'ert)
(require 'vterm nil t) ;; Attempt to load vterm, but don't fail if not available
(require 'ecc-variables)
(require 'ecc-api)
(require 'ecc-buffer-local)
(require 'ecc-api)
(require 'ecc-buffer-api)
(require 'ecc-api)
(require 'ecc-auto-response-buffer-local)
(require 'ecc-api)

;; Check if vterm is available
(defconst ecc-vterm-available (featurep 'vterm)
  "Whether vterm is available for testing.")

;; Skip tests if vterm is not available
(defmacro ecc-with-vterm-or-skip (&rest body)
  "Run BODY if vterm is available, otherwise skip the test."
  `(if ecc-vterm-available
       (progn ,@body)
     (ert-skip "VTerm is not available")))

;; Test vterm buffers
(defvar ecc-vterm-test-buffer-1 nil "First test vterm buffer.")
(defvar ecc-vterm-test-buffer-2 nil "Second test vterm buffer.")

;; Utilities for vterm testing
(defun ecc-vterm-setup-test-buffers ()
  "Set up test vterm buffers."
  (ecc-with-vterm-or-skip
   ;; Create vterm buffers
   (let ((vterm-shell "bash"))
     (setq ecc-vterm-test-buffer-1 (vterm "*ecc-vterm-test-1*"))
     (setq ecc-vterm-test-buffer-2 (vterm "*ecc-vterm-test-2*")))
   
   ;; Register buffers
   (ecc-buffer-register ecc-vterm-test-buffer-1)
   (ecc-buffer-register ecc-vterm-test-buffer-2)
   
   ;; Configure buffer-local settings
   (with-current-buffer ecc-vterm-test-buffer-1
     (setq-local ecc-buffer-auto-response-y/n "vterm1-yes")
     (setq-local ecc-buffer-auto-response-waiting "/vterm1-continue"))
   
   (with-current-buffer ecc-vterm-test-buffer-2
     (setq-local ecc-buffer-auto-response-y/n "vterm2-yes")
     (setq-local ecc-buffer-auto-response-waiting "/vterm2-continue"))))

(defun ecc-vterm-teardown-test-buffers ()
  "Clean up test vterm buffers."
  (when (buffer-live-p ecc-vterm-test-buffer-1)
    (kill-buffer ecc-vterm-test-buffer-1))
  (when (buffer-live-p ecc-vterm-test-buffer-2)
    (kill-buffer ecc-vterm-test-buffer-2))
  (setq ecc-vterm-test-buffer-1 nil)
  (setq ecc-vterm-test-buffer-2 nil))

(defun ecc-vterm-inject-content (buffer content)
  "Inject CONTENT into vterm BUFFER to simulate Claude output."
  (with-current-buffer buffer
    ;; Use vterm's internal buffer to inject content
    (when (and (boundp 'vterm--internal-buffer)
               (buffer-live-p vterm--internal-buffer))
      (with-current-buffer vterm--internal-buffer
        (erase-buffer)
        (insert content)))
    
    ;; Force vterm buffer synchronization
    (when (fboundp 'vterm--update)
      (vterm--update))
    
    ;; Wait for content to appear
    (sleep-for a0.1)))

;; Tests for vterm integration
(ert-deftest ecc-test-vterm-buffer-local-state-detection ()
  "Test buffer-local state detection in real vterm buffers."
  (ecc-with-vterm-or-skip
   (ecc-vterm-setup-test-buffers)
   (unwind-protect
       (progn
         ;; Inject content for y/n prompt in first buffer
         (ecc-vterm-inject-content 
          ecc-vterm-test-buffer-1
          "Claude output with [Y/n] prompt")
         
         ;; Inject content for waiting prompt in second buffer
         (ecc-vterm-inject-content 
          ecc-vterm-test-buffer-2
          "Claude output with continue> prompt")
         
         ;; Detect states in each buffer
         (with-current-buffer ecc-vterm-test-buffer-1
           (should (eq (ecc-buffer-state-detect) :y/n)))
         
         (with-current-buffer ecc-vterm-test-buffer-2
           (should (eq (ecc-buffer-state-detect) :waiting))))
     (ecc-vterm-teardown-test-buffers))))

(ert-deftest ecc-test-vterm-buffer-local-auto-response ()
  "Test buffer-local auto-response in real vterm buffers."
  (ecc-with-vterm-or-skip
   (ecc-vterm-setup-test-buffers)
   (unwind-protect
       (let ((responses-1 nil)
             (responses-2 nil))
         ;; Mock vterm-send-string to capture responses
         (cl-letf (((symbol-function 'vterm-send-string)
                    (lambda (str)
                      (cond
                       ((eq (current-buffer) ecc-vterm-test-buffer-1)
                        (push str responses-1))
                       ((eq (current-buffer) ecc-vterm-test-buffer-2)
                        (push str responses-2)))))
                   ((symbol-function 'vterm-send-return)
                    (lambda () t)))
           
           ;; Enable auto-response for both buffers
           (with-current-buffer ecc-vterm-test-buffer-1
             (setq-local ecc-buffer-auto-response-enabled t))
           
           (with-current-buffer ecc-vterm-test-buffer-2
             (setq-local ecc-buffer-auto-response-enabled t))
           
           ;; Inject different content in each buffer
           (ecc-vterm-inject-content 
            ecc-vterm-test-buffer-1
            "Claude output with [Y/n] prompt")
           
           (ecc-vterm-inject-content 
            ecc-vterm-test-buffer-2
            "Claude output with continue> prompt")
           
           ;; Run auto-response check for each buffer
           (ecc-auto-response-buffer-local-check ecc-vterm-test-buffer-1)
           (ecc-auto-response-buffer-local-check ecc-vterm-test-buffer-2)
           
           ;; Verify different responses were sent to each buffer
           (should (equal (car responses-1) "vterm1-yes"))
           (should (equal (car responses-2) "/vterm2-continue"))))
     (ecc-vterm-teardown-test-buffers))))

(ert-deftest ecc-test-vterm-buffer-output-hook ()
  "Test buffer-local auto-response connected to vterm output hook."
  (ecc-with-vterm-or-skip
   (ecc-vterm-setup-test-buffers)
   (unwind-protect
       (let ((hook-responses nil))
         ;; Mock response function
         (cl-letf (((symbol-function 'ecc-buffer-send-response)
                    (lambda (response type)
                      (push (cons response type) hook-responses)))
                   ((symbol-function 'vterm-send-string)
                    (lambda (str) str))
                   ((symbol-function 'vterm-send-return)
                    (lambda () t)))
           
           ;; Connect auto-response to vterm hook in first buffer
           (with-current-buffer ecc-vterm-test-buffer-1
             (setq-local ecc-buffer-auto-response-enabled t)
             (ecc-auto-response-buffer-local-connect-to-vterm-hook))
           
           ;; Inject content that should trigger auto-response
           (ecc-vterm-inject-content 
            ecc-vterm-test-buffer-1
            "Claude output with [Y/n] prompt")
           
           ;; Manually trigger the vterm-update-functions hook
           (run-hook-with-args 'vterm-update-functions)
           
           ;; Verify the hook triggered a response
           (should hook-responses)
           (should (equal (caar hook-responses) "vterm1-yes"))
           (should (equal (cdar hook-responses) "Y/N"))))
     (ecc-vterm-teardown-test-buffers))))

(ert-deftest ecc-test-vterm-buffer-multi-instance ()
  "Test multiple vterm instances with buffer-local configuration."
  (ecc-with-vterm-or-skip
   (ecc-vterm-setup-test-buffers)
   (unwind-protect
       (progn
         ;; Configure each buffer differently
         (with-current-buffer ecc-vterm-test-buffer-1
           (setq-local ecc-buffer-auto-response-enabled t)
           (setq-local ecc-buffer-debug-enabled t))
         
         (with-current-buffer ecc-vterm-test-buffer-2
           (setq-local ecc-buffer-auto-response-enabled nil)
           (setq-local ecc-buffer-debug-enabled nil))
         
         ;; Verify settings are independent
         (should (with-current-buffer ecc-vterm-test-buffer-1
                   ecc-buffer-auto-response-enabled))
         
         (should-not (with-current-buffer ecc-vterm-test-buffer-2
                        ecc-buffer-auto-response-enabled))
         
         ;; Configure each buffer with different response patterns
         (ecc-buffer-auto-response-set-y/n "buffer1-yes" ecc-vterm-test-buffer-1)
         (ecc-buffer-auto-response-set-y/n "buffer2-yes" ecc-vterm-test-buffer-2)
         
         ;; Verify settings remained independent
         (should (string= (with-current-buffer ecc-vterm-test-buffer-1
                            ecc-buffer-auto-response-y/n)
                          "buffer1-yes"))
         
         (should (string= (with-current-buffer ecc-vterm-test-buffer-2
                            ecc-buffer-auto-response-y/n)
                          "buffer2-yes")))
     (ecc-vterm-teardown-test-buffers))))

(provide 'test-buffer-local-vterm)

;;; test-buffer-local-vterm.el ends here