;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-27 15:15:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-auto-response-start-stop-messages.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'ecc-auto-response)

(ert-deftest test-auto-response-buffer-start-shows-message ()
  "Test that buffer start shows consistent message format."
  (let ((test-buffer (generate-new-buffer "*test-claude-start*"))
        (captured-messages nil)
        (original-message-function (symbol-function 'message)))
    (unwind-protect
        (progn
          ;; Mock message function to capture messages
          (fset 'message (lambda (format-string &rest args)
                          (push (apply #'format format-string args) captured-messages)
                          (apply original-message-function format-string args)))
          
          ;; Test starting auto-response
          (with-current-buffer test-buffer
            (ecc-auto-response-buffer-start test-buffer))
          
          ;; Verify message was shown
          (should captured-messages)
          (should (string-match "Starting buffer-local auto-response for \\*test-claude-start\\*"
                               (car captured-messages))))
      
      ;; Cleanup
      (fset 'message original-message-function)
      (when (buffer-live-p test-buffer)
        (with-current-buffer test-buffer
          (setq-local ecc-auto-response-buffer-enabled nil))
        (kill-buffer test-buffer)))))

(ert-deftest test-auto-response-buffer-stop-shows-message ()
  "Test that buffer stop shows consistent message format."
  (let ((test-buffer (generate-new-buffer "*test-claude-stop*"))
        (captured-messages nil)
        (original-message-function (symbol-function 'message)))
    (unwind-protect
        (progn
          ;; Setup: Start auto-response first
          (with-current-buffer test-buffer
            (setq-local ecc-auto-response-buffer-enabled t))
          
          ;; Mock message function to capture messages
          (fset 'message (lambda (format-string &rest args)
                          (push (apply #'format format-string args) captured-messages)
                          (apply original-message-function format-string args)))
          
          ;; Test stopping auto-response
          (with-current-buffer test-buffer
            (ecc-auto-response-buffer-stop test-buffer))
          
          ;; Verify message was shown
          (should captured-messages)
          (should (string-match "Stopping buffer-local auto-response for \\*test-claude-stop\\*"
                               (car captured-messages))))
      
      ;; Cleanup
      (fset 'message original-message-function)
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer)))))

(ert-deftest test-auto-response-buffer-toggle-shows-both-messages ()
  "Test that toggle function shows appropriate start/stop messages."
  (let ((test-buffer (generate-new-buffer "*test-claude-toggle*"))
        (captured-messages nil)
        (original-message-function (symbol-function 'message)))
    (unwind-protect
        (progn
          ;; Mock message function to capture messages
          (fset 'message (lambda (format-string &rest args)
                          (push (apply #'format format-string args) captured-messages)
                          (apply original-message-function format-string args)))
          
          ;; Test first toggle (should start)
          (with-current-buffer test-buffer
            (ecc-auto-response-buffer-toggle test-buffer))
          
          ;; Verify start message was shown
          (should captured-messages)
          (should (string-match "Starting buffer-local auto-response for \\*test-claude-toggle\\*"
                               (car captured-messages)))
          
          ;; Clear messages
          (setq captured-messages nil)
          
          ;; Test second toggle (should stop)
          (with-current-buffer test-buffer
            (ecc-auto-response-buffer-toggle test-buffer))
          
          ;; Verify stop message was shown
          (should captured-messages)
          (should (string-match "Stopping buffer-local auto-response for \\*test-claude-toggle\\*"
                               (car captured-messages))))
      
      ;; Cleanup
      (fset 'message original-message-function)
      (when (buffer-live-p test-buffer)
        (with-current-buffer test-buffer
          (setq-local ecc-auto-response-buffer-enabled nil))
        (kill-buffer test-buffer)))))

(provide 'test-auto-response-start-stop-messages)

;;; test-auto-response-start-stop-messages.el ends here