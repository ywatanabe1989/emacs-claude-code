;;; -*- coding: utf-8; lexical-binding: t -*-
;;; test-auto-response-multiple-buffers.el --- Test buffer names with multiple buffers
;;; Author: ywatanabe
;;; Timestamp: <2025-05-25>

;;; Commentary:
;;; Test that auto-response notifications correctly show buffer names
;;; when multiple buffers are active

;; Add src directory to load path
(add-to-list 'load-path (expand-file-name "../src" (file-name-directory load-file-name)))

(require 'ert)
(require 'ecc-auto-response)
(require 'ecc-vterm-utils)

(ert-deftest test-auto-response-multiple-buffer-names ()
  "Test that each buffer's auto-response shows its own name."
  (let ((buffer1 (generate-new-buffer "*CLAUDE-PROJECT-A*"))
        (buffer2 (generate-new-buffer "*CLAUDE-PROJECT-B*"))
        (buffer3 (generate-new-buffer "*claude-debug*"))
        (captured-messages '()))
    (unwind-protect
        (progn
          ;; Capture all messages
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (when (string-match-p "Auto-response" format-string)
                         (push (apply #'format format-string args) captured-messages)))))
            
            ;; Enable notifications
            (setq ecc-auto-response-notify t)
            
            ;; Test buffer 1
            (with-current-buffer buffer1
              (ecc-auto-response--send-to-buffer buffer1 "1" "Y/N")
              (ecc-auto-response--send-to-buffer buffer1 "/user:auto" "Continue"))
            
            ;; Test buffer 2
            (with-current-buffer buffer2
              (ecc-auto-response--send-to-buffer buffer2 "2" "Y/Y/N")
              (ecc-auto-response--send-to-buffer buffer2 "/user:understand-guidelines" "Initial-Waiting"))
            
            ;; Test buffer 3
            (with-current-buffer buffer3
              (ecc-auto-response--send-to-buffer buffer3 "y" "Y/N"))
            
            ;; Verify all messages contain correct buffer names
            (setq captured-messages (reverse captured-messages))
            
            ;; Check buffer 1 messages
            (should (string-match-p "\\[\\*CLAUDE-PROJECT-A\\*\\] Auto-response to Y/N: 1" 
                                   (nth 0 captured-messages)))
            (should (string-match-p "\\[\\*CLAUDE-PROJECT-A\\*\\] Auto-response to Continue: /user:auto" 
                                   (nth 1 captured-messages)))
            
            ;; Check buffer 2 messages
            (should (string-match-p "\\[\\*CLAUDE-PROJECT-B\\*\\] Auto-response to Y/Y/N: 2" 
                                   (nth 2 captured-messages)))
            (should (string-match-p "\\[\\*CLAUDE-PROJECT-B\\*\\] Auto-response to Initial-Waiting: /user:understand-guidelines" 
                                   (nth 3 captured-messages)))
            
            ;; Check buffer 3 message
            (should (string-match-p "\\[\\*claude-debug\\*\\] Auto-response to Y/N: y" 
                                   (nth 4 captured-messages)))
            
            ;; Verify we captured all 5 messages
            (should (= (length captured-messages) 5))))
      
      ;; Cleanup
      (when (buffer-live-p buffer1) (kill-buffer buffer1))
      (when (buffer-live-p buffer2) (kill-buffer buffer2))
      (when (buffer-live-p buffer3) (kill-buffer buffer3)))))

(ert-deftest test-auto-response-buffer-name-special-chars ()
  "Test buffer names with special characters are handled correctly."
  (let ((buffers-and-names 
         '(("*CLAUDE<2>*" . "\\[\\*CLAUDE<2>\\*\\]")
           ("*claude|test*" . "\\[\\*claude|test\\*\\]")
           ("*claude@server*" . "\\[\\*claude@server\\*\\]")
           ("*claude [workspace]*" . "\\[\\*claude \\[workspace\\]\\*\\]")))
        (captured-message nil))
    
    (dolist (buf-spec buffers-and-names)
      (let* ((buf-name (car buf-spec))
             (expected-pattern (cdr buf-spec))
             (test-buffer (generate-new-buffer buf-name)))
        (unwind-protect
            (progn
              ;; Capture message
              (cl-letf (((symbol-function 'message)
                         (lambda (format-string &rest args)
                           (setq captured-message (apply #'format format-string args)))))
                
                (setq ecc-auto-response-notify t)
                (ecc-auto-response--send-to-buffer test-buffer "1" "Y/N")
                
                ;; Verify buffer name appears correctly
                (should captured-message)
                (should (string-match-p expected-pattern captured-message))
                (should (string-match-p "Auto-response to Y/N: 1" captured-message))))
          
          ;; Cleanup
          (when (buffer-live-p test-buffer)
            (kill-buffer test-buffer)))))))

(provide 'test-auto-response-multiple-buffers)
;;; test-auto-response-multiple-buffers.el ends here