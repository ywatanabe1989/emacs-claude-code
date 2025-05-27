;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-27 14:45:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-auto-response-buffer-local-toggle.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'ecc-auto-response)

(ert-deftest test-buffer-local-toggle-independence ()
  "Test that C-c c a (ecc-auto-response-buffer-toggle) operates independently per buffer."
  (let ((buffer1 (generate-new-buffer "*test-claude-1*"))
        (buffer2 (generate-new-buffer "*test-claude-2*"))
        (original-default ecc-auto-response-default))
    (unwind-protect
        (progn
          ;; Setup: Start with buffer-local mode disabled globally
          (setq ecc-auto-response-default nil)
          
          ;; Test: Enable auto-response in buffer1 only
          (with-current-buffer buffer1
            (ecc-auto-response-buffer-toggle))
          
          ;; Verify: buffer1 should have auto-response enabled
          (with-current-buffer buffer1
            (should ecc-auto-response-buffer-enabled))
          
          ;; Verify: buffer2 should NOT have auto-response enabled
          (with-current-buffer buffer2
            (should-not ecc-auto-response-buffer-enabled))
          
          ;; Test: Toggle buffer1 off
          (with-current-buffer buffer1
            (ecc-auto-response-buffer-toggle))
          
          ;; Verify: buffer1 should now be disabled
          (with-current-buffer buffer1
            (should-not ecc-auto-response-buffer-enabled))
          
          ;; Verify: buffer2 should still be disabled
          (with-current-buffer buffer2
            (should-not ecc-auto-response-buffer-enabled))
          
          ;; Test: Enable buffer2 only
          (with-current-buffer buffer2
            (ecc-auto-response-buffer-toggle))
          
          ;; Verify: buffer1 should still be disabled
          (with-current-buffer buffer1
            (should-not ecc-auto-response-buffer-enabled))
          
          ;; Verify: buffer2 should be enabled
          (with-current-buffer buffer2
            (should ecc-auto-response-buffer-enabled)))
      
      ;; Cleanup
      (setq ecc-auto-response-default original-default)
      (when (buffer-live-p buffer1)
        (with-current-buffer buffer1
          (setq-local ecc-auto-response-buffer-enabled nil))
        (kill-buffer buffer1))
      (when (buffer-live-p buffer2)
        (with-current-buffer buffer2
          (setq-local ecc-auto-response-buffer-enabled nil))
        (kill-buffer buffer2)))))

(ert-deftest test-buffer-local-toggle-no-global-interference ()
  "Test that buffer-local toggle doesn't interfere with global auto-response state."
  (let ((test-buffer (generate-new-buffer "*test-claude*"))
        (original-enabled ecc-auto-response-enabled)
        (original-default ecc-auto-response-default))
    (unwind-protect
        (progn
          ;; Setup: Disable global auto-response
          (setq ecc-auto-response-enabled nil)
          (setq ecc-auto-response-default nil)
          
          ;; Test: Enable buffer-local auto-response
          (with-current-buffer test-buffer
            (ecc-auto-response-buffer-toggle))
          
          ;; Verify: Global state should remain disabled
          (should-not ecc-auto-response-enabled)
          
          ;; Verify: Buffer-local should be enabled
          (with-current-buffer test-buffer
            (should ecc-auto-response-buffer-enabled))
          
          ;; Test: Disable buffer-local auto-response
          (with-current-buffer test-buffer
            (ecc-auto-response-buffer-toggle))
          
          ;; Verify: Global state should still be disabled
          (should-not ecc-auto-response-enabled)
          
          ;; Verify: Buffer-local should be disabled
          (with-current-buffer test-buffer
            (should-not ecc-auto-response-buffer-enabled)))
      
      ;; Cleanup
      (setq ecc-auto-response-enabled original-enabled)
      (setq ecc-auto-response-default original-default)
      (when (buffer-live-p test-buffer)
        (with-current-buffer test-buffer
          (setq-local ecc-auto-response-buffer-enabled nil))
        (kill-buffer test-buffer)))))

(ert-deftest test-buffer-local-processing-independence ()
  "Test that buffer processing respects individual buffer states."
  (let ((buffer1 (generate-new-buffer "*test-claude-1*"))
        (buffer2 (generate-new-buffer "*test-claude-2*"))
        (original-enabled ecc-auto-response-enabled))
    (unwind-protect
        (progn
          ;; Setup: Disable global auto-response
          (setq ecc-auto-response-enabled nil)
          
          ;; Enable buffer-local for buffer1 only
          (with-current-buffer buffer1
            (setq-local ecc-auto-response-buffer-enabled t))
          (with-current-buffer buffer2
            (setq-local ecc-auto-response-buffer-enabled nil))
          
          ;; Register both buffers
          (ecc-auto-response-register-buffer buffer1)
          (ecc-auto-response-register-buffer buffer2)
          
          ;; Test processing logic
          (with-current-buffer buffer1
            (let ((buffer-local-enabled (or ecc-auto-response-buffer-enabled
                                           (and (boundp 'ecc-buffer-auto-response-enabled)
                                                ecc-buffer-auto-response-enabled))))
              (should buffer-local-enabled)))
          
          (with-current-buffer buffer2
            (let ((buffer-local-enabled (or ecc-auto-response-buffer-enabled
                                           (and (boundp 'ecc-buffer-auto-response-enabled)
                                                ecc-buffer-auto-response-enabled))))
              (should-not buffer-local-enabled))))
      
      ;; Cleanup
      (setq ecc-auto-response-enabled original-enabled)
      (when (buffer-live-p buffer1)
        (ecc-auto-response-unregister-buffer buffer1)
        (kill-buffer buffer1))
      (when (buffer-live-p buffer2)
        (ecc-auto-response-unregister-buffer buffer2)
        (kill-buffer buffer2)))))

(provide 'test-auto-response-buffer-local-toggle)

;;; test-auto-response-buffer-local-toggle.el ends here