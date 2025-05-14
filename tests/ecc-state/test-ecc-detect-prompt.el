;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-14 21:45:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-state/test-ecc-detect-prompt.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'ecc-state-detect-prompt)

(ert-deftest test-ecc-detect-prompt-loadable ()
  "Test that the ecc-state-detect-prompt module loads properly."
  (should (featurep 'ecc-state-detect-prompt)))

(ert-deftest test-ecc-detect-prompt-defined ()
  "Test that the --ecc-state-detect-prompt function is defined."
  (should (fboundp '--ecc-state-detect-prompt)))

;; This test is no longer relevant with the new architecture
;; since ecc-buffer-name should never be nil
(ert-deftest test-ecc-detect-prompt-handles-nil-buffer ()
  "Test that --ecc-state-detect-prompt handles nil buffer."
  ;; Save original buffer name
  (let ((orig-buffer-name ecc-buffer-name))
    (unwind-protect
        (progn
          ;; Set to a non-existent buffer name
          (setq ecc-buffer-name "non-existent-buffer-name")
          ;; Call the function - should return nil without errors
          (should-not (--ecc-state-detect-prompt "any text"))
          
          ;; Also test with invalid prompt
          (should-not (--ecc-state-detect-prompt nil)))
      
      ;; Restore original buffer name
      (setq ecc-buffer-name orig-buffer-name))))

(ert-deftest test-ecc-detect-prompt-returns-nil-when-not-found ()
  "Test that --ecc-state-detect-prompt returns nil when text is not found."
  (let ((buffer (generate-new-buffer "*temp-test*")))
    (unwind-protect
        (progn 
          (with-current-buffer buffer
            (insert "Some random text that doesn't match anything"))
          (let ((ecc-buffer-name (buffer-name buffer)))
            (should-not (--ecc-state-detect-prompt "non-existent text"))))
      (kill-buffer buffer))))

(provide 'test-ecc-detect-prompt)