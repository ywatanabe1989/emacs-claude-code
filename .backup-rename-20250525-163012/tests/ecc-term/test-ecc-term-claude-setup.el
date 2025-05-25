;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 00:01:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-term/test-ecc-term-claude-setup.el

;;; Commentary:
;;; Tests for the ecc-term-claude-setup module.

(require 'ert)
(require 'ecc-term-claude-setup)
(require 'ecc-api)

;;; Code:

;; Mock functions to avoid actual timer and hook operations during tests
(defvar ecc-test-timer-created nil
  "Whether a timer was created in tests.")

(defvar ecc-test-hooks-added nil
  "Whether hooks were added in tests.")

(defvar ecc-test-buffer-registered nil
  "Whether buffer was registered in tests.")

(defun ecc-test-setup-mock-functions ()
  "Set up mock functions for testing."
  (setq ecc-test-timer-created nil
        ecc-test-hooks-added nil
        ecc-test-buffer-registered nil))

(defmacro with-mock-functions (&rest body)
  "Execute BODY with mock functions."
  (declare (indent 0))
  `(progn
     (ecc-test-setup-mock-functions)
     (cl-letf (((symbol-function 'ecc-term-claude-setup-timer)
                (lambda (&optional _)
                  (setq ecc-test-timer-created t)))
               ((symbol-function 'ecc-term-claude-setup-hooks)
                (lambda (&optional _)
                  (setq ecc-test-hooks-added t)))
               ((symbol-function 'ecc-term-claude-register-buffer)
                (lambda (&optional _)
                  (setq ecc-test-buffer-registered t))))
       ,@body)))

(ert-deftest test-ecc-term-claude-validate-buffer-valid ()
  "Test buffer validation with a valid buffer."
  (let ((temp-buffer (generate-new-buffer "*test-buffer*")))
    (unwind-protect
        (should (eq (ecc-term-claude-validate-buffer temp-buffer nil) temp-buffer))
      (kill-buffer temp-buffer))))

(ert-deftest test-ecc-term-claude-validate-buffer-invalid ()
  "Test buffer validation with an invalid buffer name."
  (should-error (ecc-term-claude-validate-buffer "*non-existent-buffer*")))

(ert-deftest test-ecc-term-claude-validate-buffer-wrong-mode ()
  "Test buffer validation with wrong mode requirement."
  (let ((temp-buffer (generate-new-buffer "*test-buffer*")))
    (unwind-protect
        (progn
          (with-current-buffer temp-buffer
            (fundamental-mode))
          (should-error (ecc-term-claude-validate-buffer temp-buffer 'vterm-mode)))
      (kill-buffer temp-buffer))))

(ert-deftest test-ecc-term-claude-setup-common ()
  "Test common setup logic."
  (let ((temp-buffer (generate-new-buffer "*test-buffer*")))
    (unwind-protect
        (with-mock-functions
          ;; Mock the major-mode check in validate-buffer
          (cl-letf (((symbol-function 'ecc-term-claude-validate-buffer)
                     (lambda (buf &optional _) buf)))
            (ecc-term-claude-setup-common temp-buffer)
            (should ecc-test-buffer-registered)
            (should ecc-test-timer-created)
            (should ecc-test-hooks-added)))
      (kill-buffer temp-buffer))))

(ert-deftest test-ecc-term-claude-mode-line-state-indicator ()
  "Test mode line state indicator."
  ;; Test different states
  (cl-letf (((symbol-function 'ecc-term-claude-get-state)
             (lambda () :y/n)))
    (should (string= (ecc-term-claude-mode-line-state-indicator) " [Y/N]")))
  
  (cl-letf (((symbol-function 'ecc-term-claude-get-state)
             (lambda () :waiting)))
    (should (string= (ecc-term-claude-mode-line-state-indicator) " [Waiting]")))
  
  (cl-letf (((symbol-function 'ecc-term-claude-get-state)
             (lambda () nil)))
    (should (string= (ecc-term-claude-mode-line-state-indicator) ""))))

(ert-deftest test-ecc-term-claude-cleanup-buffer ()
  "Test buffer cleanup."
  (let ((ecc-term-claude-state-timer (run-with-timer 100 nil #'ignore))
        (ecc-buffer-registered-buffers-alist (list (cons (current-buffer) nil))))
    (ecc-term-claude-cleanup-buffer)
    (should-not ecc-term-claude-state-timer)
    (should-not (assoc (current-buffer) ecc-buffer-registered-buffers-alist))))

(provide 'test-ecc-term-claude-setup)

;;; test-ecc-term-claude-setup.el ends here