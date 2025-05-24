;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 17:00:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-state-detection.el

;;; Commentary:
;;; Tests for state detection module functionality.
;;; These tests verify Claude's prompt state detection capabilities
;;; using various detection methods.

;;; Code:
(require 'ert)
(require 'ecc-variables)
(require 'ecc-state-detection)

;;;; Test helpers

(defmacro with-test-buffer (content &rest body)
  "Create a temp buffer with CONTENT, execute BODY, then kill buffer.
Returns the result of BODY."
  (declare (indent 1))
  `(let ((test-buffer (generate-new-buffer "*state-detection-test*")))
     (unwind-protect
         (progn
           (with-current-buffer test-buffer
             (insert ,content)
             (goto-char (point-max))
             ,@body))
       (kill-buffer test-buffer))))

;;;; Basic detection tests

(ert-deftest test-state-detection-y-n ()
  "Test detection of Y/N prompts."
  (with-test-buffer (concat "Some text before the prompt\n"
                          "❯ 1. Yes\n"
                          "  2. No\n")
    (should (eq (ecc-detect-state) :y/n))))

(ert-deftest test-state-detection-y-y-n ()
  "Test detection of Y/Y/N prompts."
  (with-test-buffer (concat "Some text before the prompt\n"
                          "❯ 1. Yes\n"
                          "  2. Yes, and keep using my code\n"
                          "  3. No\n")
    (should (eq (ecc-detect-state) :y/y/n))))

(ert-deftest test-state-detection-waiting ()
  "Test detection of waiting prompt.
Note: ecc-state-prompt-waiting uses non-breaking spaces (char 160) after
the pipe and arrow characters. This is critical for pattern matching."
  (with-test-buffer (concat "Some text before the prompt\n"
                          ecc-state-prompt-waiting "\n")
    (should (eq (ecc-detect-state) :waiting))))

(ert-deftest test-state-detection-initial-waiting ()
  "Test detection of initial waiting prompt."
  (with-test-buffer (concat "Some introduction\n"
                          "Claude is ready\n"
                          "│ > Try \n")
    (should (eq (ecc-detect-state) :initial-waiting))))

(ert-deftest test-state-detection-no-prompt ()
  "Test detection when no prompt is present."
  (with-test-buffer "Just normal text with no prompt"
    (should-not (ecc-detect-state))))

;;;; Line-based detection tests

(ert-deftest test-state-detection-line-based-y-n ()
  "Test line-based detection of Y/N prompts."
  (with-test-buffer (concat (make-string 1000 ?X) "\n"
                          "Some text before the prompt\n"
                          "❯ 1. Yes\n"
                          "  2. No\n")
    (should (eq (ecc-detect-prompt-in-last-lines 3) :y/n))))

(ert-deftest test-state-detection-line-based-too-far ()
  "Test line-based detection when prompt is beyond check range."
  (with-test-buffer (concat "❯ 1. Yes\n"
                          "  2. No\n"
                          (make-string 10 ?\n)
                          "More recent text with no prompt")
    (should-not (ecc-detect-prompt-in-last-lines 3))))

;;;; Region-based detection tests

(ert-deftest test-state-detection-region ()
  "Test region-based detection."
  (with-test-buffer "Text at start\n❯ 1. Yes\n  2. No\nText at end"
    (let ((y-n-start (save-excursion (goto-char (point-min))
                                    (search-forward "❯" nil t)
                                    (beginning-of-line)
                                    (point)))
          (y-n-end (save-excursion (goto-char (point-min))
                                  (search-forward "No" nil t)
                                  (end-of-line)
                                  (point))))
      (should (eq (ecc-detect-prompt-in-region y-n-start y-n-end) :y/n))
      (should-not (ecc-detect-prompt-in-region (point-min) y-n-start))
      (should-not (ecc-detect-prompt-in-region y-n-end (point-max))))))

;;;; Utility function tests

(ert-deftest test-state-get-name ()
  "Test state symbol to name conversion."
  (should (string= (ecc-state-get-name :y/n) "Y/N"))
  (should (string= (ecc-state-get-name :y/y/n) "Y/Y/N"))
  (should (string= (ecc-state-get-name :waiting) "Continue"))
  (should (string= (ecc-state-get-name :initial-waiting) "Initial-Waiting"))
  (should (string= (ecc-state-get-name :unknown) ":unknown")))

(ert-deftest test-state-symbols ()
  "Test state symbols list."
  (let ((symbols (ecc-state-symbols)))
    (should (memq :y/n symbols))
    (should (memq :y/y/n symbols))
    (should (memq :waiting symbols))
    (should (memq :initial-waiting symbols))
    (should (= (length symbols) 4))))

;;;; Alternative pattern tests

(ert-deftest test-state-detection-alternative-initial ()
  "Test detection of alternative initial waiting patterns."
  (with-test-buffer "Ready for your request"
    (should (eq (ecc-detect-state) :initial-waiting))))

;;;; Custom pattern tests

(ert-deftest test-state-detection-custom-patterns ()
  "Test detection with customized patterns."
  (let ((original-y-n ecc-state-prompt-y/n)
        (original-y-y-n ecc-state-prompt-y/y/n)
        (original-waiting ecc-state-prompt-waiting)
        (original-initial ecc-state-prompt-initial-waiting))
    
    (unwind-protect
        (progn
          ;; Set custom patterns
          (setq ecc-state-prompt-y/n "CUSTOM Y/N")
          (setq ecc-state-prompt-y/y/n "CUSTOM Y/Y/N")
          (setq ecc-state-prompt-waiting "CUSTOM WAITING")
          (setq ecc-state-prompt-initial-waiting "CUSTOM INITIAL")
          
          ;; Test with custom patterns
          (with-test-buffer "Text with CUSTOM Y/N pattern"
            (should (eq (ecc-detect-state) :y/n)))
          
          (with-test-buffer "Text with CUSTOM Y/Y/N pattern"
            (should (eq (ecc-detect-state) :y/y/n)))
          
          (with-test-buffer "Text with CUSTOM WAITING pattern"
            (should (eq (ecc-detect-state) :waiting)))
          
          (with-test-buffer "Text with CUSTOM INITIAL pattern"
            (should (eq (ecc-detect-state) :initial-waiting))))
      
      ;; Restore original patterns
      (setq ecc-state-prompt-y/n original-y-n)
      (setq ecc-state-prompt-y/y/n original-y-y-n)
      (setq ecc-state-prompt-waiting original-waiting)
      (setq ecc-state-prompt-initial-waiting original-initial))))

;;;; Backward compatibility tests

(ert-deftest test-state-detection-backward-compat ()
  "Test backward compatibility function aliases."
  (with-test-buffer "❯ 1. Yes\n  2. No\n"
    (should (eq (ecc-detect-simple-state) :y/n))
    (should (eq (ecc-detect-enhanced-state) :y/n))
    (should (eq (ecc-detect-prompt-state) :y/n))))

;;;; Integration with notification tests

(ert-deftest test-state-notify-if-prompt-detected ()
  "Test integration with notification system."
  (let ((ecc-auto-notify-on-claude-prompt t)
        (notify-called nil))
    
    ;; Mock the notification function
    (cl-letf (((symbol-function 'ecc-notification-check-state)
               (lambda (state) (setq notify-called state))))
      
      ;; Test with a prompt
      (with-test-buffer "❯ 1. Yes\n  2. No\n"
        (should (eq (ecc-state-notify-if-prompt-detected test-buffer) :y/n))
        (should (eq notify-called :y/n)))
      
      ;; Test with no prompt
      (with-test-buffer "No prompt here"
        (should-not (ecc-state-notify-if-prompt-detected test-buffer))
        (should (eq notify-called :y/n))))))

;;;; Debug info test

(ert-deftest test-state-detection-debug-info ()
  "Test debug info generation."
  (with-test-buffer "❯ 1. Yes\n  2. No\n"
    (let ((debug-info (ecc-state-detection-debug-info)))
      (should (string-match-p "Detected State: Y/N" debug-info))
      (should (string-match-p "via basic method: Y/N" debug-info))
      (should (string-match-p "via line method: Y/N" debug-info))
      (should (string-match-p "Buffer size: " debug-info)))))

(provide 'test-ecc-state-detection)

;;; test-ecc-state-detection.el ends here