;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-12 13:40:15>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/buffer/test-ecc-buffer-state.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ert)
(require 'ecc-variables)
(require 'ecc-buffer-current)
(require 'ecc-buffer-state)

(ert-deftest test-ecc-buffer-state-loadable ()
  "Test that ecc-buffer-state loads properly."
  (should (featurep 'ecc-buffer-state)))

(ert-deftest test-ecc-state-initialize-exists ()
  "Test that ecc-state-initialize function exists."
  (should (fboundp 'ecc-state-initialize)))

(ert-deftest test-ecc-state-buffer-exists-p-exists ()
  "Test that ecc-state-buffer-exists-p function exists."
  (should (fboundp 'ecc-state-buffer-exists-p)))

(ert-deftest test-ecc-state-get-exists ()
  "Test that ecc-state-get function exists."
  (should (fboundp 'ecc-state-get)))

(ert-deftest test-ecc-state-buffer-exists-p ()
  "Test buffer existence check functionality."
  (let ((original-current ecc-buffer-current-buffer)
        (test-buffer (generate-new-buffer "*test-claude-state*")))
    (unwind-protect
        (progn
          ;; Setup test environment
          (setq ecc-buffer-current-buffer test-buffer)
          
          ;; Mock vterm-mode
          (cl-letf (((symbol-function 'derived-mode-p) 
                     (lambda (mode) (eq mode 'vterm-mode))))
            
            ;; Test with valid buffer
            (with-current-buffer test-buffer
              (should (ecc-state-buffer-exists-p test-buffer))
              
              ;; Test with current buffer
              (should (ecc-state-buffer-exists-p)))
            
            ;; Test with non-existent buffer
            (kill-buffer test-buffer)
            (should-not (ecc-state-buffer-exists-p test-buffer))
            (should-not (ecc-state-buffer-exists-p))))
      
      ;; Cleanup
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer))
      (setq ecc-buffer-current-buffer original-current))))

(ert-deftest test-ecc-state-detect-prompt ()
  "Test prompt detection."
  (let ((original-current ecc-buffer-current-buffer)
        (test-buffer (generate-new-buffer "*test-claude-prompt*")))
    (unwind-protect
        (progn
          ;; Setup test environment
          (setq ecc-buffer-current-buffer test-buffer)
          
          ;; Insert some test prompts
          (with-current-buffer test-buffer
            ;; Setup buffer content with prompt patterns
            (insert "Some content here\n")
            (insert ecc-prompt-pattern-waiting "\n")
            (insert "More content\n")
            (insert ecc-prompt-pattern-y/n "\n")
            
            ;; Mock vterm-mode and pulse functions
            (cl-letf (((symbol-function 'derived-mode-p) 
                       (lambda (mode) (eq mode 'vterm-mode)))
                      ((symbol-function 'pulse-momentary-highlight-region) #'ignore))
              
              ;; Test prompt detection
              (should (--ecc-state-detect-prompt ecc-prompt-pattern-waiting))
              (should (--ecc-state-detect-prompt ecc-prompt-pattern-y/n))
              (should-not (--ecc-state-detect-prompt ecc-prompt-pattern-y/y/n))
              
              ;; Test with line limitation
              (should (--ecc-state-detect-prompt ecc-prompt-pattern-waiting nil 10))
              (should-not (--ecc-state-detect-prompt ecc-prompt-pattern-waiting nil 1)))))
      
      ;; Cleanup
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer))
      (setq ecc-buffer-current-buffer original-current))))

(ert-deftest test-ecc-state-get-detection ()
  "Test state detection with ecc-state-get."
  (let ((original-current ecc-buffer-current-buffer)
        (test-buffer (generate-new-buffer "*test-claude-state-get*"))
        (original-y/y/n ecc-prompt-pattern-y/y/n))
    (unwind-protect
        (progn
          ;; Setup test environment
          (setq ecc-buffer-current-buffer test-buffer)
          (setq ecc-prompt-pattern-y/y/n "[Y/y/n]") ;; Ensure this is set for test
          
          ;; Mock functions
          (cl-letf (((symbol-function 'derived-mode-p) 
                     (lambda (mode) (eq mode 'vterm-mode)))
                    ((symbol-function 'pulse-momentary-highlight-region) #'ignore)
                    ((symbol-function '--ecc-state-detect-prompt)
                     (lambda (pattern buf &optional n-lines) 
                       (when (and pattern (string= pattern "[Y/y/n]")) t)))
                    ((symbol-function '--ecc-state-is-y/n-p) 
                     (lambda (buf) nil))
                    ((symbol-function '--ecc-state-is-waiting-p) 
                     (lambda (buf) nil))
                    ((symbol-function '--ecc-state-is-initial-waiting-p) 
                     (lambda (buf) nil))
                    ((symbol-function '--ecc-state-is-running-p) 
                     (lambda (buf) nil)))
            
            ;; Test with active y/y/n state
            (should (eq :y/y/n (ecc-state-get test-buffer)))
            
            ;; Test with y/n state
            (cl-letf (((symbol-function '--ecc-state-detect-prompt)
                       (lambda (pattern buf &optional n-lines) 
                         (cond
                          ((and pattern (string= pattern "[Y/y/n]")) nil)
                          ((and pattern (string= pattern ecc-prompt-pattern-y/n)) t)
                          (t nil)))))
              (should (eq :y/n (ecc-state-get test-buffer))))
            
            ;; Test with waiting state
            (cl-letf (((symbol-function '--ecc-state-detect-prompt)
                       (lambda (pattern buf &optional n-lines) 
                         (cond
                          ((and pattern (string= pattern "[Y/y/n]")) nil)
                          ((and pattern (string= pattern ecc-prompt-pattern-y/n)) nil)
                          ((and pattern (string= pattern ecc-prompt-pattern-waiting)) t)
                          (t nil)))))
              (should (eq :waiting (ecc-state-get test-buffer))))
            
            ;; Test with initial-waiting state
            (cl-letf (((symbol-function '--ecc-state-detect-prompt)
                       (lambda (pattern buf &optional n-lines)
                         (cond
                          ((and pattern (string= pattern "[Y/y/n]")) nil)
                          ((and pattern (string= pattern ecc-prompt-pattern-y/n)) nil)
                          ((and pattern (string= pattern ecc-prompt-pattern-waiting)) nil)
                          ((and pattern (string= pattern ecc-prompt-pattern-initial-waiting)) t)
                          (t nil)))))
              (should (eq :initial-waiting (ecc-state-get test-buffer))))
            
            ;; Test with running state
            (cl-letf (((symbol-function '--ecc-state-detect-prompt)
                       (lambda (pattern buf &optional n-lines) nil)))
              (should (eq :running (ecc-state-get test-buffer)))))
      
      ;; Cleanup
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer))
      (setq ecc-buffer-current-buffer original-current)
      (setq ecc-prompt-pattern-y/y/n original-y/y/n))))

(provide 'test-ecc-buffer-state)

(when (not load-file-name)
  (message "test-ecc-buffer-state.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))