;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 12:15:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-term/test-ecc-term-claude-performance.el

;;; Commentary:
;;; Tests for the ecc-term-claude-performance module.

(require 'ert)
(require 'ecc-term-claude-performance)

;;; Code:

;; Test setup helpers
(defmacro with-test-buffer (&rest body)
  "Execute BODY with a test buffer."
  (declare (indent 0))
  `(let ((test-buffer (generate-new-buffer "*claude-perf-test*")))
     (unwind-protect
         (with-current-buffer test-buffer
           ,@body)
       (when (buffer-live-p test-buffer)
         (kill-buffer test-buffer)))))

(defun ecc-test-generate-large-content (size)
  "Generate a large content string of approximately SIZE characters."
  (let* ((base-text "This is a test of Claude performance optimization. ")
         (base-len (length base-text))
         (repeats (/ size base-len))
         (result ""))
    (dotimes (_ repeats)
      (setq result (concat result base-text)))
    result))

(defun ecc-test-generate-content-with-prompt (size prompt-type)
  "Generate content of SIZE with a prompt of PROMPT-TYPE at the end."
  (let ((content (ecc-test-generate-large-content size))
        (prompt (cond
                ((eq prompt-type :y/n) " [y/n] ")
                ((eq prompt-type :y/y/n) " [Y/y/n] ")
                ((eq prompt-type :waiting) " continue> ")
                ((eq prompt-type :initial-waiting) " Enter 'y' to begin: ")
                (t ""))))
    (concat content prompt)))

;; Tests for optimization functions
(ert-deftest test-ecc-term-claude-optimize-buffer ()
  "Test buffer optimization settings."
  (with-test-buffer
    ;; Apply optimizations
    (ecc-term-claude-optimize-buffer)
    
    ;; Check if settings were applied
    (should (= scroll-conservatively ecc-term-claude-scroll-conservatively))
    (should (= scroll-margin 0))
    (should (= scroll-step 1))
    (should fast-but-imprecise-scrolling)
    (should-not auto-window-vscroll)
    (should truncate-lines)
    (should line-move-visual)
    (should inhibit-field-text-motion)
    (should (= jit-lock-defer-time ecc-term-claude-jit-lock-defer-time))
    (should redisplay-skip-fontification-on-input)
    (should redisplay-dont-pause)))

(ert-deftest test-ecc-term-claude-with-gc-optimization ()
  "Test GC optimization wrapper."
  (let ((orig-threshold gc-cons-threshold)
        (test-threshold ecc-term-claude-gc-threshold)
        (result-val nil)
        (gc-during-execution nil))
    
    ;; Run a function with GC optimization
    (setq result-val
          (ecc-term-claude-with-gc-optimization
           (lambda ()
             (setq gc-during-execution gc-cons-threshold)
             42)))
    
    ;; Check if GC threshold was temporarily changed
    (should (= gc-during-execution test-threshold))
    
    ;; Check if GC threshold was restored
    (should (= gc-cons-threshold orig-threshold))
    
    ;; Check if function result was returned
    (should (= result-val 42))))

(ert-deftest test-ecc-term-claude-analyze-text-for-state ()
  "Test optimized state detection from text."
  ;; Test Y/N detection
  (let ((text "Some context here [y/n] "))
    (should (eq (ecc-term-claude-analyze-text-for-state text) :y/n)))
  
  ;; Test Y/Y/N detection
  (let ((text "Some context here [Y/y/n] "))
    (should (eq (ecc-term-claude-analyze-text-for-state text) :y/y/n)))
  
  ;; Test waiting detection
  (let ((text "Some context here continue> "))
    (should (eq (ecc-term-claude-analyze-text-for-state text) :waiting)))
  
  ;; Test custom pattern detection
  (let ((ecc-state-prompt-initial-waiting "Enter 'y' to begin: ")
        (text "Some context here Enter 'y' to begin: "))
    (should (eq (ecc-term-claude-analyze-text-for-state text) :initial-waiting)))
  
  ;; Test no state detection
  (let ((text "Some context with no prompt"))
    (should-not (ecc-term-claude-analyze-text-for-state text))))

(ert-deftest test-ecc-term-claude-get-state-optimized ()
  "Test optimized state detection function."
  (with-test-buffer
    ;; Test with small buffer
    (insert "Small buffer with [y/n] prompt")
    (should (eq (ecc-term-claude-get-state-optimized) :y/n))
    
    ;; Test with slightly larger buffer
    (erase-buffer)
    (insert (ecc-test-generate-content-with-prompt 1000 :waiting))
    (should (eq (ecc-term-claude-get-state-optimized) :waiting))
    
    ;; Test with no recognizable state
    (erase-buffer)
    (insert (ecc-test-generate-large-content 500))
    (should-not (ecc-term-claude-get-state-optimized))))

(ert-deftest test-ecc-term-claude-get-state-optimized-large-buffer ()
  "Test optimized state detection in very large buffer."
  (with-test-buffer
    ;; Skip test if it would create too large a buffer
    (skip-unless (< large-file-warning-threshold (* 5 1024 1024)))
    
    ;; Create a large buffer with prompt at the end
    (insert (ecc-test-generate-content-with-prompt 3000 :y/y/n))
    
    ;; Should still detect the state efficiently
    (should (eq (ecc-term-claude-get-state-optimized) :y/y/n))))

(ert-deftest test-ecc-term-claude-optimize-hooks ()
  "Test hook optimization."
  (let ((ecc-term-claude-update-functions (list #'ignore #'ignore #'ignore)))
    ;; Should consolidate multiple functions into one
    (ecc-term-claude-optimize-hooks)
    (should (= (length ecc-term-claude-update-functions) 1))))

(ert-deftest test-ecc-term-claude-defer-updates ()
  "Test update deferral function."
  (let* ((counter 0)
         (test-func (lambda () (setq counter (1+ counter))))
         (deferred-func (ecc-term-claude-defer-updates test-func)))
    
    ;; Call the deferred function
    (funcall deferred-func)
    
    ;; Counter shouldn't be incremented yet
    (should (= counter 0))
    
    ;; Wait for timer to execute
    (with-timeout (1 (error "Timer didn't fire"))
      (while (= counter 0)
        (sit-for 0.2)))
    
    ;; Now counter should be incremented
    (should (= counter 1))))

(provide 'test-ecc-term-claude-performance)

;;; test-ecc-term-claude-performance.el ends here