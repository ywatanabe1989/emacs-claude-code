;;; test-ecc-term-claude-setup.el --- Tests for term-claude setup functions -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Created: 2025-05-21
;; Version: 1.0.0
;; Keywords: convenience, testing

;;; Commentary:
;; Unit tests for term-claude setup functions.
;; These tests verify that the setup functions correctly configure
;; Claude terminal buffers with all required features.

;;; Code:

(require 'ert)

;; Load the actual module instead of a mock
(require 'ecc-term-claude-setup)
(require 'ecc-api)

;; Define mock mode for testing
(defvar vterm-mode-map (make-sparse-keymap))
(defvar-local vterm-update-functions nil)
(define-derived-mode vterm-mode fundamental-mode "VTerm"
  "Major mode for vterm terminal emulator.")
(defvar-local vterm--term nil)

;;;; Test Utilities

(defun ecc-test-with-temp-buffer (content &rest body)
  "Create a temporary buffer with CONTENT and execute BODY in it.
Automatically cleans up buffer after execution."
  (let ((temp-buffer (generate-new-buffer "*ecc-test*")))
    (unwind-protect
        (progn
          (with-current-buffer temp-buffer
            (insert content)
            (vterm-mode)  ;; Use the mock vterm-mode
            (apply #'funcall body)))
      (kill-buffer temp-buffer))))

;;;; Buffer Validation Tests

(ert-deftest test-ecc-term-claude-validate-buffer ()
  "Test buffer validation functionality."
  ;; Test validating an existing buffer
  (ecc-test-with-temp-buffer
   "Test content"
   (lambda ()
     (should (eq (ecc-term-claude-validate-buffer (current-buffer))
                (current-buffer)))))
  
  ;; Test validation with a buffer name
  (ecc-test-with-temp-buffer
   "Test content"
   (lambda ()
     (let ((buf-name (buffer-name (current-buffer))))
       (should (eq (ecc-term-claude-validate-buffer buf-name)
                  (current-buffer))))))
  
  ;; Test validation with a non-existent buffer
  (should-error (ecc-term-claude-validate-buffer "non-existent-buffer")
               :type 'user-error)
  
  ;; Test mode validation
  (ecc-test-with-temp-buffer
   "Test content"
   (lambda ()
     ;; Should succeed with the correct mode
     (should (eq (ecc-term-claude-validate-buffer (current-buffer) 'vterm-mode)
                (current-buffer)))
     
     ;; Should fail with the wrong mode
     (should-error (ecc-term-claude-validate-buffer (current-buffer) 'text-mode)
                  :type 'user-error)
     
     ;; Should succeed with a list including the correct mode
     (should (eq (ecc-term-claude-validate-buffer (current-buffer) '(vterm-mode text-mode))
                (current-buffer))))))

;;;; Buffer Registration Tests

(ert-deftest test-ecc-term-claude-register-buffer-setup-vterm ()
  "Test buffer registration functionality."
  (let ((orig-buffers ecc-buffer-registered-buffers-alist)
        (orig-current-buffer ecc-buffer-current-buffer))
    (unwind-protect
        (progn
          ;; Reset the buffer registrations for testing
          (setq ecc-buffer-registered-buffers-alist nil)
          (setq ecc-buffer-current-buffer nil)
          
          ;; Test registering a new buffer
          (ecc-test-with-temp-buffer
           "Test content"
           (lambda ()
             (let ((buf (current-buffer)))
               ;; Register the buffer
               (should (eq (ecc-term-claude-register-buffer buf) buf))
               
               ;; Check it was added to the alist
               (should (assoc buf ecc-buffer-registered-buffers-alist))
               
               ;; Check it was set as the current buffer
               (should (eq ecc-buffer-current-buffer buf))
               
               ;; Test registering again (should not add duplicate)
               (let ((alist-length (length ecc-buffer-registered-buffers-alist)))
                 (ecc-term-claude-register-buffer buf)
                 (should (= (length ecc-buffer-registered-buffers-alist) alist-length)))))))
      
      ;; Restore the original buffer registrations
      (setq ecc-buffer-registered-buffers-alist orig-buffers)
      (setq ecc-buffer-current-buffer orig-current-buffer))))

;;;; Setup Functions Tests

(ert-deftest test-ecc-term-claude-setup-common-vterm ()
  "Test common setup functionality."
  ;; Track function calls
  (let ((register-called nil)
        (mode-line-called nil)
        (timer-called nil)
        (hooks-called nil)
        (follow-called nil)
        (orig-register (symbol-function 'ecc-term-claude-register-buffer))
        (orig-mode-line (symbol-function 'ecc-term-claude-setup-mode-line))
        (orig-timer (symbol-function 'ecc-term-claude-setup-timer))
        (orig-hooks (symbol-function 'ecc-term-claude-setup-hooks))
        (orig-follow (symbol-function 'ecc-term-claude-setup-follow-bottom)))
    
    (unwind-protect
        (progn
          ;; Mock the setup functions
          (cl-letf (((symbol-function 'ecc-term-claude-register-buffer)
                     (lambda (&optional _) (setq register-called t)))
                    ((symbol-function 'ecc-term-claude-setup-mode-line)
                     (lambda (&optional _) (setq mode-line-called t)))
                    ((symbol-function 'ecc-term-claude-setup-timer)
                     (lambda (&optional _) (setq timer-called t)))
                    ((symbol-function 'ecc-term-claude-setup-hooks)
                     (lambda (&optional _) (setq hooks-called t)))
                    ((symbol-function 'ecc-term-claude-setup-follow-bottom)
                     (lambda (&optional _) (setq follow-called t))))
            
            ;; Test the common setup function
            (ecc-test-with-temp-buffer
             "Test content"
             (lambda ()
               (ecc-term-claude-setup-common (current-buffer))
               
               ;; Check that all the setup functions were called
               (should register-called)
               (should mode-line-called)
               (should timer-called)
               (should hooks-called)
               (should follow-called)))))
      
      ;; Restore the original functions
      (fset 'ecc-term-claude-register-buffer orig-register)
      (fset 'ecc-term-claude-setup-mode-line orig-mode-line)
      (fset 'ecc-term-claude-setup-timer orig-timer)
      (fset 'ecc-term-claude-setup-hooks orig-hooks)
      (fset 'ecc-term-claude-setup-follow-bottom orig-follow))))

(ert-deftest test-ecc-term-claude-setup-timer ()
  "Test timer setup functionality."
  (let ((cancel-called nil)
         (orig-cancel-timer (symbol-function 'cancel-timer))
         (run-timer-called nil)
         (orig-run-with-timer (symbol-function 'run-with-timer)))
    
    (unwind-protect
        (progn
          ;; Mock timer functions
          (cl-letf (((symbol-function 'cancel-timer)
                     (lambda (_) (setq cancel-called t)))
                    ((symbol-function 'run-with-timer)
                     (lambda (&rest _) (setq run-timer-called t) 'mock-timer)))
            
            ;; Test with no existing timer
            (let ((ecc-term-claude-state-timer nil))
              (ecc-test-with-temp-buffer
               "Test content"
               (lambda ()
                 (ecc-term-claude-setup-timer)
                 (should-not cancel-called)
                 (should run-timer-called)
                 (should (boundp 'ecc-term-claude-state-timer))
                 (should (local-variable-p 'ecc-term-claude-state-timer)))))
            
            ;; Test with existing timer
            (setq run-timer-called nil)
            (let ((ecc-term-claude-state-timer 'existing-timer))
              (ecc-test-with-temp-buffer
               "Test content"
               (lambda ()
                 (ecc-term-claude-setup-timer)
                 (should cancel-called)
                 (should run-timer-called))))))
      
      ;; Restore original functions
      (fset 'cancel-timer orig-cancel-timer)
      (fset 'run-with-timer orig-run-with-timer))))

(ert-deftest test-ecc-term-claude-setup-hooks ()
  "Test hook setup functionality."
  (ecc-test-with-temp-buffer
   "Test content"
   (lambda ()
     ;; Set up hooks
     (ecc-term-claude-setup-hooks)
     
     ;; Check vterm-update-functions has a hook
     (should vterm-update-functions)
     
     ;; Check kill-buffer-hook has our cleanup function
     (should (member 'ecc-term-claude-cleanup-buffer
                    (buffer-local-value 'kill-buffer-hook (current-buffer)))))))

(ert-deftest test-ecc-term-claude-setup-keys ()
  "Test keybinding setup functionality."
  (ecc-test-with-temp-buffer
   "Test content"
   (lambda ()
     ;; Set up keys
     (ecc-term-claude-setup-keys)
     
     ;; Check a few key bindings
     (should (eq (lookup-key (current-local-map) (kbd "C-c C-y"))
                'ecc-term-claude-send-yes))
     (should (eq (lookup-key (current-local-map) (kbd "C-c C-n"))
                'ecc-term-claude-send-no))
     (should (eq (lookup-key (current-local-map) (kbd "C-c C-a"))
                'ecc-term-claude-toggle-auto-mode)))))

(ert-deftest test-ecc-term-claude-cleanup-buffer-vterm ()
  "Test buffer cleanup functionality."
  (let ((ecc-buffer-registered-buffers-alist nil)
        (ecc-term-claude-state-timer (run-with-idle-timer 1000 nil #'ignore)))
    
    (ecc-test-with-temp-buffer
     "Test content"
     (lambda ()
       ;; Register the buffer
       (push (cons (current-buffer) nil) ecc-buffer-registered-buffers-alist)
       
       ;; Set up a timer
       (setq-local ecc-term-claude-state-timer ecc-term-claude-state-timer)
       
       ;; Clean up the buffer
       (ecc-term-claude-cleanup-buffer)
       
       ;; Check the timer was cancelled
       (should-not ecc-term-claude-state-timer)
       
       ;; Check the buffer was unregistered
       (should-not (assoc (current-buffer) ecc-buffer-registered-buffers-alist))))))

(provide 'test-ecc-term-claude-setup)

;;; test-ecc-term-claude-setup.el ends here