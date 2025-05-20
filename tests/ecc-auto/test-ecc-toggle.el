;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 21:15:10>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-start-stop.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ert)
(require 'ecc-auto)
(require 'cl-lib)

(declare-function cl-letf "cl-lib" (bindings &rest body))

(ert-deftest test-ecc-auto-loadable ()
  (should (featurep 'ecc-auto)))

(ert-deftest test-ecc-auto-toggle-defined ()
  (should (fboundp 'ecc-auto-toggle)))

(ert-deftest test-ecc-auto-toggle-starts-when-inactive ()
  (let ((started nil)
        (stopped nil)
        (orig-timer ecc-timer))
    ;; Ensure vterm-update-functions is defined before the test
    (defvar vterm-update-functions nil)
    (let ((orig-update-funcs vterm-update-functions))
      (unwind-protect
          (progn
            (setq ecc-timer nil
                  vterm-update-functions nil)
            (cl-letf (((symbol-function 'ecc-auto-enable)
                      (lambda () (setq started t)))
                      ((symbol-function 'ecc-auto-disable)
                      (lambda () (setq stopped t))))
              (ecc-auto-toggle)
              (should started)
              (should-not stopped)))
        (setq ecc-timer orig-timer
              vterm-update-functions orig-update-funcs)))))

(ert-deftest test-ecc-auto-toggle-stops-when-active ()
  (let ((started nil)
        (stopped nil)
        (orig-timer ecc-timer))
    ;; Ensure vterm-update-functions is defined before the test
    (defvar vterm-update-functions nil)
    (let ((orig-update-funcs vterm-update-functions))
      (unwind-protect
          (progn
            (setq ecc-timer t  ;; Set timer to non-nil to indicate active state
                  vterm-update-functions '(ecc-term-send-accept))  ;; Add hook to show it's active
            (cl-letf
                (((symbol-function 'ecc-auto-enable)
                  (lambda () (setq started nil)))
                ((symbol-function 'ecc-auto-disable)
                  (lambda () (setq stopped t))))
              (ecc-auto-toggle)
              (should-not started)
              (should stopped)))
        (setq ecc-timer orig-timer
              vterm-update-functions orig-update-funcs)))))

(ert-deftest test-ecc-buffer-rename-buffer-when-enabled ()
  "Test that buffer is renamed correctly when auto mode is enabled."
  ;; We'll use a more direct approach with our newly added function
  (let ((orig-buffer ecc-buffer-current-buffer)
        (test-buffer (generate-new-buffer "*CLAUDE-CODE-01*"))
        (rename-called nil)
        (rename-args nil))
    
    (unwind-protect
        (progn
          ;; Set up the test buffer with a buffer-local variable for the original name
          (with-current-buffer test-buffer
            (set (make-local-variable 'ecc-original-name) "*CLAUDE-CODE-01*"))
          
          ;; Set as current buffer
          (setq ecc-buffer-current-buffer test-buffer)
          
          ;; Mock rename-buffer to capture calls
          (cl-letf (((symbol-function 'rename-buffer)
                     (lambda (name &optional unique)
                       (setq rename-called t
                             rename-args (list name unique))))
                    ((symbol-function 'buffer-live-p) (lambda (&rest _) t)))
                             
            ;; Call function under test - should add [A] suffix
            (ecc-buffer-rename-buffer t)
            
            ;; Verify that rename was called with correct arguments
            (should rename-called)
            (should (string-match-p "\\[A\\]$" (car rename-args)))))
      
      ;; Clean up
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer))
      (setq ecc-buffer-current-buffer orig-buffer))))

(ert-deftest test-ecc-buffer-rename-buffer-when-disabled ()
  "Test that buffer is renamed correctly when auto mode is disabled."
  (let ((orig-buffer ecc-buffer-current-buffer)
        (test-buffer (generate-new-buffer "*CLAUDE-CODE-01*[A]"))
        (rename-called nil)
        (rename-args nil))
    
    (unwind-protect
        (progn
          ;; Set up the test buffer with a buffer-local variable for the original name
          (with-current-buffer test-buffer
            (set (make-local-variable 'ecc-original-name) "*CLAUDE-CODE-01*"))
          
          ;; Set as current buffer
          (setq ecc-buffer-current-buffer test-buffer)
          
          ;; Mock rename-buffer to capture calls
          (cl-letf (((symbol-function 'rename-buffer)
                     (lambda (name &optional unique)
                       (setq rename-called t
                             rename-args (list name unique))))
                    ((symbol-function 'buffer-live-p) (lambda (&rest _) t)))
            
            ;; Call function under test - should remove [A] suffix
            (ecc-buffer-rename-buffer nil)
            
            ;; Verify rename was called with correct arguments
            (should rename-called)
            (should (equal (car rename-args) "*CLAUDE-CODE-01*"))))
      
      ;; Clean up
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer))
      (setq ecc-buffer-current-buffer orig-buffer))))

(ert-deftest test-ecc-auto-enable-uses-current-buffer-when-no-buffer-exists ()
  "Test that ecc-auto-enable properly registers current buffer when no buffer exists."
  :expected-result :passed ;; We expect this test to pass now
  (let ((orig-buffer ecc-buffer-current-buffer)
        (test-buffer (generate-new-buffer "*test-claude*")))
    
    (unwind-protect
        (progn
          ;; Set up test environment
          (setq ecc-buffer-current-buffer nil)
          
          ;; Set up test buffer
          (with-current-buffer test-buffer
            (setq major-mode 'vterm-mode)
            (setq mode-name "vterm"))
          
          ;; Mock necessary functions
          (cl-letf (((symbol-function 'current-buffer) 
                     (lambda () test-buffer))
                    ((symbol-function 'derived-mode-p) 
                     (lambda (mode &optional _) (eq mode 'vterm-mode)))
                    ((symbol-function 'buffer-live-p) 
                     (lambda (&rest _) t))
                    ((symbol-function 'ecc-buffer-registry-cleanup-buffers) 
                     #'ignore)
                    ((symbol-function 'ecc-update-mode-line-all-buffers) 
                     #'ignore)
                    ((symbol-function 'buffer-name) 
                     (lambda (&rest _) "*test-claude*"))
                    ((symbol-function 'add-hook) #'ignore)
                    ((symbol-function 'remove-hook) #'ignore)
                    ((symbol-function 'run-with-timer) 
                     (lambda (&rest _) 'mock-timer))
                    ((symbol-function 'cancel-timer) #'ignore)
                    ((symbol-function 'ecc-buffer-rename-buffer) #'ignore)
                    ((symbol-function '--ecc-buffer-register-buffer)
                     (lambda (buf) 
                       (setq ecc-buffer-current-buffer buf)
                       (setq ecc-buffers (list buf))
                       buf)))
            
            ;; Run the function under test
            (ecc-auto-enable)
            
            ;; Test that the current buffer was set
            (should (eq ecc-buffer-current-buffer test-buffer))))
      
      ;; Clean up
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer))
      (setq ecc-buffer-current-buffer orig-buffer))))

(ert-deftest test-ecc-auto-enable-adds-hook-and-starts-timer ()
  (let ((orig-active-buffer ecc-buffer-current-buffer)
        (orig-timer ecc-timer)
        (hook-added nil)
        (timer-started nil))
    (unwind-protect
        (progn
          (setq ecc-buffer-current-buffer (current-buffer)
                ecc-timer nil)
          (cl-letf
              (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
               ((symbol-function 'add-hook)
                (lambda (hook function)
                  (when (eq hook 'vterm-update-functions)
                    (setq hook-added function))))
               ((symbol-function 'run-with-timer)
                (lambda (secs repeat function)
                  (setq timer-started function)
                  'mock-timer))
               ((symbol-function 'remove-hook) #'ignore)
               ((symbol-function 'ecc-buffer-rename-buffer) #'ignore)
               ((symbol-function 'ecc-update-mode-line) #'ignore)
               ((symbol-function 'ecc-update-mode-line-all-buffers) #'ignore)
               ((symbol-function 'vterm-send-key) #'ignore)
               ((symbol-function 'ecc-buffer-registry-cleanup-buffers) #'ignore))
            (ecc-auto-enable)
            (should (eq hook-added 'ecc-term-send-accept))
            (should (eq timer-started 'ecc-auto-check-and-restart))
            (should (eq ecc-timer 'mock-timer))))
      (setq ecc-buffer-current-buffer orig-active-buffer
            ecc-timer orig-timer))))

(ert-deftest test-ecc-auto-disable-removes-hook-and-cancels-timer ()
  (let ((orig-active-buffer ecc-buffer-current-buffer)
        (orig-timer ecc-timer)
        (hook-removed nil)
        (timer-cancelled nil))
    (unwind-protect
        (progn
          (setq ecc-buffer-current-buffer (current-buffer)
                ecc-timer 'mock-timer)
          (cl-letf (((symbol-function 'remove-hook)
                     (lambda (hook function)
                       (when (and (eq hook 'vterm-update-functions)
                                  (eq function 'ecc-term-send-accept))
                         (setq hook-removed t))))
                    ((symbol-function 'cancel-timer)
                     (lambda (timer)
                       (when (eq timer 'mock-timer)
                         (setq timer-cancelled t))))
                    ((symbol-function 'ecc-buffer-rename-buffer) #'ignore)
                    ((symbol-function 'ecc-update-mode-line) #'ignore))
            (ecc-auto-disable)
            (should hook-removed)
            (should timer-cancelled)
            (should-not ecc-timer)))
      (setq ecc-buffer-current-buffer orig-active-buffer
            ecc-timer orig-timer))))

(ert-deftest test---ecc-auto-check-and-restart-adds-hook-when-missing ()
  "Test that the auto check function adds the necessary hook."
  :expected-result :passed ;; We expect this test to pass now
  (let ((orig-buffer ecc-buffer-current-buffer))
    ;; Define vterm-update-functions if needed
    (unless (boundp 'vterm-update-functions)
      (defvar vterm-update-functions nil))
    
    (let ((test-buffer (generate-new-buffer "*test-claude*"))
          (vterm-update-functions '()))  ;; Clear out hook functions
      
      (unwind-protect
          (progn
            ;; Set up test environment
            (setq ecc-buffer-current-buffer test-buffer)
            (with-current-buffer test-buffer
              (setq major-mode 'vterm-mode)
              (setq mode-name "vterm"))
            
            ;; Mock necessary functions
            (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
                      ((symbol-function 'buffer-live-p) (lambda (&rest _) t))
                      ((symbol-function 'ecc-term-send-accept) #'ignore)
                      ((symbol-function 'ecc-update-mode-line-all-buffers) #'ignore)
                      ;; Mock add-hook to modify vterm-update-functions directly
                      ((symbol-function 'add-hook) 
                       (lambda (hook function)
                         (when (eq hook 'vterm-update-functions)
                           (push function vterm-update-functions)))))
              
              ;; Run the function
              (--ecc-auto-check-and-restart)
              
              ;; Verify that the hook was added
              (should (member 'ecc-term-send-accept vterm-update-functions))))
        
        ;; Clean up
        (when (buffer-live-p test-buffer)
          (kill-buffer test-buffer))
        (setq ecc-buffer-current-buffer orig-buffer)))
    ;; Restore original value
    (when (boundp 'orig-update-funcs)
      (setq vterm-update-functions orig-update-funcs))))

(ert-deftest test---ecc-auto-check-and-restart-finds-vterm-buffer-when-needed ()
  "Test that auto-check function can find a vterm buffer when needed."
  :expected-result :passed ;; We expect this test to pass now
  (let ((orig-buffer ecc-buffer-current-buffer)
        (test-buffer (generate-new-buffer "*test-claude*")))
    
    (unwind-protect
        (progn
          ;; Start with no buffer to trigger search
          (setq ecc-buffer-current-buffer nil)
          
          ;; Set up test buffer
          (with-current-buffer test-buffer
            (setq major-mode 'vterm-mode)
            (setq mode-name "vterm"))
          
          ;; Mock necessary functions
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda () (list test-buffer)))
                    ((symbol-function 'derived-mode-p) 
                     (lambda (mode &optional _) (eq mode 'vterm-mode)))
                    ((symbol-function 'buffer-live-p)
                     (lambda (buf) (eq buf test-buffer)))
                    ((symbol-function 'ecc-send-accept) #'ignore)
                    ((symbol-function 'ecc-update-mode-line-all-buffers) #'ignore)
                    ((symbol-function 'add-hook) #'ignore))
            
            ;; Run the function
            (--ecc-auto-check-and-restart)
            
            ;; Test that the buffer was found and set
            (should (eq ecc-buffer-current-buffer test-buffer))))
      
      ;; Clean up
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer))
      (setq ecc-buffer-current-buffer orig-buffer))))


(provide 'test-ecc-start-stop)

(when (not load-file-name)
  (message "test-ecc-start-stop.el loaded: %s"
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))