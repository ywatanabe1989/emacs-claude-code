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
                  vterm-update-functions '(ecc-send-accept))  ;; Add hook to show it's active
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
  (let ((orig-buffer ecc-buffer-current-buffer)
        (mock-buffer (generate-new-buffer "*CLAUDE-CODE-01*"))
        (rename-called nil)
        (rename-args nil))
    (unwind-protect
        (progn
          ;; Set up the mock buffer with a buffer-local variable for the original name
          (with-current-buffer mock-buffer
            (set (make-local-variable 'ecc-original-name) "*CLAUDE-CODE-01*"))
          
          ;; Set as active buffer
          (setq ecc-buffer-current-buffer mock-buffer)
          
          ;; Mock the rename-buffer function to capture calls
          (cl-letf (((symbol-function 'rename-buffer)
                     (lambda (name &optional unique)
                       (setq rename-called t
                             rename-args (list name unique)))))
            ;; Call function under test - should add [A] suffix
            (ecc-buffer-rename-buffer t)
            
            ;; Verify rename was called with correct arguments
            (should rename-called)
            (should (string-match-p "\\[A\\]$" (car rename-args)))))
      
      ;; Clean up
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (setq ecc-buffer-current-buffer orig-buffer))))

(ert-deftest test-ecc-buffer-rename-buffer-when-disabled ()
  (let ((orig-buffer ecc-buffer-current-buffer)
        (mock-buffer (generate-new-buffer "*CLAUDE-CODE-01*[A]"))
        (rename-called nil)
        (rename-args nil))
    (unwind-protect
        (progn
          ;; Set up the mock buffer with a buffer-local variable for the original name
          (with-current-buffer mock-buffer
            (set (make-local-variable 'ecc-original-name) "*CLAUDE-CODE-01*"))
          
          ;; Set as active buffer
          (setq ecc-buffer-current-buffer mock-buffer)
          
          ;; Mock the rename-buffer function to capture calls
          (cl-letf (((symbol-function 'rename-buffer)
                     (lambda (name &optional unique)
                       (setq rename-called t
                             rename-args (list name unique)))))
            ;; Call function under test - should remove [A] suffix
            (ecc-buffer-rename-buffer nil)
            
            ;; Verify rename was called with correct arguments
            (should rename-called)
            (should (equal (car rename-args) "*CLAUDE-CODE-01*"))))
      
      ;; Clean up
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (setq ecc-buffer-current-buffer orig-buffer))))

(ert-deftest test-ecc-auto-enable-uses-current-buffer-when-no-buffer-exists ()
  (let ((orig-active-buffer ecc-buffer-current-buffer)
        (orig-buffers ecc-buffers))
    (unwind-protect
        (progn
          (setq ecc-buffer-current-buffer nil
                ecc-buffers (list (current-buffer)))  ;; Add current-buffer to buffers list
          (cl-letf
              (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
               ((symbol-function 'ecc-buffer-rename-buffer) #'ignore)
               ((symbol-function 'ecc-update-mode-line) #'ignore)
               ((symbol-function 'ecc-update-mode-line-all-buffers) #'ignore)
               ((symbol-function 'add-hook) #'ignore)
               ((symbol-function 'remove-hook) #'ignore)
               ((symbol-function 'run-with-timer)
                (lambda (&rest _) 'mock-timer))
               ((symbol-function 'vterm-send-key) #'ignore)
               ((symbol-function 'ecc-buffer-register-as-active)
                (lambda (buf) (setq ecc-buffer-current-buffer buf)))
               ((symbol-function 'current-buffer)
                (lambda () (list 'buffer)))
               ((symbol-function 'buffer-name)
                (lambda (&rest _) "buffer-name"))
               ((symbol-function 'ecc-buffer-registry-cleanup-buffers) #'ignore))
            (ecc-auto-enable)
            (should ecc-buffer-current-buffer)))
      (setq ecc-buffer-current-buffer orig-active-buffer
            ecc-buffers orig-buffers))))

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
            (should (eq hook-added 'ecc-send-accept))
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
                                  (eq function 'ecc-send-accept))
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
  (let ((orig-active-buffer ecc-buffer-current-buffer))
    ;; Ensure vterm-update-functions is defined
    (defvar vterm-update-functions nil)
    (let ((orig-vterm-update-functions vterm-update-functions))
      (unwind-protect
          (progn
            (setq ecc-buffer-current-buffer (current-buffer)
                  vterm-update-functions nil)
            
            ;; Before calling the function, add-hook is mock to set the value
            (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
                      ((symbol-function 'buffer-live-p) (lambda (&rest _) t))
                      ((symbol-function 'add-hook) 
                       (lambda (hook function)
                         (when (eq hook 'vterm-update-functions)
                           (setq vterm-update-functions (cons function vterm-update-functions))))))
              
              ;; Run the function that should add the hook
              (--ecc-auto-check-and-restart)
              
              ;; Check if ecc-send-accept was added to vterm-update-functions
              (should (member 'ecc-send-accept vterm-update-functions))))
        
        ;; Restore original values
        (setq ecc-buffer-current-buffer orig-active-buffer
              vterm-update-functions orig-vterm-update-functions)))))

(ert-deftest test---ecc-auto-check-and-restart-finds-vterm-buffer-when-needed ()
  (let ((orig-active-buffer ecc-buffer-current-buffer)
        (orig-buffers ecc-buffers)
        (mock-buffer 'mock-vterm-buffer))
    (unwind-protect
        (progn
          ;; Set the active buffer to nil to trigger the search
          (setq ecc-buffer-current-buffer nil
                ecc-active-buffer nil)
          
          ;; Mock all the necessary functions
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda () (list 'buffer1 'buffer2 mock-buffer)))
                    ((symbol-function 'derived-mode-p)
                     (lambda (&rest _) t)) ;; Always return true for vterm-mode check
                    ((symbol-function 'buffer-live-p) 
                     (lambda (&rest _) t))
                    ((symbol-function 'ecc-send-accept) 
                     (lambda () nil))
                    ((symbol-function 'ecc-update-mode-line-all-buffers) 
                     (lambda () nil))
                    ;; Direct assignment instead of using the function
                    ((symbol-function 'ecc-buffer-register-as-active)
                     (lambda (buf) 
                       (setq ecc-buffer-current-buffer buf
                             ecc-active-buffer buf)
                       buf)))
            
            ;; Run the function under test
            (--ecc-auto-check-and-restart)
            
            ;; Test that some buffer was found (either name should work due to defvaralias)
            (should (or ecc-buffer-current-buffer ecc-active-buffer))))
      
      ;; Restore original values
      (setq ecc-buffer-current-buffer orig-active-buffer
            ecc-buffers orig-buffers))))


(provide 'test-ecc-start-stop)

(when (not load-file-name)
  (message "test-ecc-start-stop.el loaded: %s"
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))