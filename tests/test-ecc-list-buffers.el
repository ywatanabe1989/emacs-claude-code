;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-27 15:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-list-buffers.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'ecc-convenience-commands)

(ert-deftest test-ecc-list-buffers-function-exists ()
  "Test that ecc-list-buffers function exists and is interactive."
  (should (fboundp 'ecc-list-buffers))
  (should (commandp 'ecc-list-buffers)))

(ert-deftest test-ecc-buffer-list-mode-exists ()
  "Test that ecc-buffer-list-mode is defined."
  (should (fboundp 'ecc-buffer-list-mode)))

(ert-deftest test-ecc-buffer-list-get-buffer-name ()
  "Test buffer name extraction from buffer list line."
  (with-temp-buffer
    (insert "*test-buffer*       Buffer-Local    vterm-mode           Yes    Yes\n")
    (goto-char (point-min))
    (should (string= "*test-buffer*" (ecc-buffer-list-get-buffer-name)))))

(ert-deftest test-ecc-list-buffers-creates-buffer ()
  "Test that ecc-list-buffers creates the Claude Buffers buffer."
  (let ((original-buffers (buffer-list)))
    (unwind-protect
        (progn
          ;; Call the function
          (ecc-list-buffers)
          
          ;; Verify buffer was created
          (should (get-buffer "*Claude Buffers*"))
          
          ;; Verify buffer content
          (with-current-buffer "*Claude Buffers*"
            (should (string-match-p "Claude Auto-Response Buffer Status" 
                                   (buffer-string)))
            (should (string-match-p "Global Auto-Response:" 
                                   (buffer-string)))
            
            ;; Verify mode is set correctly
            (should (eq major-mode 'ecc-buffer-list-mode))))
      
      ;; Cleanup
      (when (get-buffer "*Claude Buffers*")
        (kill-buffer "*Claude Buffers*")))))

(ert-deftest test-ecc-buffer-list-mode-keymap ()
  "Test that ecc-buffer-list-mode has the correct key bindings."
  (with-temp-buffer
    (ecc-buffer-list-mode)
    
    ;; Test key bindings exist
    (should (eq (lookup-key ecc-buffer-list-mode-map (kbd "RET"))
               'ecc-buffer-list-select))
    (should (eq (lookup-key ecc-buffer-list-mode-map (kbd "SPC"))
               'ecc-buffer-list-select))
    (should (eq (lookup-key ecc-buffer-list-mode-map (kbd "q"))
               'quit-window))
    (should (eq (lookup-key ecc-buffer-list-mode-map (kbd "g"))
               'ecc-list-buffers))
    (should (eq (lookup-key ecc-buffer-list-mode-map (kbd "a"))
               'ecc-buffer-list-toggle-auto-response))))

(ert-deftest test-ecc-buffer-list-select-with-existing-buffer ()
  "Test buffer selection with an existing buffer."
  (let ((test-buffer (generate-new-buffer "*test-selection*")))
    (unwind-protect
        (with-temp-buffer
          (ecc-buffer-list-mode)
          (insert "*test-selection*    Disabled        fundamental-mode     No     No\n")
          (goto-char (point-min))
          
          ;; Mock switch-to-buffer to capture the call
          (let ((switched-buffer nil)
                (original-switch-to-buffer (symbol-function 'switch-to-buffer)))
            (fset 'switch-to-buffer (lambda (buffer)
                                     (setq switched-buffer buffer)))
            
            (unwind-protect
                (progn
                  (ecc-buffer-list-select)
                  (should (eq switched-buffer test-buffer)))
              
              ;; Restore original function
              (fset 'switch-to-buffer original-switch-to-buffer))))
      
      ;; Cleanup
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer)))))

(ert-deftest test-ecc-list-buffers-keybinding-exists ()
  "Test that C-c c l is bound to ecc-list-buffers."
  (should (eq (lookup-key ecc-mode-map (kbd "C-c c l"))
             'ecc-list-buffers)))

(provide 'test-ecc-list-buffers)

;;; test-ecc-list-buffers.el ends here