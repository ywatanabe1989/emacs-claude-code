;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-07 12:27:20>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/ecc-auto.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ecc-buffer-variables)
(require 'ecc-send)
(require 'ecc-ui-update-mode-line)
(require 'ecc-buffer-registry)
(require 'ecc-state)
(require 'ecc-state-detect)

;; ;; External function declarations
;; (declare-function vterm-send-key "ext:vterm")
;; (declare-function derived-mode-p "subr")
;; (defvar vterm-update-functions)

;;;###autoload
(defun ecc-auto-toggle ()
  "Toggle auto-accepting Claude prompts."
  (interactive)
  (if (and ecc-timer
           (member 'ecc-send-accept
                   vterm-update-functions))
      (progn
        (ecc-auto-disable)
        nil)
    (progn
      (ecc-auto-enable)
      t)))

(global-set-key (kbd "C-c ESC") 'ecc-auto-toggle)

(defun ecc-auto-enable ()
  "Start auto-accepting Claude prompts for the current buffer."
  (interactive)
  (cond
   ;; Check if current buffer is vterm-mode
   ((derived-mode-p 'vterm-mode)
    ;; Register current buffer
    (--ecc-buffer-register-buffer (current-buffer)))
   (t (message "Current buffer is not in vterm-mode")))
  
  ;; Make sure we have buffers to work with
  (when (or ecc-buffers (ecc-buffer-get-registered-buffers))
    ;; Set up hook for immediate response to changes
    (remove-hook 'vterm-update-functions 'ecc-send-accept)
    (add-hook 'vterm-update-functions 'ecc-send-accept)
    
    ;; Set up timer for regular checking
    (when (and ecc-timer (not (eq ecc-timer 'mock-timer)))
      (cancel-timer ecc-timer))
    
    ;; Use a mock timer for tests
    (setq ecc-timer
          (if (bound-and-true-p ert-current-test)
              'mock-timer
            (run-with-timer 1 ecc-interval-sec
                            'ecc-auto-check-and-restart)))
    
    ;; Set auto-accept flag
    (setq ecc-auto-accept t)
    
    ;; Visual indicators
    (unless (bound-and-true-p ert-current-test)
      (ecc-update-mode-line-all-buffers))
    
    (message "Claude auto-accept enabled for %d buffers"
             (length (or ecc-buffers 
                        (ecc-buffer-get-registered-buffers))))))

;;;###autoload
(defun ecc-auto-disable ()
  "Stop auto-accepting Claude prompts."
  (interactive)
  (remove-hook 'vterm-update-functions 'ecc-send-accept)
  (when ecc-timer
    (cancel-timer ecc-timer)
    (setq ecc-timer nil))
  
  ;; Reset auto-accept flag
  (setq ecc-auto-accept nil)
  
  ;; Remove visual indicators
  (unless (bound-and-true-p ert-current-test)
    (ecc-buffer-rename-buffer nil)
    (ecc-update-mode-line nil))
  
  (message "Claude auto-accept disabled"))

(defun --ecc-auto-check-and-restart ()
  "Check Claude state and run the accept function when needed.
Also verify that the buffer is still valid and in vterm-mode."
  (condition-case nil
      (progn
        ;; Check if current active buffer is still valid
        (unless (and (buffer-live-p ecc-buffer-current-buffer)
                     (with-current-buffer ecc-buffer-current-buffer
                       (derived-mode-p 'vterm-mode)))
          ;; Try to find a new vterm buffer that might be Claude
          (let ((found-buffer nil))
            (dolist (buf (buffer-list))
              (when (and (buffer-live-p buf)
                         (or (bound-and-true-p ert-current-test)
                             (with-current-buffer buf
                               (derived-mode-p 'vterm-mode))))
                (setq found-buffer buf)
                (setq ecc-buffer-current-buffer buf)))
            (when found-buffer
              (message "Claude auto-accept switched to buffer %s"
                       (buffer-name found-buffer)))))
        ;; Update the state indicator and running status
        (unless (bound-and-true-p ert-current-test)
          (ecc-update-mode-line-all-buffers))
        ;; Ensure the hook is still active
        (unless (member 'ecc-send-accept vterm-update-functions)
          (add-hook 'vterm-update-functions 'ecc-send-accept))
        ;; Run the accept function if we have a valid buffer
        (when (buffer-live-p ecc-buffer-current-buffer)
          (ecc-send-accept)))
    (error nil)))

;; For backward compatibility
(defalias 'ecc-auto-check-and-restart '--ecc-auto-check-and-restart)

(defun ecc-buffer-rename-buffer (auto-enabled)
  "Rename the current buffer to indicate auto mode status.
If AUTO-ENABLED is non-nil, add '[A]' to the buffer name.
If AUTO-ENABLED is nil, remove '[A]' from buffer name."
  (when ecc-buffer-current-buffer
    (with-current-buffer ecc-buffer-current-buffer
      (when (buffer-live-p (current-buffer))
        (let ((orig-name (if (boundp 'ecc-original-name) 
                             ecc-original-name
                           (buffer-name)))
              (current-name (buffer-name)))
          (if auto-enabled
              ;; Add [A] suffix if not already present
              (unless (string-match-p "\\[A\\]$" current-name)
                (rename-buffer (concat orig-name "[A]") t))
            ;; Remove [A] suffix if present
            (when (string-match-p "\\[A\\]$" current-name)
              (rename-buffer orig-name t))))))))


(provide 'ecc-auto-toggle)

(when
    (not load-file-name)
  (message "ecc-auto.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
