;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-07 12:27:20>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/ecc-auto.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ecc-variables)
(require 'ecc-send)
(require 'ecc-update-mode-line)
(require 'ecc-buffer/ecc-buffer-registry)
(require 'ecc-state/ecc-state)
(require 'ecc-state/ecc-state-detect)

;; External function declarations
(declare-function vterm-send-key "ext:vterm")
(declare-function derived-mode-p "subr")
(defvar vterm-update-functions)

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

(defun ecc-auto-enable ()
  "Start auto-accepting Claude prompts for the current buffer."
  (interactive)
  (cond
   ;; Check if current buffer is vterm-mode
   ((derived-mode-p 'vterm-mode)
    ;; Register current buffer
    (--ecc-buffer-register-buffer (current-buffer)))
   (t (message "Current buffer is not in vterm-mode")))
  ;; Set up hooks for all registered buffers
  (ecc-buffer-registry-cleanup-buffers)
  (when ecc-buffers
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
    ;; Visual indicators
    (ecc-update-mode-line-all-buffers)
    (message "Claude auto-accept enabled for %d buffers"
             (length ecc-buffers))))

;;;###autoload
(defun ecc-auto-disable ()
  "Stop auto-accepting Claude prompts."
  (interactive)
  (remove-hook 'vterm-update-functions 'ecc-send-accept)
  (when ecc-timer
    (cancel-timer ecc-timer)
    (setq ecc-timer nil))
  ;; Remove visual indicators
  (ecc-buffer-rename-buffer nil)
  (ecc-update-mode-line nil)
  (message "Claude auto-accept disabled"))

;; (defun ecc-auto-check-and-restart ()
;;   "Check Claude state and run the accept function when needed.
;; Also verify that the buffer is still valid and in vterm-mode."
;;   (condition-case nil
;;       (progn
;;         ;; Check if active buffer is still valid
;;         (unless (and (buffer-live-p ecc-active-buffer)
;;                      (with-current-buffer ecc-active-buffer
;;                        (derived-mode-p 'vterm-mode)))
;;           ;; Try to find a new vterm buffer that might be Claude
;;           (let ((found-buffer nil))
;;             (dolist (buf (buffer-list))
;;               (when (buffer-live-p buf)
;;                 (with-current-buffer buf
;;                   (when (and (derived-mode-p 'vterm-mode)
;;                              (not found-buffer))
;;                     (setq found-buffer buf)))))
;;             (when found-buffer
;;               ;; Register the found buffer
;;               (--ecc-buffer-register-buffer found-buffer)
;;               (message "Claude auto-accept switched to buffer %s"
;;                        (buffer-name found-buffer)))))

;;         ;; Update the state indicator and running status
;;         (ecc-update-mode-line-all-buffers)

;;         ;; Ensure the hook is still active
;;         (unless (member 'ecc-send-accept vterm-update-functions)
;;           (add-hook 'vterm-update-functions 'ecc-send-accept))

;;         ;; Run the accept function if we have a valid buffer
;;         (when (buffer-live-p ecc-active-buffer)
;;           (ecc-send-accept)))
;;     (error nil)))

(defun ecc-auto-check-and-restart ()
  "Check Claude state and run the accept function when needed.
Also verify that the buffer is still valid and in vterm-mode."
  (condition-case nil
      (progn
        ;; Check if active buffer is still valid
        (unless (and (buffer-live-p ecc-active-buffer)
                     (with-current-buffer ecc-active-buffer
                       (derived-mode-p 'vterm-mode)))
          ;; Try to find a new vterm buffer that might be Claude
          (let ((found-buffer nil))
            (dolist (buf (buffer-list))
              (when (and (buffer-live-p buf)
                         (or (bound-and-true-p ert-current-test)
                             (with-current-buffer buf
                               (derived-mode-p 'vterm-mode))))
                (setq found-buffer buf)
                (setq ecc-active-buffer buf)))
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
        (when (buffer-live-p ecc-active-buffer)
          (ecc-send-accept)))
    (error nil)))

(defun --ecc-auto-send-template-on-y/y/n (template-text)
  "Send custom TEMPLATE-TEXT to Claude when in the y/y/n state.
This allows sending natural language responses instead of just number options."
  (interactive "sEnter your response: ")
  (when (buffer-live-p ecc-active-buffer)
    (with-current-buffer ecc-active-buffer
      (when (--ecc-state-y/y/n-p)
        (sit-for 0.5)
        (vterm-send-string template-text)
        (vterm-send-return)
        (vterm-copy-mode -1)
        (sit-for 0.5)
        (message "[ecc-send] Template Response: %s" template-text)))))


(defun --ecc-auto-send-continue ()
  "Automatically respond with continue to Claude waiting prompts."
  (interactive)
  (--ecc-auto-send-by-state
   "continue"
   (lambda ()
     (or (--ecc-state-waiting-p)
         (--ecc-state-initial-waiting-p)))))


(provide 'ecc-auto)

(when
    (not load-file-name)
  (message "ecc-auto.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
