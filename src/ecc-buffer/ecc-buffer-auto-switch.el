;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 03:16:30>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-buffer/ecc-buffer-auto-switch.el

;;; Commentary:
;;; This module provides functionality for automatically switching between
;;; Claude buffers.

;;; Code:

(require 'ecc-buffer/ecc-buffer-variables)
(require 'ecc-buffer/ecc-buffer-registry)
(require 'ecc-buffer/ecc-buffer-current)
(require 'ecc-buffer/ecc-buffer-state)

(defvar ecc-buffer-auto-switch-mode nil
  "When non-nil, enable automatic switching between Claude buffers.")

;;;###autoload
(define-minor-mode ecc-buffer-auto-switch-mode
  "Toggle Claude buffer auto-switching mode.
When enabled, provides automatic switching between Claude buffers."
  :global t
  :init-value nil
  :lighter " ClaudeSwitch"
  (if ecc-buffer-auto-switch-mode
      (message "Claude buffer auto-switching enabled")
    (message "Claude buffer auto-switching disabled")))

(defun ecc-buffer-auto-switch-toggle ()
  "Toggle the auto-switch mode for Claude buffers."
  (interactive)
  (ecc-buffer-auto-switch-mode (if ecc-buffer-auto-switch-mode -1 1)))

(defun ecc-buffer-auto-switch-next-buffer ()
  "Switch to the next Claude buffer in the list.
Returns the buffer that was switched to, or nil if no valid buffer exists.
Handles killed buffers gracefully by skipping them."
  (interactive)
  (let* ((current (ecc-buffer-get-current-buffer))
         (all-buffers (ecc-buffer-get-all-buffers))
         (valid-buffers (seq-filter #'buffer-live-p all-buffers))
         next-buffer)
    
    ;; If no valid buffers, return nil
    (if (null valid-buffers)
        (progn 
          (message "No valid Claude buffers found")
          nil)
      ;; If current buffer is not in the list, is nil, or is not live, use the first one
      (if (or (null current) 
              (not (buffer-live-p current))
              (not (memq current valid-buffers)))
          (setq next-buffer (car valid-buffers))
        ;; Otherwise, find the next buffer in the list
        (let* ((pos (seq-position valid-buffers current))
               (next-pos (if (= pos (1- (length valid-buffers)))
                             0  ; wrap around to the beginning
                           (1+ pos))))
          (setq next-buffer (nth next-pos valid-buffers))))
      
      ;; Only switch to the buffer if it's live
      (when (and next-buffer (buffer-live-p next-buffer))
        ;; Switch to the buffer
        (ecc-buffer-set-current-buffer next-buffer)
        ;; Return the buffer we switched to
        next-buffer))))

(defun ecc-buffer-auto-switch-previous-buffer ()
  "Switch to the previous Claude buffer in the list.
Returns the buffer that was switched to, or nil if no valid buffer exists.
Handles killed buffers gracefully by skipping them."
  (interactive)
  (let* ((current (ecc-buffer-get-current-buffer))
         (all-buffers (ecc-buffer-get-all-buffers))
         (valid-buffers (seq-filter #'buffer-live-p all-buffers))
         prev-buffer)
    
    ;; If no valid buffers, return nil
    (if (null valid-buffers)
        (progn 
          (message "No valid Claude buffers found")
          nil)
      ;; If current buffer is not in the list, is nil, or is not live, use the last one
      (if (or (null current) 
              (not (buffer-live-p current))
              (not (memq current valid-buffers)))
          (setq prev-buffer (car (last valid-buffers)))
        ;; Otherwise, find the previous buffer in the list
        (let* ((pos (seq-position valid-buffers current))
               (prev-pos (if (= pos 0)
                             (1- (length valid-buffers))  ; wrap around to the end
                           (1- pos))))
          (setq prev-buffer (nth prev-pos valid-buffers))))
      
      ;; Only switch to the buffer if it's live
      (when (and prev-buffer (buffer-live-p prev-buffer))
        ;; Switch to the buffer
        (ecc-buffer-set-current-buffer prev-buffer)
        ;; Return the buffer we switched to
        prev-buffer))))

;; Provide the feature
(provide 'ecc-buffer/ecc-buffer-auto-switch)
(provide 'ecc-buffer-auto-switch)

;;; ecc-buffer-auto-switch.el ends here