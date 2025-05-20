;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 10:45:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-term-claude-buffer.el

;;; Commentary:
;;; Buffer management functions for Claude terminal mode.
;;; This module provides functions for registering, tracking, and managing
;;; Claude terminal buffers, improving the organization of buffer-related
;;; functionality previously scattered across multiple files.

(require 'ecc-variables)
(require 'ecc-term-claude-state)

;;; Code:

;;;; Buffer registration and tracking

;;;###autoload
(defun ecc-term-claude-register-buffer (&optional buffer)
  "Register BUFFER as a Claude buffer in the tracking system.
Adds the buffer to `ecc-buffer-registered-buffers-alist` and sets
it as the current Claude buffer in `ecc-buffer-current-buffer`.

Arguments:
  BUFFER: Optional. The buffer to register. If nil, uses the current
          buffer. This should be a live buffer object.

Returns:
  The registered buffer object.

Errors:
  Signals an error if the buffer is not alive."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (unless (buffer-live-p buf)
      (user-error "Buffer is not alive"))
    
    ;; Register the buffer
    (unless (assoc buf ecc-buffer-registered-buffers-alist)
      (push (cons buf nil) ecc-buffer-registered-buffers-alist)
      (when (called-interactively-p 'any)
        (message "Buffer '%s' registered as Claude buffer" (buffer-name buf))))
    
    ;; Set as current Claude buffer
    (setq ecc-buffer-current-buffer buf)
    buf))

;;;###autoload
(defun ecc-term-claude-unregister-buffer (&optional buffer)
  "Unregister BUFFER from Claude buffer tracking.
Removes the buffer from `ecc-buffer-registered-buffers-alist`.

Arguments:
  BUFFER: Optional. The buffer to unregister. If nil, uses the current
          buffer.

Returns:
  t if buffer was unregistered, nil if it wasn't registered."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (when (assoc buf ecc-buffer-registered-buffers-alist)
      (setq ecc-buffer-registered-buffers-alist
            (assq-delete-all buf ecc-buffer-registered-buffers-alist))
      (when (called-interactively-p 'any)
        (message "Buffer '%s' unregistered from Claude buffers" (buffer-name buf)))
      t)))

;;;###autoload
(defun ecc-term-claude-get-current-buffer ()
  "Get the current Claude buffer.
Returns the buffer object stored in `ecc-buffer-current-buffer`,
or nil if there is no current buffer or if it's not alive."
  (when (and (boundp 'ecc-buffer-current-buffer)
             (buffer-live-p ecc-buffer-current-buffer))
    ecc-buffer-current-buffer))

;;;###autoload
(defun ecc-term-claude-set-current-buffer (buffer)
  "Set BUFFER as the current Claude buffer.
Updates `ecc-buffer-current-buffer` with the specified buffer,
registering it first if needed.

Arguments:
  BUFFER: The buffer to set as current. Must be a live buffer.

Returns:
  The buffer that was set as current."
  (interactive "bSet as current Claude buffer: ")
  (unless (buffer-live-p buffer)
    (user-error "Buffer is not alive"))
  
  ;; Register if not already registered
  (unless (assoc buffer ecc-buffer-registered-buffers-alist)
    (ecc-term-claude-register-buffer buffer))
  
  ;; Set as current
  (setq ecc-buffer-current-buffer buffer)
  (when (called-interactively-p 'any)
    (message "Buffer '%s' set as current Claude buffer" (buffer-name buffer)))
  buffer)

;;;###autoload
(defun ecc-term-claude-list-buffers ()
  "List all registered Claude buffers.
Returns a list of buffer objects that are registered as Claude buffers.
Only includes buffers that are still alive."
  (let ((buffers nil))
    (dolist (entry ecc-buffer-registered-buffers-alist)
      (let ((buffer (car entry)))
        (when (buffer-live-p buffer)
          (push buffer buffers))))
    (nreverse buffers)))

;;;###autoload
(defun ecc-term-claude-switch-to-buffer ()
  "Switch to a registered Claude buffer.
Provides completion for choosing among all registered Claude buffers."
  (interactive)
  (let* ((buffers (ecc-term-claude-list-buffers))
         (names (mapcar #'buffer-name buffers))
         (selection (completing-read "Claude buffer: " names nil t)))
    (switch-to-buffer (get-buffer selection))))

;;;; Buffer cleanup

;;;###autoload
(defun ecc-term-claude-cleanup-buffer ()
  "Clean up Claude resources when a buffer is killed.
Cancels timers, removes hooks, and unregisters the buffer.
This function is typically added to `kill-buffer-hook`."
  (when (buffer-live-p (current-buffer))
    ;; Cancel any timers
    (when (bound-and-true-p ecc-term-claude-state-timer)
      (cancel-timer ecc-term-claude-state-timer)
      (setq ecc-term-claude-state-timer nil))
    
    ;; Clean up visual aids if available
    (when (and (featurep 'ecc-term-visual-aid)
               (fboundp 'ecc-term-visual-aid-clear-all))
      (ecc-term-visual-aid-clear-all))
    
    ;; Unregister from buffer list
    (ecc-term-claude-unregister-buffer (current-buffer))))

;;;; Buffer utilities

;;;###autoload
(defun ecc-term-claude-buffer-p (buffer)
  "Return non-nil if BUFFER is a registered Claude buffer.
Arguments:
  BUFFER: The buffer to check. Can be a buffer object or name.

Returns:
  Non-nil if the buffer is registered as a Claude buffer, nil otherwise."
  (let ((buf (if (bufferp buffer) buffer (get-buffer buffer))))
    (and buf (assoc buf ecc-buffer-registered-buffers-alist))))

;;;###autoload
(defun ecc-term-claude-rename-buffer (new-name)
  "Rename the current Claude buffer to NEW-NAME.
Updates the registration to track the buffer under its new name.

Arguments:
  NEW-NAME: The new name for the buffer. If it doesn't start with '*',
            asterisks will be added automatically."
  (interactive "sNew buffer name: ")
  (let* ((buf (current-buffer))
         (formatted-name (if (string-match-p "^\\*.*\\*$" new-name)
                            new-name
                          (format "*%s*" new-name))))
    ;; Save registration state
    (let ((was-registered (ecc-term-claude-buffer-p buf))
          (was-current (eq buf (ecc-term-claude-get-current-buffer))))
      ;; Rename the buffer
      (rename-buffer formatted-name t)
      
      ;; Re-establish registration if needed
      (when was-registered
        (ecc-term-claude-register-buffer buf)
        (when was-current
          (ecc-term-claude-set-current-buffer buf))))))

;;;; Backward compatibility

;; Legacy function name
(defalias 'ecc-register-buffer 'ecc-term-claude-register-buffer)

(provide 'ecc-term-claude-buffer)

;;; ecc-term-claude-buffer.el ends here