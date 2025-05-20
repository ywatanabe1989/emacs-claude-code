;;; mock-ecc-term-claude-buffer.el --- Mock of term-claude-buffer for testing -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Created: 2025-05-21
;; Version: 1.0.0
;; Keywords: convenience, testing

;;; Commentary:
;; A mock version of ecc-term-claude-buffer.el for testing.
;; This module mimics the functionality but avoids dependencies that are
;; problematic in test environments.

;;; Code:

(require 'cl-lib)

;; Forward declarations
(defvar ecc-buffer-registered-buffers-alist nil
  "Alist of registered Claude buffers.")

(defvar ecc-buffer-current-buffer nil
  "Currently active Claude buffer.")

;;;; Buffer registration and tracking

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

(defun ecc-term-claude-unregister-buffer (&optional buffer)
  "Unregister BUFFER from the Claude buffer tracking system.
Removes the buffer from `ecc-buffer-registered-buffers-alist`
and updates the current buffer reference if needed.

Arguments:
  BUFFER: Optional. The buffer to unregister. If nil, uses the
          current buffer. This should be a live buffer object.

Returns:
  t if the buffer was successfully unregistered, nil if it wasn't
  registered to begin with."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (when (assoc buf ecc-buffer-registered-buffers-alist)
      ;; Remove from buffer list
      (setq ecc-buffer-registered-buffers-alist
            (assq-delete-all buf ecc-buffer-registered-buffers-alist))
      
      ;; If it was the current buffer, reset current buffer
      (when (eq ecc-buffer-current-buffer buf)
        (setq ecc-buffer-current-buffer
              (car (car ecc-buffer-registered-buffers-alist))))
      
      (when (called-interactively-p 'any)
        (message "Buffer '%s' unregistered as Claude buffer" (buffer-name buf)))
      t)))

(defun ecc-term-claude-get-all-buffers ()
  "Get a list of all registered Claude buffers.
Returns a list of buffer objects that are currently registered
as Claude buffers in `ecc-buffer-registered-buffers-alist`.
The list only includes live buffers."
  (let (result)
    (dolist (item ecc-buffer-registered-buffers-alist result)
      (let ((buf (car item)))
        (when (buffer-live-p buf)
          (push buf result))))))

(defun ecc-term-claude-get-current-buffer ()
  "Get the current Claude buffer.
Returns the current buffer object stored in `ecc-buffer-current-buffer`
if it is still alive. Otherwise, returns the first live buffer from
`ecc-buffer-registered-buffers-alist` or nil if there are no live buffers."
  (cond
   ;; If current buffer is set and alive, return it
   ((and ecc-buffer-current-buffer
         (buffer-live-p ecc-buffer-current-buffer))
    ecc-buffer-current-buffer)
   
   ;; Otherwise, find the first live buffer in the alist
   (t
    (cl-loop for (buf . _) in ecc-buffer-registered-buffers-alist
             when (buffer-live-p buf)
             return buf))))

(defun ecc-term-claude-set-current-buffer (buffer)
  "Set BUFFER as the current Claude buffer.
Updates `ecc-buffer-current-buffer` to the specified buffer.
If the buffer is not already registered, it will be registered first.

Arguments:
  BUFFER: The buffer to set as current. This should be a live buffer object.

Returns:
  The buffer that was set as current.

Errors:
  Signals an error if the buffer is not alive."
  (interactive "bSet Claude buffer: ")
  (let ((buf (if (bufferp buffer) buffer (get-buffer buffer))))
    (unless (buffer-live-p buf)
      (user-error "Buffer %s is not alive" (if (bufferp buffer)
                                              (buffer-name buffer)
                                            buffer)))
    
    ;; Register the buffer if it's not registered
    (unless (assoc buf ecc-buffer-registered-buffers-alist)
      (ecc-term-claude-register-buffer buf))
    
    ;; Set as current
    (setq ecc-buffer-current-buffer buf)
    
    (when (called-interactively-p 'any)
      (message "Current Claude buffer set to '%s'" (buffer-name buf)))
    
    buf))

(defun ecc-term-claude-cleanup-dead-buffers ()
  "Clean up dead buffers from the Claude buffer tracking system.
Removes any buffers from `ecc-buffer-registered-buffers-alist` that
are no longer alive (have been killed).

Returns:
  The number of buffers that were cleaned up."
  (let ((orig-count (length ecc-buffer-registered-buffers-alist))
        (dead-count 0))
    
    ;; Remove dead buffers from the alist
    (setq ecc-buffer-registered-buffers-alist
          (cl-remove-if (lambda (item)
                         (when (not (buffer-live-p (car item)))
                           (cl-incf dead-count)
                           t)
                         nil)
                       ecc-buffer-registered-buffers-alist))
    
    ;; If current buffer is dead, pick a new one
    (when (and ecc-buffer-current-buffer
              (not (buffer-live-p ecc-buffer-current-buffer)))
      (setq ecc-buffer-current-buffer
            (car (car ecc-buffer-registered-buffers-alist))))
    
    dead-count))

(provide 'ecc-term-claude-buffer)

;;; mock-ecc-term-claude-buffer.el ends here