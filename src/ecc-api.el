;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 17:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-api.el

;;; Commentary:
;;; Public API for Claude state detection and auto-response functionality.
;;; This module provides a clean and stable interface for interacting with
;;; Claude buffers, detecting prompt states, and sending responses.

(require 'ecc-variables)
(require 'ecc-state-detection)
(require 'ecc-auto-response-unified)

;;; Code:

;; Buffer management API

;;;###autoload
(defun ecc-buffer-register (buffer)
  "Register BUFFER as a Claude buffer.
Returns the buffer that was registered."
  (interactive "bBuffer to register: ")
  (let ((buf (get-buffer buffer)))
    (unless (buffer-live-p buf)
      (user-error "Buffer does not exist or is not alive"))
    
    ;; Register the buffer
    (unless (assoc buf ecc-buffer-registered-buffers-alist)
      (push (cons buf nil) ecc-buffer-registered-buffers-alist)
      (message "Buffer '%s' registered as Claude buffer" (buffer-name buf)))
    
    ;; Set as current Claude buffer
    (setq ecc-buffer-current-buffer buf)
    buf))

;;;###autoload
(defun ecc-buffer-set-current (buffer)
  "Set BUFFER as the current active Claude buffer.
Returns the buffer that was set as current."
  (interactive "bBuffer to set as current: ")
  (let ((buf (get-buffer buffer)))
    (unless (buffer-live-p buf)
      (user-error "Buffer does not exist or is not alive"))
    
    ;; Register if not already registered
    (unless (assoc buf ecc-buffer-registered-buffers-alist)
      (push (cons buf nil) ecc-buffer-registered-buffers-alist))
    
    ;; Set as current Claude buffer
    (setq ecc-buffer-current-buffer buf)
    (message "Buffer '%s' set as current Claude buffer" (buffer-name buf))
    buf))

;;;###autoload
(defun ecc-buffer-list ()
  "Return a list of all registered Claude buffers."
  (interactive)
  (let ((buffers (mapcar #'car ecc-buffer-registered-buffers-alist)))
    (when (called-interactively-p 'any)
      (message "Registered Claude buffers: %s"
               (mapconcat #'buffer-name buffers ", ")))
    buffers))

;;;###autoload
(defun ecc-buffer-current ()
  "Return the current active Claude buffer."
  (interactive)
  (when (called-interactively-p 'any)
    (if ecc-buffer-current-buffer
        (message "Current Claude buffer: %s"
                 (buffer-name ecc-buffer-current-buffer))
      (message "No current Claude buffer set")))
  ecc-buffer-current-buffer)

;; State detection API

;;;###autoload
(defun ecc-state-detect (&optional buffer)
  "Detect Claude prompt state in BUFFER or current buffer.
Returns one of :y/y/n, :y/n, :waiting, :initial-waiting, or nil."
  (interactive)
  (let* ((buf (or buffer (ecc-buffer-current)))
         (state (when (buffer-live-p buf)
                  (with-current-buffer buf
                    (ecc-detect-state)))))
    (when (called-interactively-p 'any)
      (message "Detected state: %s" (ecc-state-get-name state)))
    state))

;;;###autoload
(defun ecc-state-waiting-p (&optional buffer)
  "Return non-nil if BUFFER or current buffer has a waiting prompt."
  (interactive)
  (let ((state (ecc-state-detect buffer)))
    (when (called-interactively-p 'any)
      (message "Waiting prompt? %s" (if (eq state :waiting) "Yes" "No")))
    (eq state :waiting)))

;;;###autoload
(defun ecc-state-y/n-p (&optional buffer)
  "Return non-nil if BUFFER or current buffer has a Y/N prompt."
  (interactive)
  (let ((state (ecc-state-detect buffer)))
    (when (called-interactively-p 'any)
      (message "Y/N prompt? %s" (if (eq state :y/n) "Yes" "No")))
    (eq state :y/n)))

;;;###autoload
(defun ecc-state-y/y/n-p (&optional buffer)
  "Return non-nil if BUFFER or current buffer has a Y/Y/N prompt."
  (interactive)
  (let ((state (ecc-state-detect buffer)))
    (when (called-interactively-p 'any)
      (message "Y/Y/N prompt? %s" (if (eq state :y/y/n) "Yes" "No")))
    (eq state :y/y/n)))

;;;###autoload
(defun ecc-state-initial-waiting-p (&optional buffer)
  "Return non-nil if BUFFER or current buffer has an initial waiting prompt."
  (interactive)
  (let ((state (ecc-state-detect buffer)))
    (when (called-interactively-p 'any)
      (message "Initial waiting prompt? %s" 
               (if (eq state :initial-waiting) "Yes" "No")))
    (eq state :initial-waiting)))

;; Auto-response API

;;;###autoload
(defun ecc-auto-response-enable (&optional buffer)
  "Enable auto-response for Claude prompts in BUFFER or current buffer."
  (interactive)
  (when buffer
    (ecc-buffer-set-current buffer))
  (ecc-auto-response-start)
  (message "Auto-response enabled for buffer '%s'"
           (buffer-name ecc-buffer-current-buffer)))

;;;###autoload
(defun ecc-auto-response-disable ()
  "Disable auto-response for Claude prompts."
  (interactive)
  (ecc-auto-response-stop)
  (message "Auto-response disabled"))

;;;###autoload
(defun ecc-auto-response-send (response &optional buffer)
  "Send RESPONSE to Claude in BUFFER or current buffer."
  (interactive "sResponse to send: ")
  (let ((buf (or buffer (ecc-buffer-current))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (ecc-auto-response-send-response response "Custom")))))

;;;###autoload
(defun ecc-auto-response-send-state-based (&optional buffer)
  "Send appropriate response based on current state in BUFFER.
This detects the current prompt state and sends the configured
response for that state."
  (interactive)
  (let* ((buf (or buffer (ecc-buffer-current)))
         (state (when (buffer-live-p buf)
                  (with-current-buffer buf
                    (ecc-detect-state)))))
    (when (and state (buffer-live-p buf))
      (with-current-buffer buf
        (cond
         ((eq state :y/y/n)
          (ecc-auto-response-send-response ecc-auto-response-y/y/n "Y/Y/N"))
         ((eq state :y/n)
          (ecc-auto-response-send-response ecc-auto-response-y/n "Y/N"))
         ((eq state :initial-waiting)
          (ecc-auto-response-send-response ecc-auto-response-initial-waiting "Initial-Waiting"))
         ((eq state :waiting)
          (ecc-auto-response-send-response ecc-auto-response-waiting "Continue")))))))

;; Command aliases for common operations

;;;###autoload
(defalias 'ecc-auto-start 'ecc-auto-response-enable
  "Alias for enabling auto-response.")

;;;###autoload
(defalias 'ecc-auto-stop 'ecc-auto-response-disable
  "Alias for disabling auto-response.")

;;;###autoload
(defalias 'ecc-auto-toggle 'ecc-auto-response-toggle
  "Alias for toggling auto-response.")

;;;###autoload
(defalias 'ecc-yes 'ecc-auto-response-yes
  "Alias for sending yes response.")

;;;###autoload
(defalias 'ecc-yes-plus 'ecc-auto-response-yes-plus
  "Alias for sending yes-plus response.")

;;;###autoload
(defalias 'ecc-continue 'ecc-auto-response-continue
  "Alias for sending continue response.")

;; Debug API

;;;###autoload
(defun ecc-debug-toggle ()
  "Toggle debug message output for Claude."
  (interactive)
  (setq ecc-debug-enabled (not ecc-debug-enabled))
  (message "Claude debug messages %s" 
           (if ecc-debug-enabled "enabled" "disabled")))

(provide 'ecc-api)

;;; ecc-api.el ends here