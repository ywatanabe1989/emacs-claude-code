;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 19:15:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-auto-response-buffer-local.el

;;; Commentary:
;;; Buffer-local auto-response system for Claude prompts.
;;; This module implements an auto-response system that uses buffer-local
;;; configuration and state tracking, allowing multiple Claude instances
;;; to operate independently with their own settings.

(require 'ecc-variables)
(require 'ecc-buffer-local)
(require 'ecc-state-detection)
(require 'ecc-buffer-api)

;;; Code:

;; Core auto-response functions with buffer-local awareness

;;;###autoload
(defun ecc-auto-response-buffer-local-start (&optional buffer)
  "Start the buffer-local auto-response system for BUFFER or current buffer.
This function initializes and activates the auto-response system
with buffer-local configuration."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      ;; Enable auto-response mode for this buffer
      (setq-local ecc-buffer-auto-response-enabled t)
      
      ;; Ensure we have buffer-local timer tracking
      (unless (local-variable-p 'ecc-buffer-auto-response-timer)
        (setq-local ecc-buffer-auto-response-timer nil))
      
      ;; Initialize or restart the buffer-local timer
      (when ecc-buffer-auto-response-timer
        (cancel-timer ecc-buffer-auto-response-timer))
      
      (setq-local ecc-buffer-auto-response-timer
                  (run-with-timer 1 
                                 (if (boundp 'ecc-auto-response-timer-interval)
                                     ecc-auto-response-timer-interval
                                   0.5) 
                                 #'ecc-auto-response-buffer-local-check
                                 buf))
      
      ;; Run initial checks with staggered timing to ensure we catch the prompt
      (ecc-auto-response-buffer-local-initial-check buf)
      (run-at-time 0.5 nil #'ecc-auto-response-buffer-local-initial-check buf)
      (run-at-time 1.0 nil #'ecc-auto-response-buffer-local-initial-check buf)
      (run-at-time 2.0 nil #'ecc-auto-response-buffer-local-initial-check buf)
      
      (message "Buffer-local auto-response started for '%s'" (buffer-name)))))

;;;###autoload
(defun ecc-auto-response-buffer-local-stop (&optional buffer)
  "Stop the buffer-local auto-response system for BUFFER or current buffer."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      ;; Disable auto-response for this buffer
      (setq-local ecc-buffer-auto-response-enabled nil)
      
      ;; Cancel the buffer-local timer if it exists
      (when (and (local-variable-p 'ecc-buffer-auto-response-timer)
                ecc-buffer-auto-response-timer)
        (cancel-timer ecc-buffer-auto-response-timer)
        (setq-local ecc-buffer-auto-response-timer nil))
      
      ;; Reset all timestamps and active state for this buffer
      (setq-local ecc-buffer-last-time-alist
                  '((:y/n . 0.0)
                    (:y/y/n . 0.0)
                    (:waiting . 0.0)
                    (:initial-waiting . 0.0)))
      (setq-local ecc-buffer-active-state nil)
      
      (message "Buffer-local auto-response stopped for '%s'" (buffer-name)))))

;;;###autoload
(defun ecc-auto-response-buffer-local-toggle (&optional buffer)
  "Toggle buffer-local auto-response for BUFFER or current buffer."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      (if (and (local-variable-p 'ecc-buffer-auto-response-enabled)
               ecc-buffer-auto-response-enabled)
          (ecc-auto-response-buffer-local-stop buf)
        (ecc-auto-response-buffer-local-start buf)))))

;; Check and respond function with buffer-local awareness

(defun ecc-auto-response-buffer-local-check (buffer)
  "Check BUFFER state and respond automatically if appropriate.
Uses buffer-local configuration and state tracking."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (local-variable-p 'ecc-buffer-auto-response-enabled)
                ecc-buffer-auto-response-enabled)
        ;; First check if we should notify about the prompt itself
        (when (and (local-variable-p 'ecc-buffer-auto-notify-on-prompt)
                   ecc-buffer-auto-notify-on-prompt)
          (let ((state (ecc-buffer-state-detect)))
            (when state
              (message "Claude prompt detected in '%s': %s" 
                       (buffer-name) (ecc-state-get-name state)))))
        
        ;; Detect the current state
        (let ((state (ecc-buffer-state-detect)))
          ;; Add debug logging
          (ecc-buffer-local-debug-message 
           "Auto-response check in '%s' - State detected: %s" 
           (buffer-name) state)
          
          ;; Special case for initial-waiting - always respond regardless of throttling
          (when (eq state :initial-waiting)
            (ecc-buffer-local-debug-message "Detected initial-waiting state - forcing response")
            (setq-local ecc-buffer-active-state state)
            (ecc-buffer-local-update-time state)
            (ecc-auto-response-buffer-local-process-state :initial-waiting))
          
          ;; Handle other states with throttling
          (when (and state 
                    (not (eq state :initial-waiting))
                    (not (ecc-buffer-local-throttled-p state)))
            ;; Set active state to prevent duplicates during processing
            (setq-local ecc-buffer-active-state state)
            ;; Update the timestamp for this state
            (ecc-buffer-local-update-time state)
            ;; Process the response based on state
            (ecc-auto-response-buffer-local-process-state state)))))))

(defun ecc-auto-response-buffer-local-process-state (state)
  "Process auto-response for detected STATE in the current buffer.
Uses buffer-local configuration for response patterns."
  (cond
   ((eq state :y/y/n)
    (when (and (local-variable-p 'ecc-buffer-auto-response-y/y/n)
               ecc-buffer-auto-response-y/y/n)
      (ecc-buffer-send-response ecc-buffer-auto-response-y/y/n "Y/Y/N")))
   
   ((eq state :y/n)
    (when (and (local-variable-p 'ecc-buffer-auto-response-y/n)
               ecc-buffer-auto-response-y/n)
      (ecc-buffer-send-response ecc-buffer-auto-response-y/n "Y/N")))
   
   ((eq state :initial-waiting)
    (when (and (local-variable-p 'ecc-buffer-auto-response-initial-waiting)
               ecc-buffer-auto-response-initial-waiting)
      (ecc-buffer-send-response ecc-buffer-auto-response-initial-waiting "Initial-Waiting")))
   
   ((eq state :waiting)
    (when (and (local-variable-p 'ecc-buffer-auto-response-waiting)
               ecc-buffer-auto-response-waiting)
      (ecc-buffer-send-response ecc-buffer-auto-response-waiting "Continue")))))

;; Initial checking with buffer-local awareness

(defun ecc-auto-response-buffer-local-initial-check (buffer)
  "Perform an initial check for prompts in BUFFER.
Uses buffer-local configuration and state tracking."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (local-variable-p 'ecc-buffer-auto-response-enabled)
                ecc-buffer-auto-response-enabled)
        ;; Force a state check
        (let ((state (ecc-buffer-state-detect)))
          ;; When state is detected, reset throttling to ensure response happens
          (when state
            (ecc-buffer-local-debug-message 
             "Initial check detected state in '%s': %s" 
             (buffer-name) state)
            ;; Reset throttling for initial check to ensure we respond
            (setq-local ecc-buffer-active-state nil)
            (setf (alist-get state ecc-buffer-last-time-alist) 0.0)
            ;; Run the check and respond function
            (ecc-auto-response-buffer-local-check buffer)))))))

;; Convenience aliases

;;;###autoload
(defalias 'ecc-buffer-auto-start 'ecc-auto-response-buffer-local-start
  "Alias for starting buffer-local auto-response.")

;;;###autoload
(defalias 'ecc-buffer-auto-stop 'ecc-auto-response-buffer-local-stop
  "Alias for stopping buffer-local auto-response.")

;;;###autoload
(defalias 'ecc-buffer-auto-toggle 'ecc-auto-response-buffer-local-toggle
  "Alias for toggling buffer-local auto-response.")

;; Connect auto-response to vterm output hook

;;;###autoload
(defun ecc-auto-response-buffer-local-connect-to-vterm-hook ()
  "Connect buffer-local auto-response to vterm output hooks."
  (interactive)
  (when (derived-mode-p 'vterm-mode)
    (add-hook 'vterm-update-functions
              (lambda (&rest _)
                (when (and (local-variable-p 'ecc-buffer-auto-response-enabled)
                           ecc-buffer-auto-response-enabled)
                  (ecc-auto-response-buffer-local-check (current-buffer))))
              nil t)
    (message "Connected buffer-local auto-response to vterm output hook")))

(provide 'ecc-auto-response-buffer-local)

;;; ecc-auto-response-buffer-local.el ends here