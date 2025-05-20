;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 19:00:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-buffer-api.el

;;; Commentary:
;;; Public API for buffer-specific Claude configuration and interaction.
;;; This module extends the Claude API to provide per-buffer configuration
;;; and state management, allowing multiple Claude instances to operate
;;; independently with their own settings.

(require 'ecc-variables)
(require 'ecc-buffer-local)
(require 'ecc-state-detection)
(require 'ecc-auto-response-unified)

;;; Code:

;; Buffer Registration API

;;;###autoload
(defun ecc-buffer-register (buffer)
  "Register BUFFER as a Claude buffer with buffer-local configuration.
Returns the buffer that was registered."
  (interactive "bBuffer to register: ")
  (ecc-buffer-register-with-local-config buffer))

;;;###autoload
(defun ecc-buffer-set-current (buffer)
  "Set BUFFER as the current active Claude buffer.
Initializes buffer-local configuration if not already done.
Returns the buffer that was set as current."
  (interactive "bBuffer to set as current: ")
  (let ((buf (get-buffer buffer)))
    (unless (buffer-live-p buf)
      (user-error "Buffer does not exist or is not alive"))
    
    ;; Register if not already registered
    (unless (assoc buf ecc-buffer-registered-buffers-alist)
      (ecc-buffer-register-with-local-config buf))
    
    ;; Set as current Claude buffer
    (setq ecc-buffer-current-buffer buf)
    (message "Buffer '%s' set as current Claude buffer" (buffer-name buf))
    buf))

;; Buffer Settings API

;;;###autoload
(defun ecc-buffer-settings-get (setting &optional buffer)
  "Get buffer-local SETTING for BUFFER or current buffer.
SETTING should be a symbol like 'ecc-buffer-auto-response-y/n'."
  (with-current-buffer (or buffer (current-buffer))
    (when (boundp setting)
      (buffer-local-value setting (current-buffer)))))

;;;###autoload
(defun ecc-buffer-settings-set (setting value &optional buffer)
  "Set buffer-local SETTING to VALUE for BUFFER or current buffer.
SETTING should be a symbol like 'ecc-buffer-auto-response-y/n'."
  (with-current-buffer (or buffer (current-buffer))
    (set (make-local-variable setting) value)))

;;;###autoload
(defun ecc-buffer-auto-response-set-y/n (response &optional buffer)
  "Set Y/N response pattern to RESPONSE for BUFFER or current buffer."
  (interactive "sY/N response: ")
  (with-current-buffer (or buffer (current-buffer))
    (setq-local ecc-buffer-auto-response-y/n response)
    (message "Y/N response set to \"%s\" for buffer %s" 
             response (buffer-name))))

;;;###autoload
(defun ecc-buffer-auto-response-set-y/y/n (response &optional buffer)
  "Set Y/Y/N response pattern to RESPONSE for BUFFER or current buffer."
  (interactive "sY/Y/N response: ")
  (with-current-buffer (or buffer (current-buffer))
    (setq-local ecc-buffer-auto-response-y/y/n response)
    (message "Y/Y/N response set to \"%s\" for buffer %s" 
             response (buffer-name))))

;;;###autoload
(defun ecc-buffer-auto-response-set-waiting (response &optional buffer)
  "Set waiting response pattern to RESPONSE for BUFFER or current buffer."
  (interactive "sWaiting response: ")
  (with-current-buffer (or buffer (current-buffer))
    (setq-local ecc-buffer-auto-response-waiting response)
    (message "Waiting response set to \"%s\" for buffer %s" 
             response (buffer-name))))

;;;###autoload
(defun ecc-buffer-auto-response-set-initial (response &optional buffer)
  "Set initial waiting response to RESPONSE for BUFFER or current buffer."
  (interactive "sInitial waiting response: ")
  (with-current-buffer (or buffer (current-buffer))
    (setq-local ecc-buffer-auto-response-initial-waiting response)
    (message "Initial waiting response set to \"%s\" for buffer %s" 
             response (buffer-name))))

;; State Detection API with Buffer-Local Tracking

;;;###autoload
(defun ecc-buffer-state-detect (&optional buffer)
  "Detect Claude prompt state in BUFFER or current buffer.
Updates buffer-local state tracking and returns the detected state.
Returns one of: :y/y/n, :y/n, :waiting, :initial-waiting, or nil."
  (interactive)
  (let* ((buf (or buffer (current-buffer)))
         (state (when (buffer-live-p buf)
                  (with-current-buffer buf
                    (let ((detected-state (ecc-detect-state)))
                      ;; Update buffer-local state tracking
                      (when detected-state
                        (ecc-buffer-local-update-state detected-state))
                      detected-state)))))
    (when (called-interactively-p 'any)
      (message "Detected state in %s: %s" 
               (buffer-name buf)
               (if state
                   (ecc-state-get-name state)
                 "none")))
    state))

;;;###autoload
(defun ecc-buffer-state-waiting-p (&optional buffer)
  "Return non-nil if BUFFER or current buffer has a waiting prompt.
Updates buffer-local state tracking."
  (interactive)
  (eq (ecc-buffer-state-detect buffer) :waiting))

;;;###autoload
(defun ecc-buffer-state-y/n-p (&optional buffer)
  "Return non-nil if BUFFER or current buffer has a Y/N prompt.
Updates buffer-local state tracking."
  (interactive)
  (eq (ecc-buffer-state-detect buffer) :y/n))

;;;###autoload
(defun ecc-buffer-state-y/y/n-p (&optional buffer)
  "Return non-nil if BUFFER or current buffer has a Y/Y/N prompt.
Updates buffer-local state tracking."
  (interactive)
  (eq (ecc-buffer-state-detect buffer) :y/y/n))

;;;###autoload
(defun ecc-buffer-state-initial-waiting-p (&optional buffer)
  "Return non-nil if BUFFER or current buffer has an initial waiting prompt.
Updates buffer-local state tracking."
  (interactive)
  (eq (ecc-buffer-state-detect buffer) :initial-waiting))

;; Auto-Response API with Buffer-Local Settings

;;;###autoload
(defun ecc-buffer-auto-response-enable (&optional buffer)
  "Enable auto-response for Claude prompts in BUFFER or current buffer.
Uses buffer-local configuration for response patterns."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      (setq-local ecc-buffer-auto-response-enabled t)
      (message "Auto-response enabled for buffer '%s'"
               (buffer-name)))))

;;;###autoload
(defun ecc-buffer-auto-response-disable (&optional buffer)
  "Disable auto-response for Claude prompts in BUFFER or current buffer."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      (setq-local ecc-buffer-auto-response-enabled nil)
      (message "Auto-response disabled for buffer '%s'"
               (buffer-name)))))

;;;###autoload
(defun ecc-buffer-auto-response-toggle (&optional buffer)
  "Toggle auto-response for Claude prompts in BUFFER or current buffer."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      (if ecc-buffer-auto-response-enabled
          (ecc-buffer-auto-response-disable)
        (ecc-buffer-auto-response-enable)))))

;;;###autoload
(defun ecc-buffer-auto-response-send (response &optional buffer)
  "Send RESPONSE to Claude in BUFFER or current buffer.
Uses buffer-local state tracking."
  (interactive "sResponse to send: ")
  (let ((buf (or buffer (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        ;; Use a buffer-local version of send-response
        (ecc-buffer-send-response response "Custom")))))

;; Buffer-specific response handler
(defun ecc-buffer-send-response (response type)
  "Send RESPONSE to Claude in the current buffer.
TYPE is used for notification messages.
Uses buffer-local configuration and state tracking."
  (cond
   ;; vterm mode
   ((derived-mode-p 'vterm-mode)
    (ecc-buffer-send-vterm-response response))
   
   ;; Default fallback
   (t
    (ecc-buffer-local-debug-message
     "Cannot send auto-response to buffer in %s mode" major-mode)))
  
  ;; Notify user about the response
  (when ecc-buffer-auto-notify-completions
    (let ((msg (format "Auto-responded: %s (\"%s\")" type response)))
      (message msg))))

;; Buffer-specific vterm response handler
(defun ecc-buffer-send-vterm-response (response)
  "Send RESPONSE to Claude in the current vterm buffer.
Uses buffer-local state tracking."
  (when (fboundp 'vterm-send-string)
    ;; Ensure interaction counter is properly bound
    (unless (boundp 'ecc-interaction-counter)
      (setq ecc-interaction-counter 0))
    
    ;; Save current point position
    (let ((first-interaction-p (= ecc-interaction-counter 0))
          (delay-base (if (= ecc-interaction-counter 0) 1.5 1.0))
          (distance-from-end (- (point-max) (point))))
      
      ;; Output debug information
      (ecc-buffer-local-debug-message 
       "DEBUG: Interaction Counter: %d" ecc-interaction-counter)
      (ecc-buffer-local-debug-message 
       "DEBUG: Point: %d, Max: %d, Distance from end: %d" 
       (point) (point-max) distance-from-end)
      
      ;; For the first interaction, we always send at current point
      ;; For later interactions, check if user is reading earlier content
      (if (or first-interaction-p
              (< distance-from-end 40)) ; Point is near the end
          ;; User is at end of buffer or it's first interaction - send directly
          (progn
            (ecc-buffer-local-debug-message "DEBUG: Sending directly at current point")
            (sit-for delay-base)
            (vterm-send-string response)
            (sit-for delay-base)                    
            (vterm-send-return)
            (sit-for delay-base))
        ;; User might be reading earlier content, so don't move point
        (save-excursion
          (ecc-buffer-local-debug-message "DEBUG: Using save-excursion to send at end")
          (goto-char (point-max))
          (sit-for delay-base)
          (vterm-send-string response)
          (sit-for delay-base)            
          (vterm-send-return)
          (sit-for delay-base)))
      
      ;; Increment interaction counter after the first successful response
      (when first-interaction-p
        (ecc-buffer-local-debug-message "DEBUG: Incrementing interaction counter")
        (setq ecc-interaction-counter (1+ ecc-interaction-counter))
        ;; Add timestamp for this interaction
        (when (boundp 'ecc-interaction-timestamps)
          (push (current-time) ecc-interaction-timestamps)))
      
      (ecc-buffer-local-debug-message "DEBUG: Send complete. Final point: %d" (point)))))

;; Debug API

;;;###autoload
(defun ecc-buffer-debug-toggle (&optional buffer)
  "Toggle debug message output for BUFFER or current buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (setq-local ecc-buffer-debug-enabled (not ecc-buffer-debug-enabled))
    (message "Claude debug messages %s for buffer '%s'" 
             (if ecc-buffer-debug-enabled "enabled" "disabled")
             (buffer-name))))

;; Command aliases for convenience

;;;###autoload
(defalias 'ecc-buffer-auto-start 'ecc-buffer-auto-response-enable
  "Alias for enabling buffer-specific auto-response.")

;;;###autoload
(defalias 'ecc-buffer-auto-stop 'ecc-buffer-auto-response-disable
  "Alias for disabling buffer-specific auto-response.")

;;;###autoload
(defalias 'ecc-buffer-auto-toggle 'ecc-buffer-auto-response-toggle
  "Alias for toggling buffer-specific auto-response.")

(provide 'ecc-buffer-api)

;;; ecc-buffer-api.el ends here