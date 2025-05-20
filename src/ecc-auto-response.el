;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 07:45:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-auto/ecc-auto-response.el

;;; Commentary:
;;; Auto-response functionality for Claude prompts.

(require 'ecc-variables)

;;;###autoload
(defun ecc-auto-accept-send ()
  "Automatically check and respond to Claude prompts in the current buffer."
  (interactive)
  (ecc-auto-response-send (current-buffer)))

;;;###autoload
(defun ecc-auto-response-send (buffer &optional state)
  "Automatically respond to Claude prompts in BUFFER.
Optional STATE can be provided to override automatic state detection."
  (interactive (list (current-buffer)))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((state (or state (ecc-detect-simple-state))))
        (cond
         ((eq state :y/y/n)
          (ecc-auto--send-response buffer 
                                ecc-auto-response-y/y/n 
                                "Y/Y/N"))
         ((eq state :y/n)
          (ecc-auto--send-response buffer 
                                ecc-auto-response-y/n 
                                "Y/N"))
         ((eq state :initial-waiting)
          (ecc-auto--send-response buffer 
                                ecc-auto-response-initial-waiting 
                                "Initial-Waiting"))
         ((eq state :waiting)
          (ecc-auto--send-response buffer 
                                ecc-auto-response-waiting 
                                "Continue")))))))

(defun ecc-auto--send-response (buffer response type)
  "Send RESPONSE to Claude in BUFFER.
TYPE is used for notification messages."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      ;; Ensure we have the right mode to send commands
      (cond
       ;; vterm mode
       ((derived-mode-p 'vterm-mode)
        (ecc-auto--send-vterm-response response))
       
       ;; Default fallback
       (t
        (message "Cannot send auto-response to buffer in %s mode" major-mode)))))
  
  ;; Notify user about the response
  (when (and (boundp 'ecc-auto-notify-completions) ecc-auto-notify-completions)
    (ecc-auto--notify type response)))

(defun ecc-auto--send-vterm-response (response)
  "Send RESPONSE to Claude in a vterm buffer."
  (when (fboundp 'vterm-send-string)
    ;; Save current point position
    (let ((old-point (point)))
      ;; Check if point is near the end (user not reading earlier content)
      (if (> (point-max) (+ (point) 40))
          ;; User might be reading earlier content, so don't move point
          (save-excursion
            (goto-char (point-max))
            (sit-for 0.3)
            (vterm-send-string response)
            (vterm-send-return)
            (sit-for 0.3))
        ;; User is at end of buffer, proceed normally
        (sit-for 0.3)
        (vterm-send-string response)
        (vterm-send-return)
        (sit-for 0.3)))))

(defun ecc-auto--notify (type response)
  "Display notification about auto-response of TYPE with actual RESPONSE string.
TYPE is a description of the response context (e.g., \"Y/N\").
RESPONSE is the actual string sent to Claude."
  (let ((msg (format "Auto-responded: %s (\"%s\")" type response)))
    (message msg)))

;; Specific response functions with defaults (useful for interactive use)

;;;###autoload
(defun ecc-auto-response-continue (&optional buffer)
  "Automatically send continue to Claude waiting prompt.
If BUFFER is nil, use current buffer."
  (interactive)
  (ecc-auto--send-response (or buffer (current-buffer))
                       ecc-auto-response-waiting
                       "Continue"))

;;;###autoload
(defun ecc-auto-response-yes (&optional buffer)
  "Automatically send Y response to Claude Y/N prompt.
If BUFFER is nil, use current buffer."
  (interactive)
  (ecc-auto--send-response (or buffer (current-buffer))
                       ecc-auto-response-y/n
                       "Y/N"))

;;;###autoload
(defun ecc-auto-response-yes-plus (&optional buffer)
  "Automatically send Y response to Claude Y/Y/N prompt.
If BUFFER is nil, use current buffer."
  (interactive)
  (ecc-auto--send-response (or buffer (current-buffer))
                       ecc-auto-response-y/y/n
                       "Y/Y/N"))

;;;###autoload
(defun ecc-auto-response-template (template-text)
  "Send custom TEMPLATE-TEXT to Claude.
This allows sending natural language responses instead of just number options."
  (interactive "sEnter your response: ")
  (let ((buffer (current-buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (ecc-auto--send-response buffer
                             template-text
                             (format "Template: %s" template-text))))))

;;;###autoload
(defun ecc-start-auto-response (&optional response-y/n response-y/y/n response-waiting)
  "Start automatic response system for Claude prompts.
RESPONSE-Y/N: Response for Y/N prompts (default: \"1\")  
RESPONSE-Y/Y/N: Response for Y/Y/N prompts (default: \"2\")
RESPONSE-WAITING: Response for waiting prompts (default: \"/auto\")"
  (interactive)
  
  ;; Set response values
  (when response-y/n
    (setq ecc-auto-response-y/n response-y/n))
  (when response-y/y/n
    (setq ecc-auto-response-y/y/n response-y/y/n))
  (when response-waiting
    (setq ecc-auto-response-waiting response-waiting))
  
  ;; Enable auto-response mode globally  
  (setq ecc-buffer-auto-response-enabled t)
  
  ;; Initialize timer for periodic checking
  (when (bound-and-true-p ecc-auto-response-timer)
    (cancel-timer ecc-auto-response-timer))
  
  (setq ecc-auto-response-timer
        (run-with-timer 1 0.5 #'ecc-check-and-respond))
  
  (message "Auto-response started: Y/N=%s, Y/Y/N=%s, Continue=%s" 
           ecc-auto-response-y/n 
           ecc-auto-response-y/y/n
           ecc-auto-response-waiting))

;;;###autoload
(defun ecc-check-and-respond ()
  "Check current buffer state and respond automatically."
  (when (and ecc-buffer-auto-response-enabled
             (buffer-live-p ecc-buffer-current-buffer))
    (with-current-buffer ecc-buffer-current-buffer
      (let ((state (ecc-detect-simple-state)))
        (cond
         ((eq state :y/y/n)
          (ecc-auto--send-response ecc-buffer-current-buffer
                                 ecc-auto-response-y/y/n
                                 "Y/Y/N"))
         ((eq state :y/n)
          (ecc-auto--send-response ecc-buffer-current-buffer
                                 ecc-auto-response-y/n
                                 "Y/N"))
         ((eq state :initial-waiting)
          (ecc-auto--send-response ecc-buffer-current-buffer
                                 ecc-auto-response-initial-waiting
                                 "Initial-Waiting"))
         ((eq state :waiting)
          (ecc-auto--send-response ecc-buffer-current-buffer
                                 ecc-auto-response-waiting
                                 "Continue")))))))

;;;###autoload
(defun ecc-stop-auto-response ()
  "Stop the auto-response system."
  (interactive)
  (when (bound-and-true-p ecc-auto-response-timer)
    (cancel-timer ecc-auto-response-timer)
    (setq ecc-auto-response-timer nil))
  (setq ecc-buffer-auto-response-enabled nil)
  (message "Auto-response stopped"))

;;;###autoload
(defun ecc-toggle-auto-response ()
  "Toggle auto-response on/off."
  (interactive)
  (if ecc-buffer-auto-response-enabled
      (progn
        (ecc-stop-auto-response)
        (message "Auto-response disabled"))
    (progn
      (ecc-register-buffer)
      (ecc-start-auto-response)
      (message "Auto-response enabled"))))


(provide 'ecc-auto-response)

;;; ecc-auto-response.el ends here
