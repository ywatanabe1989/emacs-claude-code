;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 19:40:30>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-auto/ecc-auto-response.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ecc-auto-variables)
(require 'ecc-buffer-variables)
(require 'ecc-state-variables)

;; Define the function used in the vterm hook
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
      (let ((state (or state 
                       (when (fboundp 'ecc-state-get)
                         (ecc-state-get))
                       (when (fboundp 'ecc-state-detect)
                         (ecc-state-detect)))))
        (cond
         ((memq state '(:y/y/n y/y/n))
          (ecc-auto--send-response buffer 
                                  ecc-auto-response-y/y/n 
                                  "Y/Y/N"))
         ((memq state '(:y/n y/n))
          (ecc-auto--send-response buffer 
                                  ecc-auto-response-y/n 
                                  "Y/N"))
         ((memq state '(:waiting waiting :initial-waiting initial-waiting))
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
       
       ;; Add additional modes as needed for other buffer types
       
       ;; Default fallback
       (t
        (message "Cannot send auto-response to buffer in %s mode" major-mode)))))
  
  ;; Notify user about the response
  (when ecc-auto-notify-completions
    (ecc-auto--notify type)))

(defun ecc-auto--send-vterm-response (response)
  "Send RESPONSE to Claude in a vterm buffer."
  (when (fboundp 'vterm-send-string)
    (sit-for 0.3)
    (vterm-send-string response)
    (vterm-send-return)
    (when (fboundp 'vterm-copy-mode)
      (vterm-copy-mode -1))
    (sit-for 0.3)))

(defun ecc-auto--notify (type)
  "Display notification about auto-response of TYPE."
  (let ((msg (format "Auto-responded: %s" type)))
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

(provide 'ecc-auto-response)

(when
    (not load-file-name)
  (message "ecc-auto-response.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))