;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 15:25:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-auto-response-fix-improved.el

;;; Commentary:
;;; Improved version of the auto-response system with better initial-waiting detection.

(require 'ecc-variables)
(require 'ecc-auto-response)

;; Improved check-and-respond with special handling for initial-waiting
(defun ecc-check-and-respond-improved ()
  "Improved implementation of `ecc-check-and-respond' with better initial-waiting detection.
This function adds several enhancements to the original:
1. Uses enhanced detection methods if available
2. Checks for alternative initial-waiting patterns as fallback
3. Special case handling for initial-waiting to bypass throttling
4. Provides debug logging for easier troubleshooting"
  (when (and (boundp 'ecc-buffer-auto-response-enabled)
             ecc-buffer-auto-response-enabled
             (boundp 'ecc-buffer-current-buffer)
             (buffer-live-p ecc-buffer-current-buffer))
    (with-current-buffer ecc-buffer-current-buffer
      ;; First check if we should notify about the prompt itself
      (when (boundp 'ecc-auto-notify-on-claude-prompt)
        (let ((state (ecc-detect-simple-state)))
          (when (fboundp 'ecc-auto-notify-check-state)
            (ecc-auto-notify-check-state state))))

      ;; Try to use enhanced detection if available, fall back to simple detection
      (let ((state (cond
                    ;; Try preferred detection methods first
                    ((fboundp 'ecc-detect-enhanced-state)
                     (ecc-detect-enhanced-state))
                    ((fboundp 'ecc-detect-prompt-in-last-lines)
                     (ecc-detect-prompt-in-last-lines))
                    ;; Fall back to simple detection
                    (t (ecc-detect-simple-state)))))
        
        ;; If no state detected, try checking for alternative initial waiting patterns
        (unless state
          (when (boundp 'ecc-state-prompt-initial-waiting-alternatives)
            (let ((buffer-text (buffer-substring-no-properties 
                              (max (- (point-max) 2000) (point-min))
                              (point-max))))
              (catch 'found
                (dolist (pattern ecc-state-prompt-initial-waiting-alternatives)
                  (when (string-match-p pattern buffer-text)
                    (setq state :initial-waiting)
                    (throw 'found t)))))))
        
        ;; Add debug logging
        (ecc-debug-message "Auto-response check - State detected: %s" state)
        
        ;; Special case for initial-waiting - always respond regardless of throttling
        (when (eq state :initial-waiting)
          (ecc-debug-message "Detected initial-waiting state - forcing response")
          (setq ecc-auto-response-active-state state)
          (ecc-auto-response-update-time state)
          (ecc-auto--send-response ecc-buffer-current-buffer
                                  ecc-auto-response-initial-waiting
                                  "Initial-Waiting"))
        
        ;; Handle other states with throttling
        (when (and state 
                  (not (eq state :initial-waiting))
                  (not (ecc-auto-response-throttled-p state)))
          ;; Set active state to prevent duplicates during processing
          (let ((ecc-auto-response-active-state state))
            ;; Update the timestamp for this state
            (ecc-auto-response-update-time state)
            ;; Process the response
            (cond
             ((eq state :y/y/n)
              (ecc-auto--send-response ecc-buffer-current-buffer
                                      ecc-auto-response-y/y/n
                                      "Y/Y/N"))
             ((eq state :y/n)
              (ecc-auto--send-response ecc-buffer-current-buffer
                                      ecc-auto-response-y/n
                                      "Y/N"))
             ((eq state :waiting)
              (ecc-auto--send-response ecc-buffer-current-buffer
                                      ecc-auto-response-waiting
                                      "Continue")))))))))

;; Replace the original function
(defun ecc-install-improved-check-and-respond ()
  "Install the improved check-and-respond function.
This replaces the original function with our improved version."
  (interactive)
  (advice-add 'ecc-check-and-respond :override
              #'ecc-check-and-respond-improved)
  (message "Installed improved check-and-respond function"))

;; Install the improved version automatically when this file is loaded
(ecc-install-improved-check-and-respond)

;; Ensure we have state-detect-prompt features loaded if available
(when (locate-library "ecc-state-detect-prompt")
  (require 'ecc-state-detect-prompt))

(provide 'ecc-auto-response-fix-improved)

;;; ecc-auto-response-fix-improved.el ends here