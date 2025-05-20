;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 09:40:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/examples/advanced/interaction-limits.el

;;; Commentary:
;;; Example usage of interaction limits for Claude.

(require 'ecc-variables)
(require 'ecc-interaction-tracker)
(require 'ecc-interaction-limiter)

;; Enable interaction limiting
(setq ecc-interaction-limit-enabled t)

;; Configure session limits
(setq ecc-interaction-limit-count 50)  ; Max 50 interactions per session
(setq ecc-interaction-limit-warn-threshold 0.8) ; Warn at 80% (40 interactions)

;; Configure time-based limits
(setq ecc-interaction-limit-per-time-enabled t) ; Enable hourly limits
(setq ecc-interaction-limit-per-hour 20)       ; Max 20 interactions per hour

;; Set limit action to prompt
(setq ecc-interaction-limit-action 'prompt) ; Options: 'warn, 'block, 'prompt

;; Define custom handler for limits
(setq ecc-interaction-limit-handler-function
      (lambda (limit-type count)
        (let ((buffer (get-buffer-create "*Claude Limit Reached*")))
          (with-current-buffer buffer
            (erase-buffer)
            (insert (format "# Claude Interaction Limit Reached\n\n"))
            (insert (format "* Limit type: %s\n" 
                           (if (eq limit-type 'count) "Session" "Hourly")))
            (insert (format "* Current count: %d\n" count))
            (insert (format "* Maximum allowed: %d\n\n" 
                           (if (eq limit-type 'count) 
                               ecc-interaction-limit-count
                             ecc-interaction-limit-per-hour)))
            (insert "## Recommended Actions\n\n")
            (insert "1. Consider consolidating your queries\n")
            (insert "2. Focus on specific questions\n")
            (insert "3. Save your conversation and start a new session\n\n")
            (insert "## Current Interaction Statistics\n\n")
            (insert (format "* Total interactions: %d\n" 
                           ecc-interaction-counter))
            (insert (format "* Last hour: %d interactions\n" 
                           (ecc-interaction-count-in-last-hour)))
            (goto-char (point-min))
            (when (fboundp 'org-mode)
              (org-mode)))
          (display-buffer buffer))))

;; Helper function to simulate interactions for testing
(defun test-claude-interactions (count)
  "Simulate COUNT Claude interactions for testing."
  (interactive "nNumber of interactions to simulate: ")
  (dotimes (i count)
    (condition-case err
        (progn
          (ecc-track-interaction)
          (message "Interaction %d tracked successfully" (1+ i)))
      (error
       (message "Error on interaction %d: %s" (1+ i) (error-message-string err))
       (sleep-for 1)))))

;; Configure keybindings
(global-set-key (kbd "C-c c L") ecc-interaction-limits-map)

;; Example usage:
;; 
;; Toggle interaction limits:
;; M-x ecc-toggle-interaction-limits
;;
;; Set session limit:
;; M-x ecc-set-interaction-limit
;;
;; Toggle hourly limits:
;; M-x ecc-toggle-time-based-limits
;;
;; Set hourly limit:
;; M-x ecc-set-hourly-interaction-limit
;;
;; Test with simulated interactions:
;; M-x test-claude-interactions

(provide 'interaction-limits)

;;; interaction-limits.el ends here