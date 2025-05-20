;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 22:15:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-auto-notify-fix.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; Commentary:
;;; Fixes and improvements for the auto-notify system to work with
;;; the new centralized state detection module.

(require 'ecc-variables)
(require 'ecc-auto-notify)

;; Try to load state detection module if available
(when (locate-library "ecc-state-detection")
  (require 'ecc-state-detection))

;; Customization group for notify improvements
(defgroup ecc-auto-notify-improvements nil
  "Improvements for Claude auto-notify system."
  :group 'ecc
  :prefix "ecc-auto-notify-")

;; Updated check state function to work with new detection system
;;;###autoload
(defun ecc-auto-notify-check-state-improved (state)
  "Improved version of `ecc-auto-notify-check-state` that works with new detection system.
Check if STATE requires notification and notify if needed."
  (when (and ecc-auto-notify-on-claude-prompt
             state
             (memq state ecc-auto-notify-prompt-types)
             (or 
              ;; Always notify for new state
              (not (eq state ecc-auto-notify--last-state))
              ;; For same state, only notify after interval
              (> (- (float-time) ecc-auto-notify--last-time) ecc-auto-notify-interval)))
    (ecc-auto-notify-prompt state)
    (setq ecc-auto-notify--last-state state)
    (setq ecc-auto-notify--last-time (float-time))))

;; Apply advice to check-state function
(advice-add 'ecc-auto-notify-check-state :override #'ecc-auto-notify-check-state-improved)

;; Updated buffer setup function
;;;###autoload
(defun ecc-auto-notify-setup-for-buffer-improved ()
  "Set up notifications for the current buffer with improved state detection.
This version works with the centralized state detection module."
  (when (derived-mode-p 'ecc-term-claude-mode 'vterm-mode)
    ;; Add state check hook for this buffer using the unified detection function
    (add-hook 'ecc-term-claude-update-functions
              (lambda ()
                (let ((state (if (fboundp 'ecc-detect-state)
                                ;; Use new detection system if available
                                (ecc-detect-state)
                              ;; Fall back to simple state detection
                              (ecc-detect-simple-state))))
                  (ecc-auto-notify-check-state state)))
              nil t)))

;; Apply advice to setup function
(advice-add 'ecc-auto-notify-setup-for-buffer :override #'ecc-auto-notify-setup-for-buffer-improved)

;; Update hook function to add buffer-local detection
(defun ecc-auto-notify-buffer-change-handler ()
  "Handler for buffer content changes that checks for Claude prompts.
This function works with the new state detection system."
  (when (and ecc-buffer-current-buffer
           (buffer-live-p ecc-buffer-current-buffer)
           ecc-auto-notify-on-claude-prompt)
    (with-current-buffer ecc-buffer-current-buffer
      ;; Use unified detection function if available, or fall back
      (let ((state (cond
                   ((fboundp 'ecc-detect-state)
                    (ecc-detect-state))
                   ((fboundp 'ecc-detect-prompt-in-last-lines)
                    (ecc-detect-prompt-in-last-lines))
                   (t (ecc-detect-simple-state)))))
        (when state
          (ecc-auto-notify-check-state state))))))

;; Make sure our improvements are loaded after the main module
(with-eval-after-load 'ecc-auto-notify
  ;; Only apply these changes if auto-notify is loaded
  (message "Applied auto-notify improvements for new state detection"))

(provide 'ecc-auto-notify-fix)

;;; ecc-auto-notify-fix.el ends here