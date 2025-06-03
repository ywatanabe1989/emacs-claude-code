;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-06-02 14:41:34>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;; 1. Package configuration
;; ----------------------------------------

(defgroup ecc nil
  "Emacs Claude Code package."
  :prefix "--ecc-"
  :group 'tools)

(defconst --ecc-version "2.0.0"
  "Current version of the emacs-claude-code package.")


;; 2. Dependencies
;; ----------------------------------------

(require 'ecc-debug)
(require 'ecc-state-detection)
(require 'ecc-notification)
(require 'ecc-auto-response)
(require 'ecc-vterm-utils)
(require 'ecc-list)
(require 'ecc-auto-periodical)
(require 'ecc-vterm-yank-as-file)


;; 3. Main entry point
;; ----------------------------------------

;;;###autoload
(defun --ecc-create-vterm ()
  "Create a new vterm buffer with Claude auto-response enabled."
  (interactive)
  (--ecc-debug-message "Creating new Claude vterm buffer...")
  (require 'vterm nil t)
  (if (not (fboundp 'vterm))
      (user-error "vterm is not installed")
    (let ((buffer (vterm "*Claude-vterm*")))
      (with-current-buffer buffer
        (--ecc-auto-response-enable-buffer))
      (--ecc-debug-message "Created vterm buffer: %s" (buffer-name buffer))
      buffer)))


;; 4. Timer management utilities
;; ----------------------------------------

(defun ecc-refresh-timers ()
  "Refresh all timers in the emacs-claude-code system.
This function stops and restarts all timers to resolve timer-related issues."
  (interactive)
  (let ((refreshed-count 0)
        (active-buffers nil))
    
    ;; 1. Handle global auto-response timer
    (when (boundp '--ecc-auto-response--timer)
      (when --ecc-auto-response--timer
        (cancel-timer --ecc-auto-response--timer)
        (setq --ecc-auto-response--timer nil)
        (setq refreshed-count (1+ refreshed-count))))
    
    ;; 2. Find all buffers with auto-response enabled
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (boundp '--ecc-auto-response--enabled)
                   --ecc-auto-response--enabled)
          (push buffer active-buffers))))
    
    ;; 3. Stop all buffer-local timers
    (dolist (buffer active-buffers)
      (with-current-buffer buffer
        ;; Stop periodic timer
        (when (and (boundp '--ecc-auto-response--periodic-timer)
                   --ecc-auto-response--periodic-timer)
          (cancel-timer --ecc-auto-response--periodic-timer)
          (setq --ecc-auto-response--periodic-timer nil)
          (setq refreshed-count (1+ refreshed-count)))
        
        ;; Stop pulse timer
        (when (and (boundp '--ecc-auto-response--pulse-timer)
                   --ecc-auto-response--pulse-timer)
          (cancel-timer --ecc-auto-response--pulse-timer)
          (setq --ecc-auto-response--pulse-timer nil)
          (setq refreshed-count (1+ refreshed-count)))))
    
    ;; 4. Stop notification flash timer if exists
    (when (and (boundp '--ecc-notification--flash-timer)
               --ecc-notification--flash-timer)
      (cancel-timer --ecc-notification--flash-timer)
      (setq --ecc-notification--flash-timer nil)
      (setq refreshed-count (1+ refreshed-count)))
    
    ;; 5. Restart timers for active features
    (when active-buffers
      ;; Restart global auto-response timer
      (when (fboundp '--ecc-auto-response--start-timer)
        (--ecc-auto-response--start-timer))
      
      ;; Restart buffer-local timers
      (dolist (buffer active-buffers)
        (with-current-buffer buffer
          ;; Restart periodic timer if enabled
          (when (and (boundp '--ecc-auto-response-periodic-enabled)
                     --ecc-auto-response-periodic-enabled
                     (fboundp '--ecc-auto-response--start-periodic-timer))
            (--ecc-auto-response--start-periodic-timer buffer))
          
          ;; Restart pulse timer
          (when (fboundp '--ecc-auto-response--start-pulse-timer)
            (--ecc-auto-response--start-pulse-timer))
          
          ;; Update mode-line
          (when (fboundp '--ecc-auto-response--update-mode-line)
            (--ecc-auto-response--update-mode-line))
          (force-mode-line-update))))
    
    ;; 6. Report results
    (message "ECC timers refreshed: %d timers restarted for %d active buffers"
             refreshed-count (length active-buffers))
    
    ;; Return status
    (list :refreshed-count refreshed-count
          :active-buffers (length active-buffers))))


(provide 'ecc)

(when
    (not load-file-name)
  (message "ecc.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))