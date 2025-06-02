;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-06-01 11:17:16>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-auto-periodical.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; Commentary:
;;; Automatically execute predefined commands at regular interaction intervals.

;; 1. Dependencies
;; ----------------------------------------

(require 'cl-lib)
(require 'ecc-debug)
(require 'ecc-vterm-utils)

;; 2. Configuration
;; ----------------------------------------

(defcustom ecc-auto-periodical-enabled t
  "Whether to enable automatic periodic command execution."
  :type 'boolean
  :group 'ecc)

(defcustom ecc-auto-periodical-commands
  '(10 . "/compact")
  "Alist of (INTERVAL . COMMAND) pairs for periodic execution.
INTERVAL is the number of interactions between executions.
COMMAND is the string to send to the buffer."
  :type '(alist :key-type integer :value-type string)
  :group 'ecc)

(defcustom ecc-auto-periodical-prompt-before-execute nil
  "Whether to prompt user before executing periodic commands."
  :type 'boolean
  :group 'ecc)

(defcustom ecc-auto-periodical-notify t
  "Whether to show notifications when executing periodic commands."
  :type 'boolean
  :group 'ecc)

;; 3. Variables
;; ----------------------------------------

(defvar-local --ecc-auto-periodical-interaction-counter 0
  "Counter for interactions in current buffer.")

(defvar-local --ecc-auto-periodical-last-executed
    (make-hash-table :test 'equal)
  "Hash table tracking last execution count for each command.")

(defvar-local --ecc-auto-periodical-enabled-local nil
  "Buffer-local override for periodic command execution.")

;; 4. Main Functions
;; ----------------------------------------

(defun ecc-auto-periodical-increment ()
  "Increment interaction counter and check for periodic commands."
  (when (and ecc-auto-periodical-enabled
             (or --ecc-auto-periodical-enabled-local
                 (not
                  (local-variable-p
                   '--ecc-auto-periodical-enabled-local))))
    (setq-local --ecc-auto-periodical-interaction-counter
                (1+ --ecc-auto-periodical-interaction-counter))
    (--ecc-debug-message "[Periodical] Counter incremented to: %d"
                         --ecc-auto-periodical-interaction-counter)
    ;; Show counter in minibuffer when debug is enabled
    (when --ecc-debug-enabled
      (message "[ECC Periodical] Interaction count: %d" 
               --ecc-auto-periodical-interaction-counter))
    (ecc-auto-periodical-check)))

(defun ecc-auto-periodical-check ()
  "Check if any periodic command should be executed."
  (dolist (entry ecc-auto-periodical-commands)
    (let ((interval (car entry))
          (command (cdr entry)))
      (when (ecc-auto-periodical--should-execute-p interval command)
        (ecc-auto-periodical-execute command interval)))))

(defun ecc-auto-periodical--should-execute-p (interval command)
  "Check if COMMAND should be executed based on INTERVAL."
  (and (> --ecc-auto-periodical-interaction-counter 0)
       (= (mod --ecc-auto-periodical-interaction-counter interval) 0)
       ;; Prevent duplicate execution at same count
       (not (= (gethash command --ecc-auto-periodical-last-executed 0)
               --ecc-auto-periodical-interaction-counter))))

(defun ecc-auto-periodical-execute (command interval)
  "Execute periodic COMMAND after checking conditions."
  (when (or (not ecc-auto-periodical-prompt-before-execute)
            (y-or-n-p (format
                       "Execute periodic command '%s' (interval: %d)? "
                       command interval)))
    ;; Record execution
    (puthash command --ecc-auto-periodical-interaction-counter
             --ecc-auto-periodical-last-executed)

    ;; Notify if enabled
    (when ecc-auto-periodical-notify
      (message
       "[ECC Auto-Periodical] 🔄 Executing: %s (every %d interactions, count: %d)"
       command interval --ecc-auto-periodical-interaction-counter))

    ;; Execute the command
    (ecc-auto-periodical--send-command command)

    (--ecc-debug-message
     "Executed periodic command: %s at interaction %d"
     command
     --ecc-auto-periodical-interaction-counter)))

(defun ecc-auto-periodical--send-command (command)
  "Send COMMAND to the current buffer."
  (cond
   ((derived-mode-p 'vterm-mode)
    (when (fboundp 'vterm-send-string)
      (vterm-send-string command)
      (vterm-send-return)))
   ((derived-mode-p 'comint-mode)
    (goto-char (point-max))
    (insert command)
    (comint-send-input))
   (t
    (goto-char (point-max))
    (insert command)
    (insert "\n")))
  ;; Show what was actually sent
  (message "[ECC Auto-Periodical] ✅ Sent: %s" command))

;; 5. Interactive Commands
;; ----------------------------------------

(defun ecc-auto-periodical-toggle ()
  "Toggle periodic command execution globally."
  (interactive)
  (setq ecc-auto-periodical-enabled (not ecc-auto-periodical-enabled))
  (message "Auto periodic commands %s"
           (if ecc-auto-periodical-enabled "enabled" "disabled")))

(defun ecc-auto-periodical-toggle-buffer ()
  "Toggle periodic command execution for current buffer."
  (interactive)
  (setq-local --ecc-auto-periodical-enabled-local
              (not (if
                       (local-variable-p
                        '--ecc-auto-periodical-enabled-local)
                       --ecc-auto-periodical-enabled-local
                     ecc-auto-periodical-enabled)))
  (message "Auto periodic commands %s in buffer %s"
           (if --ecc-auto-periodical-enabled-local "enabled"
             "disabled")
           (buffer-name)))

(defun ecc-auto-periodical-reset-counter ()
  "Reset the interaction counter for current buffer."
  (interactive)
  (setq-local --ecc-auto-periodical-interaction-counter 0)
  (clrhash --ecc-auto-periodical-last-executed)
  (message "Interaction counter reset to 0"))

(defun ecc-auto-periodical-status ()
  "Show current periodic command status."
  (interactive)
  (let ((status-lines
         (list
          (format "Global: %s"
                  (if ecc-auto-periodical-enabled "enabled"
                    "disabled"))
          (format "Buffer: %s" (if (and
                                    (local-variable-p
                                     '--ecc-auto-periodical-enabled-local)
                                    --ecc-auto-periodical-enabled-local)
                                   "enabled"
                                 "disabled"))
          (format "Counter: %d"
                  --ecc-auto-periodical-interaction-counter)
          "Commands:")))
    (dolist (entry ecc-auto-periodical-commands)
      (let* ((interval (car entry))
             (command (cdr entry))
             (last-exec
              (gethash command --ecc-auto-periodical-last-executed 0))
             (next-exec
              (* interval
                 (1+
                  (/ --ecc-auto-periodical-interaction-counter
                     interval)))))
        (push (format "  %s every %d (last: %d, next: %d)"
                      command interval last-exec next-exec)
              status-lines)))
    (message "%s" (string-join (nreverse status-lines) "\n"))))

;; 6. Integration Hook
;; ----------------------------------------

(defun ecc-auto-periodical-setup-hook ()
  "Setup hook for integrating with auto-response system."
  ;; This will be called from ecc-auto-response when a response is sent
  (ecc-auto-periodical-increment))

;;; ecc-auto-periodical.el ends here


(provide 'ecc-auto-periodical)

(when
    (not load-file-name)
  (message "ecc-auto-periodical.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))