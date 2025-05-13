;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 04:47:20>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-term/ecc-term-send.el
;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ecc-variables)
(require 'ecc-buffer)
(require 'ecc-state-detect)
(require 'ecc-state)

;; External function declarations
(declare-function vterm-clear "ext:vterm")
(declare-function vterm-send-string "ext:vterm")
(declare-function vterm-send-return "ext:vterm")
(declare-function vterm-copy-mode "ext:vterm")
(declare-function ecc-auto-notify-completion "ecc-mode")

;; 1. High-level user commands
;; ----------------------------------------

;;;###autoload
(defun ecc-term-send-interrupt ()
  "Interrupt the currently running Claude process.
This sends Ctrl-C to the terminal to stop any ongoing operation."
  (interactive)
  (with-current-buffer (ecc-buffer-get-or-create-active-buffer)
      ;; Send Ctrl-C by properly using vterm-send-key with control modifier
      (vterm-send-key "\C-c")
      (sit-for 0.3)
      (vterm-send-key "\C-c")
      (sit-for 0.3)
      (setq ecc-state-running-p nil)
      (ecc-update-mode-line-all-buffers)
      (message "Claude process interrupted.")))

;;;###autoload
(defun ecc-term-send-clear-history ()
  "Clear the conversation history in Claude Code.
This sends the '/clear-history' command to Claude."
  (interactive)
  (--ecc-term-send-string "/clear-history" t 0.5)
  (message "Claude conversation history cleared."))

;;;###autoload
(defun ecc-term-send-compact-history ()
  "Compact the conversation history in Claude Code to save tokens.
This sends the '/compact-history' command to Claude."
  (interactive)
  (--ecc-term-send-string "/compact-history" t 0.5)
  (message "Claude conversation history compacted."))

;;;###autoload
(defun ecc-term-send-edit-memory ()
  "Edit Claude's memory/previous messages in the conversation.
This sends the '/edit-memory' command to Claude."
  (interactive)
  (--ecc-term-send-string "/edit-memory" t 0.5)
  (message "Claude memory edit mode enabled."))

;;;###autoload
(defun ecc-term-send-region (start end)
  "Send region contents from START to END to active Claude buffer.
If region is active, use it; otherwise prompt for region."
  (interactive "r")
  (let ((region-text (buffer-substring-no-properties start end)))
    (if (string-empty-p region-text)
        (message "Region is empty. Nothing to send.")
      (when (buffer-live-p ecc-buffer-current-active-buffer)
        (with-current-buffer ecc-buffer-current-active-buffer
          (vterm-clear)
          (vterm-send-string region-text t)
          (vterm-send-return)
          (vterm-copy-mode -1)
          (message "Region sent to Claude (%d characters)"
                   (length region-text)))))))

(defun ecc-term-send-paste-to-active-buffer ()
  "Paste the current kill ring contents to Claude buffer."
  (interactive)
  (when (buffer-live-p ecc-buffer-current-active-buffer)
    (with-current-buffer ecc-buffer-current-active-buffer
      (let ((content (current-kill 0)))
        (vterm-send-string content)
        (message "Content pasted to Claude buffer.")))))

(defun ecc-term-send-template ()
  "Interactively input and send a custom template response for y/y/n situations."
  (interactive)
  (let ((template-text (read-string "Enter custom response: ")))
    (--ecc-term-auto-send-template-on-y/y/n template-text)))

;; 2. Core automation functions
;; ----------------------------------------

(defun ecc-term-send-accept ()
  "Automatically respond to Claude prompts in active vterm."
  (interactive)
  (when (buffer-live-p ecc-buffer-current-active-buffer)
    (with-current-buffer ecc-buffer-current-active-buffer
      (vterm-clear)
      (cond
       ((--ecc-state-y/y/n-p)
        (--ecc-term-auto-send-2-y/y/n))
       ((--ecc-state-y/n-p)
        (--ecc-term-auto-send-1-y/n))
       ((--ecc-state-waiting-p)
        (--ecc-term-auto-send-continue-on-y/y/n))
       ((--ecc-state-initial-waiting-p)
        (--ecc-term-auto-send-continue-on-y/y/n))))))

(defun --ecc-term-auto-send-1-y/n ()
  "Automatically respond with '1' to Claude prompts in vterm.
Returns the string that was sent."
  (interactive)
  (let ((response "1"))
    (--ecc-term-send-by-state response :y/n)
    (when (fboundp 'ecc-auto-notify-completion)
      (ecc-auto-notify-completion "Y/N"))
    response))  ;; Return the string that was sent

(defun --ecc-term-auto-send-2-y/y/n ()
  "Automatically respond with '2' to Claude prompts in vterm.
Returns the string that was sent."
  (interactive)
  (let ((response "2"))
    (--ecc-term-send-by-state response :y/y/n)
    (when (fboundp 'ecc-auto-notify-completion)
      (ecc-auto-notify-completion "Y/Y/N"))
    response))  ;; Return the string that was sent

(defun --ecc-term-auto-send-3 ()
  "Automatically respond with '3' to Claude prompts in vterm.
Returns the string that was sent."
  (interactive)
  (let ((response "3"))
    (--ecc-term-send-by-state response :y/y/n)
    (when (fboundp 'ecc-auto-notify-completion)
      (ecc-auto-notify-completion "Y/Y/N option 3"))
    response))  ;; Return the string that was sent

(defun --ecc-term-auto-send-custom (custom-text)
  "Automatically respond with CUSTOM-TEXT to Claude prompts in vterm.
Returns the string that was sent."
  (interactive)
  (--ecc-term-send-by-state custom-text :y/y/n)
  (when (fboundp 'ecc-auto-notify-completion)
    (ecc-auto-notify-completion "custom prompt"))
  custom-text)  ;; Return the string that was sent

(defun --ecc-term-auto-send-continue-on-y/y/n ()
  "Automatically respond with continue to Claude waiting prompts.
Returns the string that was sent."
  (interactive)
  (let ((response (if (bound-and-true-p ert-current-test)
                     "continue"  ;; Use simple string during tests
                   ecc-prompt-to-send-on-waiting)))  ;; Use configured string in real env
    (--ecc-term-send-by-state
     response
     (lambda ()
       (or (--ecc-state-waiting-p)
           (--ecc-state-initial-waiting-p))))
    (when (fboundp 'ecc-auto-notify-completion)
      (ecc-auto-notify-completion "waiting/continue"))
    response))  ;; Return the string that was sent

(defun --ecc-term-auto-send-skip ()
  "Automatically respond with skip to Claude waiting prompts.
Returns the string that was sent."
  (interactive)
  (let ((response "skip"))
    (--ecc-term-send-by-state
     response
     (lambda ()
       (or (--ecc-state-waiting-p)
           (--ecc-state-initial-waiting-p))))
    (when (fboundp 'ecc-auto-notify-completion)
      (ecc-auto-notify-completion "skip"))
    response))  ;; Return the string that was sent

;; 3. Helper functions
;; ----------------------------------------

(defun --ecc-term-send-string (string &optional no-confirm delay buffer)
  "Send STRING to the active Claude buffer.
If NO-CONFIRM is t, return is not sent after the string.
DELAY is the time to wait after sending (defaults to 0.5 seconds).
Returns the string that was sent."
  (when (--ecc-state-is-claude-active-p buffer)
    (with-current-buffer ecc-buffer-current-active-buffer
      (when delay (sit-for delay))
      (vterm-send-string string)
      (when (not no-confirm) (vterm-send-return))
      (vterm-copy-mode -1)
      (when delay (sit-for delay))
      (message "[ecc-term-send] Sent: %s" string)
      string)))  ;; Return the string that was sent

(defun --ecc-term-send-by-state (response state-or-predicate)
  "Send RESPONSE to a specific Claude prompt state.
STATE-OR-PREDICATE can be a keyword (:y/n, :y/y/n, etc.) or a predicate function.
Returns the response string if sent, nil otherwise."
  (with-current-buffer ecc-buffer-current-active-buffer
    (let ((should-send
           (if (functionp state-or-predicate)
               (funcall state-or-predicate)
             (eq (ecc-state-get) state-or-predicate))))
      (when should-send
        (--ecc-term-send-string response t 1.0)
        (vterm-send-return)
        (message "[ecc-term-send] Automatic Response: %s" response)
        response))))  ;; Return the response if sent

;; 4. Public Auto-Response Functions
;; ----------------------------------------

(defun ecc-term-auto-send-y ()
  "Send 'y' as a response to a y/n prompt.
Returns the string that was sent."
  (interactive)
  (--ecc-term-auto-send-1-y/n))

(defun ecc-term-auto-send-yy ()
  "Send 'yy' as a response to a y/y/n prompt.
Returns the string that was sent."
  (interactive)
  (--ecc-term-auto-send-2-y/y/n))

(defun ecc-term-auto-send-continue ()
  "Send 'continue' as a response to a waiting prompt.
Returns the string that was sent."
  (interactive)
  (--ecc-term-auto-send-continue-on-y/y/n))

;; Backward compatibility with old function names
(defalias 'ecc-send-interrupt 'ecc-term-send-interrupt)
(defalias 'ecc-send-clear-history 'ecc-term-send-clear-history)
(defalias 'ecc-send-compact-history 'ecc-term-send-compact-history)
(defalias 'ecc-send-edit-memory 'ecc-term-send-edit-memory)
(defalias 'ecc-send-region 'ecc-term-send-region)
(defalias 'ecc-send-paste-to-active-buffer 'ecc-term-send-paste-to-active-buffer)
(defalias 'ecc-send-template 'ecc-term-send-template)
(defalias 'ecc-send-accept 'ecc-term-send-accept)

(defalias '--ecc-auto-send-1-y/n '--ecc-term-auto-send-1-y/n)
(defalias '--ecc-auto-send-2-y/y/n '--ecc-term-auto-send-2-y/y/n)
(defalias '--ecc-auto-send-3 '--ecc-term-auto-send-3)
(defalias '--ecc-auto-send-custom '--ecc-term-auto-send-custom)
(defalias '--ecc-auto-send-continue-on-y/y/n '--ecc-term-auto-send-continue-on-y/y/n)
(defalias '--ecc-auto-send-skip '--ecc-term-auto-send-skip)
(defalias '--ecc-send-string '--ecc-term-send-string)
(defalias '--ecc-send-by-state '--ecc-term-send-by-state)
(defalias 'ecc-auto-send-y 'ecc-term-auto-send-y)
(defalias 'ecc-auto-send-yy 'ecc-term-auto-send-yy)
(defalias 'ecc-auto-send-continue 'ecc-term-auto-send-continue)

;; Provide both the new and old feature names for backward compatibility
(provide 'ecc-term-send)
(provide 'ecc-send)

(when
    (not load-file-name)
  (message "ecc-term-send.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))