;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 03:19:20>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-send.el
;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ecc-variables)
(require 'ecc-buffer)
(require 'ecc-state)
(require 'ecc-ui-mode-line)

;; External function declarations
(declare-function vterm-clear "ext:vterm")
(declare-function vterm-send-string "ext:vterm")
(declare-function vterm-send-return "ext:vterm")
(declare-function vterm-copy-mode "ext:vterm")
(declare-function ecc-auto-notify-completion "ecc-mode")

;; 1. High-level user commands
;; ----------------------------------------

;;;###autoload
(defun ecc-send-interrupt ()
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
      (ecc-ui-highlight-all-buffers)
      (message "Claude process interrupted.")))

;;;###autoload
(defun ecc-send-clear-history ()
  "Clear the conversation history in Claude Code.
This sends the '/clear-history' command to Claude."
  (interactive)
  (--ecc-send-string "/clear-history" t 0.5)
  (message "Claude conversation history cleared."))

;;;###autoload
(defun ecc-send-compact-history ()
  "Compact the conversation history in Claude Code to save tokens.
This sends the '/compact-history' command to Claude."
  (interactive)
  (--ecc-send-string "/compact-history" t 0.5)
  (message "Claude conversation history compacted."))

;;;###autoload
(defun ecc-send-region (start end)
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

(defun ecc-send-paste-to-active-buffer ()
  "Paste the current kill ring contents to Claude buffer."
  (interactive)
  (when (buffer-live-p ecc-buffer-current-active-buffer)
    (with-current-buffer ecc-buffer-current-active-buffer
      (let ((content (current-kill 0)))
        (vterm-send-string content)
        (message "Content pasted to Claude buffer.")))))

(defun ecc-send-template ()
  "Interactively input and send a custom template response for y/y/n situations."
  (interactive)
  (let ((template-text (read-string "Enter custom response: ")))
    (--ecc-auto-send-template-on-y/y/n template-text)))


;; 3. Helper functions
;; ----------------------------------------

(defun --ecc-send-string (string &optional no-confirm delay buffer)
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
      (message "[ecc-send] Sent: %s" string)
      string)))

(defun --ecc-send-by-state (response state-or-predicate)
  "Send RESPONSE to a specific Claude prompt state.
STATE-OR-PREDICATE can be a keyword (:y/n, :y/y/n, etc.) or a predicate function.
Returns the response string if sent, nil otherwise."
  (with-current-buffer ecc-buffer-current-active-buffer
    (let ((should-send
           (if (functionp state-or-predicate)
               (funcall state-or-predicate)
             (eq (ecc-state-get) state-or-predicate))))
      (when should-send
        (--ecc-send-string response t 1.0)
        (vterm-send-return)
        (message "[ecc-send] Automatic Response: %s" response)
        response))))


(provide 'ecc-send)
(when
    (not load-file-name)
  (message "ecc-send.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
