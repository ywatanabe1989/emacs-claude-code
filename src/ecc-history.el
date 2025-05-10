;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 19:50:22>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/ecc-history.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ecc-variables)
(require 'ecc-buffer/ecc-buffer-registry)
(require 'ecc-send)

;; History Data Structure
;; ------------------------------
(defvar ecc-history-entries (make-hash-table :test 'eq)
  "Hash table mapping Claude buffers to their prompt history entries.
Each entry is a list of (timestamp . prompt-text) cons cells.")

(defun ecc-history-record-entry (buffer prompt)
  "Record PROMPT in the history for BUFFER with current timestamp."
  (let* ((timestamp (current-time))
         (entry (cons timestamp prompt))
         (current-entries (gethash buffer ecc-history-entries)))
    (puthash buffer (cons entry current-entries) ecc-history-entries)))

(defun ecc-history-get-entries (buffer)
  "Get the history entries for BUFFER.
Returns a list of (timestamp . prompt-text) pairs, newest first."
  (gethash buffer ecc-history-entries))

;; History Browser
;; ------------------------------
(defvar ecc-history-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'ecc-history-use-prompt-at-point)
    (define-key map (kbd "r") 'ecc-history-refresh)
    (define-key map (kbd "q") 'ecc-history-quit)
    map)
  "Keymap for `ecc-history-mode'.")

(define-derived-mode ecc-history-mode special-mode "ecc-history"
  "Major mode for browsing Claude prompt history.
\\{ecc-history-mode-map}"
  :group 'emacs-claude
  (read-only-mode 1))

(defun ecc-history-browser-create (buffer)
  "Create a history browser buffer for BUFFER.
Returns the browser buffer."
  (let* ((buffer-name (format "*Claude History: %s*" (buffer-name buffer)))
         (browser-buffer (get-buffer-create buffer-name))
         (entries (ecc-history-get-entries buffer))
         (inhibit-read-only t))
    
    (with-current-buffer browser-buffer
      (erase-buffer)
      (ecc-history-mode)
      
      ;; Store source buffer reference
      (set (make-local-variable 'ecc-history-source-buffer) buffer)
      
      ;; Header
      (insert (format "History for buffer: %s\n\n" (buffer-name buffer)))
      (insert "Press RET to send a prompt, r to refresh, q to quit\n\n")
      
      ;; Display history entries
      (if (null entries)
          (insert "No history entries found.\n")
        (let ((count 0))
          (dolist (entry entries)
            (let* ((timestamp (car entry))
                   (prompt (cdr entry))
                   (formatted-time (format-time-string "%Y-%m-%d %H:%M:%S" timestamp))
                   (start-pos (point)))
              
              ;; Insert with properties
              (insert (format "[%d] %s\n%s\n\n" 
                             count 
                             formatted-time
                             prompt))
              
              ;; Add text properties for navigation
              (add-text-properties 
               start-pos (point)
               `(ecc-history-entry (,count . ,prompt)))
              
              (setq count (1+ count))))))
      
      ;; Setup mode
      (goto-char (point-min))
      (toggle-truncate-lines 1))
    
    browser-buffer))

(defun ecc-history-use-prompt-at-point ()
  "Reuse the prompt at point in the source buffer."
  (interactive)
  (let* ((pos (point))
         (props (text-properties-at pos))
         (entry-prop (plist-get props 'ecc-history-entry)))
    (if entry-prop
        (let ((index (car entry-prop))
              (source-buffer ecc-history-source-buffer))
          (if (buffer-live-p source-buffer)
              (progn
                (ecc-history-reuse-prompt source-buffer index)
                (message "Sent prompt to Claude."))
            (message "Source buffer no longer exists.")))
      (message "No history entry at point."))))

(defun ecc-history-reuse-prompt (buffer index)
  "Reuse the prompt at INDEX in the history of BUFFER."
  (let* ((entries (ecc-history-get-entries buffer))
         (entry (nth index entries)))
    (when entry
      (let ((prompt (cdr entry)))
        (with-current-buffer buffer
          (--ecc-send-string prompt t 0.5))))))

(defun ecc-history-refresh ()
  "Refresh the history browser."
  (interactive)
  (let ((source-buffer ecc-history-source-buffer))
    (if (buffer-live-p source-buffer)
        (let ((browser-buffer (current-buffer))
              (point-pos (point)))
          (kill-buffer browser-buffer)
          (let ((new-buffer (ecc-history-browser-create source-buffer)))
            (switch-to-buffer new-buffer)
            (goto-char (min point-pos (point-max)))))
      (message "Source buffer no longer exists."))))

(defun ecc-history-quit ()
  "Quit the history browser."
  (interactive)
  (kill-buffer (current-buffer)))

;; User Commands
;; ------------------------------
(defun ecc-history-browse ()
  "Browse the history of prompts for the current Claude buffer."
  (interactive)
  (let ((buffer (ecc-buffer-current-get-buffer)))
    (if buffer
        (switch-to-buffer (ecc-history-browser-create buffer))
      (message "No active Claude buffer found."))))

;; Integration with send functions
;; ------------------------------
(defun --ecc-history-after-send-advice (orig-fun string &rest args)
  "Advice function to record prompt in history after sending.
Calls ORIG-FUN with STRING and ARGS."
  (let ((result (apply orig-fun string args)))
    ;; Record in history if it's not a special command and the string is valid
    (when (and (stringp string) 
               (not (string-match-p "^/" string))
               (not (string-empty-p string)))
      (let ((buffer (ecc-buffer-current-get-buffer)))
        (when buffer
          (ecc-history-record-entry buffer string))))
    result))

;; Apply advice to the send function
(when (fboundp '--ecc-send-string)
  (advice-add '--ecc-send-string :around #'--ecc-history-after-send-advice))

(provide 'ecc-history)

(when (not load-file-name)
  (message "ecc-history.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))