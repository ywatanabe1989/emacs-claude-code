;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-12 13:14:46>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-buffer/ecc-buffer-state.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'pulse)
(require 'ecc-variables)
(require 'ecc-buffer-current)

;; State initialization
;; ------------------------------

;;;###autoload
(defun ecc-state-initialize (&optional buffer)
  "Clear the current Claude Code conversation and restart it."
  (interactive)
  (let ((buf (or buffer (ecc-buffer-get-current-buffer))))
    (when buf
      (with-current-buffer buf
        (when (--ecc-state-is-claude-active-p buf)
          ;; Interrupt any running process first
          (ecc-send-interrupt)
          (sit-for 0.3)
          ;; Exit current process if any
          (--ecc-send-string "exit" t 0.7))
        ;; Clear screen to ensure a clean slate
        (vterm-clear)
        (sit-for 0.3)
        ;; Start a new Claude session
        (--ecc-send-string "claude\C-m" t 0.5)
        (message "Claude conversation restarted."))
      (display-buffer buf))))

(defun ecc-state-buffer-exists-p (&optional buffer)
  "Check if BUFFER exists and is in vterm-mode."
  (let ((buf (or buffer (ecc-buffer-get-current-buffer))))
    (and (bufferp buf)
         (buffer-live-p buf)
         (with-current-buffer buf
           (derived-mode-p 'vterm-mode)))))

;; Detectors
;; ------------------------------

(defun --ecc-state-is-claude-active-p (&optional buffer)
  "Check if Claude is in an active state by checking various prompt patterns.
Optional argument BUFFER defaults to the current buffer."
  (let ((buf (or buffer (ecc-buffer-get-current-buffer))))
    (when (ecc-state-buffer-exists-p buf)
      (or (--ecc-state-is-waiting-p buf)
          (--ecc-state-is-initial-waiting-p buf)
          (--ecc-state-is-y/n-p buf)
          (--ecc-state-is-y/y/n-p buf)))))

(defun --ecc-state-is-waiting-p (&optional buffer)
  "Detect waiting prompt in Claude buffer."
  (--ecc-state-detect-prompt ecc-prompt-pattern-waiting buffer))

(defun --ecc-state-is-initial-waiting-p (&optional buffer)
  "Detect initial waiting prompt in Claude buffer."
  (--ecc-state-detect-prompt ecc-prompt-pattern-initial-waiting buffer))

(defun --ecc-state-is-y/n-p (&optional buffer)
  "Detect y/n prompt in Claude buffer."
  (and (--ecc-state-detect-prompt ecc-prompt-pattern-y/n buffer)
       (not (--ecc-state-is-y/y/n-p buffer))))

(defun --ecc-state-is-y/y/n-p (&optional buffer)
  "Detect y/y/n prompt in Claude buffer."
  (when ecc-prompt-pattern-y/y/n
    (--ecc-state-detect-prompt ecc-prompt-pattern-y/y/n buffer)))

(defun --ecc-state-is-running-p (&optional buffer)
  "Detect if Claude is in running state."
  (let ((buf (or buffer (ecc-buffer-get-current-buffer))))
    (when (ecc-state-buffer-exists-p buf)
      (with-current-buffer buf
        (not (--ecc-state-is-claude-active-p buf))))))

;; State detection
;; ------------------------------

(defun ecc-state-get (&optional buffer)
  "Detect the current state of Claude prompt in BUFFER.
Returns one of: :y/y/n, :y/n, :waiting, :initial-waiting, :running, or nil."
  (let ((buf (or buffer (ecc-buffer-get-current-buffer))))
    (when (ecc-state-buffer-exists-p buf)
      (cond
       ((--ecc-state-is-y/y/n-p buf) :y/y/n)
       ((--ecc-state-is-y/n-p buf) :y/n)
       ((--ecc-state-is-waiting-p buf) :waiting)
       ((--ecc-state-is-initial-waiting-p buf) :initial-waiting)
       ((--ecc-state-is-running-p buf) :running)
       (t nil)))))

;; Helper functions
;; ------------------------------

(defun --ecc-state-detect-prompt
    (prompt-pattern &optional buffer n-lines)
  "Find prompt in Claude buffer using PROMPT-PATTERN within the last N-LINES lines."
  (let ((buf (or buffer (ecc-buffer-get-current-buffer)))
        (n-lines (or n-lines 50)))
    (when (and prompt-pattern (ecc-state-buffer-exists-p buf))
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-max))
          (skip-chars-backward " \t\n\r")
          (let* ((current-line (line-number-at-pos))
                 (min-line (line-number-at-pos (point-min)))
                 (search-end-line
                  (max min-line (- current-line n-lines)))
                 (search-end (save-excursion
                               (goto-char (point-min))
                               (forward-line (1- search-end-line))
                               (point))))
            (let
                ((found (search-backward prompt-pattern search-end t)))
              (when found
                (condition-case nil
                    (save-excursion
                      (goto-char found)
                      (beginning-of-line)
                      (let ((line-start (point)))
                        (end-of-line)
                        (pulse-momentary-highlight-region line-start
                                                          (point))))
                  (error nil)))
              found)))))))


;; Register this feature with standard naming

;; Also provide with prefix to match test expectations


(provide 'ecc-buffer-state)

(when
    (not load-file-name)
  (message "ecc-buffer-state.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
