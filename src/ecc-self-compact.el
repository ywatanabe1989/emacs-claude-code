;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-03-26 03:05:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-self-compact.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)

;; Self-compact and context monitoring for Claude Code sessions.
;; Used by emacs_mcp_server to enable Claude agents to compact their own context.

;; 1. Dependencies
;; ----------------------------------------
(require 'ecc-state-detection)
(require 'ecc-vterm-utils)

;; 2. Configuration
;; ----------------------------------------

(defcustom --ecc-self-compact-context-threshold 80
  "Context percentage threshold above which self-compact is recommended."
  :type 'integer
  :group 'ecc)

(defcustom --ecc-self-compact-delay-after-compact 3
  "Seconds to wait after sending /compact before sending follow-up."
  :type 'number
  :group 'ecc)

;; 3. Context Percentage Extraction
;; ----------------------------------------

(defun --ecc-self-compact-get-context-percent (&optional buffer)
  "Extract context usage percentage from Claude Code status bar in BUFFER.
Returns integer percentage or nil if not found."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((text (buffer-substring-no-properties
                  (max (- (point-max) 500) (point-min))
                  (point-max)))
           (match (string-match "\\([0-9]+\\)%" text)))
      (when match
        (string-to-number (match-string 1 text))))))

(defun --ecc-self-compact-get-context-info (&optional buffer)
  "Get full context info from Claude Code status bar in BUFFER.
Returns plist with :percent, :model, :time-remaining, or nil."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((text (buffer-substring-no-properties
                  (max (- (point-max) 500) (point-min))
                  (point-max)))
           (percent (when (string-match "\\([0-9]+\\)%" text)
                      (string-to-number (match-string 1 text))))
           (model (when (string-match "\\[\\([^]]+\\)\\]" text)
                    (match-string 1 text))))
      (when percent
        (list :percent percent
              :model (or model "unknown")
              :needs-compact
	      (>= percent --ecc-self-compact-context-threshold))))))

;; 4. Self-Compact
;; ----------------------------------------

(defun --ecc-self-compact-execute (&optional buffer follow-up-message)
  "Execute self-compact in Claude Code BUFFER.
Checks state first — only sends /compact if Claude is :waiting.
FOLLOW-UP-MESSAGE is sent after compact (default: \"continue\").
Returns plist with :success, :state, :reason."
  (let* ((buf (or buffer (current-buffer)))
         (state (with-current-buffer buf
                  (--ecc-state-detection-detect)))
         (msg (or follow-up-message "continue")))
    (cond
     ;; Not waiting — unsafe to send
     ((not (eq state :waiting))
      (list :success nil
            :state (--ecc-state-detection-get-name state)
            :reason (format "Claude is %s, not waiting for input"
                            (--ecc-state-detection-get-name state))))
     ;; Waiting — safe to compact
     (t
      (with-current-buffer buf
        (vterm-send-string "/compact")
        (vterm-send-return)
        (sit-for --ecc-self-compact-delay-after-compact)
        (vterm-send-string msg)
        (vterm-send-return))
      (list :success t
            :state "waiting"
            :reason "Compact sent successfully"
            :follow-up msg)))))

(defun --ecc-self-compact-if-needed
    (&optional buffer follow-up-message threshold)
  "Execute self-compact only if context percentage exceeds THRESHOLD.
THRESHOLD defaults to `--ecc-self-compact-context-threshold'."
  (let* ((buf (or buffer (current-buffer)))
         (thresh (or threshold --ecc-self-compact-context-threshold))
         (percent (--ecc-self-compact-get-context-percent buf)))
    (cond
     ((not percent)
      (list :success nil :reason "Could not read context percentage"))
     ((< percent thresh)
      (list :success nil
            :reason (format "Context at %d%%, below threshold %d%%"
                            percent thresh)
            :percent percent))
     (t
      (let
	  ((result (--ecc-self-compact-execute buf follow-up-message)))
        (plist-put result :percent percent))))))

;; 5. Vterm Output Cleaning
;; ----------------------------------------

(defun --ecc-self-compact-get-clean-output
    (&optional buffer last-n-lines)
  "Get clean output from vterm BUFFER, stripping trailing whitespace.
LAST-N-LINES limits output to last N non-empty lines."
  (with-current-buffer (or buffer (current-buffer))
    (let*
	((content
	  (buffer-substring-no-properties (point-min) (point-max)))
         (lines (split-string content "\n"))
         (trimmed (mapcar #'string-trim-right lines))
         ;; Remove trailing empty lines
         (cleaned (let ((result (reverse trimmed)))
                    (while
			(and result (string-empty-p (car result)))
                      (setq result (cdr result)))
                    (reverse result))))
      (if last-n-lines
          (string-join (last cleaned last-n-lines) "\n")
        (string-join cleaned "\n")))))

;; 6. Provide
;; ----------------------------------------

(provide 'ecc-self-compact)

(when (not load-file-name)
  (message "ecc-self-compact.el loaded."))
