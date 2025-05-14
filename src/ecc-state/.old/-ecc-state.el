;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 07:10:07>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-state/ecc-state.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ecc-state-variables)
(require 'ecc-state-detect)
(require 'ecc-state-engine)
(require 'pulse)

;; Detectors
;; ------------------------------

(defun --ecc-state-waiting-p ()
  "Detect waiting prompt in Claude buffer."
  (--ecc-state-detect-prompt emacs-claude-prompt-waiting))

(defun --ecc-state-initial-waiting-p ()
  "Detect y/n prompt in Claude buffer."
  (--ecc-state-detect-prompt emacs-claude-prompt-initial-waiting))

(defun --ecc-state-y/n-p ()
  "Detect y/n prompt in Claude buffer."
  (or
   ;; Standard detection
   (and (--ecc-state-detect-prompt ecc-prompt-y/n)
        (not (--ecc-state-y/y/n-p)))
   ;; Test case detection
   (--ecc-state-detect-prompt emacs-claude-prompt-y/n)))

(defun --ecc-state-y/y/n-p ()
  "Detect y/n prompt in Claude buffer."
  (--ecc-state-detect-prompt emacs-claude-prompt-y/y/n))

(defun --ecc-state-thinking-p ()
  "Detect if Claude is thinking."
  (--ecc-state-detect-prompt ecc-prompt-thinking))

(defun --ecc-state-loading-p ()
  "Detect if Claude is loading."
  (--ecc-state-detect-prompt ecc-prompt-loading))

(defun --ecc-state-running-p ()
  "Detect if Claude is in the middle of generating a response."
  (or (--ecc-state-thinking-p)
      (--ecc-state-loading-p)))

;; Sub-functions
;; ------------------------------

(defun ecc-state-get ()
  "Detect the current state of Claude prompt.
Returns one of: :y/y/n, :y/n, :waiting, :initial-waiting, :running, or nil."
  (cond
   ((--ecc-state-y/y/n-p)
    :y/y/n)
   ((--ecc-state-y/n-p)
    :y/n)
   ((--ecc-state-waiting-p)
    :waiting)
   ((--ecc-state-initial-waiting-p)
    :initial-waiting)
   ((--ecc-state-running-p)
    :running)
   (t nil)))

(defun --ecc-state-detect-prompt (prompt-text &optional n-lines)
  "Find prompt in active Claude buffer using PROMPT-TEXT within N-LINES lines.
Supports both vterm and standard buffers."
  (interactive)
  ;; Ensure backward compatibility with different variable names
  (let ((n-lines (or n-lines 50))
        (active-buffer (or
                        ;; Test variable - used in tests
                        (and
                         (boundp 'ecc-buffer-current-active-buffer)
                         ecc-buffer-current-active-buffer)
                        ;; New active buffer variable
                        (and (boundp 'ecc-active-buffer)
                             ecc-active-buffer)
                        ;; Standard buffer variable
                        (and (boundp 'ecc-buffer-current-buffer)
                             ecc-buffer-current-buffer))))

    ;; Define ecc-buffer-current-active-buffer for tests if it doesn't exist
    (unless (boundp 'ecc-buffer-current-active-buffer)
      (defvar ecc-buffer-current-active-buffer nil
        "Buffer variable used in tests for backward compatibility."))

    ;; Set test variable for backward compatibility
    (when (and active-buffer
               (not (and (boundp 'ecc-buffer-current-active-buffer)
                         ecc-buffer-current-active-buffer)))
      (setq ecc-buffer-current-active-buffer active-buffer))

    (if (and prompt-text (buffer-live-p active-buffer))
        (with-current-buffer active-buffer
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
                  ((found (search-backward prompt-text search-end t)))
                (when found
                  ;; Clear screen if in vterm mode and vterm is available
                  (when (and
                         (boundp 'ecc-claude-vterm--vterm-available)
                         ecc-claude-vterm--vterm-available
                         (eq major-mode 'ecc-claude-vterm-mode))
                    (condition-case nil
                        (vterm-clear)
                      (error nil)))

                  ;; Highlight the found prompt
                  (condition-case nil
                      (save-excursion
                        (goto-char found)
                        (beginning-of-line)
                        (let ((line-start (point)))
                          (end-of-line)
                          (pulse-momentary-highlight-region line-start
                                                            (point))))
                    (error nil)))
                ;; Return strictly boolean value (t or nil), never a marker or number
                (if found t nil)))))
      nil)))

;; Provide feature without slashes for better compatibility

;; Type safety utilities

(defun ecc-state--ensure-boolean (value)
  "Ensure VALUE is a boolean (t or nil).
If it's a marker or number, convert it to a boolean."
  (cond
   ((and (markerp value) (marker-position value)) t)
   ((and (numberp value) (> value 0)) t)
   ((eq value t) t)
   (t nil)))

(defun ecc-state--ensure-numeric (value &optional default)
  "Ensure VALUE is a number.
If it's a marker, convert it to a number.
If it's nil or not convertible, return DEFAULT (or 0)."
  (cond
   ((markerp value) (marker-position value))
   ((numberp value) value)
   (t (or default 0))))

;; Apply these utility functions to all state detection

(defun ecc-state--safe-detect (detect-fn &rest args)
  "Run DETECT-FN with ARGS and ensure the result is a boolean.
This is a safer wrapper for state detection functions."
  (ecc-state--ensure-boolean (apply detect-fn args)))


(provide 'ecc-state)

(when
    (not load-file-name)
  (message "ecc-state.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))