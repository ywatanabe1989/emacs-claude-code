;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-06-04 08:47:04>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-debug.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;; 1. Dependencies
;; ----------------------------------------
;; No dependencies for this module

;; 2. Configuration
;; ----------------------------------------

(defcustom --ecc-debug-enabled nil
  "Whether debugging is globally enabled."
  :type 'boolean
  :group 'ecc)

;; 3. Variables
;; ----------------------------------------

(defvar --ecc-debug-prefix "[ECC] "
  "Prefix for debug messages.")

(defcustom --ecc-debug-log-max 30
  "Maximum number of recent events to keep in the debug log ring."
  :type 'integer
  :group 'ecc)

(defvar --ecc-debug-log nil
  "List of recent debug events (newest first).
Each entry is (TIMESTAMP . MESSAGE).")

;; 4. Main Entry Points
;; ----------------------------------------

(defun ecc-debug-toggle ()
  "Toggle global debug output."
  (interactive)
  (setq --ecc-debug-enabled (not --ecc-debug-enabled))
  (message "ECC debug %s"
           (if --ecc-debug-enabled "enabled" "disabled")))

;; 5. Core Functions
;; ----------------------------------------

(defun --ecc-debug-message (format-string &rest args)
  "Output a debug message if debugging is enabled.
Always records to the event log ring for display in the buffer list."
  (let ((msg (apply #'format format-string args)))
    ;; Always log to ring (for buffer list display)
    (push (cons (float-time) msg) --ecc-debug-log)
    (when (> (length --ecc-debug-log) --ecc-debug-log-max)
      (setq --ecc-debug-log
	    (seq-take --ecc-debug-log --ecc-debug-log-max)))
    ;; Only print to *Messages* when debug enabled
    (when --ecc-debug-enabled
      (message "%s%s" --ecc-debug-prefix msg))))

(defun --ecc-debug-log-clear ()
  "Clear the debug event log."
  (interactive)
  (setq --ecc-debug-log nil)
  (message "ECC debug log cleared"))

;; 6. Helper/Utility Functions
;; ----------------------------------------
;; No helper functions in this file

(when
    (not load-file-name)
  (--ecc-debug-message "ecc-debug.el loaded."
                       (file-name-nondirectory
                        (or load-file-name buffer-file-name))))

(provide 'ecc-debug)

(when
    (not load-file-name)
  (message "ecc-debug.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
