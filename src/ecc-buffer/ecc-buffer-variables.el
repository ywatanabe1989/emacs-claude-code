;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 18:50:30>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-buffer/ecc-buffer-variables.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(defgroup emacs-claude-buffer nil
  "Customization group for Claude buffer management."
  :group 'emacs-claude
  :prefix "ecc-buffer-")

;; Buffer Name and Creation
;; ------------------------------

(defcustom ecc-buffer-name "*CLAUDE-CODE*"
  "Name for the Claude interaction buffer."
  :type 'string
  :group 'emacs-claude-buffer)

;; Buffer Tracking
;; ------------------------------

(defvar ecc-buffer-current-buffer nil
  "The current Claude buffer being used.")

(defvar ecc-buffer--active-buffer nil
  "Buffer currently being displayed and interacted with.")

(defvar ecc-buffer--registered-buffers nil
  "List of all registered Claude buffers.")

(defvar ecc-buffer-registered-buffers-alist nil
  "Alist of (buffer . state) for registered Claude buffers.")

;; Buffer Properties
;; ------------------------------

(defvar ecc-buffer-property-defaults
  '((role . "assistant")
    (project . nil))
  "Default properties for Claude buffers.")

;; Buffer Management
;; ------------------------------

(defvar ecc-buffer-switch-mode nil
  "Enable automatic switching between Claude buffers.")

(defcustom ecc-buffer-switch-interval 2
  "Time interval in seconds for checking buffer switching conditions."
  :type 'number
  :group 'emacs-claude-buffer)

;; Buffer Stale Detection
;; ------------------------------

(defcustom ecc-buffer-stale-timeout 300
  "Timeout in seconds after which a buffer is considered stale."
  :type 'number
  :group 'emacs-claude-buffer)

(defvar ecc-buffer--stale-buffers nil
  "List of buffers that are considered stale.")

;; Buffer Timestamp Tracking
;; ------------------------------

(defvar ecc-buffer--timestamps (make-hash-table :test 'equal)
  "Hash table mapping buffer names to last access timestamps.")

(defvar ecc-buffer--update-history (make-hash-table :test 'equal)
  "Hash table tracking buffer update history for analytics.")


(provide 'ecc-buffer-variables)

(when
    (not load-file-name)
  (message "ecc-buffer-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))