;;; fix-names.el --- Fix function and variable name compatibility for tests

;; This file provides compatibility aliases for functions and variables that have been
;; renamed or moved between modules.

;; Require all buffer modules
(require 'ecc-buffer/ecc-buffer-variables)
(require 'ecc-buffer/ecc-buffer-registry)
(require 'ecc-buffer/ecc-buffer-verification)
(require 'ecc-buffer/ecc-buffer-current)
(require 'ecc-buffer/ecc-buffer-state)
(require 'ecc-buffer/ecc-buffer-stale)
(require 'ecc-buffer/ecc-buffer-navigation)
(require 'ecc-buffer/ecc-buffer-timestamp)
(require 'ecc-buffer/ecc-buffer)

;; Require state modules
(require 'ecc-state/ecc-state)
(require 'ecc-state/ecc-state-detect)

;; Require other modules
(require 'ecc-variables)
(require 'ecc-auto)
(require 'ecc-send)
(require 'ecc-update-mode-line)

;; Variable mapping - define legacy variables and map them to new ones
(defvar ecc-auto-timer nil)
(defvar ecc-active-buffer nil)
(defvar ecc-buffers nil)

;; Fix naming inconsistencies
(defvar ecc-timer nil)

;; Map variable names for backward compatibility
(with-no-warnings
  ;; Map ecc-auto-timer to ecc-timer
  (defvaralias 'ecc-auto-timer 'ecc-timer)
  
  ;; Map active buffer names
  (defvaralias 'ecc-active-buffer 'ecc-buffer-current-buffer)
  (defvaralias 'ecc-buffer-current-active-buffer 'ecc-buffer-current-buffer)
  
  ;; Map buffer registry names
  (defvaralias 'ecc-buffers 'ecc-buffer-registered-buffers-alist))

;; Buffer registry functions
(defun ecc-buffer-registry-cleanup-buffers ()
  "Compatibility function for ecc-buffer-cleanup-buffer-registry."
  (when (fboundp 'ecc-buffer-cleanup-buffer-registry)
    (ecc-buffer-cleanup-buffer-registry)))

(defun ecc-buffer-get-all-buffers ()
  "Compatibility function for ecc-buffer-get-registered-buffers."
  (when (fboundp 'ecc-buffer-get-registered-buffers)
    (ecc-buffer-get-registered-buffers)))

;; Buffer current functions
(defun ecc-buffer-current-get-buffer ()
  "Compatibility function for getting current active buffer."
  ecc-buffer-current-buffer)

;; Buffer renaming
(defun ecc-buffer-rename-buffer (active)
  "Compatibility function for buffer renaming."
  (when (fboundp 'ecc-update-mode-line) 
    (ecc-update-mode-line active)))

;; Auto functions
(defun --ecc-auto-check-and-restart ()
  "Compatibility function for ecc-auto-check-and-restart."
  (when (fboundp 'ecc-auto-check-and-restart)
    (ecc-auto-check-and-restart)))

;; Buffer registration
(defun --ecc-buffer-register-buffer (buffer)
  "Compatibility function for buffer registration."
  (when (fboundp 'ecc-buffer-register-buffer)
    (ecc-buffer-register-buffer buffer)))

(defun ecc-buffer-register-as-active (buffer)
  "Compatibility function for buffer registration as active."
  (setq ecc-buffer-current-buffer buffer)
  (setq ecc-active-buffer buffer)
  (setq ecc-buffer-current-active-buffer buffer))

;; Activate these variables that aren't declared in the current code
(defvar ecc-buffer-registered-buffers (make-hash-table))
(defvar --ecc-send-string-calls nil)

;; Make ecc-buffer-registered-buffers point to ecc-buffer-registered-buffers-alist
(setq ecc-buffer-registered-buffers ecc-buffer-registered-buffers-alist)

;; Create stub functions for vterm
(unless (fboundp 'vterm-send-key)
  (defun vterm-send-key (key &optional times)
    "Stub function for vterm-send-key when vterm is not available.
Simulates sending KEY TIMES times to a vterm buffer."
    (ignore key times)))

(unless (fboundp 'vterm-send-return)
  (defun vterm-send-return ()
    "Stub function for vterm-send-return when vterm is not available."
    nil))

(unless (fboundp 'vterm-send-string)
  (defun vterm-send-string (string &optional paste-p)
    "Stub function for vterm-send-string when vterm is not available."
    (ignore string paste-p)))

(unless (fboundp 'vterm-copy-mode)
  (defun vterm-copy-mode (arg)
    "Stub function for vterm-copy-mode when vterm is not available."
    (ignore arg)))

(provide 'fix-names)