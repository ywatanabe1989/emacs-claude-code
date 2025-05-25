;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-23 12:23:23>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-term-claude-auto.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; Commentary:
;;; Consolidated auto-response functionality for Claude prompts.
;;; This module provides a unified system for automatically responding to
;;; different Claude prompt types, eliminating duplicated code and improving
;;; maintainability.

(require 'ecc-variables)
(require 'ecc-term-claude-state)
(require 'vterm nil t)  ; Optional - don't fail if vterm not available

;; Forward declarations to prevent free variable warnings

(defvar ecc-term-claude-update-functions nil
  "List of functions to call when the Claude terminal buffer updates.")

(defvar ecc-auto-response-y/n "y"
  "Response to send for Y/N prompts.")

(defvar ecc-auto-response-y/y/n "y"
  "Response to send for Y/Y/N prompts.")

(defvar ecc-auto-response-waiting ""
  "Response to send for 'continue>' prompts.")

(defvar ecc-auto-response-initial-waiting ""
  "Response to send for initial waiting prompts.")

;;; Code:

;;;; Vterm compatibility wrappers

(defun ecc-term-claude--vterm-send-string (string)
  "Send STRING to vterm if available."
  (if (fboundp 'vterm-send-string)
      (vterm-send-string string)
    (ecc-debug-message "vterm not available, cannot send: %s" string)))

(defun ecc-term-claude--vterm-send-return ()
  "Send return to vterm if available."
  (if (fboundp 'vterm-send-return)
      (vterm-send-return)
    (ecc-debug-message "vterm not available, cannot send return")))

;;;; Auto-response configuration

(defgroup ecc-term-claude-auto nil
  "Auto-response settings for Claude terminal mode."
  :group 'ecc-term-claude
  :prefix "ecc-term-claude-auto-")

(defcustom ecc-term-claude-auto-mode nil
  "When non-nil, automatically respond to Claude prompts.
When enabled, the system will detect common Claude prompts like
yes/no questions and continue prompts, and automatically respond
to them based on the configured responses."
  :type 'boolean
  :group 'ecc-term-claude-auto)

(defcustom ecc-term-claude-auto-delay 1.0
  "Delay in seconds before sending an automatic response.
A value of 0 means no delay. Adding a small delay can make the
auto-response behavior feel less jarring to the user."
  :type 'number
  :group 'ecc-term-claude-auto)

(defcustom ecc-term-claude-auto-debug nil
  "When non-nil, log detailed information about auto-responses.
Enables verbose logging to help diagnose issues with the
auto-response system."
  :type 'boolean
  :group 'ecc-term-claude-auto)

;;;; Auto-response mapping

(defvar ecc-term-claude-auto-response-map
  '((:y/n . ecc-auto-response-y/n)
    (:y/y/n . ecc-auto-response-y/y/n)
    (:waiting . ecc-auto-response-waiting)
    (:initial-waiting . ecc-auto-response-initial-waiting))
  "Mapping from state symbols to auto-response variables.
Each entry maps a state symbol to the variable containing the
response text to send for that state.")

;;;; Core auto-response functions

(defun ecc-term-claude-auto-send (state)
  "Automatically respond based on STATE.
STATE should be one of: :y/y/n, :y/n, :waiting, :initial-waiting.

Sends the appropriate response configured for the given state.
Uses the mapping in `ecc-term-claude-auto-response-map` to determine
which response variable to use for each state.

Returns t if a response was sent, nil otherwise."
  ;; Validate state
  (unless (memq state (ecc-term-claude-state-symbols))
    (error "Invalid state: %s" state))

  (let*
      ((response-var
        (cdr (assq state ecc-term-claude-auto-response-map)))
       (response (and response-var (boundp response-var)
                      (symbol-value response-var)))
       (state-name (ecc-term-claude-state-name state)))

    ;; Log debug info if enabled
    (when ecc-term-claude-auto-debug
      (ecc-debug-message "Auto-response debug: state=%s, var=%s, value=%s"
               state response-var response))

    ;; Validate response
    (unless (and response-var (boundp response-var))
      (error "No response variable defined for state: %s" state))

    (unless response
      (error "Response variable %s has no value" response-var))

    ;; Send the response
    (if (> ecc-term-claude-auto-delay 0)
        ;; Delayed response
        (run-with-timer ecc-term-claude-auto-delay nil
                        (lambda ()
                          (when (buffer-live-p (current-buffer))
                            (ecc-term-claude--vterm-send-string response)
                            (ecc-term-claude--vterm-send-return)
                            (ecc-debug-message "Auto-responded to %s prompt: %s"
                                     state-name response))))
      ;; Immediate response
      (ecc-term-claude--vterm-send-string response)
      (ecc-term-claude--vterm-send-return)
      (ecc-debug-message "Auto-responded to %s prompt: %s" state-name response))
    t))

(defun ecc-term-claude-auto-send-accept ()
  "Automatically respond to Claude prompts in vterm mode.
Uses the unified state detection to identify the current prompt type
and sends the appropriate response if auto-mode is enabled.

This function is typically added to `ecc-term-claude-update-functions`
to automatically check for and respond to prompts after each update."
  (when ecc-term-claude-auto-mode
    (condition-case err
        (let ((state (ecc-term-claude-get-state)))
          (when state
            (ecc-term-claude-auto-send state)))
      (error
       (ecc-debug-message "Auto-response error: %s" (error-message-string err))
       nil))))

;;;; Convenience functions

(defun ecc-term-claude-toggle-auto-mode ()
  "Toggle automatic response to Claude prompts.
Enables or disables the auto-response mode that automatically
responds to Claude prompts based on their type."
  (interactive)
  (setq ecc-term-claude-auto-mode (not ecc-term-claude-auto-mode))
  (ecc-debug-message "Claude auto-mode %s"
           (if ecc-term-claude-auto-mode "enabled" "disabled"))

  ;; Set up hooks for auto-responses
  (if ecc-term-claude-auto-mode
      (add-to-list 'ecc-term-claude-update-functions
                   'ecc-term-claude-auto-send-accept)
    (setq ecc-term-claude-update-functions
          (remove 'ecc-term-claude-auto-send-accept
                  ecc-term-claude-update-functions))))

;;;; Backward compatibility functions

;; These functions provide backward compatibility with the old API
;; while leveraging the new unified implementation.

(defun ecc-term-claude-auto-send-y/n ()
  "Automatically respond with y to Y/N prompts."
  (ecc-term-claude-auto-send :y/n))

(defun ecc-term-claude-auto-send-y/y/n ()
  "Automatically respond with y to Y/Y/N prompts."
  (ecc-term-claude-auto-send :y/y/n))

(defun ecc-term-claude-auto-send-continue ()
  "Automatically respond to continue prompts."
  (ecc-term-claude-auto-send :waiting))

(defun ecc-term-claude-auto-send-initial-waiting ()
  "Automatically respond to initial waiting prompts."
  (ecc-term-claude-auto-send :initial-waiting))

;; Legacy alias

(defalias 'ecc-term-claude-auto-mode-toggle
  'ecc-term-claude-toggle-auto-mode)

;;; ecc-term-claude-auto.el ends here


(provide 'ecc-term-claude-auto)

(when
    (not load-file-name)
  (ecc-debug-message "ecc-term-claude-auto.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))