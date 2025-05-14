;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 18:08:01>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-send/ecc-send-accept.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ecc-buffer)
(require 'ecc-state-detect)
(require 'ecc-state)

;; 2. Core automation functions
;; ----------------------------------------

(defun ecc-send-accept ()
  "Automatically respond to Claude prompts in active vterm."
  (interactive)
  (when (buffer-live-p ecc-buffer-current-active-buffer)
    (with-current-buffer ecc-buffer-current-active-buffer
      (vterm-clear)
      (cond
       ((--ecc-state-y/y/n-p)
        (--ecc-auto-send-2-y/y/n))
       ((--ecc-state-y/n-p)
        (--ecc-auto-send-1-y/n))
       ((--ecc-state-waiting-p)
        (--ecc-auto-send-string-on-y/y/n))
       ((--ecc-state-initial-waiting-p)
        (--ecc-auto-send-string-on-y/y/n))))))

(defun --ecc-auto-send-1-y/n ()
  "Automatically respond with '1' to Claude prompts in vterm.
Returns the string that was sent."
  (interactive)
  (let ((response "1"))
    (--ecc-send-by-state response :y/n)
    (when (fboundp 'ecc-auto-notify-completion)
      (ecc-auto-notify-completion "Y/N"))
    response))

(defun --ecc-auto-send-2-y/y/n ()
  "Automatically respond with '2' to Claude prompts in vterm.
Returns the string that was sent."
  (interactive)
  (let ((response "2"))
    (--ecc-send-by-state response :y/y/n)
    (when (fboundp 'ecc-auto-notify-completion)
      (ecc-auto-notify-completion "Y/Y/N"))
    response))

(defun --ecc-auto-send-3 ()
  "Automatically respond with '3' to Claude prompts in vterm.
Returns the string that was sent."
  (interactive)
  (let ((response "3"))
    (--ecc-send-by-state response :y/y/n)
    (when (fboundp 'ecc-auto-notify-completion)
      (ecc-auto-notify-completion "Y/Y/N option 3"))
    response))

(defun --ecc-auto-send-command-on-y/y/n ()
  "Automatically respond with continue to Claude waiting prompts.
Returns the string that was sent."
  (interactive)
  (--ecc-send-by-state
   ecc-prompt-to-send-on-waiting
   (lambda ()
     (or (--ecc-state-waiting-p)
         (--ecc-state-initial-waiting-p))))
  (when (fboundp 'ecc-auto-notify-completion)
    (ecc-auto-notify-completion "waiting/continue"))
  response)

(defun --ecc-auto-send-template-on-y/y/n (template-text)
  "Send custom TEMPLATE-TEXT to Claude when in the y/y/n state.
This allows sending natural language responses instead of just number options."
  (interactive "sEnter your response: ")
  (when (buffer-live-p ecc-active-buffer)
    (with-current-buffer ecc-active-buffer
      (when (--ecc-state-y/y/n-p)
        (sit-for 0.5)
        (vterm-send-string template-text)
        (vterm-send-return)
        (vterm-copy-mode -1)
        (sit-for 0.5)
        (message "[ecc-send] Template Response: %s" template-text)))))

;; (defun --ecc-auto-send-continue ()
;;   "Automatically respond with continue to Claude waiting prompts."
;;   (interactive)
;;   (--ecc-auto-send-by-state
;;    "continue"
;;    (lambda ()
;;      (or (--ecc-state-waiting-p)
;;          (--ecc-state-initial-waiting-p)))))

;; ;; Define ecc-buffer-current-active-buffer if not already defined
;; ;; This is for backward compatibility with tests
;; (defvar ecc-buffer-current-active-buffer nil
;;   "Legacy variable for current active buffer used by older tests.")

;; (defun --ecc-auto-send-by-state (response state-check-fn)
;;   "Send RESPONSE to Claude when STATE-CHECK-FN returns non-nil.
;; STATE-CHECK-FN is a function that should return non-nil when
;; we want to send the response."
;;   ;; Handle current buffer first
;;   (when (buffer-live-p ecc-buffer-current-buffer)
;;     (with-current-buffer ecc-buffer-current-buffer
;;       (when (funcall state-check-fn)
;;         (sit-for 0.5)
;;         (vterm-send-string response)
;;         (vterm-send-return)
;;         (vterm-copy-mode -1)
;;         (sit-for 0.5)
;;         (message "[ecc-send] Auto Response: %s" response))))

;;   ;; For backward compatibility - if only the legacy variable is set
;;   (when (and (not ecc-buffer-current-buffer)
;;              (boundp 'ecc-buffer-current-active-buffer)
;;              (buffer-live-p ecc-buffer-current-active-buffer))
;;     (with-current-buffer ecc-buffer-current-active-buffer
;;       (when (funcall state-check-fn)
;;         (sit-for 0.5)
;;         (vterm-send-string response)
;;         (vterm-send-return)
;;         (vterm-copy-mode -1)
;;         (sit-for 0.5)
;;         (message "[ecc-send] Auto Response: %s" response)))))

;; (defun --ecc-auto-send-1-y/n ()
;;   "Automatically send '1' response to Claude y/n prompts."
;;   (interactive)
;;   (--ecc-auto-send-by-state
;;    "1"
;;    (lambda () (--ecc-state-y/n-p)))
;;   ;; Send notification if configured
;;   ;; This must be unconditional for the test to work properly
;;   (ecc-auto-notify-completion "Y/N"))

;; (defun --ecc-auto-send-2-y/y/n ()
;;   "Automatically send '2' response to Claude y/y/n prompts."
;;   (interactive)
;;   (--ecc-auto-send-by-state
;;    "2"
;;    (lambda () (--ecc-state-y/y/n-p)))
;;   ;; Send notification if configured
;;   ;; This must be unconditional for the test to work properly
;;   (ecc-auto-notify-completion "Y/Y/N"))

;; (defun --ecc-auto-send-continue-on-y/y/n ()
;;   "Automatically send 'continue' response to Claude prompts."
;;   (interactive)
;;   (--ecc-auto-send-by-state
;;    "continue"
;;    (lambda () (or (--ecc-state-waiting-p)
;;                  (--ecc-state-initial-waiting-p))))
;;   ;; Send notification if configured
;;   ;; This must be unconditional for the test to work properly
;;   (ecc-auto-notify-completion "waiting/continue"))


(provide 'ecc-send-accept)

(when
    (not load-file-name)
  (message "ecc-send-accept.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))