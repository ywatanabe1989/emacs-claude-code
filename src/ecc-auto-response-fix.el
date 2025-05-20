;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 10:21:14>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-auto-response-fix.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; Commentary:
;;; Fixes and improvements for the auto-response system.

(require 'ecc-variables)
(require 'ecc-auto-response)

;; Customization group for auto-response improvements

(defgroup ecc-auto-response-improvements nil
  "Improvements for Claude auto-response system."
  :group 'ecc
  :prefix "ecc-auto-response-")

(defcustom ecc-auto-response-throttle-time 5.0
  "Minimum seconds between auto-responses to the same state.
Prevents rapid consecutive auto-responses to waiting prompts."
  :type 'float
  :group 'ecc-auto-response-improvements)

(defcustom ecc-auto-response-esc-disables t
  "Whether pressing ESC disables auto-response mode."
  :type 'boolean
  :group 'ecc-auto-response-improvements)

;; Track last responses to prevent consecutive firing

(defvar ecc-auto-response-last-time-alist
  '((:y/n . 0.0)
    (:y/y/n . 0.0)
    (:waiting . 0.0)
    (:initial-waiting . 0.0))
  "Alist tracking last time each type of response was sent.")

(defvar ecc-auto-response-active-state nil
  "The currently active Claude prompt state being processed.
Used to prevent duplicate responses to the same prompt.")

;; Fix for consecutive auto-responses

(defun ecc-auto-response-throttled-p (state)
  "Check if auto-response for STATE should be throttled.
Returns t if we responded to this state recently and should wait."
  (let* ((now (float-time))
         (last-time
          (alist-get state ecc-auto-response-last-time-alist 0.0))
         (elapsed (- now last-time)))
    (or
     ;; Check if this is a duplicate of the currently active state
     (eq state ecc-auto-response-active-state)
     ;; Check if we need to throttle based on time
     (< elapsed ecc-auto-response-throttle-time))))

(defun ecc-auto-response-update-time (state)
  "Update the last response time for STATE."
  (setf (alist-get state ecc-auto-response-last-time-alist)
        (float-time)))

;; Advise the auto-response functions to add throttling

(defun ecc-auto-response-send-advised
    (orig-fun buffer &optional state)
  "Advice around `ecc-auto-response-send' to add throttling.
ORIG-FUN is the original function.
BUFFER and STATE are passed to the original function."
  (let ((detected-state (or state
                            (with-current-buffer buffer
                              (ecc-detect-simple-state)))))
    ;; Only proceed if not throttled
    (unless
        (and detected-state
             (ecc-auto-response-throttled-p detected-state))
      ;; Set active state to prevent duplicates during processing
      (let ((ecc-auto-response-active-state detected-state))
        ;; Update the timestamp for this state
        (when detected-state
          (ecc-auto-response-update-time detected-state))
        ;; Call original function
        (funcall orig-fun buffer state)))))

;; Apply the advice to the auto-response function
(advice-add 'ecc-auto-response-send :around
            #'ecc-auto-response-send-advised)

;; Add similar throttling to the check-and-respond function

(defun ecc-check-and-respond-advised ()
  "Override implementation of `ecc-check-and-respond' with throttling.
Replaces the original function completely."
  (when (and (boundp 'ecc-buffer-auto-response-enabled)
             ecc-buffer-auto-response-enabled
             (boundp 'ecc-buffer-current-buffer)
             (buffer-live-p ecc-buffer-current-buffer))
    (with-current-buffer ecc-buffer-current-buffer
      ;; First check if we should notify about the prompt itself
      (when (boundp 'ecc-auto-notify-on-claude-prompt)
        (let ((state (ecc-detect-simple-state)))
          (when (fboundp 'ecc-auto-notify-check-state)
            (ecc-auto-notify-check-state state))))

      ;; Then handle auto-response, but only if not throttled
      (let ((state (ecc-detect-simple-state)))
        (when (and state (not (ecc-auto-response-throttled-p state)))
          ;; Set active state to prevent duplicates during processing
          (let ((ecc-auto-response-active-state state))
            ;; Update the timestamp for this state
            (ecc-auto-response-update-time state)
            ;; Process the response
            (cond
             ((eq state :y/y/n)
              (ecc-auto--send-response ecc-buffer-current-buffer
                                       ecc-auto-response-y/y/n
                                       "Y/Y/N"))
             ((eq state :y/n)
              (ecc-auto--send-response ecc-buffer-current-buffer
                                       ecc-auto-response-y/n
                                       "Y/N"))
             ((eq state :initial-waiting)
              (ecc-auto--send-response ecc-buffer-current-buffer
                                       ecc-auto-response-initial-waiting
                                       "Initial-Waiting"))
             ((eq state :waiting)
              (ecc-auto--send-response ecc-buffer-current-buffer
                                       ecc-auto-response-waiting
                                       "Continue")))))))))

;; Replace the original function instead of adding advice
(advice-add 'ecc-check-and-respond :override
            #'ecc-check-and-respond-advised)

;; ESC key binding to disable auto-response

(defun ecc-esc-disable-auto-response ()
  "Disable auto-response when ESC is pressed."
  (interactive)
  (when (and ecc-auto-response-esc-disables
             (boundp 'ecc-buffer-auto-response-enabled)
             ecc-buffer-auto-response-enabled)
    (vterm-send-escape)
    (ecc-stop-auto-response)
    (message "Auto-response disabled by ESC"))
  ;; Call the regular ESC function
  (call-interactively 'keyboard-escape-quit))

;; Setup ESC key to disable auto-response in vterm mode

(defun ecc-setup-esc-key-in-vterm ()
  "Setup ESC key to disable auto-response in vterm mode."
  (when (boundp 'vterm-mode-map)
    ;; Use both [escape] and "\e" key sequences to ensure compatibility
    (define-key vterm-mode-map [escape] 'ecc-esc-disable-auto-response)
    (define-key vterm-mode-map "\e" 'ecc-esc-disable-auto-response)
    ;; Also set up ESC key for ecc-term-claude-mode if it exists
    (when (boundp 'ecc-term-claude-mode-map)
      (define-key ecc-term-claude-mode-map [escape] 'ecc-esc-disable-auto-response)
      (define-key ecc-term-claude-mode-map "\e" 'ecc-esc-disable-auto-response))
    (message "ESC key binding for auto-response disable configured")))

;; Make sure all required variables are defined to prevent errors

(defvar ecc-auto-notify-completions t
  "Whether to notify when auto-response completes.")

;; Ensure we set up ESC key after vterm is loaded
(with-eval-after-load 'vterm
  (ecc-setup-esc-key-in-vterm))

;; Also set up ESC key after our mode is loaded
(with-eval-after-load 'ecc-term-claude-mode
  (ecc-setup-esc-key-in-vterm))

;; Reset state when auto-response is stopped

(defun ecc-auto-response-stop-advised (orig-fun)
  "Advice around `ecc-stop-auto-response` to reset state.
ORIG-FUN is the original function."
  (funcall orig-fun)
  ;; Reset all timestamps and active state
  (setq ecc-auto-response-last-time-alist
        '((:y/n . 0.0)
          (:y/y/n . 0.0)
          (:waiting . 0.0)
          (:initial-waiting . 0.0)))
  (setq ecc-auto-response-active-state nil))

(advice-add 'ecc-stop-auto-response :around
            #'ecc-auto-response-stop-advised)

;; Commands to adjust throttle time

(defun ecc-auto-response-set-throttle (seconds)
  "Set the auto-response throttle time to SECONDS."
  (interactive "nThrottle time in seconds: ")
  (setq ecc-auto-response-throttle-time (float seconds))
  (message "Auto-response throttle time set to %.1f seconds"
           ecc-auto-response-throttle-time))

(defun ecc-auto-response-toggle-esc-disable ()
  "Toggle whether ESC disables auto-response."
  (interactive)
  (setq ecc-auto-response-esc-disables
        (not ecc-auto-response-esc-disables))
  (message "ESC key %s auto-response"
           (if ecc-auto-response-esc-disables "will disable"
             "won't disable")))

;;; ecc-auto-response-fix.el ends here


(provide 'ecc-auto-response-fix)

(when
    (not load-file-name)
  (message "ecc-auto-response-fix.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))