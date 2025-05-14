;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-14 07:15:22>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-state/ecc-state-engine.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ecc-buffer-uid)
(require 'ecc-buffer-metadata)
(require 'ecc-state-variables)
(require 'ecc-state-detect-prompt)

;; ---- State engine core ----

(defvar ecc-state--buffer-cache (make-hash-table :test 'eq)
  "Cache of buffer state information. Maps buffer UIDs to states.")

(defun ecc-state-engine-get-state (buffer-or-uid)
  "Get the cached state for BUFFER-OR-UID.
Returns nil if no state has been cached."
  (let ((uid (if (bufferp buffer-or-uid)
                 (ecc-buffer-get-uid buffer-or-uid)
               buffer-or-uid)))
    (when uid
      (gethash uid ecc-state--buffer-cache nil))))

(defun ecc-state-engine-set-state (buffer-or-uid state)
  "Cache STATE for BUFFER-OR-UID.
Updates buffer metadata if using metadata system."
  (let ((uid (if (bufferp buffer-or-uid)
                 (ecc-buffer-get-uid buffer-or-uid)
               buffer-or-uid)))
    (when uid
      ;; Update cache
      (puthash uid state ecc-state--buffer-cache)
      
      ;; Update metadata if available
      (when (fboundp 'ecc-buffer-metadata-set)
        (ecc-buffer-metadata-set uid 'claude-state state))
      
      ;; Return the state
      state)))

(defun ecc-state-engine-detect-state (buffer-or-uid)
  "Detect the state of Claude in BUFFER-OR-UID.
Returns one of the following states:
- initial-waiting - Waiting for user to tell Claude to continue from initial prompt
- waiting - Waiting for user to tell Claude to continue 
- y/n - Waiting for yes/no response
- y/y/n - Waiting for Yes/yes/no response
- running - Currently running/processing
- ready - Ready for input
- nil - Unable to determine state"
  (let* ((buffer (cond
                 ((bufferp buffer-or-uid) buffer-or-uid)
                 ((stringp buffer-or-uid) (get-buffer buffer-or-uid))
                 (t (if (fboundp 'ecc-buffer-uid-to-buffer)
                        (ecc-buffer-uid-to-buffer buffer-or-uid)
                      nil))))
         (state nil))
    
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        ;; Try to detect various states
        (cond
         ;; Initial waiting state (highest priority)
         ((and (fboundp '--ecc-state-detect-prompt-initial-waiting)
               (--ecc-state-detect-prompt-initial-waiting))
          (setq state 'initial-waiting))
         
         ;; y/y/n prompt
         ((and (fboundp '--ecc-state-detect-prompt-y/y/n)
               (--ecc-state-detect-prompt-y/y/n))
          (setq state 'y/y/n))
         
         ;; y/n prompt
         ((and (fboundp '--ecc-state-detect-prompt-y/n)
               (--ecc-state-detect-prompt-y/n))
          (setq state 'y/n))
         
         ;; Waiting for continuation
         ((and (fboundp '--ecc-state-detect-prompt-waiting)
               (--ecc-state-detect-prompt-waiting))
          (setq state 'waiting))
         
         ;; Default to ready state if no other state detected
         (t
          (setq state 'ready))))
      
      ;; Cache the detected state
      (ecc-state-engine-set-state buffer-or-uid state))
    
    ;; Return the detected state
    state))

;; ---- State buffer caching ----

(defvar ecc-state--last-states (make-hash-table :test 'eq)
  "Last known states for each buffer UID.")

;; ---- Status change notification ----

(defun ecc-state-engine-notify-state-change (buffer-or-uid old-state new-state)
  "Notify about state change from OLD-STATE to NEW-STATE for BUFFER-OR-UID."
  (when (and old-state new-state (not (eq old-state new-state)))
    (run-hook-with-args 'ecc-state-change-hook buffer-or-uid old-state new-state)))

(defun ecc-state-engine-check-state-change (buffer-or-uid)
  "Check if state has changed for BUFFER-OR-UID and trigger notifications."
  (let* ((uid (if (bufferp buffer-or-uid)
                 (ecc-buffer-get-uid buffer-or-uid)
               buffer-or-uid))
         (old-state (gethash uid ecc-state--last-states nil))
         (new-state (ecc-state-engine-detect-state buffer-or-uid)))
    
    ;; If we have a state change, notify
    (when (and uid new-state (not (eq old-state new-state)))
      (ecc-state-engine-notify-state-change buffer-or-uid old-state new-state)
      (puthash uid new-state ecc-state--last-states))
    
    new-state))

;; ---- Compatibility functions ----

;; For backward compatibility with older function names
(defalias 'ecc-state-get 'ecc-state-engine-detect-state)
(defalias 'ecc-state-set 'ecc-state-engine-set-state)

(provide 'ecc-state-engine)

(when
    (not load-file-name)
  (message "ecc-state-engine.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))