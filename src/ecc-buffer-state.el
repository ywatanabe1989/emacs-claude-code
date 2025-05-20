;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 21:00:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-buffer-state.el

;;; Commentary:
;;; Enhanced buffer-local state management for Claude interactions.
;;; This module provides robust state containers for managing Claude states
;;; on a per-buffer basis, allowing multiple Claude instances to operate
;;; independently with full state isolation.

(require 'ecc-variables)
(require 'ecc-buffer-local)

;;; Code:

;; Buffer-local state container and accessors

(defvar-local ecc-buffer-state-container (make-hash-table :test 'eq)
  "Buffer-local hash table for state information.
Each key is a state property and value is the associated data.")

(defun ecc-buffer-state-get (key &optional buffer default)
  "Get buffer-local state value for KEY in BUFFER.
If BUFFER is nil, use current buffer.
Return DEFAULT if key is not found."
  (with-current-buffer (or buffer (current-buffer))
    (gethash key ecc-buffer-state-container default)))

(defun ecc-buffer-state-set (key value &optional buffer)
  "Set buffer-local state KEY to VALUE in BUFFER.
If BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (puthash key value ecc-buffer-state-container)))

(defun ecc-buffer-state-has-key-p (key &optional buffer)
  "Return non-nil if KEY exists in BUFFER state container.
If BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (let ((default (make-symbol "ecc-buffer-state-not-found")))
      (not (eq (gethash key ecc-buffer-state-container default) default)))))

(defun ecc-buffer-state-remove (key &optional buffer)
  "Remove KEY from BUFFER state container.
If BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (remhash key ecc-buffer-state-container)))

(defun ecc-buffer-state-clear (&optional buffer)
  "Clear all state values in BUFFER.
If BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (clrhash ecc-buffer-state-container)))

;; Enhanced state tracking functions 

(defun ecc-buffer-state-update-prompt (state &optional buffer)
  "Update buffer-local prompt STATE for BUFFER.
Also updates the timestamp and related state information.
Returns the state that was set."
  (with-current-buffer (or buffer (current-buffer))
    ;; Store current prompt state
    (ecc-buffer-state-set 'prompt-state state)
    ;; Store detection timestamp
    (ecc-buffer-state-set 'last-detection-time (float-time))
    ;; Update the last time this specific state was seen
    (ecc-buffer-state-set 
     (intern (format "last-%s-time" (symbol-name state)))
     (float-time))
    ;; For tracking active state being processed
    (ecc-buffer-state-set 'active-state state)
    ;; Return state that was set
    state))

(defun ecc-buffer-state-get-prompt (&optional buffer)
  "Get current buffer-local prompt state for BUFFER.
If BUFFER is nil, use current buffer.
Returns one of: :y/y/n, :y/n, :waiting, :initial-waiting, or nil."
  (with-current-buffer (or buffer (current-buffer))
    (ecc-buffer-state-get 'prompt-state nil)))

(defun ecc-buffer-state-clear-active (&optional buffer)
  "Clear the active state being processed for BUFFER.
If BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (ecc-buffer-state-set 'active-state nil)))

(defun ecc-buffer-state-get-active (&optional buffer)
  "Get the active state being processed for BUFFER.
If BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (ecc-buffer-state-get 'active-state nil)))

;; Advanced state management

(defun ecc-buffer-state-throttled-p (state &optional buffer)
  "Check if responses for STATE should be throttled in BUFFER.
Uses buffer-local throttling state to determine if we've recently
responded to this state and should wait."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((now (float-time))
           (state-time-key (intern (format "last-%s-time" (symbol-name state))))
           (last-time (ecc-buffer-state-get state-time-key nil 0.0))
           (elapsed (- now last-time))
           (throttle-time (if (boundp 'ecc-auto-response-throttle-time)
                              ecc-auto-response-throttle-time
                            5.0)))
      (or
       ;; Check if this is a duplicate of the currently active state
       (eq state (ecc-buffer-state-get-active))
       ;; Check if we need to throttle based on time
       (< elapsed throttle-time)))))

(defun ecc-buffer-state-export-standard (&optional buffer)
  "Export state to standard buffer-local variables for compatibility.
If BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    ;; Update traditional ecc-buffer-* variables for compatibility
    (when (boundp 'ecc-buffer-state)
      (setq-local ecc-buffer-state (ecc-buffer-state-get 'prompt-state)))
    (when (boundp 'ecc-buffer-last-state-time)
      (setq-local ecc-buffer-last-state-time 
                 (ecc-buffer-state-get 'last-detection-time 0.0)))
    (when (boundp 'ecc-buffer-active-state)
      (setq-local ecc-buffer-active-state 
                 (ecc-buffer-state-get 'active-state)))))

(defun ecc-buffer-state-import-standard (&optional buffer)
  "Import state from standard buffer-local variables for compatibility.
If BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    ;; Import from traditional ecc-buffer-* variables
    (when (and (boundp 'ecc-buffer-state) ecc-buffer-state)
      (ecc-buffer-state-set 'prompt-state ecc-buffer-state))
    (when (and (boundp 'ecc-buffer-last-state-time) ecc-buffer-last-state-time)
      (ecc-buffer-state-set 'last-detection-time ecc-buffer-last-state-time))
    (when (and (boundp 'ecc-buffer-active-state) ecc-buffer-active-state)
      (ecc-buffer-state-set 'active-state ecc-buffer-active-state))))

;; Buffer state initialization

(defun ecc-buffer-state-init (&optional buffer)
  "Initialize the buffer state container for BUFFER.
If BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    ;; Clear any existing state
    (ecc-buffer-state-clear)
    ;; Initialize standard state values
    (ecc-buffer-state-set 'prompt-state nil)
    (ecc-buffer-state-set 'last-detection-time 0.0)
    (ecc-buffer-state-set 'active-state nil)
    ;; Initialize timestamps for each state
    (ecc-buffer-state-set 'last-:y/n-time 0.0)
    (ecc-buffer-state-set 'last-:y/y/n-time 0.0)
    (ecc-buffer-state-set 'last-:waiting-time 0.0)
    (ecc-buffer-state-set 'last-:initial-waiting-time 0.0)
    ;; Import existing state if available (for migration)
    (ecc-buffer-state-import-standard)))

;; Detect and update in one operation

(defun ecc-buffer-state-detect-and-update (&optional buffer)
  "Detect Claude state and update buffer-local state for BUFFER.
If BUFFER is nil, use current buffer.
Returns the detected state or nil."
  (with-current-buffer (or buffer (current-buffer))
    (when-let ((state (ecc-detect-state)))
      (ecc-buffer-state-update-prompt state)
      (ecc-buffer-state-export-standard)
      state)))

;; Buffer state predicates for specific states

(defun ecc-buffer-state-waiting-p (&optional buffer)
  "Return non-nil if BUFFER has a waiting prompt state.
If BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (eq (ecc-buffer-state-get-prompt) :waiting)))

(defun ecc-buffer-state-y/n-p (&optional buffer)
  "Return non-nil if BUFFER has a Y/N prompt state.
If BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (eq (ecc-buffer-state-get-prompt) :y/n)))

(defun ecc-buffer-state-y/y/n-p (&optional buffer)
  "Return non-nil if BUFFER has a Y/Y/N prompt state.
If BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (eq (ecc-buffer-state-get-prompt) :y/y/n)))

(defun ecc-buffer-state-initial-waiting-p (&optional buffer)
  "Return non-nil if BUFFER has an initial waiting prompt state.
If BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (eq (ecc-buffer-state-get-prompt) :initial-waiting)))

;; Debugging helpers

(defun ecc-buffer-state-debug-info (&optional buffer)
  "Return debug info about buffer state for BUFFER.
If BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (let ((now (float-time)))
      (format "Buffer: %s
Current State: %s
Last Detection: %.1f seconds ago
Active State: %s
Last Y/N: %.1f seconds ago
Last Y/Y/N: %.1f seconds ago
Last Waiting: %.1f seconds ago
Last Initial: %.1f seconds ago"
              (buffer-name)
              (ecc-buffer-state-get 'prompt-state)
              (- now (ecc-buffer-state-get 'last-detection-time 0.0))
              (ecc-buffer-state-get 'active-state)
              (- now (ecc-buffer-state-get 'last-:y/n-time 0.0))
              (- now (ecc-buffer-state-get 'last-:y/y/n-time 0.0))
              (- now (ecc-buffer-state-get 'last-:waiting-time 0.0))
              (- now (ecc-buffer-state-get 'last-:initial-waiting-time 0.0))))))

;; Public API aliases for backwards compatibility

;;;###autoload
(defalias 'ecc-buffer-state-update 'ecc-buffer-state-update-prompt
  "Compatibility alias for updating buffer state.")

;;;###autoload
(defalias 'ecc-buffer-state-get 'ecc-buffer-state-get-prompt
  "Compatibility alias for getting buffer state.")

;;;###autoload
(defalias 'ecc-buffer-state-detect 'ecc-buffer-state-detect-and-update
  "Compatibility alias for detecting and updating buffer state.")

(provide 'ecc-buffer-state)

;;; ecc-buffer-state.el ends here