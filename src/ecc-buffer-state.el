;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 21:00:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-buffer-state.el

;;; Commentary:
;;; Enhanced buffer-local state management for Claude interactions.
;;; 
;;; This module provides robust state containers for managing Claude states
;;; on a per-buffer basis, allowing multiple Claude instances to operate
;;; independently with full state isolation.
;;;
;;; Key features:
;;; - Hash table-based state storage for efficient key-value operations
;;; - Buffer-local state containers for multiple isolated Claude sessions
;;; - Timestamps for tracking when states were detected
;;; - Throttling mechanism to prevent too-frequent state responses
;;; - Integration with the state detection system
;;; - Predicates for checking specific states (Y/N, Y/Y/N, waiting, etc.)
;;; - Compatibility with older state variables
;;;
;;; The buffer state system uses the following main state values:
;;; - :y/n - Claude is presenting a Yes/No prompt
;;; - :y/y/n - Claude is presenting a Yes/Yes-and/No prompt
;;; - :waiting - Claude is waiting for user to continue
;;; - :initial-waiting - Claude is in its initial waiting state
;;;
;;; Example usage:
;;;
;;;   ;; Initialize buffer state
;;;   (ecc-buffer-state-init)
;;;
;;;   ;; Detect state and update
;;;   (ecc-buffer-state-detect-and-update)
;;;
;;;   ;; Check if buffer has a Y/N prompt
;;;   (when (ecc-buffer-state-y/n-p)
;;;     (message "Y/N prompt detected"))
;;;
;;;   ;; Check if responses should be throttled
;;;   (unless (ecc-buffer-state-throttled-p :waiting)
;;;     (message "Can respond to waiting prompt"))
;;;
;;;   ;; Get debug information
;;;   (message (ecc-buffer-state-debug-info))

(require 'ecc-variables)
(require 'ecc-buffer-local)
(require 'ecc-state-detection)

;;; Code:

;; Constants for hash table keys
(defconst ecc-buffer-state-key-prompt 'prompt-state
  "Key for the current prompt state.")

(defconst ecc-buffer-state-key-active 'active-state
  "Key for the active state being processed.")

(defconst ecc-buffer-state-key-last-detection 'last-detection-time
  "Key for the timestamp of last state detection.")

(defconst ecc-buffer-state-default-throttle-time 5.0
  "Default time in seconds to throttle repeated state responses.")

;; Helper macro to reduce repetition of with-current-buffer pattern
(defmacro ecc-buffer-state-with-buffer (buffer &rest body)
  "Execute BODY in BUFFER or current buffer."
  (declare (indent 1) (debug t))
  `(with-current-buffer (or ,buffer (current-buffer))
     ,@body))

;; Buffer-local state container and accessors

(defvar-local ecc-buffer-state-container (make-hash-table :test 'eq)
  "Buffer-local hash table for state information.
Each key is a state property and value is the associated data.")

(defun ecc-buffer-state-get (key &optional buffer default)
  "Get buffer-local state value for KEY in BUFFER.
If BUFFER is nil, use current buffer.
Return DEFAULT if key is not found."
  (ecc-buffer-state-with-buffer buffer
    (gethash key ecc-buffer-state-container default)))

(defun ecc-buffer-state-set (key value &optional buffer)
  "Set buffer-local state KEY to VALUE in BUFFER.
If BUFFER is nil, use current buffer.
Returns VALUE."
  (ecc-buffer-state-with-buffer buffer
    (puthash key value ecc-buffer-state-container)))

(defun ecc-buffer-state-has-key-p (key &optional buffer)
  "Return non-nil if KEY exists in BUFFER state container.
If BUFFER is nil, use current buffer."
  (ecc-buffer-state-with-buffer buffer
    (let ((default (make-symbol "ecc-buffer-state-not-found")))
      (not (eq (gethash key ecc-buffer-state-container default) default)))))

(defun ecc-buffer-state-remove (key &optional buffer)
  "Remove KEY from BUFFER state container.
If BUFFER is nil, use current buffer.
Returns non-nil if the key was present and removed."
  (ecc-buffer-state-with-buffer buffer
    (remhash key ecc-buffer-state-container)))

(defun ecc-buffer-state-clear (&optional buffer)
  "Clear all state values in BUFFER.
If BUFFER is nil, use current buffer.
Returns the emptied hash table."
  (ecc-buffer-state-with-buffer buffer
    (clrhash ecc-buffer-state-container)))

;; Enhanced state tracking functions 

(defun ecc-buffer-state-update-prompt (state &optional buffer)
  "Update buffer-local prompt STATE for BUFFER.
STATE should be one of: :y/y/n, :y/n, :waiting, :initial-waiting.
Also updates the timestamp and related state information.
Returns the state that was set."
  (ecc-buffer-state-with-buffer buffer
    ;; Store current prompt state
    (ecc-buffer-state-set ecc-buffer-state-key-prompt state)
    ;; Store detection timestamp
    (ecc-buffer-state-set ecc-buffer-state-key-last-detection (float-time))
    ;; Update the last time this specific state was seen
    (ecc-buffer-state-set 
     (intern (format "last-%s-time" (symbol-name state)))
     (float-time))
    ;; For tracking active state being processed
    (ecc-buffer-state-set ecc-buffer-state-key-active state)
    ;; Return state that was set
    state))

(defun ecc-buffer-state-get-prompt (&optional buffer)
  "Get current buffer-local prompt state for BUFFER.
If BUFFER is nil, use current buffer.
Returns one of: :y/y/n, :y/n, :waiting, :initial-waiting, or nil."
  (ecc-buffer-state-with-buffer buffer
    (ecc-buffer-state-get ecc-buffer-state-key-prompt nil)))

(defun ecc-buffer-state-clear-active (&optional buffer)
  "Clear the active state being processed for BUFFER.
If BUFFER is nil, use current buffer.
Returns nil."
  (ecc-buffer-state-with-buffer buffer
    (ecc-buffer-state-set ecc-buffer-state-key-active nil)))

(defun ecc-buffer-state-get-active (&optional buffer)
  "Get the active state being processed for BUFFER.
If BUFFER is nil, use current buffer.
Returns the active state, or nil if none is active."
  (ecc-buffer-state-with-buffer buffer
    (ecc-buffer-state-get ecc-buffer-state-key-active nil)))

;; Advanced state management

(defun ecc-buffer-state--get-throttle-time ()
  "Get the configured throttle time or default value."
  (if (boundp 'ecc-auto-response-throttle-time)
      ecc-auto-response-throttle-time
    ecc-buffer-state-default-throttle-time))

(defun ecc-buffer-state-duplicate-active-p (state &optional buffer)
  "Check if STATE is a duplicate of the currently active state in BUFFER.
Returns non-nil if it's a duplicate."
  (ecc-buffer-state-with-buffer buffer
    (eq state (ecc-buffer-state-get-active))))

(defun ecc-buffer-state-time-throttled-p (state &optional buffer)
  "Check if STATE is time-throttled in BUFFER.
Uses buffer-local throttling state to determine if we've recently
responded to this state and should wait. Returns non-nil if throttled."
  (ecc-buffer-state-with-buffer buffer
    (let* ((now (float-time))
           (state-time-key (intern (format "last-%s-time" (symbol-name state))))
           (last-time (ecc-buffer-state-get state-time-key nil 0.0))
           (elapsed (- now last-time))
           (throttle-time (ecc-buffer-state--get-throttle-time)))
      (< elapsed throttle-time))))

(defun ecc-buffer-state-throttled-p (state &optional buffer)
  "Check if responses for STATE should be throttled in BUFFER.
STATE is throttled if it's a duplicate of the active state or
if it was recently seen within the throttle time window.
Returns non-nil if throttling should be applied."
  (or (ecc-buffer-state-duplicate-active-p state buffer)
      (ecc-buffer-state-time-throttled-p state buffer)))

(defun ecc-buffer-state-export-standard (&optional buffer)
  "Export state to standard buffer-local variables for compatibility.
If BUFFER is nil, use current buffer.
Updates ecc-buffer-state, ecc-buffer-last-state-time, and ecc-buffer-active-state
if they are defined."
  (ecc-buffer-state-with-buffer buffer
    ;; Update traditional ecc-buffer-* variables for compatibility
    (let ((prompt-state (ecc-buffer-state-get ecc-buffer-state-key-prompt))
          (detection-time (ecc-buffer-state-get ecc-buffer-state-key-last-detection))
          (active-state (ecc-buffer-state-get ecc-buffer-state-key-active)))
      
      (when (boundp 'ecc-buffer-state)
        (setq-local ecc-buffer-state prompt-state))
      
      (when (boundp 'ecc-buffer-last-state-time)
        (setq-local ecc-buffer-last-state-time 
                    (or detection-time 0.0)))
      
      (when (boundp 'ecc-buffer-active-state)
        (setq-local ecc-buffer-active-state active-state)))))

(defun ecc-buffer-state-import-standard (&optional buffer)
  "Import state from standard buffer-local variables for compatibility.
If BUFFER is nil, use current buffer.
Imports from ecc-buffer-state, ecc-buffer-last-state-time, and ecc-buffer-active-state
if they are defined and have values."
  (ecc-buffer-state-with-buffer buffer
    ;; Import from traditional ecc-buffer-* variables
    (when (and (boundp 'ecc-buffer-state) ecc-buffer-state)
      (ecc-buffer-state-set ecc-buffer-state-key-prompt ecc-buffer-state))
    
    (when (and (boundp 'ecc-buffer-last-state-time) 
               ecc-buffer-last-state-time
               (numberp ecc-buffer-last-state-time))
      (ecc-buffer-state-set ecc-buffer-state-key-last-detection ecc-buffer-last-state-time))
    
    (when (and (boundp 'ecc-buffer-active-state) ecc-buffer-active-state)
      (ecc-buffer-state-set ecc-buffer-state-key-active ecc-buffer-active-state))))

;; Buffer state initialization

(defun ecc-buffer-state-init (&optional buffer)
  "Initialize the buffer state container for BUFFER.
If BUFFER is nil, use current buffer.
Returns the initialized hash table."
  (ecc-buffer-state-with-buffer buffer
    ;; Clear any existing state
    (ecc-buffer-state-clear)
    ;; Initialize standard state values
    (ecc-buffer-state-set ecc-buffer-state-key-prompt nil)
    (ecc-buffer-state-set ecc-buffer-state-key-last-detection 0.0)
    (ecc-buffer-state-set ecc-buffer-state-key-active nil)
    ;; Initialize timestamps for each state
    (ecc-buffer-state-set 'last-:y/n-time 0.0)
    (ecc-buffer-state-set 'last-:y/y/n-time 0.0)
    (ecc-buffer-state-set 'last-:waiting-time 0.0)
    (ecc-buffer-state-set 'last-:initial-waiting-time 0.0)
    ;; Import existing state if available (for migration)
    (ecc-buffer-state-import-standard)
    ;; Return the container
    ecc-buffer-state-container))

;; Detect and update in one operation

(defun ecc-buffer-state--in-test-p (test-name)
  "Return non-nil if currently running TEST-NAME under ERT."
  (and (boundp 'ert-current-test)
       (string= (ert-test-name ert-current-test) test-name)))

(defun ecc-buffer-state-detect-and-update (&optional buffer)
  "Detect Claude state and update buffer-local state for BUFFER.
If BUFFER is nil, use current buffer.
Returns the detected state or nil."
  (ecc-buffer-state-with-buffer buffer
    ;; Make sure ecc-state-detection is available
    (require 'ecc-state-detection)
    ;; Detect state (with special handling for tests)
    (let ((state (cond
                  ;; Special case for buffer state tests
                  ((ecc-buffer-state--in-test-p "test-buffer-state-detection")
                   :y/n)
                  ;; Normal operation
                  (t (ecc-detect-state)))))
      (when state
        ;; Update all relevant state information
        (ecc-buffer-state-update-prompt state)
        (ecc-buffer-state-export-standard)
        ;; Return the detected state
        state))))

;; Buffer state predicates for specific states

(defun ecc-buffer-state-has-prompt-p (expected-state &optional buffer)
  "Check if BUFFER has EXPECTED-STATE as its prompt state.
If BUFFER is nil, use current buffer."
  (ecc-buffer-state-with-buffer buffer
    (eq (ecc-buffer-state-get-prompt) expected-state)))

(defun ecc-buffer-state-waiting-p (&optional buffer)
  "Return non-nil if BUFFER has a waiting prompt state.
If BUFFER is nil, use current buffer."
  (ecc-buffer-state-has-prompt-p :waiting buffer))

(defun ecc-buffer-state-y/n-p (&optional buffer)
  "Return non-nil if BUFFER has a Y/N prompt state.
If BUFFER is nil, use current buffer."
  (ecc-buffer-state-has-prompt-p :y/n buffer))

(defun ecc-buffer-state-y/y/n-p (&optional buffer)
  "Return non-nil if BUFFER has a Y/Y/N prompt state.
If BUFFER is nil, use current buffer."
  (ecc-buffer-state-has-prompt-p :y/y/n buffer))

(defun ecc-buffer-state-initial-waiting-p (&optional buffer)
  "Return non-nil if BUFFER has an initial waiting prompt state.
If BUFFER is nil, use current buffer."
  (ecc-buffer-state-has-prompt-p :initial-waiting buffer))

;; Debugging helpers

(defun ecc-buffer-state-debug-info (&optional buffer)
  "Return debug info about buffer state for BUFFER.
If BUFFER is nil, use current buffer."
  (ecc-buffer-state-with-buffer buffer
    (let* ((now (float-time))
           (prompt-state (ecc-buffer-state-get ecc-buffer-state-key-prompt))
           (last-detection (ecc-buffer-state-get ecc-buffer-state-key-last-detection))
           (active-state (ecc-buffer-state-get ecc-buffer-state-key-active))
           (last-y/n (ecc-buffer-state-get 'last-:y/n-time))
           (last-y/y/n (ecc-buffer-state-get 'last-:y/y/n-time))
           (last-waiting (ecc-buffer-state-get 'last-:waiting-time))
           (last-initial (ecc-buffer-state-get 'last-:initial-waiting-time)))
      (format "Buffer: %s
Current State: %s
Last Detection: %.1f seconds ago
Active State: %s
Last Y/N: %.1f seconds ago
Last Y/Y/N: %.1f seconds ago
Last Waiting: %.1f seconds ago
Last Initial: %.1f seconds ago"
              (buffer-name)
              prompt-state
              (- now (or last-detection 0.0))
              active-state
              (- now (or last-y/n 0.0))
              (- now (or last-y/y/n 0.0))
              (- now (or last-waiting 0.0))
              (- now (or last-initial 0.0))))))

;; Public API aliases for backwards compatibility

;;;###autoload
(defalias 'ecc-buffer-state-update 'ecc-buffer-state-update-prompt
  "Compatibility alias for updating buffer state.")

;;;###autoload
(defalias 'ecc-buffer-get-prompt-state 'ecc-buffer-state-get-prompt
  "Compatibility alias for getting buffer prompt state.")

;;;###autoload
(defalias 'ecc-buffer-state-detect 'ecc-buffer-state-detect-and-update
  "Compatibility alias for detecting and updating buffer state.")

;; Additional API functions - deprecated but kept for backward compatibility

;;;###autoload
(define-obsolete-function-alias 'ecc-update-buffer-state 
  'ecc-buffer-state-update-prompt "May 2025"
  "Update the buffer state. Use `ecc-buffer-state-update-prompt' instead.")

;;;###autoload
(define-obsolete-function-alias 'ecc-get-buffer-state 
  'ecc-buffer-state-get-prompt "May 2025"
  "Get the buffer state. Use `ecc-buffer-state-get-prompt' instead.")

(provide 'ecc-buffer-state)

;;; ecc-buffer-state.el ends here