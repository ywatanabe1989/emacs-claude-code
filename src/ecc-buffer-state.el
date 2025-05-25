;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 23:15:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-buffer-state.el

;;; Commentary:
;;; Buffer-local state tracking for Claude detection and auto-response.
;;; 
;;; This module provides enhanced buffer-local state tracking to ensure more
;;; reliable and consistent state detection across different buffers. It builds
;;; on the centralized state detection infrastructure but adds buffer-specific
;;; tracking to prevent cross-buffer interference.
;;;
;;; Key features:
;;; - Buffer-local state tracking with history
;;; - Independent throttling for each buffer
;;; - Enhanced detection reliability through state persistence
;;; - Automatic cleanup for dead buffers
;;; - Clear API for buffer registration and state querying

(require 'ecc-variables)
(require 'ecc-state-detection)
(require 'ecc-auto-core)
(require 'ecc-debug-utils)

;;; Code:

;; Customization options

(defgroup ecc-buffer-state nil
  "Settings for buffer-local Claude state tracking."
  :group 'ecc
  :prefix "ecc-buffer-state-")

(defcustom ecc-buffer-state-history-size 10
  "Number of previous states to keep in buffer history."
  :type 'integer
  :group 'ecc-buffer-state)

(defcustom ecc-buffer-state-debug nil
  "Whether to show debug messages for buffer state tracking."
  :type 'boolean
  :group 'ecc-buffer-state)

;; Buffer-local variables

(defvar-local ecc-buffer-state-current nil
  "Current Claude prompt state in this buffer.")

(defvar-local ecc-buffer-state-prompt nil
  "Current Claude prompt state (alias for ecc-buffer-state-current for test compatibility).")

(defvar-local ecc-buffer-state-history nil
  "History of Claude prompt states in this buffer.")

(defvar-local ecc-buffer-state-last-update 0
  "Timestamp of last state update.")

(defvar-local ecc-buffer-state-enabled nil
  "Whether buffer-local state tracking is enabled.")

(defvar-local ecc-buffer-state-data nil
  "Plist storage for arbitrary buffer state data.")

(defvar-local ecc-buffer-auto-response-enabled nil
  "Whether auto-response is enabled for this buffer.")

;; Internal tracking variables

(defvar ecc-buffer-state-registered-buffers nil
  "List of buffers with active state tracking.")

;; Core functions

;;;###autoload
(defun ecc-buffer-state-init (&optional buffer)
  "Initialize buffer-local state tracking for BUFFER or current buffer.
This is an alias for `ecc-buffer-state-enable' that's used in tests."
  (interactive)
  (ecc-buffer-state-enable buffer))

;;;###autoload
(defun ecc-buffer-state-enable (&optional buffer)
  "Enable buffer-local state tracking for BUFFER or current buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (setq ecc-buffer-state-enabled t
          ecc-buffer-state-current nil
          ecc-buffer-state-prompt nil
          ecc-buffer-state-history nil
          ecc-buffer-state-last-update 0
          ecc-buffer-state-data nil)
    (add-to-list 'ecc-buffer-state-registered-buffers (current-buffer))
    (ecc-buffer-state-debug-message "Enabled buffer-local state tracking")))

;;;###autoload
(defun ecc-buffer-state-disable (&optional buffer)
  "Disable buffer-local state tracking for BUFFER or current buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (setq ecc-buffer-state-enabled nil)
    (setq ecc-buffer-state-registered-buffers
          (delq (current-buffer) ecc-buffer-state-registered-buffers))
    (ecc-buffer-state-debug-message "Disabled buffer-local state tracking")))

;;;###autoload
(defun ecc-buffer-state-update (&optional buffer force)
  "Update buffer-local state for BUFFER or current buffer.
When FORCE is non-nil, forces update even if throttled."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (when (or (not ecc-buffer-state-enabled)
              (not (ecc-buffer-state-should-update-p)))
      (ecc-buffer-state-debug-message "Skipping update due to throttling")
      (cl-return-from ecc-buffer-state-update nil))
    
    ;; Detect current state
    (let ((new-state (ecc-detect-state)))
      (when (or new-state force (not ecc-buffer-state-current))
        ;; Update timestamp and state
        (setq ecc-buffer-state-last-update (float-time))
        
        ;; Only record if we have a new state or being forced
        (when (or new-state force)
          ;; Update history
          (push new-state ecc-buffer-state-history)
          (when (> (length ecc-buffer-state-history) ecc-buffer-state-history-size)
            (setq ecc-buffer-state-history 
                  (seq-take ecc-buffer-state-history ecc-buffer-state-history-size)))
          
          ;; Update current state
          (setq ecc-buffer-state-current new-state)
          
          ;; Debug output
          (ecc-buffer-state-debug-message "Updated state to %s" 
                                          (ecc-state-get-name new-state))
          
          ;; Return the new state
          new-state)))))


;;;###autoload
(defun ecc-buffer-state-get-last-update (&optional buffer)
  "Get timestamp of last state update for BUFFER or current buffer.
Returns 0 if state tracking is not enabled."
  (with-current-buffer (or buffer (current-buffer))
    (if ecc-buffer-state-enabled
        ecc-buffer-state-last-update
      0)))

;;;###autoload
(defun ecc-buffer-state-update-prompt (state &optional buffer)
  "Update buffer's prompt STATE for BUFFER or current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (setq ecc-buffer-state-current state)
    (push state ecc-buffer-state-history)
    (when (> (length ecc-buffer-state-history) ecc-buffer-state-history-size)
      (setq ecc-buffer-state-history 
            (seq-take ecc-buffer-state-history ecc-buffer-state-history-size)))
    (setq ecc-buffer-state-last-update (float-time))
    state))

;;;###autoload
(defun ecc-buffer-state-export-standard (&optional buffer)
  "Export buffer's state to standard variables for BUFFER or current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (when ecc-buffer-state-current
      ;; Export to standard detection variables if they exist
      (when (boundp 'ecc-last-detected-state)
        (setq-default ecc-last-detected-state ecc-buffer-state-current))
      ;; Any other exports could be added here
      )
    ecc-buffer-state-current))
;;;###autoload
(defun ecc-buffer-state-set (key value &optional buffer)
  "Set buffer-local state KEY to VALUE for BUFFER or current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (unless ecc-buffer-state-enabled 
      (ecc-buffer-state-init))
    (put 'ecc-buffer-state-current key value)
    value))

;;;###autoload
(defun ecc-buffer-state-get (&optional key-or-buffer buffer-if-key-provided)
  "Get buffer-local state from BUFFER.
If KEY-OR-BUFFER is a symbol and not a buffer, treats it as a property key.
If KEY-OR-BUFFER is a buffer or nil, treats it as the buffer parameter.
For backward compatibility with both calling patterns:
  (ecc-buffer-state-get) -> get current state from current buffer
  (ecc-buffer-state-get buffer) -> get current state from buffer
  (ecc-buffer-state-get 'key) -> get property 'key from current buffer
  (ecc-buffer-state-get 'key buffer) -> get property 'key from buffer"
  (let ((key nil)
        (buffer nil))
    ;; Parse arguments to support both calling patterns
    (cond
     ;; No arguments: get current state from current buffer
     ((null key-or-buffer)
      (setq buffer (current-buffer)))
     ;; First arg is a buffer: get current state from that buffer
     ((bufferp key-or-buffer)
      (setq buffer key-or-buffer))
     ;; First arg is a symbol: treat as key, second arg as buffer
     ((symbolp key-or-buffer)
      (setq key key-or-buffer)
      (setq buffer (or buffer-if-key-provided (current-buffer))))
     ;; Fallback: treat first arg as buffer
     (t
      (setq buffer key-or-buffer)))
    
    (with-current-buffer buffer
      (if key
          ;; Get specific property key
          (progn
            (unless ecc-buffer-state-enabled 
              (ecc-buffer-state-init))
            (get 'ecc-buffer-state-current key))
        ;; Get current state (original behavior)
        (if ecc-buffer-state-enabled
            ;; Return tracked state or update if needed
            (or ecc-buffer-state-current
                (ecc-buffer-state-update))
          ;; Detect on demand if tracking isn't enabled
          (ecc-detect-state))))))

;;;###autoload
(defun ecc-buffer-state-get-prompt (&optional buffer)
  "Get prompt state from BUFFER or current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (ecc-buffer-state-get)))

;;;###autoload
(defun ecc-buffer-state-has-key-p (key &optional buffer)
  "Check if buffer state has KEY in BUFFER or current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (and ecc-buffer-state-enabled
         (get 'ecc-buffer-state-current key))))

;; Auto-response integration

;;;###autoload
(defun ecc-buffer-state-enable-auto-response (&optional buffer)
  "Enable both state tracking and auto-response for BUFFER or current buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    ;; Enable state tracking first
    (ecc-buffer-state-enable)
    
    ;; Enable auto-response
    (setq ecc-buffer-auto-response-enabled t)
    
    ;; Register with auto-core
    (ecc-auto-core-register-buffer (current-buffer))
    
    (ecc-buffer-state-debug-message "Enabled auto-response with buffer-local state tracking")
    
    ;; Return the buffer
    (current-buffer)))

;;;###autoload
(defun ecc-buffer-state-disable-auto-response (&optional buffer)
  "Disable auto-response for BUFFER or current buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    ;; Disable auto-response
    (setq ecc-buffer-auto-response-enabled nil)
    
    ;; Unregister from auto-core
    (ecc-auto-core-unregister-buffer (current-buffer))
    
    (ecc-buffer-state-debug-message "Disabled auto-response")))

;; Utility functions

(defun ecc-buffer-state-should-update-p ()
  "Return t if state should be updated based on throttling.
This prevents too-frequent updates to improve performance."
  (> (- (float-time) ecc-buffer-state-last-update)
     (or (and (boundp 'ecc-auto-core-throttle-time)
              ecc-auto-core-throttle-time)
         1.0)))

(defun ecc-buffer-state-cleanup-buffers ()
  "Clean up the buffer registry by removing dead buffers."
  (setq ecc-buffer-state-registered-buffers
        (seq-filter #'buffer-live-p ecc-buffer-state-registered-buffers))
  ecc-buffer-state-registered-buffers)

(defun ecc-buffer-state-debug-message (format-string &rest args)
  "Output a debug message if buffer state debugging is enabled.
Uses FORMAT-STRING and ARGS like `message`."
  (when ecc-buffer-state-debug
    (apply #'message
           (concat "[Buffer State] " format-string)
           args)))

;; State history and analysis

;;;###autoload
(defun ecc-buffer-state-history (&optional buffer)
  "Get history of Claude prompt states for BUFFER or current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (if ecc-buffer-state-enabled
        ecc-buffer-state-history
      nil)))

;;;###autoload  
(defun ecc-buffer-state-add-to-history (state &optional buffer)
  "Add STATE to history for BUFFER or current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (when ecc-buffer-state-enabled
      ;; Add to front of history list
      (push state ecc-buffer-state-history)
      ;; Trim history to max size
      (when (> (length ecc-buffer-state-history) ecc-buffer-state-history-size)
        (setq ecc-buffer-state-history
              (cl-subseq ecc-buffer-state-history 0 ecc-buffer-state-history-size))))))

;;;###autoload
(defun ecc-buffer-state-changed-p (&optional buffer)
  "Return t if state has changed in BUFFER or current buffer.
Compares current state with previous state in history."
  (with-current-buffer (or buffer (current-buffer))
    (when (and ecc-buffer-state-enabled
               ecc-buffer-state-history
               (> (length ecc-buffer-state-history) 1))
      (not (eq (nth 0 ecc-buffer-state-history)
               (nth 1 ecc-buffer-state-history))))))

;; Status reporting

;;;###autoload
(defun ecc-buffer-state-status (&optional buffer)
  "Return status string for buffer state tracking in BUFFER or current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (format "Buffer State Status for %s:
  State Tracking: %s
  Auto-Response: %s
  Current State: %s
  Last Update: %.2f seconds ago
  History Size: %d states"
            (buffer-name)
            (if ecc-buffer-state-enabled "Enabled" "Disabled")
            (if ecc-buffer-auto-response-enabled "Enabled" "Disabled")
            (if ecc-buffer-state-current
                (ecc-state-get-name ecc-buffer-state-current)
              "None")
            (- (float-time) ecc-buffer-state-last-update)
            (length ecc-buffer-state-history))))

;;;###autoload
(defun ecc-buffer-state-print-status (&optional buffer)
  "Print status information about buffer state tracking to messages."
  (interactive)
  (ecc-debug-message "%s" (ecc-buffer-state-status buffer)))

;; Global management

;;;###autoload
(defun ecc-buffer-state-update-all-buffers ()
  "Update state for all registered buffers."
  (interactive)
  (ecc-buffer-state-cleanup-buffers)
  (let ((count 0))
    (dolist (buffer ecc-buffer-state-registered-buffers)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (ecc-buffer-state-update)
            (setq count (1+ count))))))
    (ecc-buffer-state-debug-message "Updated %d buffers" count)
    count))

;;;###autoload
(defun ecc-buffer-state-toggle-debug ()
  "Toggle debug output for buffer state tracking."
  (interactive)
  (setq ecc-buffer-state-debug (not ecc-buffer-state-debug))
  (ecc-debug-message "Buffer state debug %s"
           (if ecc-buffer-state-debug "enabled" "disabled")))

;; Test compatibility API functions
;; These functions provide the interface expected by tests

;;;###autoload
(defun ecc-buffer-state-update-prompt (state &optional buffer)
  "Update prompt state to STATE for BUFFER or current buffer.
This is a test-compatible wrapper around the main state tracking."
  (with-current-buffer (or buffer (current-buffer))
    (when ecc-buffer-state-enabled
      (setq ecc-buffer-state-current state
            ecc-buffer-state-prompt state)
      (ecc-buffer-state-add-to-history state))))

;;;###autoload
(defun ecc-buffer-state-get-prompt (&optional buffer)
  "Get current prompt state for BUFFER or current buffer.
This is a test-compatible function that returns the stored prompt state."
  (with-current-buffer (or buffer (current-buffer))
    (if ecc-buffer-state-enabled
        ecc-buffer-state-current
      nil)))

;;;###autoload  
(defun ecc-buffer-state-set (key value &optional buffer)
  "Set arbitrary state KEY to VALUE for BUFFER or current buffer.
This provides a generic key-value store for buffer state."
  (with-current-buffer (or buffer (current-buffer))
    (when ecc-buffer-state-enabled
      ;; Store in a simple plist structure
      (setq ecc-buffer-state-data
            (plist-put (or ecc-buffer-state-data nil) key value)))))

;;;###autoload
(defun ecc-buffer-state-get (key &optional buffer)
  "Get value for state KEY from BUFFER or current buffer.
This retrieves values from the generic key-value store."
  (with-current-buffer (or buffer (current-buffer))
    (when ecc-buffer-state-enabled
      (plist-get ecc-buffer-state-data key))))

(provide 'ecc-buffer-state)

;;; ecc-buffer-state.el ends here