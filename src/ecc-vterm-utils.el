;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 22:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-vterm-utils.el

;;; Commentary:
;;; Utility functions for vterm interaction with Claude.
;;; 
;;; This module provides shared functionality for sending commands and
;;; responses to vterm buffers in a consistent way. It centralizes all
;;; vterm-related functionality to ensure consistent behavior across
;;; the codebase.
;;;
;;; Key features:
;;; - Preserves cursor position when user is reading earlier content
;;; - Handles debug output consistently
;;; - Provides standardized delay timing for better stability
;;; - Works with both global and buffer-local configurations

;;; Code:

(require 'ecc-variables)
(require 'ecc-debug-utils)

;; Customization options
(defgroup ecc-vterm-utils nil
  "Settings for vterm utilities in Claude Code."
  :group 'ecc
  :prefix "ecc-vterm-utils-")

(defcustom ecc-vterm-utils-distance-threshold 40
  "Maximum distance from end of buffer to send directly at point.
When the cursor is more than this many characters from the end of
the buffer, responses will be sent using save-excursion to preserve
the user's reading position."
  :type 'integer
  :group 'ecc-vterm-utils)

(defcustom ecc-vterm-utils-delay 1.0
  "Delay in seconds between vterm commands for stability.
A small delay helps ensure commands are processed correctly by the terminal."
  :type 'number
  :group 'ecc-vterm-utils)

;;;###autoload
(defun ecc-vterm-utils-send-string (buffer response &optional debug-fn)
  "Send RESPONSE string to vterm in BUFFER.
Preserves cursor position if user is reading earlier content.
If DEBUG-FN is provided, it should be a function taking a format
string and arguments for debug output."
  (when (and (buffer-live-p buffer)
             (fboundp 'vterm-send-string))
    (with-current-buffer buffer
      ;; Save current point information
      (let ((distance-from-end (- (point-max) (point)))
            ;; Use provided debug-fn or create a standard one if needed
            (debug-function (or debug-fn 
                               (ecc-debug-utils-make-debug-fn buffer))))
        
        ;; Send debug messages
        (when debug-function
          (funcall debug-function "Sending to vterm in %s: %s"
                   (buffer-name) response)
          (funcall debug-function "Point: %d, Max: %d, Distance from end: %d" 
                   (point) (point-max) distance-from-end))
        
        ;; If user is close to end of buffer, send directly
        ;; Otherwise preserve reading position
        (if (< distance-from-end ecc-vterm-utils-distance-threshold)
            ;; Near end of buffer - send directly
            (progn
              (when debug-function
                (funcall debug-function "Sending directly at current point"))
              (sit-for ecc-vterm-utils-delay)
              (vterm-send-string response)
              (sit-for ecc-vterm-utils-delay)
              (vterm-send-return))
          
          ;; Far from end - preserve position with save-excursion
          (save-excursion
            (when debug-function
              (funcall debug-function "Using save-excursion to send at end"))
            (goto-char (point-max))
            (sit-for ecc-vterm-utils-delay)
            (vterm-send-string response)
            (sit-for ecc-vterm-utils-delay)
            (vterm-send-return)))
        
        ;; Final debug message about completion
        (when debug-function
          (funcall debug-function "Send complete. Final point: %d" (point)))))))

;;;###autoload
(defun ecc-vterm-utils-send-command (buffer command &optional debug-fn)
  "Send COMMAND to vterm in BUFFER.
Always sends the command at the end of the buffer, preserving
cursor position. If DEBUG-FN is provided, it should be a function
taking a format string and arguments for debug output."
  (when (and (buffer-live-p buffer)
             (fboundp 'vterm-send-string))
    (with-current-buffer buffer
      ;; Set up debug function
      (let ((debug-function (or debug-fn 
                               (ecc-debug-utils-make-debug-fn buffer))))
        ;; Always preserve position for commands
        (save-excursion
          ;; Debug output
          (when debug-function
            (funcall debug-function "Sending command to vterm: %s" command))
          
          ;; Send the command
          (goto-char (point-max))
          (sit-for ecc-vterm-utils-delay)
          (vterm-send-string command)
          (sit-for ecc-vterm-utils-delay)
          (vterm-send-return))
        
        ;; Final debug message
        (when debug-function
          (funcall debug-function "Command sent. Buffer: %s" (buffer-name)))))))

;;;###autoload
(defun ecc-vterm-utils-buffer-p (&optional buffer)
  "Check if BUFFER (or current buffer) is a vterm buffer.
Returns non-nil if the buffer has vterm-mode or supports vterm operations."
  (with-current-buffer (or buffer (current-buffer))
    (and (derived-mode-p 'vterm-mode)
         (fboundp 'vterm-send-string))))

;;;###autoload
(defun ecc-vterm-utils-prepare-shell ()
  "Prepare the current vterm buffer for command input.
Makes sure the cursor is positioned properly and the buffer is
in a good state for command input."
  (when (ecc-vterm-utils-buffer-p)
    ;; Move to end if needed
    (when (< (point) (point-max))
      (goto-char (point-max)))
    
    ;; Press return to ensure we have a fresh prompt
    (vterm-send-return)
    (sit-for ecc-vterm-utils-delay)))

(provide 'ecc-vterm-utils)

;;; ecc-vterm-utils.el ends here
