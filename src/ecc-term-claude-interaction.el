;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 10:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-term-claude-interaction.el

;;; Commentary:
;;; Core interaction functions for Claude terminal mode.
;;; This module provides functions for sending commands and responses to Claude,
;;; handling scrolling and view management, and other direct interaction features.

;; Optional dependency - vterm must be installed separately
(require 'vterm nil t)
(require 'ecc-variables)

;; Forward declarations to prevent free variable warnings
(defvar ecc-vterm-always-follow-bottom nil
  "When non-nil, automatically scroll to show the bottom of the buffer after new output.")
(defvar ecc-vterm-follow-bottom-margin 2
  "Number of lines to keep between cursor and bottom of window.")

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

(defun ecc-term-claude--vterm-clear ()
  "Clear vterm buffer if available."
  (if (fboundp 'vterm-clear)
      (vterm-clear)
    (ecc-debug-message "vterm not available, cannot clear buffer")))

;;; Code:

;;;; Basic Claude interaction commands


(defun ecc-term-claude-send-yes ()
  "Send y response to Claude prompt.
Sends the text y followed by a return keypress to the current
vterm buffer, simulating the user typing y and pressing Enter.

This function is typically used to respond affirmatively to Claude's
yes/no prompts.

Side Effects:
  Sends text to the vterm process.
  May trigger Claude to continue processing or change its behavior."
  (interactive)
  (ecc-term-claude--vterm-send-string "y")
  (ecc-term-claude--vterm-send-return))


(defun ecc-term-claude-send-no ()
  "Send n response to Claude prompt.
Sends the text n followed by a return keypress to the current
vterm buffer, simulating the user typing n and pressing Enter.

This function is typically used to respond negatively to Claude's
yes/no prompts.

Side Effects:
  Sends text to the vterm process.
  May trigger Claude to continue processing or change its behavior."
  (interactive)
  (ecc-term-claude--vterm-send-string "n")
  (ecc-term-claude--vterm-send-return))


(defun ecc-term-claude-clear-buffer ()
  "Clear the Claude vterm buffer.
Removes all content from the buffer using the vterm clear command.
This is useful for starting fresh conversations or removing
sensitive information.

Side Effects:
  Clears all content from the vterm buffer."
  (interactive)
  (ecc-term-claude--vterm-clear))


(defun ecc-term-claude-send-string (text)
  "Send TEXT string to Claude prompt.
Sends the provided text followed by a return keypress to the current
vterm buffer.

Arguments:
  TEXT: The string to send to Claude.

Side Effects:
  Sends text to the vterm process.
  May trigger Claude to process the input."
  (interactive "sText to send: ")
  (vterm-send-string text)
  (vterm-send-return))

;;;; Scrolling and view management


(defun ecc-term-claude-follow-bottom-after-output ()
  "Scroll to bottom after vterm produces new output.
When enabled via `ecc-vterm-always-follow-bottom`, this function
ensures that the view always follows the most recent output.
This is useful for monitoring ongoing Claude responses.

This function is typically added to `ecc-term-claude-update-functions`
to be called after each vterm output update."
  (when ecc-vterm-always-follow-bottom
    (ecc-term-claude-scroll-to-bottom)))


(defun ecc-term-claude-scroll-to-bottom ()
  "Scroll to show the bottom of the buffer.
Adjusts the view to show the most recent content at the bottom
of the window, keeping a small margin defined by
`ecc-vterm-follow-bottom-margin`.

Side Effects:
  Changes the scroll position of the current window."
  (when (and ecc-vterm-always-follow-bottom
             (buffer-live-p (current-buffer)))
    (let ((window (get-buffer-window (current-buffer))))
      (when window
        (with-selected-window window
          (recenter (- (window-height) 
                      ecc-vterm-follow-bottom-margin
                      1)))))))


(defun ecc-term-claude-toggle-follow-bottom ()
  "Toggle the always-follow-bottom feature.
When enabled, the buffer will automatically scroll to show the bottom
after new content is received from Claude. This is useful to ensure
you always see the latest output.

The margin between the bottom of the window and the bottom of the
buffer is controlled by `ecc-vterm-follow-bottom-margin`."
  (interactive)
  (setq ecc-vterm-always-follow-bottom 
        (not ecc-vterm-always-follow-bottom))
  (ecc-debug-message "Always follow bottom %s"
           (if ecc-vterm-always-follow-bottom "enabled" "disabled"))
  (when ecc-vterm-always-follow-bottom
    (ecc-term-claude-scroll-to-bottom)))

;;;; History management functions


(defun ecc-term-claude-browse-history ()
  "Browse through Claude interaction history.
Opens a buffer displaying the history of recent Claude interactions.
This allows reviewing earlier conversations and responses.

Not yet implemented."
  (interactive)
  (ecc-debug-message "History browsing is not yet implemented"))

;;;; Backward compatibility aliases

;; Legacy function names for interaction commands
(defalias 'ecc-term-claude-yes 'ecc-term-claude-send-yes)
(defalias 'ecc-term-claude-no 'ecc-term-claude-send-no)
(defalias 'ecc-term-claude-clear 'ecc-term-claude-clear-buffer)

(provide 'ecc-term-claude-interaction)

;;; ecc-term-claude-interaction.el ends here