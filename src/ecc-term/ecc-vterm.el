;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 03:07:43>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-vterm.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; ecc-vterm.el --- vterm integration for Claude AI -*- lexical-binding: t -*-

;; Author: ywatanabe <ywatanabe@alumni.u-tokyo.ac.jp>
;; Maintainer: ywatanabe <ywatanabe@alumni.u-tokyo.ac.jp>
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1") (vterm "0.0.1"))
;; Keywords: ai, convenience, tools, terminals
;; URL: https://github.com/ywatanabe1989/emacs-claude-code

;;; Commentary:
;;
;; This module provides integration between vterm and the new Claude AI
;; architecture. It allows for running Claude AI in a terminal buffer
;; with enhanced interaction capabilities provided by vterm.
;;
;; The vterm integration enhances Claude AI by providing a more
;; terminal-like experience, better handling of control characters,
;; and improved performance for large outputs.

;;; Code:

(require 'cl-lib)
(require 'vterm nil t)
(require 'ecc-state-engine)
(require 'ecc-buffer-manager)
(require 'ecc-command)
(require 'ecc-integration)

;; ---- Variables and Customization ----

(defgroup ecc-vterm nil
  "Claude AI vterm integration."
  :group 'ecc)

(defcustom ecc-vterm-auto-detect-state t
  "Whether to automatically detect Claude state in vterm buffers."
  :type 'boolean
  :group 'ecc-vterm)

(defcustom ecc-vterm-poll-interval 0.5
  "Interval in seconds to poll vterm buffer for state changes."
  :type 'number
  :group 'ecc-vterm)

(defvar ecc-vterm--timers (make-hash-table :test 'eq)
  "Hash table of state detection timers for vterm buffers.")

(defvar ecc-vterm--command-history nil
  "History of commands sent to vterm.")

(defvar ecc-vterm--output-ring (make-ring 5)
  "Ring buffer of recent outputs from vterm.")

;; ---- Core vterm Integration ----

(defun ecc-vterm-create ()
  "Create a new vterm buffer for Claude interaction."
  (interactive)

  ;; Create a vterm buffer
  (let* ((buffer-name (format "*Claude vterm %d*"
                              (1+ (length (ecc-vterm-list-buffers)))))
         (vterm-buffer (save-window-excursion
                         (vterm buffer-name)
                         (current-buffer))))

    ;; Register with the buffer manager
    (let
        ((claude-buffer
          (ecc-buffer-manager-create buffer-name vterm-buffer)))
      ;; Set metadata
      (ecc-buffer-manager-set-metadata claude-buffer 'vterm t)
      (ecc-buffer-manager-set-metadata claude-buffer 'created-time
                                       (current-time))

      ;; Setup state monitoring
      (when ecc-vterm-auto-detect-state
        (ecc-vterm--setup-state-detection vterm-buffer))

      ;; Return the vterm buffer
      (switch-to-buffer vterm-buffer)
      vterm-buffer)))

(defun ecc-vterm-list-buffers ()
  "List all vterm buffers registered for Claude."
  (let ((result nil))
    (dolist (cb (ecc-buffer-manager-get-all))
      (when (and (ecc-buffer-manager-get-metadata cb 'vterm)
                 (buffer-live-p (ecc-buffer-buffer cb)))
        (push (ecc-buffer-buffer cb) result)))
    result))

(defun ecc-vterm-send-command (buffer command)
  "Send COMMAND to the Claude vterm in BUFFER."
  (interactive
   (let* ((buffers (ecc-vterm-list-buffers))
          (buf (if (= (length buffers) 1)
                   (car buffers)
                 (completing-read "vterm buffer: "
                                  (mapcar #'buffer-name buffers)
                                  nil t)))
          (cmd
           (read-string "Command: " nil 'ecc-vterm--command-history)))
     (list (if (stringp buf) (get-buffer buf) buf) cmd)))

  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (vterm-send-string command)
      (vterm-send-return)

      ;; Update command history
      (add-to-history 'ecc-vterm--command-history command)

      ;; Ensure this buffer is current in buffer manager
      (let ((claude-buffer (ecc-buffer-manager-get-by-buffer buffer)))
        (when claude-buffer
          (ecc-buffer-manager-set-current claude-buffer)))

      ;; Force state detection
      (when ecc-vterm-auto-detect-state
        (ecc-vterm--detect-state buffer)))))

(defun ecc-vterm-send-input (buffer input &optional no-return)
  "Send INPUT to the Claude vterm in BUFFER.
If NO-RETURN is non-nil, don't send return after the input."
  (interactive
   (list (current-buffer)
         (read-string "Input: ")
         current-prefix-arg))

  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (vterm-send-string input)
      (unless no-return
        (vterm-send-return))

      ;; Ensure this buffer is current in buffer manager
      (let ((claude-buffer (ecc-buffer-manager-get-by-buffer buffer)))
        (when claude-buffer
          (ecc-buffer-manager-set-current claude-buffer))))))

(defun ecc-vterm-send-yes (buffer)
  "Send 'Yes' to Claude in BUFFER."
  (interactive (list (current-buffer)))
  (with-current-buffer buffer
    (vterm-send-string "1")
    (vterm-send-return)))

(defun ecc-vterm-send-no (buffer)
  "Send 'No' to Claude in BUFFER."
  (interactive (list (current-buffer)))
  (with-current-buffer buffer
    (vterm-send-string "2")
    (vterm-send-return)))

(defun ecc-vterm-send-yes-but (buffer)
  "Send 'Yes, but' to Claude in BUFFER."
  (interactive (list (current-buffer)))
  (with-current-buffer buffer
    (vterm-send-string "2")
    (vterm-send-return)))

(defun ecc-vterm-send-interrupt (buffer)
  "Send interrupt (Ctrl-C) to vterm in BUFFER."
  (interactive (list (current-buffer)))
  (with-current-buffer buffer
    (vterm-send-C-c)))

;; ---- State Detection ----

(defun ecc-vterm--setup-state-detection (buffer)
  "Set up state detection for vterm BUFFER."
  (when (buffer-live-p buffer)
    ;; Cancel any existing timer
    (when-let ((timer (gethash buffer ecc-vterm--timers)))
      (cancel-timer timer))

    ;; Create a new timer
    (puthash buffer
             (run-with-timer ecc-vterm-poll-interval
                             ecc-vterm-poll-interval
                             #'ecc-vterm--detect-state
                             buffer)
             ecc-vterm--timers)))

(defun ecc-vterm--cleanup-state-detection (buffer)
  "Clean up state detection for vterm BUFFER."
  (when-let ((timer (gethash buffer ecc-vterm--timers)))
    (cancel-timer timer)
    (remhash buffer ecc-vterm--timers)))

(defun ecc-vterm--detect-state (buffer)
  "Detect Claude state in vterm BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      ;; Get vterm content
      (let ((content (if (fboundp 'vterm-get-buffer-string)
                         (vterm-get-buffer-string)
                       (buffer-string))))
        ;; Store in output ring
        (ring-insert ecc-vterm--output-ring content)

        ;; Create a temporary buffer with the content for state detection
        (with-temp-buffer
          (insert content)
          (let
              ((state (ecc-state-engine-detect-state (current-buffer))))
            (when state
              ;; Update state in buffer manager
              (let
                  ((claude-buffer
                    (ecc-buffer-manager-get-by-buffer buffer)))
                (when claude-buffer
                  (ecc-buffer-manager-set-state claude-buffer state))))))))))

;; ---- Mode Definition ----

(define-minor-mode ecc-vterm-mode
  "Minor mode for Claude AI interaction via vterm."
  :lighter " Claude-vterm"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-y") 'ecc-vterm-send-yes)
            (define-key map (kbd "C-c C-n") 'ecc-vterm-send-no)
            (define-key map (kbd "C-c C-b") 'ecc-vterm-send-yes-but)
            (define-key map (kbd "C-c C-c") 'ecc-vterm-send-interrupt)
            (define-key map (kbd "C-c C-s") 'ecc-vterm-send-input)
            map)

  (if ecc-vterm-mode
      ;; Enable
      (progn
        ;; Register with buffer manager if not already
        (let
            ((claude-buffer
              (ecc-buffer-manager-get-by-buffer (current-buffer))))
          (if claude-buffer
              ;; If we already have a buffer record, use it
              (ecc-buffer-manager-set-metadata claude-buffer 'vterm t)
            ;; Otherwise, create a new one and then set metadata
            (setq claude-buffer
                  (ecc-buffer-manager-create (buffer-name)
                                             (current-buffer)))
            (when claude-buffer
              (ecc-buffer-manager-set-metadata claude-buffer 'vterm t))))

        ;; Setup state detection
        (when ecc-vterm-auto-detect-state
          (ecc-vterm--setup-state-detection (current-buffer)))

        ;; Add hook to cleanup on kill
        (add-hook 'kill-buffer-hook #'ecc-vterm--on-kill nil t))

    ;; Disable
    (ecc-vterm--cleanup-state-detection (current-buffer))
    (remove-hook 'kill-buffer-hook #'ecc-vterm--on-kill t)))

(defun ecc-vterm--on-kill ()
  "Handle vterm buffer being killed."
  (ecc-vterm--cleanup-state-detection (current-buffer))

  ;; Remove from buffer manager
  (let
      ((claude-buffer
        (ecc-buffer-manager-get-by-buffer (current-buffer))))
    (when claude-buffer
      (ecc-buffer-manager-kill claude-buffer))))

;; Auto-enable for Claude vterm buffers

(defun ecc-vterm--maybe-enable ()
  "Enable ecc-vterm-mode if this looks like a Claude vterm buffer."
  (when (and (eq major-mode 'vterm-mode)
             (string-match-p "\\*Claude\\|claude" (buffer-name)))
    (ecc-vterm-mode 1)))

(add-hook 'vterm-mode-hook #'ecc-vterm--maybe-enable)

;;; ecc-vterm.el ends here


(provide 'ecc-vterm)

(when
    (not load-file-name)
  (message "ecc-vterm.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))