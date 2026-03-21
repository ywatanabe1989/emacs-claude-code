;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-03-20 08:27:30>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-auto-response-ui.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)


;;; Commentary:
;;; UI functions for ecc-auto-response: pulse timer, mode-line, visual modes.

;; 1. Dependencies
;; ----------------------------------------

(require 'cl-lib)
(require 'ecc-debug)
(require 'ecc-state-speaking-flash-feedback)
(require 'ecc-tab-highlight)

;; Variable stubs (defined in ecc-auto-response.el)

(defvar --ecc-auto-response--pulse-timer nil)

(defvar-local --ecc-auto-response--enabled nil)

(defvar-local --ecc-auto-response--pulse-state nil)

(defvar-local --ecc-auto-response--yellow-flash-state nil)

(defvar-local --ecc-auto-response--disabled-modes nil)

(defvar-local --ecc-auto-response--original-mode-line nil)

;; Function stubs (defined in ecc-auto-response.el)
(declare-function --ecc-auto-response-get-registered-buffers
		          "ecc-auto-response" ())

;; Face definition (duplicated here for standalone load safety)

(defface ecc-auto-indicator-face
  '((t :background "#700000" :foreground "#ffffff" :weight bold))
  "Face for AUTO indicator in mode-line."
  :group 'ecc)

;; 2. Pulse Timer
;; ----------------------------------------

(defun --ecc-auto-response--start-pulse-timer ()
  "Start the global pulse timer for mode-line indicator across all buffers.
Also checks speaking patterns for green flash feedback."
  (when --ecc-auto-response--pulse-timer
    (cancel-timer --ecc-auto-response--pulse-timer))
  (setq --ecc-auto-response--pulse-timer
        (run-with-timer 0 1.0
                        (lambda ()
                          ;; Speaking detection (separate from state)
                          (ecc-speaking--check-all-buffers)
                          (dolist
                              (buffer
                               (--ecc-auto-response-get-registered-buffers))
                            (when (buffer-live-p buffer)
                              (with-current-buffer buffer
                                (when --ecc-auto-response--enabled
                                  (setq-local
                                   --ecc-auto-response--pulse-state
                                   (not
                                    --ecc-auto-response--pulse-state))
                                  (force-mode-line-update)))))
                          ;; Update tab-bar highlight
                          (ecc-tab-highlight--update)))))

(defun --ecc-auto-response--stop-pulse-timer ()
  "Stop the global pulse timer."
  (when --ecc-auto-response--pulse-timer
    (cancel-timer --ecc-auto-response--pulse-timer)
    (setq --ecc-auto-response--pulse-timer nil)))

;; 3. Flash / Encouragement
;; ----------------------------------------

(defun --ecc-auto-response--flash-yellow (buffer)
  "Flash the mode-line indicator dark for 5.0 seconds in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local --ecc-auto-response--yellow-flash-state t)
      (force-mode-line-update)
      (run-with-timer 5.0 nil
                      (lambda (buf)
                        (when (buffer-live-p buf)
                          (with-current-buffer buf
                            (setq-local
                             --ecc-auto-response--yellow-flash-state
                             nil)
                            (force-mode-line-update))))
                      buffer))))

(defun --ecc-auto-response--show-encouragement (buffer text)
  "Highlight the sent TEXT in BUFFER with yellow background."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-max))
        (when (search-backward text nil t)
          (let
              ((overlay
                (make-overlay (match-beginning 0) (match-end 0))))
            (overlay-put overlay 'face
                         '(:background "#8B7500" :foreground "#000000"
                                       :weight bold))
            (overlay-put overlay 'priority 1000)
            (run-with-timer 2.0 nil
                            (lambda (ov)
                              (when (overlayp ov)
                                (delete-overlay ov)))
                            overlay)))))))

;; 4. Visual Mode Management
;; ----------------------------------------

(defun --ecc-auto-response--disable-visual-modes ()
  "Disable performance-heavy visual modes during auto-response."
  (setq-local --ecc-auto-response--disabled-modes nil)
  (when
      (and (boundp 'highlight-parentheses-mode)
           highlight-parentheses-mode)
    (push 'highlight-parentheses-mode
          --ecc-auto-response--disabled-modes)
    (highlight-parentheses-mode -1))
  (when (and (boundp 'show-paren-mode) show-paren-mode)
    (push 'show-paren-mode --ecc-auto-response--disabled-modes)
    (show-paren-mode -1))
  (when
      (and (boundp 'rainbow-delimiters-mode) rainbow-delimiters-mode)
    (push 'rainbow-delimiters-mode
          --ecc-auto-response--disabled-modes)
    (rainbow-delimiters-mode -1))
  (--ecc-debug-message "Disabled visual modes: %s"
                       --ecc-auto-response--disabled-modes))

(defun --ecc-auto-response--restore-visual-modes ()
  "Restore visual modes that were disabled."
  (dolist (mode --ecc-auto-response--disabled-modes)
    (when (fboundp mode)
      (funcall mode 1)))
  (setq-local --ecc-auto-response--disabled-modes nil)
  (--ecc-debug-message "Restored visual modes"))

;; 5. Mode-Line
;; ----------------------------------------

(defun --ecc-auto-response--update-mode-line ()
  "Update mode-line to show auto-response status."
  (if --ecc-auto-response--enabled
      (progn
        (unless
            (local-variable-p
             '--ecc-auto-response--original-mode-line)
          (setq-local --ecc-auto-response--original-mode-line
                      (if (local-variable-p 'mode-line-format)
                          mode-line-format
                        (default-value 'mode-line-format))))
        (let ((indicator '(:eval (when
                                     --ecc-auto-response--enabled
                                   (propertize " ⚡ AUTO CLAUDE "
                                               'face (cond
                                                      (ecc-speaking--flash-active
                                                       (ecc-speaking--mode-line-face
                                                        --ecc-auto-response--pulse-state))
                                                      (--ecc-auto-response--yellow-flash-state
                                                       '(:background
                                                         "#1a0f00"
                                                         :foreground
                                                         "#888888"
                                                         :weight
                                                         bold))
                                                      (--ecc-auto-response--pulse-state
                                                       'ecc-auto-indicator-face)
                                                      (t
                                                       '(:background
                                                         "#5c0000"
                                                         :foreground
                                                         "#ffffff"
                                                         :weight
                                                         bold)))
                                               'help-echo
                                               "Auto-response is active")))))
          (let
              ((original --ecc-auto-response--original-mode-line))
            (if (listp original)
                (let ((new-format (copy-sequence original))
                      (buffer-id-pos
                       (cl-position
                        'mode-line-buffer-identification original)))
                  (if buffer-id-pos
                      (setq mode-line-format
                            (append
                             (cl-subseq new-format 0
                                        (1+ buffer-id-pos))
                             (list indicator)
                             (cl-subseq new-format
                                        (1+ buffer-id-pos))))
                    (setq mode-line-format
                          (cons indicator new-format))))
              (setq mode-line-format (list indicator original))))))
    (when
        (local-variable-p '--ecc-auto-response--original-mode-line)
      (setq mode-line-format
            --ecc-auto-response--original-mode-line)
      (kill-local-variable
       '--ecc-auto-response--original-mode-line)))
  (force-mode-line-update))

(defun --ecc-auto-response-refresh-all-mode-lines ()
  "Refresh mode-lines for all buffers with auto-response enabled."
  (interactive)
  (dolist (buffer (--ecc-auto-response-get-registered-buffers))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when --ecc-auto-response--enabled
          (when
              (local-variable-p
               '--ecc-auto-response--original-mode-line)
            (setq mode-line-format
                  --ecc-auto-response--original-mode-line)
            (kill-local-variable
             '--ecc-auto-response--original-mode-line))
          (--ecc-auto-response--update-mode-line)))))
  (message "Refreshed mode-lines for all auto-response buffers"))


(provide 'ecc-auto-response-ui)

(when
    (not load-file-name)
  (message "ecc-auto-response-ui.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))