;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-03-20 08:10:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-tab-highlight.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;;; Highlights tab-bar tabs that contain Claude Code buffers with
;;; auto-response enabled.  Mirrors the mode-line color states:
;;;   - Pulsing red:   auto-response active
;;;   - Green:         speaking flash
;;;   - Dark yellow:   sending text

;; 1. Dependencies
;; ----------------------------------------

(require 'ecc-debug)

;; Variable stubs (defined in ecc-auto-response.el / ui.el)

(defvar-local --ecc-auto-response--enabled nil)

(defvar-local --ecc-auto-response--pulse-state nil)

(defvar-local --ecc-auto-response--yellow-flash-state nil)

(defvar-local ecc-speaking--flash-active nil)

;; Function stubs
(declare-function --ecc-auto-response-get-registered-buffers
                  "ecc-auto-response" ())
(declare-function ecc-speaking--mode-line-face
                  "ecc-state-speaking-flash-feedback" (pulse-state))

;; 2. Configuration
;; ----------------------------------------

(defcustom ecc-tab-highlight-enabled t
  "Whether to highlight tab-bar tabs for Claude Code buffers."
  :type 'boolean
  :group 'ecc)

;; 3. Variables
;; ----------------------------------------

(defcustom ecc-tab-highlight-default-bg "dark green"
  "Default background color for tab-bar-tab face when restoring.
Should match your tab manager's active tab color."
  :type 'color
  :group 'ecc)

(defcustom ecc-tab-highlight-default-fg "gray60"
  "Default foreground color for tab-bar-tab face when restoring."
  :type 'color
  :group 'ecc)

(defvar ecc-tab-highlight--active nil
  "Non-nil when tab highlight is currently applied.")

;; 4. Face Computation
;; ----------------------------------------

(defun ecc-tab-highlight--compute-face ()
  "Compute the face for tab-bar-tab based on current Claude state.
Checks all registered buffers and returns the highest-priority state face.
Priority: speaking > yellow-flash > pulse-active > pulse-dim.
Returns nil if no registered buffer has auto-response enabled."
  (let ((any-enabled nil)
        (any-speaking nil)
        (any-yellow nil)
        (any-pulse nil))
    (dolist (buffer (--ecc-auto-response-get-registered-buffers))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when --ecc-auto-response--enabled
            (setq any-enabled t)
            (when ecc-speaking--flash-active
              (setq any-speaking t))
            (when --ecc-auto-response--yellow-flash-state
              (setq any-yellow t))
            (when --ecc-auto-response--pulse-state
              (setq any-pulse t))))))
    (cond
     ((not any-enabled) nil)
     (any-speaking
      (if any-pulse
          '(:background "#005500" :foreground "#ffffff" :weight bold)
        '(:background "#003300" :foreground "#aaffaa" :weight bold)))
     (any-yellow
      '(:background "#1a0f00" :foreground "#888888" :weight bold))
     (any-pulse
      '(:background "#700000" :foreground "#ffffff" :weight bold))
     (t
      '(:background "#5c0000" :foreground "#ffffff" :weight bold)))))

;; 5. Tab Face Update
;; ----------------------------------------

(defun ecc-tab-highlight--update ()
  "Update the tab-bar-tab face based on Claude state.
Called from the pulse timer in ecc-auto-response-ui."
  (when ecc-tab-highlight-enabled
    (let ((face (ecc-tab-highlight--compute-face)))
      (if face
          (progn
            (set-face-attribute 'tab-bar-tab nil
                                :background
				(plist-get face :background)
                                :foreground
				(plist-get face :foreground)
                                :weight
				(or (plist-get face :weight) 'bold))
            (setq ecc-tab-highlight--active t))
        ;; No active buffers - restore original face
        (when ecc-tab-highlight--active
          (ecc-tab-highlight--restore))))))

(defun ecc-tab-highlight--restore ()
  "Restore the original tab-bar-tab face using configured defaults."
  (custom-set-faces
   `(tab-bar-tab
     ((t (:inherit tab-bar
                   :background ,ecc-tab-highlight-default-bg
                   :foreground ,ecc-tab-highlight-default-fg)))))
  (setq ecc-tab-highlight--active nil))

;; 6. Provide
;; ----------------------------------------

(provide 'ecc-tab-highlight)

(when (not load-file-name)
  (message "ecc-tab-highlight.el loaded."))
