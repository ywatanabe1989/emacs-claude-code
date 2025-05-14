;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 10:00:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/mode-line/libs/test-helper.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;; This is a helper file for mode-line tests

;; Define required variables for tests
(defvar ecc-mode-line-indicator " [Claude-Auto]"
  "Mode line indicator for Claude auto-accept mode.")

(defvar ecc-buffer-current-active-buffer nil
  "Current active buffer for Claude integration.")

(defvar-local ecc-buffer-name-overlay nil
  "Overlay for highlighting the buffer name in mode line.")

;; Buffer variables that would normally be defined in ecc-buffer-variables
(defvar ecc-buffer-name "*CLAUDE-CODE*"
  "Name for the Claude interaction buffer.")

(defvar ecc-buffer nil
  "Buffer for Claude interaction.")

(defvar ecc-buffer-current-buffer nil
  "Current buffer for Claude integration.")

(defvar ecc-buffer-registered-buffers nil
  "List of registered buffers for Claude integration.")

;; Simple implementation for tests
(defun ecc-update-mode-line (enable)
  "Update mode line with indicator when auto-accept is enabled.
ENABLE is a boolean that determines whether to enable or disable the mode line indicator."
  (if enable
      (unless (member ecc-mode-line-indicator global-mode-string)
        (setq global-mode-string
              (if global-mode-string
                  (append global-mode-string (list ecc-mode-line-indicator))
                (list ecc-mode-line-indicator))))
    ;; Remove indicator
    (when (and global-mode-string (member ecc-mode-line-indicator global-mode-string))
      (setq global-mode-string (delete ecc-mode-line-indicator global-mode-string))))
  
  ;; Handle buffer overlay highlighting
  (when (buffer-live-p ecc-buffer-current-active-buffer)
    (with-current-buffer ecc-buffer-current-active-buffer
      (if enable
          (progn
            (when ecc-buffer-name-overlay
              (delete-overlay ecc-buffer-name-overlay))
            (setq ecc-buffer-name-overlay (make-overlay (point-min) (point-min)))
            (overlay-put ecc-buffer-name-overlay 'before-string
                         (propertize (buffer-name)
                                    'face '(:background "orange"
                                                      :foreground "black"))))
        (when ecc-buffer-name-overlay
          (delete-overlay ecc-buffer-name-overlay)
          (setq ecc-buffer-name-overlay nil)))))
  
  (force-mode-line-update t))

(provide 'test-helper)

;; Local Variables:
;; coding: utf-8
;; End: