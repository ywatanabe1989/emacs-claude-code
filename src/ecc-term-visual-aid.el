;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 07:55:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-term/ecc-term-visual-aid.el

;;; Commentary:
;;; Visual aids for Claude terminal mode.

(require 'ecc-variables)

;; Customization
(defgroup ecc-term-visual-aid nil
  "Visual aids for Claude terminal mode."
  :group 'ecc
  :prefix "ecc-term-visual-aid-")

(defcustom ecc-term-visual-aid-enabled t
  "Whether to enable visual aids in Claude terminal mode."
  :type 'boolean
  :group 'ecc-term-visual-aid)

(defcustom ecc-term-visual-aid-frame-width 2
  "Width of frame highlight in characters."
  :type 'integer
  :group 'ecc-term-visual-aid)

(defcustom ecc-term-visual-aid-prompt-highlight t
  "Whether to highlight prompt areas with background colors."
  :type 'boolean
  :group 'ecc-term-visual-aid)

(defcustom ecc-term-visual-aid-indicators t
  "Whether to display state indicators in the buffer."
  :type 'boolean
  :group 'ecc-term-visual-aid)

;; Face definitions
(defface ecc-term-visual-aid-waiting-face
  '((t :background "#264F78" :foreground "white" :weight bold))
  "Face for waiting prompt highlights."
  :group 'ecc-term-visual-aid)

(defface ecc-term-visual-aid-yes-no-face
  '((t :background "#5A5A0A" :foreground "white" :weight bold))
  "Face for yes/no prompt highlights."
  :group 'ecc-term-visual-aid)

(defface ecc-term-visual-aid-running-face
  '((t :background "#0E5C10" :foreground "white" :weight bold))
  "Face for running state highlights."
  :group 'ecc-term-visual-aid)

(defface ecc-term-visual-aid-frame-face
  '((t :box (:line-width 2 :color "#5D99DD" :style released-button)))
  "Face for highlighting the frame during active prompts."
  :group 'ecc-term-visual-aid)

;; Internal variables
(defvar-local ecc-term-visual-aid--overlays nil
  "List of overlays used for visual aids.")

(defvar-local ecc-term-visual-aid--frame-overlay nil
  "Overlay used for frame highlighting.")

(defvar-local ecc-term-visual-aid--indicator-overlay nil
  "Overlay used for state indicators.")

(defvar-local ecc-term-visual-aid--last-state nil
  "Last detected Claude state.")

;; Core functions
(defun ecc-term-visual-aid-clear-all ()
  "Clear all visual aid overlays."
  (when ecc-term-visual-aid--overlays
    (mapc #'delete-overlay ecc-term-visual-aid--overlays)
    (setq ecc-term-visual-aid--overlays nil))
  
  (when ecc-term-visual-aid--frame-overlay
    (delete-overlay ecc-term-visual-aid--frame-overlay)
    (setq ecc-term-visual-aid--frame-overlay nil))
  
  (when ecc-term-visual-aid--indicator-overlay
    (delete-overlay ecc-term-visual-aid--indicator-overlay)
    (setq ecc-term-visual-aid--indicator-overlay nil)))

(defun ecc-term-visual-aid-update ()
  "Update visual aids based on current Claude state."
  (when (and ecc-term-visual-aid-enabled
             (derived-mode-p 'vterm-mode))
    (let ((state (ecc-detect-simple-state)))
      ;; Only update if state has changed
      (unless (eq state ecc-term-visual-aid--last-state)
        (ecc-term-visual-aid-clear-all)
        (setq ecc-term-visual-aid--last-state state)
        
        (cond
         ((eq state :waiting)
          (ecc-term-visual-aid-apply-waiting))
         ((eq state :y/n)
          (ecc-term-visual-aid-apply-yes-no))
         ((eq state :y/y/n)
          (ecc-term-visual-aid-apply-yes-no)))))))

;; State-specific visual aids
(defun ecc-term-visual-aid-apply-waiting ()
  "Apply visual aids for waiting state."
  (when ecc-term-visual-aid-enabled
    (ecc-term-visual-aid-add-frame 'ecc-term-visual-aid-waiting-face)
    
    ;; Prompt highlight
    (when ecc-term-visual-aid-prompt-highlight
      (ecc-term-visual-aid-highlight-prompt "continue>"
                                         'ecc-term-visual-aid-waiting-face)
      (ecc-term-visual-aid-highlight-prompt "Continue>"
                                         'ecc-term-visual-aid-waiting-face))
    
    ;; State indicator
    (when ecc-term-visual-aid-indicators
      (ecc-term-visual-aid-add-indicator "Waiting for your input..." 
                                      'ecc-term-visual-aid-waiting-face))))

(defun ecc-term-visual-aid-apply-yes-no ()
  "Apply visual aids for yes/no prompt state."
  (when ecc-term-visual-aid-enabled
    (ecc-term-visual-aid-add-frame 'ecc-term-visual-aid-yes-no-face)
    
    ;; Prompt highlight
    (when ecc-term-visual-aid-prompt-highlight
      (ecc-term-visual-aid-highlight-prompt "\\[y/n\\]"
                                         'ecc-term-visual-aid-yes-no-face)
      (ecc-term-visual-aid-highlight-prompt "\\[Y/n\\]"
                                         'ecc-term-visual-aid-yes-no-face)
      (ecc-term-visual-aid-highlight-prompt "\\[Y/y/n\\]"
                                         'ecc-term-visual-aid-yes-no-face))
    
    ;; State indicator
    (when ecc-term-visual-aid-indicators
      (ecc-term-visual-aid-add-indicator "Please respond with y or n" 
                                      'ecc-term-visual-aid-yes-no-face))))

;; Helper functions
(defun ecc-term-visual-aid-add-frame (face)
  "Add a frame overlay with FACE around the buffer content."
  (when ecc-term-visual-aid-enabled
    (let ((overlay (make-overlay (point-min) (point-max))))
      (overlay-put overlay 'face face)
      (overlay-put overlay 'priority 100)
      (overlay-put overlay 'ecc-term-visual-aid t)
      (setq ecc-term-visual-aid--frame-overlay overlay)
      (push overlay ecc-term-visual-aid--overlays))))

(defun ecc-term-visual-aid-highlight-prompt (prompt-regexp face)
  "Highlight text matching PROMPT-REGEXP with FACE."
  (save-excursion
    (goto-char (point-max))
    (let ((search-end-line (max (line-number-at-pos (point-min))
                               (- (line-number-at-pos) 10)))
          (search-end (save-excursion
                        (goto-char (point-min))
                        (forward-line (- search-end-line 1))
                        (point))))
      (when (re-search-backward prompt-regexp search-end t)
        (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
          (overlay-put overlay 'face face)
          (overlay-put overlay 'priority 200)
          (overlay-put overlay 'ecc-term-visual-aid t)
          (push overlay ecc-term-visual-aid--overlays))))))

(defun ecc-term-visual-aid-add-indicator (text face)
  "Add indicator with TEXT using FACE near the point-max."
  (when (and ecc-term-visual-aid-enabled ecc-term-visual-aid-indicators)
    (save-excursion
      (goto-char (point-max))
      (let ((indicator-pos (point))
            (overlay (make-overlay (point-max) (point-max))))
        (overlay-put overlay 'after-string 
                    (propertize (concat "\n" text "\n") 'face face))
        (overlay-put overlay 'priority 300)
        (overlay-put overlay 'ecc-term-visual-aid t)
        (setq ecc-term-visual-aid--indicator-overlay overlay)
        (push overlay ecc-term-visual-aid--overlays)))))

;; Toggle function
(defun ecc-term-visual-aid-toggle ()
  "Toggle visual aids for Claude terminal mode."
  (interactive)
  (setq ecc-term-visual-aid-enabled (not ecc-term-visual-aid-enabled))
  (if ecc-term-visual-aid-enabled
      (progn
        (ecc-term-visual-aid-update)
        (message "Claude visual aids enabled"))
    (ecc-term-visual-aid-clear-all)
    (message "Claude visual aids disabled")))

;; Setup for Claude vterm buffers
(defun ecc-term-visual-aid-setup ()
  "Set up visual aids for Claude terminal mode."
  (when (and ecc-term-visual-aid-enabled
             (or (derived-mode-p 'ecc-term-claude-mode)
                 (and (derived-mode-p 'vterm-mode)
                      (string-match-p "\\*CLAUDE.*\\*" (buffer-name)))))
    ;; Initialize overlays list
    (setq-local ecc-term-visual-aid--overlays nil)
    (setq-local ecc-term-visual-aid--frame-overlay nil)
    (setq-local ecc-term-visual-aid--indicator-overlay nil)
    (setq-local ecc-term-visual-aid--last-state nil)
    
    ;; Add update hook
    (add-hook 'vterm-update-functions
              (lambda (&rest _) (ecc-term-visual-aid-update))
              nil t)
    
    ;; Initial update
    (ecc-term-visual-aid-update)
    
    ;; Add cleanup hook
    (add-hook 'kill-buffer-hook 'ecc-term-visual-aid-clear-all nil t)))

;; Add hook for auto-setup
(add-hook 'ecc-term-claude-mode-hook 'ecc-term-visual-aid-setup)
(add-hook 'vterm-mode-hook
          (lambda ()
            (when (string-match-p "\\*CLAUDE.*\\*" (buffer-name))
              (ecc-term-visual-aid-setup))))

(provide 'ecc-term-visual-aid)

;;; ecc-term-visual-aid.el ends here