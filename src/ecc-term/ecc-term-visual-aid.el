;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-14 11:22:05>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-term/ecc-term-visual-aid.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;; Required dependencies
(require 'ecc-term-variables)
(require 'ecc-state)

;; Customization
;; ------------------------------

(defgroup ecc-term-visual-aid nil
  "Visual aids for Claude terminal mode."
  :group 'emacs-claude-term
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
;; ------------------------------

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
;; ------------------------------

(defvar ecc-term-visual-aid--overlays nil
  "List of overlays used for visual aids.")

(defvar ecc-term-visual-aid--frame-overlay nil
  "Overlay used for frame highlighting.")

(defvar ecc-term-visual-aid--indicator-overlay nil
  "Overlay used for state indicators.")

(defvar ecc-term-visual-aid--last-state nil
  "Last detected Claude state.")

;; Core functions
;; ------------------------------

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
             (eq major-mode 'ecc-term-vterm-mode))
    (let ((state (ecc-state-get)))
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
          (ecc-term-visual-aid-apply-yes-no))
         ((eq state :initial-waiting)
          (ecc-term-visual-aid-apply-waiting))
         ((eq state :running)
          (ecc-term-visual-aid-apply-running)))))))

;; State-specific visual aids
;; ------------------------------

(defun ecc-term-visual-aid-apply-waiting ()
  "Apply visual aids for waiting state."
  ;; Frame highlight
  (when ecc-term-visual-aid-enabled
    (ecc-term-visual-aid-add-frame 'ecc-term-visual-aid-waiting-face)
    
    ;; Prompt highlight
    (when ecc-term-visual-aid-prompt-highlight
      (ecc-term-visual-aid-highlight-prompt ecc-term-vterm-prompt-waiting
                                           'ecc-term-visual-aid-waiting-face))
    
    ;; State indicator
    (when ecc-term-visual-aid-indicators
      (ecc-term-visual-aid-add-indicator "Waiting for your input..." 
                                        'ecc-term-visual-aid-waiting-face))))

(defun ecc-term-visual-aid-apply-yes-no ()
  "Apply visual aids for yes/no prompt state."
  ;; Frame highlight
  (when ecc-term-visual-aid-enabled
    (ecc-term-visual-aid-add-frame 'ecc-term-visual-aid-yes-no-face)
    
    ;; Prompt highlight
    (when ecc-term-visual-aid-prompt-highlight
      (ecc-term-visual-aid-highlight-prompt ecc-term-vterm-prompt-y/n
                                           'ecc-term-visual-aid-yes-no-face)
      (ecc-term-visual-aid-highlight-prompt ecc-term-vterm-prompt-y/y/n
                                           'ecc-term-visual-aid-yes-no-face))
    
    ;; State indicator
    (when ecc-term-visual-aid-indicators
      (ecc-term-visual-aid-add-indicator "Please respond with y or n" 
                                        'ecc-term-visual-aid-yes-no-face))))

(defun ecc-term-visual-aid-apply-running ()
  "Apply visual aids for running state."
  ;; Frame highlight
  (when ecc-term-visual-aid-enabled
    (ecc-term-visual-aid-add-frame 'ecc-term-visual-aid-running-face)
    
    ;; State indicator
    (when ecc-term-visual-aid-indicators
      (ecc-term-visual-aid-add-indicator "Claude is thinking..." 
                                        'ecc-term-visual-aid-running-face))))

;; Helper functions
;; ------------------------------

(defun ecc-term-visual-aid-add-frame (face)
  "Add a frame overlay with FACE around the buffer content."
  (when ecc-term-visual-aid-enabled
    (let ((overlay (make-overlay (point-min) (point-max))))
      (overlay-put overlay 'face face)
      (overlay-put overlay 'priority 100)
      (overlay-put overlay 'ecc-term-visual-aid t)
      (setq ecc-term-visual-aid--frame-overlay overlay)
      (push overlay ecc-term-visual-aid--overlays))))

(defun ecc-term-visual-aid-highlight-prompt (prompt-text face)
  "Highlight PROMPT-TEXT with FACE."
  (save-excursion
    (goto-char (point-max))
    (let ((search-end-line (max (line-number-at-pos (point-min))
                               (- (line-number-at-pos) 10)))
          (search-end (save-excursion
                        (goto-char (point-min))
                        (forward-line (- search-end-line 1))
                        (point))))
      (when (search-backward prompt-text search-end t)
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

;; Integration with ecc-term-mode
;; ------------------------------

(defun ecc-term-visual-aid-setup ()
  "Set up visual aids for Claude terminal mode."
  (when ecc-term-visual-aid-enabled
    ;; Add our update function to the vterm update hook
    (add-to-list 'ecc-term--vterm-update-functions
                'ecc-term-visual-aid-update)
    
    ;; Initial update
    (ecc-term-visual-aid-update)
    
    ;; Add cleanup hook
    (add-hook 'kill-buffer-hook 'ecc-term-visual-aid-clear-all nil t)))

(defun ecc-term-visual-aid-toggle ()
  "Toggle visual aids for Claude terminal mode."
  (interactive)
  (setq ecc-term-visual-aid-enabled (not ecc-term-visual-aid-enabled))
  (if ecc-term-visual-aid-enabled
      (progn
        (ecc-term-visual-aid-setup)
        (message "Claude visual aids enabled"))
    (ecc-term-visual-aid-clear-all)
    (message "Claude visual aids disabled")))

;; Commands
;; ------------------------------

(defun ecc-term-visual-aid-customize ()
  "Open customization buffer for visual aid settings."
  (interactive)
  (customize-group 'ecc-term-visual-aid))

(provide 'ecc-term-visual-aid)

(when
    (not load-file-name)
  (message "ecc-term-visual-aid.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))