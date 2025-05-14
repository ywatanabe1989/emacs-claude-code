;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 20:00:30>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-ui/ecc-ui-mode-line.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ecc-variables)
(require 'ecc-buffer-variables)
(require 'ecc-state-variables)
(require 'ecc-auto-variables)

;; Mode line indicators
;; ------------------------------

(defvar ecc-ui--mode-line-indicators
  (make-hash-table :test 'eq)
  "Hash table of mode line indicators by feature.")

(defvar-local ecc-ui--buffer-overlay nil
  "Overlay for highlighting the buffer name in mode line.")

;; Core mode line functions
;; ------------------------------

(defun ecc-ui-mode-line-update (feature enable &optional indicator face)
  "Update mode line to indicate FEATURE status.
If ENABLE is non-nil, add the indicator; otherwise remove it.
INDICATOR is the text to display in the mode line.
FACE is the face to apply to the indicator."
  (if enable
      ;; Enable indicator
      (progn
        ;; Store indicator info
        (puthash feature
                 (cons indicator (or face '(:foreground "#FFAAAA" :weight bold)))
                 ecc-ui--mode-line-indicators)
        
        ;; Update global mode line format
        (unless (member '(:eval (ecc-ui-mode-line-status)) global-mode-string)
          (setq global-mode-string
                (append (or global-mode-string '(""))
                        '((:eval (ecc-ui-mode-line-status)))))))
    
    ;; Disable indicator
    (remhash feature ecc-ui--mode-line-indicators)
    
    ;; Remove from global mode line if no active indicators
    (when (= (hash-table-count ecc-ui--mode-line-indicators) 0)
      (setq global-mode-string
            (remove '(:eval (ecc-ui-mode-line-status)) global-mode-string))))
  
  ;; Force update
  (force-mode-line-update t))

(defun ecc-ui-mode-line-status ()
  "Return combined mode line status for all active features."
  (let ((indicators nil))
    (maphash (lambda (feature info)
               (let ((indicator (car info))
                     (face (cdr info)))
                 (push (propertize indicator 'face face)
                       indicators)))
             ecc-ui--mode-line-indicators)
    
    (when indicators
      (mapconcat 'identity (nreverse indicators) " "))))

;; Buffer highlighting
;; ------------------------------

(defun ecc-ui-highlight-buffer (buffer &optional state)
  "Highlight BUFFER name in mode line with optional STATE.
STATE determines the color:
- :running - green
- :active - orange
- otherwise - gray"
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((is-active (eq buffer ecc-buffer-current-buffer))
             (state-color (cond
                           ((eq state :running) "green")
                           (is-active "orange")
                           (t "gray"))))
        
        ;; Safe cleanup of existing overlay
        (condition-case nil
            (when (and (boundp 'ecc-ui--buffer-overlay)
                       ecc-ui--buffer-overlay)
              (delete-overlay ecc-ui--buffer-overlay))
          (error nil))
        
        ;; Create new overlay (skip during tests)
        (unless (bound-and-true-p ert-current-test)
          (setq-local ecc-ui--buffer-overlay
                      (make-overlay (point-min) (point-min)))
          (overlay-put ecc-ui--buffer-overlay
                       'before-string
                       (propertize (buffer-name)
                                  'face `(:background ,state-color
                                                      :foreground "black"))))))))

(defun ecc-ui-highlight-all-buffers ()
  "Update highlighting for all Claude buffers."
  (dolist (buf (ecc-buffer-get-registered-buffers))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((state (and (fboundp 'ecc-state-get)
                          (ecc-state-get))))
          (ecc-ui-highlight-buffer buf state))))))

;; Feature-specific update functions
;; ------------------------------

;; Auto mode indicator
(defun ecc-ui-update-auto-mode (enable)
  "Update mode line indicator for auto mode.
If ENABLE is non-nil, show the auto mode indicator."
  (ecc-ui-mode-line-update 'auto-mode enable 
                         " [AUTO]" 
                         '(:foreground "#FFAAAA" :weight bold)))

;; State indicator
(defun ecc-ui-update-state-indicator (state)
  "Update mode line indicator based on STATE."
  (let ((indicator
         (cond
          ((eq state :running) " [Running]")
          ((eq state :waiting) " [Waiting]")
          ((eq state :initial-waiting) " [Continue?]")
          ((eq state :y/n) " [Y/N]")
          ((eq state :y/y/n) " [Y/Y/N]")
          (t " [Idle]")))
        (face
         (cond
          ((eq state :running) '(:foreground "green" :weight bold))
          ((or (eq state :waiting) 
               (eq state :initial-waiting)
               (eq state :y/n)
               (eq state :y/y/n)) 
           '(:foreground "orange" :weight bold))
          (t '(:foreground "gray" :weight normal)))))
    
    (ecc-ui-mode-line-update 'state state indicator face)
    
    ;; Also update buffer highlighting to match state
    (ecc-ui-highlight-all-buffers)))

;; Initialize
(defun ecc-ui-mode-line-init ()
  "Initialize the mode line system."
  (clrhash ecc-ui--mode-line-indicators)
  (setq global-mode-string
        (remove '(:eval (ecc-ui-mode-line-status)) global-mode-string)))

;; Initialize the mode line system
(ecc-ui-mode-line-init)

(provide 'ecc-ui-mode-line)

(when
    (not load-file-name)
  (message "ecc-ui-mode-line.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))