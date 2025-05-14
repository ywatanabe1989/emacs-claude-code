;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-14 11:28:32>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-term/ecc-term-follow-bottom.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;; Required dependencies
(require 'ecc-term-variables)

;; Internal variables
;; ------------------------------

(defvar ecc-term-follow-bottom--timer nil
  "Timer for checking if we need to scroll to bottom.")

(defvar ecc-term-follow-bottom--lock nil
  "Lock to prevent recursive following.")

(defvar-local ecc-term-follow-bottom--user-scrolled nil
  "Whether user has manually scrolled away from bottom.")

;; Core functions
;; ------------------------------

(defun ecc-term-follow-bottom-scroll-to-bottom ()
  "Scroll to show the bottom of the buffer.
This respects the margin setting and keeps a few lines of context visible."
  (when (and ecc-term-vterm-always-follow-bottom
             (not ecc-term-follow-bottom--lock)
             (not ecc-term-follow-bottom--user-scrolled))
    (let ((ecc-term-follow-bottom--lock t))
      (when (buffer-live-p (current-buffer))
        (let ((window (get-buffer-window (current-buffer))))
          (when window
            (with-selected-window window
              (let ((target-point (- (point-max) 1)))
                ;; Make sure target-point is visible with margin
                (recenter (- (window-height) 
                            ecc-term-vterm-follow-bottom-margin
                            1))))))))))

(defun ecc-term-follow-bottom-after-output ()
  "Function to call after vterm produces new output."
  (when ecc-term-vterm-always-follow-bottom
    (ecc-term-follow-bottom-scroll-to-bottom)))

(defun ecc-term-follow-bottom-toggle ()
  "Toggle the always-follow-bottom feature."
  (interactive)
  (setq ecc-term-vterm-always-follow-bottom 
        (not ecc-term-vterm-always-follow-bottom))
  (setq-local ecc-term-follow-bottom--user-scrolled nil)
  (message "Always follow bottom %s"
           (if ecc-term-vterm-always-follow-bottom "enabled" "disabled"))
  (when ecc-term-vterm-always-follow-bottom
    (ecc-term-follow-bottom-scroll-to-bottom)))

;; Event handlers
;; ------------------------------

(defun ecc-term-follow-bottom-window-scroll-handler (window start)
  "Handle window scroll events to track manual scrolling.
WINDOW is the scrolled window, START is the new window start position."
  (when (and ecc-term-vterm-always-follow-bottom
             (eq (window-buffer window) (current-buffer))
             (eq major-mode 'ecc-term-vterm-mode))
    (let* ((window-end-pos (window-end window))
           (buffer-end-pos (- (point-max) 1))
           (distance-from-bottom (- buffer-end-pos window-end-pos)))
      ;; User has scrolled away if we're not near the bottom
      (setq-local ecc-term-follow-bottom--user-scrolled 
                 (> distance-from-bottom 
                    (* 2 ecc-term-vterm-follow-bottom-margin))))))

(defun ecc-term-follow-bottom-after-input ()
  "After user input, re-enable following mode."
  (when ecc-term-vterm-always-follow-bottom
    (setq-local ecc-term-follow-bottom--user-scrolled nil)
    (ecc-term-follow-bottom-scroll-to-bottom)))

;; Integration with vterm
;; ------------------------------

(defun ecc-term-follow-bottom-setup ()
  "Set up the follow-bottom feature for the current buffer."
  (when (eq major-mode 'ecc-term-vterm-mode)
    ;; Initialize state
    (setq-local ecc-term-follow-bottom--user-scrolled nil)
    
    ;; Add to vterm output hooks
    (add-to-list 'ecc-term--vterm-update-functions
                'ecc-term-follow-bottom-after-output)
    
    ;; Set up scroll detection
    (add-hook 'window-scroll-functions
              'ecc-term-follow-bottom-window-scroll-handler
              nil t)
    
    ;; Reset scroll state after user inputs
    (when ecc-term--vterm-available
      (advice-add 'vterm-send-string :after
                 (lambda (&rest _) (ecc-term-follow-bottom-after-input)))
      (advice-add 'vterm-send-return :after
                 (lambda (&rest _) (ecc-term-follow-bottom-after-input)))))
  
  ;; Initial scroll to bottom
  (ecc-term-follow-bottom-scroll-to-bottom))

;; Key bindings and menu integration
;; ------------------------------

(defun ecc-term-follow-bottom-add-menu-entry (menu-map)
  "Add follow-bottom toggle entry to MENU-MAP."
  (when menu-map
    (define-key menu-map [ecc-term-vterm-separator-follow]
                '(menu-item "--"))
    (define-key menu-map [ecc-term-vterm-follow-bottom-toggle]
                '(menu-item "Always Follow Bottom" 
                            ecc-term-follow-bottom-toggle
                            :help "Toggle automatically scrolling to show new output"
                            :button (:toggle . ecc-term-vterm-always-follow-bottom)))))

(defun ecc-term-follow-bottom-add-keybinding (mode-map)
  "Add keybinding for follow-bottom toggle to MODE-MAP."
  (when mode-map
    (define-key mode-map (kbd "C-c C-v") 'ecc-term-follow-bottom-toggle)))

(provide 'ecc-term-follow-bottom)

(when
    (not load-file-name)
  (message "ecc-term-follow-bottom.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))