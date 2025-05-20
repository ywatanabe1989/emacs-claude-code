;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 09:35:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-eye-friendly.el

;;; Commentary:
;;; Eye-friendly features to reduce strain during buffer updates and high-speed scrolling.

(require 'ecc-variables)

;; Customization group
(defgroup ecc-eye-friendly nil
  "Eye-friendly settings for Claude vterm buffers."
  :group 'ecc
  :prefix "ecc-eye-friendly-")

(defcustom ecc-eye-friendly-mode t
  "Whether to enable eye-friendly features."
  :type 'boolean
  :group 'ecc-eye-friendly)

(defcustom ecc-eye-friendly-smooth-scroll t
  "Whether to enable smooth scrolling in Claude buffers."
  :type 'boolean
  :group 'ecc-eye-friendly)

(defcustom ecc-eye-friendly-scroll-margin 5
  "Scroll margin to maintain at top and bottom of window."
  :type 'integer
  :group 'ecc-eye-friendly)

(defcustom ecc-eye-friendly-scroll-conservatively 101
  "Scroll conservatively value for smooth scrolling.
If the cursor movement is less than this value, redisplay will scroll 
the minimum amount to show the new cursor position."
  :type 'integer
  :group 'ecc-eye-friendly)

(defcustom ecc-eye-friendly-scroll-step 1
  "Number of lines to scroll when cursor moves out of view."
  :type 'integer
  :group 'ecc-eye-friendly)

(defcustom ecc-eye-friendly-max-speed 500
  "Maximum scrolling speed in lines per second."
  :type 'integer
  :group 'ecc-eye-friendly)

(defcustom ecc-eye-friendly-update-interval 0.03
  "Minimum interval between buffer updates in seconds.
Lower values give smoother scrolling but may affect performance."
  :type 'number
  :group 'ecc-eye-friendly)

(defcustom ecc-eye-friendly-visual-indicator t
  "Whether to show a visual indicator during fast scrolling."
  :type 'boolean
  :group 'ecc-eye-friendly)

;; Internal variables
(defvar-local ecc-eye-friendly--last-update-time 0
  "Time of the last buffer update.")

(defvar-local ecc-eye-friendly--original-settings nil
  "Original settings before enabling eye-friendly mode.")

(defvar-local ecc-eye-friendly--indicator-overlay nil
  "Overlay for the scrolling speed indicator.")

(defvar-local ecc-eye-friendly--throttle-timer nil
  "Timer for throttling buffer updates.")

;; Core functions
;;;###autoload
(define-minor-mode ecc-eye-friendly-mode
  "Minor mode for eye-friendly buffer updates and scrolling.
Reduces eye strain during high-speed scrolling and updates."
  :lighter " 👁"
  :global nil
  (if ecc-eye-friendly-mode
      (ecc-eye-friendly--enable)
    (ecc-eye-friendly--disable)))

(defun ecc-eye-friendly--enable ()
  "Enable eye-friendly features for the current buffer."
  ;; Save original settings
  (setq ecc-eye-friendly--original-settings
        (list :scroll-margin scroll-margin
              :scroll-conservatively scroll-conservatively
              :scroll-step scroll-step
              :auto-hscroll-mode auto-hscroll-mode))
  
  ;; Apply smooth scrolling settings
  (when ecc-eye-friendly-smooth-scroll
    (setq-local scroll-margin ecc-eye-friendly-scroll-margin)
    (setq-local scroll-conservatively ecc-eye-friendly-scroll-conservatively)
    (setq-local scroll-step ecc-eye-friendly-scroll-step)
    (setq-local auto-hscroll-mode nil))
  
  ;; Set up update throttling for vterm
  (when (derived-mode-p 'vterm-mode)
    (advice-add 'vterm--flush-timer-callback :around
                #'ecc-eye-friendly--throttled-update)
    
    ;; Set up scroll speed indicator
    (when ecc-eye-friendly-visual-indicator
      (ecc-eye-friendly--setup-indicator)))
  
  ;; Initialize last update time
  (setq ecc-eye-friendly--last-update-time (float-time))
  
  (message "Eye-friendly mode enabled"))

(defun ecc-eye-friendly--disable ()
  "Disable eye-friendly features for the current buffer."
  ;; Restore original settings
  (when ecc-eye-friendly--original-settings
    (setq-local scroll-margin 
                (plist-get ecc-eye-friendly--original-settings :scroll-margin))
    (setq-local scroll-conservatively
                (plist-get ecc-eye-friendly--original-settings :scroll-conservatively))
    (setq-local scroll-step
                (plist-get ecc-eye-friendly--original-settings :scroll-step))
    (setq-local auto-hscroll-mode
                (plist-get ecc-eye-friendly--original-settings :auto-hscroll-mode)))
  
  ;; Remove advice and timer
  (when (derived-mode-p 'vterm-mode)
    (advice-remove 'vterm--flush-timer-callback #'ecc-eye-friendly--throttled-update)
    
    ;; Clear throttle timer if it exists
    (when ecc-eye-friendly--throttle-timer
      (cancel-timer ecc-eye-friendly--throttle-timer)
      (setq ecc-eye-friendly--throttle-timer nil))
    
    ;; Remove indicator if it exists
    (when ecc-eye-friendly--indicator-overlay
      (delete-overlay ecc-eye-friendly--indicator-overlay)
      (setq ecc-eye-friendly--indicator-overlay nil)))
  
  (message "Eye-friendly mode disabled"))

(defun ecc-eye-friendly--throttled-update (orig-fun &rest args)
  "Throttle vterm updates to reduce eye strain.
Wraps around ORIG-FUN with ARGS to control update frequency."
  (let ((now (float-time))
        (elapsed (- (float-time) ecc-eye-friendly--last-update-time)))
    (if (>= elapsed ecc-eye-friendly-update-interval)
        ;; It's been long enough since the last update
        (progn
          (setq ecc-eye-friendly--last-update-time now)
          (apply orig-fun args)
          
          ;; Update the indicator if enabled
          (when (and ecc-eye-friendly-visual-indicator
                     ecc-eye-friendly--indicator-overlay)
            (ecc-eye-friendly--update-indicator)))
      
      ;; Too soon, schedule the update for later
      (unless ecc-eye-friendly--throttle-timer
        (setq ecc-eye-friendly--throttle-timer
              (run-with-timer 
               (- ecc-eye-friendly-update-interval elapsed)
               nil
               (lambda ()
                 (setq ecc-eye-friendly--throttle-timer nil)
                 (setq ecc-eye-friendly--last-update-time (float-time))
                 (when (buffer-live-p (current-buffer))
                   (apply orig-fun args)
                   
                   ;; Update the indicator if enabled
                   (when (and ecc-eye-friendly-visual-indicator
                              ecc-eye-friendly--indicator-overlay)
                     (ecc-eye-friendly--update-indicator))))))))))

(defun ecc-eye-friendly--setup-indicator ()
  "Set up the scroll speed indicator overlay."
  (when (buffer-live-p (current-buffer))
    (let ((overlay (make-overlay (point-min) (point-min))))
      (overlay-put overlay 'window (selected-window))
      (overlay-put overlay 'priority 1000)
      (setq ecc-eye-friendly--indicator-overlay overlay))))

(defun ecc-eye-friendly--update-indicator ()
  "Update the scroll speed indicator based on scrolling rate."
  (when (and (buffer-live-p (current-buffer))
             ecc-eye-friendly--indicator-overlay)
    (let* ((now (float-time))
           (elapsed (- now ecc-eye-friendly--last-update-time))
           (speed (if (> elapsed 0) (/ 1.0 elapsed) 0)))
      
      ;; Only show indicator during fast scrolling
      (if (> speed 10) ;; 10 lines per second threshold
          (let* ((percentage (min 100 (/ (* speed 100) ecc-eye-friendly-max-speed)))
                 (color (cond
                         ((< percentage 50) "#4CAF50") ;; Green
                         ((< percentage 80) "#FFC107") ;; Yellow
                         (t "#F44336"))) ;; Red
                 (indicator (format " Scroll: %d%% " (round percentage))))
            (move-overlay ecc-eye-friendly--indicator-overlay 
                          (point-min) (point-min))
            (overlay-put ecc-eye-friendly--indicator-overlay 'after-string
                         (propertize indicator 'face 
                                     `(:background ,color :foreground "black"
                                       :box (:line-width -1 :style released-button)))))
        
        ;; Hide indicator during normal speed
        (overlay-put ecc-eye-friendly--indicator-overlay 'after-string nil)))))

;;;###autoload
(defun ecc-eye-friendly-toggle ()
  "Toggle eye-friendly mode in the current buffer."
  (interactive)
  (if (derived-mode-p 'vterm-mode)
      (ecc-eye-friendly-mode (if ecc-eye-friendly-mode -1 1))
    (message "Eye-friendly mode is only supported in vterm buffers")))

;;;###autoload
(defun ecc-eye-friendly-adjust-speed (speed)
  "Adjust the maximum scrolling SPEED (lines per second)."
  (interactive "nMaximum scroll speed (lines/sec): ")
  (setq ecc-eye-friendly-max-speed speed)
  (message "Maximum scroll speed set to %d lines/second" speed))

;; Auto-setup for Claude buffers
(defun ecc-eye-friendly-auto-setup ()
  "Automatically set up eye-friendly mode for Claude buffers."
  (when (and (derived-mode-p 'vterm-mode)
             (string-match-p "\\*CLAUDE.*\\*" (buffer-name)))
    (ecc-eye-friendly-mode 1)))

;; Add hook for auto-setup
(add-hook 'vterm-mode-hook 'ecc-eye-friendly-auto-setup)

(provide 'ecc-eye-friendly)

;;; ecc-eye-friendly.el ends here