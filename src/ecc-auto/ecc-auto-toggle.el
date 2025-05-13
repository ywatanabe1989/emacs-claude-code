;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 19:15:30>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-auto/ecc-auto-toggle.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ecc-auto-variables)
(require 'ecc-buffer-variables)
(require 'ecc-state-variables)

;;;###autoload
(defun ecc-auto-toggle ()
  "Toggle auto-accepting Claude prompts."
  (interactive)
  (if ecc-auto--active
      (progn
        (ecc-auto-stop)
        (setq ecc-auto--active nil)
        (ecc-auto-update-mode-line))
    (progn
      (ecc-auto-start)
      (setq ecc-auto--active t)
      (ecc-auto-update-mode-line))))

(defun ecc-auto-update-mode-line ()
  "Update mode line to indicate auto-accept status."
  (if ecc-auto--active
      (unless (member '(:eval (ecc-auto-mode-line-status)) global-mode-string)
        (setq global-mode-string
              (append global-mode-string
                      '((:eval (ecc-auto-mode-line-status))))))
    (setq global-mode-string
          (remove '(:eval (ecc-auto-mode-line-status)) global-mode-string)))
  (force-mode-line-update t))

(defun ecc-auto-mode-line-status ()
  "Return the mode line status string for Claude auto-accept."
  (when ecc-auto--active
    (propertize ecc-auto-mode-line-indicator
                'face '(:foreground "#FFAAAA" :weight bold))))

;; Initialize the mode line
(unless (member '(:eval (ecc-auto-mode-line-status)) global-mode-string)
  (setq global-mode-string
        (append global-mode-string
                '((:eval (ecc-auto-mode-line-status))))))

;;;###autoload
(defun ecc-auto-start ()
  "Start auto-accepting Claude prompts using a hook and a timer."
  (interactive)
  (cond
   ;; 1. Check if buffer exists
   ((not (get-buffer ecc-buffer-name))
    ;; 3. Check if current buffer is vterm-mode
    (if (derived-mode-p 'vterm-mode)
        ;; 4. Rename current buffer to ecc-buffer-name
        (rename-buffer ecc-buffer-name t)
      ;; 5. Not vterm-mode, raise message
      (message "Current buffer is not in vterm-mode")))
   ;; 2. Buffer exists but check if it's in vterm-mode
   ((with-current-buffer ecc-buffer-name
      (not (derived-mode-p 'vterm-mode)))
    (message "Buffer %s is not in vterm-mode" ecc-buffer-name)))

  ;; Continue if we have a valid vterm buffer
  (when (and (get-buffer ecc-buffer-name)
             (with-current-buffer ecc-buffer-name
               (derived-mode-p 'vterm-mode)))
    ;; Set up hook for immediate response to changes
    (add-hook 'vterm-update-functions 'ecc-auto-accept-send)
    ;; Also set up a timer for regular checking
    (when ecc-state--auto-timer
      (cancel-timer ecc-state--auto-timer))
    (setq ecc-state--auto-timer
          (run-with-timer 1 ecc-auto-interval-sec
                          'ecc-auto-accept-send))
    (with-current-buffer ecc-buffer-name
      (vterm-send-key "l" nil nil t))
    (message "Claude auto-accept enabled for %s"
             ecc-buffer-name)))

;;;###autoload
(defun ecc-auto-stop ()
  "Stop auto-accepting Claude prompts."
  (interactive)
  (remove-hook 'vterm-update-functions 'ecc-auto-accept-send)

  (when ecc-state--auto-timer
    (cancel-timer ecc-state--auto-timer)
    (setq ecc-state--auto-timer nil))

  (message "Claude auto-accept disabled"))

(defun ecc-auto-notify-completion (type)
  "Display a notification when an auto-completion of TYPE occurs."
  (when ecc-auto-notify-completions
    (let ((msg (format "Claude auto-response: %s" type)))
      (message msg))))


(provide 'ecc-auto-toggle)

(when
    (not load-file-name)
  (message "ecc-auto-toggle.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))