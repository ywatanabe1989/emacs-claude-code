;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-14 16:41:10>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-auto/ecc-auto-monitor.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ecc-variables)
(require 'ecc-auto-variables)
(require 'ecc-state-variables)

;; Monitoring functions for auto mode
;; ------------------------------

(defun ecc-auto-monitor-buffer-state (buffer)
  "Monitor the state of BUFFER and perform actions based on the state."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (let ((state (ecc-buffer-get-buffer-state buffer)))
        (cond
         ((eq state 'waiting)
          (when ecc-auto-respond-to-continue
            (ecc-auto-handle-waiting buffer)))
         ((eq state 'y/n)
          (when ecc-auto-respond-to-yes-no
            (ecc-auto-handle-yes-no buffer)))
         ((eq state 'y/y/n)
          (when ecc-auto-respond-to-yes-yes-no
            (ecc-auto-handle-yes-yes-no buffer))))))))

(defun ecc-auto-monitor-all-buffers ()
  "Monitor all Claude buffers and respond based on their state."
  (let ((buffers (ecc-buffer-list-registered-buffers)))
    (dolist (buffer buffers)
      (ecc-auto-monitor-buffer-state buffer))))

;; Action functions for different states
;; ------------------------------

(defun ecc-auto-handle-waiting (buffer)
  "Handle a waiting state in BUFFER by sending continuation text."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (when (fboundp 'ecc-send-string)
        (ecc-send-string ecc-state-prompt-to-send-on-waiting)))))

(defun ecc-auto-handle-yes-no (buffer)
  "Handle a yes/no prompt in BUFFER."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (when (fboundp 'ecc-send-string)
        (ecc-send-string "y")))))

(defun ecc-auto-handle-yes-yes-no (buffer)
  "Handle a yes/yes/no prompt in BUFFER."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (when (fboundp 'ecc-send-string)
        (ecc-send-string "y")))))

;; Start monitoring with timer
;; ------------------------------

(defun ecc-auto-monitor-start ()
  "Start monitoring Claude buffers with a timer."
  (when (null ecc-auto--monitor-timer)
    (setq ecc-auto--monitor-timer
          (run-with-timer ecc-auto-interval-sec
                          ecc-auto-interval-sec
                          #'ecc-auto-monitor-all-buffers))
    (message "Claude auto-monitor started.")))

(defun ecc-auto-monitor-stop ()
  "Stop the auto-monitor timer."
  (when ecc-auto--monitor-timer
    (cancel-timer ecc-auto--monitor-timer)
    (setq ecc-auto--monitor-timer nil)
    (message "Claude auto-monitor stopped.")))

(defun ecc-auto-monitor-toggle ()
  "Toggle the auto-monitor timer."
  (interactive)
  (if ecc-auto--monitor-timer
      (ecc-auto-monitor-stop)
    (ecc-auto-monitor-start)))

;; Initialize on load
;; ------------------------------

(defvar ecc-auto--monitor-timer nil
  "Timer for monitoring Claude buffers.")

;; Start monitoring when auto mode is enabled by default
(when ecc-auto-mode
  (ecc-auto-monitor-start))

(provide 'ecc-auto-monitor)

;;; ecc-auto-monitor.el ends here