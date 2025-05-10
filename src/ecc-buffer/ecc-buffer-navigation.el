;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 16:59:12>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/ecc-buffer/ecc-buffer-navigation.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'cl-lib)
(require 'ecc-buffer/ecc-buffer-variables)
(require 'ecc-buffer/ecc-buffer-registry)
(require 'ecc-buffer/ecc-buffer-current)
(require 'ecc-buffer/ecc-buffer-stale)
(require 'ecc-buffer/ecc-buffer-timestamp)
(require 'ecc-update-mode-line)

;; Buffer switching functions
;; ----------------------------------------

(defun ecc-buffer-next ()
  "Switch to next Claude buffer.
Before navigation, removes any stale buffers from the registry.
Returns the buffer it switched to, or nil if no buffer is available."
  (interactive)
  ;; First, clean up any stale buffers
  (ecc-buffer-unregister-stale-buffers)
  
  ;; Get the updated list of registered buffers
  (let ((registered-buffers (ecc-buffer-get-registered-buffers)))
    (if (not registered-buffers)
        nil  ;; Return nil if no registered buffers
      (let* ((buffers (cl-remove-if-not #'buffer-live-p registered-buffers))
             ;; Return nil if no live buffers after cleanup
             (next-buffer 
              (when buffers
                ;; Sort buffers by their timestamps
                (let* ((sorted-buffers 
                       (sort (copy-sequence buffers)
                             (lambda (b1 b2)
                               (let ((time1 (ecc-buffer-get-timestamp b1))
                                     (time2 (ecc-buffer-get-timestamp b2)))
                                 ;; If either buffer has no timestamp, record one
                                 (unless time1
                                   (setq time1 (ecc-buffer-record-timestamp b1)))
                                 (unless time2
                                   (setq time2 (ecc-buffer-record-timestamp b2)))
                                 (time-less-p time1 time2)))))
                       (current-buffer (ecc-buffer-get-current-buffer))
                       ;; If current buffer is dead or not in list, pos will be nil
                       (pos (and current-buffer 
                                 (buffer-live-p current-buffer)
                                 (cl-position current-buffer sorted-buffers)))
                       (next-pos (mod (if (null pos) 0 (1+ pos)) 
                                     (max 1 (length sorted-buffers)))))
                  (nth next-pos sorted-buffers)))))
        
        ;; Update current buffer and display it if found
        (when next-buffer
          (ecc-buffer-set-current-buffer next-buffer)
          (ecc-update-mode-line-all-buffers)
          (display-buffer next-buffer))
        
        ;; Return the buffer we switched to
        next-buffer))))

(defun ecc-buffer-prev ()
  "Switch to previous Claude buffer.
Before navigation, removes any stale buffers from the registry.
Returns the buffer it switched to, or nil if no buffer is available."
  (interactive)
  ;; First, clean up any stale buffers
  (ecc-buffer-unregister-stale-buffers)
  
  ;; Get the updated list of registered buffers
  (let ((registered-buffers (ecc-buffer-get-registered-buffers)))
    (if (not registered-buffers)
        nil  ;; Return nil if no registered buffers
      (let* ((buffers (cl-remove-if-not #'buffer-live-p registered-buffers))
             ;; Return nil if no live buffers after cleanup
             (prev-buffer 
              (when buffers
                ;; Sort buffers by their timestamps
                (let* ((sorted-buffers 
                       (sort (copy-sequence buffers)
                             (lambda (b1 b2)
                               (let ((time1 (ecc-buffer-get-timestamp b1))
                                     (time2 (ecc-buffer-get-timestamp b2)))
                                 ;; If either buffer has no timestamp, record one
                                 (unless time1
                                   (setq time1 (ecc-buffer-record-timestamp b1)))
                                 (unless time2
                                   (setq time2 (ecc-buffer-record-timestamp b2)))
                                 (time-less-p time1 time2)))))
                       (current-buffer (ecc-buffer-get-current-buffer))
                       ;; If current buffer is dead or not in list, pos will be nil
                       (pos (and current-buffer 
                                 (buffer-live-p current-buffer)
                                 (cl-position current-buffer sorted-buffers)))
                       (prev-pos (mod (if (null pos) 0 (1- pos)) 
                                     (max 1 (length sorted-buffers)))))
                  (nth prev-pos sorted-buffers)))))
        
        ;; Update current buffer and display it if found
        (when prev-buffer
          (ecc-buffer-set-current-buffer prev-buffer)
          (ecc-update-mode-line-all-buffers)
          (display-buffer prev-buffer))
        
        ;; Return the buffer we switched to
        prev-buffer))))

(provide 'ecc-buffer/ecc-buffer-navigation)

(when (not load-file-name)
  (message "%s loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))