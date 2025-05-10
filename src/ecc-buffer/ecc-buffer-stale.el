;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 00:13:41>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/ecc-buffer/ecc-buffer-stale.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(defun ecc-buffer-get-buffer-state (buf)
  "Get the state of BUF from the registered buffers alist."
  (when buf
    (let ((buffer (if (bufferp buf) buf (get-buffer buf))))
      (cdr (assoc buffer ecc-buffer-registered-buffers-alist)))))

(defun ecc-buffer-set-buffer-state (buf state)
  "Set the state of BUF to STATE in the registered buffers alist.
STATE must be one of the values in `ecc-state-available-states'."
  (cond
   ((not (memq state ecc-state-available-states))
    (error
     "%s is not a valid Claude buffer state. Valid states are: %s"
     state ecc-state-available-states)
    nil)
   (buf
    (let* ((buffer (if (bufferp buf) buf (get-buffer buf)))
           (entry (assoc buffer ecc-buffer-registered-buffers-alist)))
      (when entry
        (setcdr entry state)
        t)))
   (t nil)))

;; (defun ecc-buffer-set-buffer-state (buf state)
;;   "Set the state of BUF to STATE in the registered buffers alist.
;; STATE must be one of the values in `ecc-state-available-states'."
;;   (when (and buf (memq state ecc-state-available-states))
;;     (let* ((buffer (if (bufferp buf) buf (get-buffer buf)))
;;            (entry (assoc buffer ecc-buffer-registered-buffers-alist)))
;;       (when entry
;;         (setcdr entry state)
;;         t))))

(defun ecc-buffer-unregister-stale-buffer (buf)
  "Unregister BUF if it's stale (not live or not in vterm-mode).
Return t if buffer was unregistered, nil otherwise."
  (let ((buffer (if (bufferp buf) buf (get-buffer buf))))
    (when (and buffer
               (or (not (buffer-live-p buffer))
                   (not (with-current-buffer buffer
                          (derived-mode-p 'vterm-mode)))))
      ;; Unregister the buffer
      (when (ecc-buffer-unregister-buffer buffer)
        ;; Clear current buffer if needed
        (when (eq buffer ecc-buffer-current-buffer)
          (setq ecc-buffer-current-buffer nil))
        t))))

(defun ecc-buffer-unregister-stale-buffers ()
  "Unregister buffers that are no longer valid from `ecc-buffer-registered-buffers-alist'.
Return the count of buffers removed."
  (let ((count 0))
    ;; Check each registered buffer
    (dolist
        (buffer-entry
         (copy-sequence ecc-buffer-registered-buffers-alist))
      (let ((buffer (car buffer-entry)))
        (when (ecc-buffer-unregister-stale-buffer buffer)
          (setq count (1+ count)))))
    count))

(defun ecc-buffer-cleanup-buffer-registry ()
  "Remove any stale (killed) buffers from the buffer registry.
This is a compatibility function that calls ecc-buffer-unregister-stale-buffers."
  (ecc-buffer-unregister-stale-buffers))


(provide 'ecc-buffer/ecc-buffer-stale)

(when
    (not load-file-name)
  (message "ecc-buffer-stale.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))