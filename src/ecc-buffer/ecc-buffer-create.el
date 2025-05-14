;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-14 14:15:24>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-buffer/ecc-buffer-create.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;; Make sure we have access to these variables and functions
(eval-and-compile
  (require 'ecc-buffer-registry))

;; External functions we use from other modules
(declare-function ecc-buffer-registry-register-buffer-with-uid "ecc-buffer-registry-uid")
(declare-function ecc-buffer-metadata-set "ecc-buffer-metadata")

;; Buffer Creation
;; ------------------------------

(defun ecc-buffer-create (&optional name display)
  "Create a new Claude buffer with NAME and register it.
If NAME is nil, generate a name automatically.
If DISPLAY is non-nil, display the buffer.
Return the new buffer."
  (interactive)
  (let* ((buffer-name (or name
                        (format "*CLAUDE-CODE-%02d*"
                               (+ (length
                                   (ecc-buffer-get-registered-buffers))
                                  1))))
         (new-buffer (get-buffer-create buffer-name)))
    
    ;; Initialize the buffer with base properties
    (with-current-buffer new-buffer
      ;; Store original name property
      (set (make-local-variable 'ecc-original-name) buffer-name))
    
    ;; Register in both systems for compatibility
    (ecc-buffer-register-buffer new-buffer)
    
    ;; Register with UID system and set metadata for dashboard
    (let ((uid (ecc-buffer-registry-register-buffer-with-uid new-buffer)))
      (ecc-buffer-metadata-set uid 'name buffer-name)
      (ecc-buffer-metadata-set uid 'creation-time (current-time))
      (ecc-buffer-metadata-set uid 'last-access (current-time))
      (ecc-buffer-metadata-set uid 'status 'active))
    
    ;; Display the buffer if requested
    (when display
      (display-buffer new-buffer))
    
    ;; Return the new buffer
    new-buffer))

(defun ecc-buffer-create-display ()
  "Create a new Claude buffer and display it."
  (interactive)
  (ecc-buffer-create nil t))

(provide 'ecc-buffer-create)

(when
    (not load-file-name)
  (message "ecc-buffer-create.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))