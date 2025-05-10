;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 00:13:40>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/ecc-buffer/ecc-buffer-current.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;; Current
;; ------------------------------

(defvar ecc-buffer-current-buffer nil
  "The current Claude buffer being used.")

(defun ecc-buffer-set-current-buffer (buf)
  "Set BUF as the current Claude buffer.
Return the set buffer or nil if buffer is invalid."
  (when buf
    (let ((buffer (if (bufferp buf) buf (get-buffer buf))))
      (when (and buffer (buffer-live-p buffer))
        (ecc-buffer-register-buffer buffer)
        (setq ecc-buffer-current-buffer buffer)
        buffer))))

(defun ecc-buffer-get-current-buffer ()
  "Get the current Claude buffer.
Return nil if no valid buffer is set."
  (when (and ecc-buffer-current-buffer
             (buffer-live-p ecc-buffer-current-buffer))
    ecc-buffer-current-buffer))

(defun ecc-buffer-get-current-buffer-state ()
  "Get the state of the current Claude buffer.
Return nil if no valid buffer is set or buffer has no state."
  (when-let ((current-buffer (ecc-buffer-get-current-buffer)))
    (ecc-buffer-get-buffer-state current-buffer)))

(defun ecc-buffer-set-current-buffer-state (state)
  "Set the state of the current Claude buffer to STATE.
Return t if state was set successfully, nil otherwise."
  (when-let ((current-buffer (ecc-buffer-get-current-buffer)))
    (ecc-buffer-set-buffer-state current-buffer state)))

;; (defun ecc-buffer-activate-buffer (buf)
;;   "Activate BUF by making it current and displaying it.
;; Return the activated buffer."
;;   (when buf
;;     (let ((buffer (if (bufferp buf) buf (get-buffer buf))))
;;       (when (and buffer (buffer-live-p buffer))
;;         (ecc-buffer-set-current-buffer buffer)
;;         (display-buffer buffer)
;;         buffer))))

;; (defun ecc-buffer-deactivate-buffer (buf)
;;   "Deactivate BUF if it is the current active buffer.
;; Return t if buffer was deactivated, nil otherwise."
;;   (when buf
;;     (let ((buffer (if (bufferp buf) buf (get-buffer buf))))
;;       (when (and buffer
;;                  (eq buffer ecc-buffer-current-active-buffer))
;;         (setq ecc-buffer-current-active-buffer nil)
;;         t))))

;; ;; 1. Public interface functions
;; ;; ----------------------------------------

;; ;;;###autoload
;; (defun ecc-buffer-create (&optional new-buffer-name working-dir)
;;   "Create a new Claude buffer and register it as
;; current active buffer. If WORKING-DIR is provided, start Claude in that directory."
;;   (interactive)
;;   (let* ((original-buffer (current-buffer))
;;          (original-point (point))
;;          (buffer-name
;;           (or new-buffer-name
;;               (format "*CLAUDE-CODE-%02d*"
;;                       (+ (length ecc-buffer-registered-buffers) 1))))
;;          (directory (or working-dir default-directory))
;;          (new-buffer (get-buffer-create buffer-name)))
;;     (switch-to-buffer-other-window new-buffer)
;;     (with-current-buffer new-buffer
;;       (vterm-mode)
;;       (set (make-local-variable 'ecc-original-name)
;;            buffer-name)
;;       (add-hook 'after-change-functions
;;                 (lambda (&rest _)
;;                   (--ecc-buffer-name-change-hook))
;;                 nil t)
;;       (sit-for 0.5)
;;       (when (and working-dir (file-directory-p directory))
;;         (vterm-send-string (format "cd %s" directory))
;;         (vterm-send-return)
;;         (sit-for 0.2))
;;       (vterm-send-string "claude")
;;       (vterm-send-return))
;;     (ecc-buffer-register-as-active new-buffer)))

;; (defun ecc-buffer-get-current-active-buffer ()
;;   "Check if the active Claude buffer exists and is in vterm-mode."
;;   (if (and (buffer-live-p ecc-buffer-current-active-buffer)
;;            (with-current-buffer ecc-buffer-current-active-buffer
;;              (derived-mode-p 'vterm-mode)))
;;       ecc-buffer-current-active-buffer
;;     nil))

;; ;;;###autoload
;; (defun ecc-buffer-get-or-create-active-buffer ()
;;   "Get or create Claude active buffer."
;;   (unless (ecc-buffer-get-current-active-buffer)
;;     (setq ecc-buffer-current-active-buffer
;;           (get-buffer-create ecc-buffer-name))
;;     (with-current-buffer ecc-buffer-current-active-buffer
;;       (unless (derived-mode-p 'vterm-mode)
;;         (vterm-mode))
;;       (rename-buffer ecc-buffer-name)
;;       (sit-for 0.5)
;;       (vterm-send-string "claude")
;;       (vterm-send-return)))
;;   (display-buffer ecc-buffer-current-active-buffer)
;;   (ecc-buffer-unregister-buffer ecc-buffer-current-active-buffer)
;;   ecc-buffer-current-active-buffer)

;; ;; ;;;###autoload
;; ;; (defun ecc-buffer-get-or-create-active-buffer ()
;; ;;   "Get or create Claude active buffer."
;; ;;   (unless (and (buffer-live-p ecc-buffer-current-active-buffer)
;; ;;                (with-current-buffer ecc-buffer-current-active-buffer
;; ;;                  (derived-mode-p 'vterm-mode)))
;; ;;     (setq ecc-buffer-current-active-buffer
;; ;;           (get-buffer-create ecc-buffer-name))
;; ;;     (with-current-buffer ecc-buffer-current-active-buffer
;; ;;       (unless (derived-mode-p 'vterm-mode)
;; ;;         (vterm-mode))
;; ;;       (rename-buffer ecc-buffer-name)
;; ;;       (sit-for 0.5)
;; ;;       (vterm-send-string "claude")
;; ;;       (vterm-send-return)))
;; ;;   (display-buffer ecc-buffer-current-active-buffer)
;; ;;   (ecc-buffer-unregister-buffer ecc-buffer-current-active-buffer)
;; ;;   ecc-buffer-current-active-buffer)

;; ;;;###autoload
;; (defun ecc-buffer-list-buffers ()
;;   "List all registered Claude buffers."
;;   (interactive)
;;   (let ((buffers (cl-remove-if-not 'buffer-live-p ecc-buffer-registered-buffers))
;;         (buffer-names
;;          (mapcar (lambda (buf)
;;                    (format "%s%s"
;;                            (if (eq buf ecc-buffer-current-active-buffer)
;;                                "* "
;;                              "  ")
;;                            (buffer-name buf)))
;;                  ecc-buffer-registered-buffers)))
;;     (setq ecc-buffer-registered-buffers buffers)
;;     (message "Claude buffers:\n%s"
;;              (mapconcat 'identity buffer-names "\n"))))

;; ;;;###autoload
;; (defun ecc-buffer-unregister-buffer (buffer-or-name)
;;   "Unregister a buffer from Claude buffers."
;;   (interactive
;;    (list (completing-read "Unregister Claude buffer: "
;;                           (mapcar 'buffer-name ecc-buffer-registered-buffers))))
;;   (let ((buffer (get-buffer buffer-or-name)))
;;     (when (member buffer ecc-buffer-registered-buffers)
;;       (setq ecc-buffer-registered-buffers (delq buffer ecc-buffer-registered-buffers))
;;       (when (eq buffer ecc-buffer-current-active-buffer)
;;         (setq ecc-buffer-current-active-buffer
;;               (car ecc-buffer-registered-buffers)))
;;       (ecc-update-mode-line-all-buffers)
;;       (message "Unregistered %s from Claude buffers"
;;                (buffer-name buffer)))))

;; ;;;###autoload
;; (defun ecc-buffer-cleanup-buffer-registry ()
;;   "Remove dead buffers from registry `ecc-buffer-registered-buffers'."
;;   (setq ecc-buffer-registered-buffers
;;         (cl-remove-if-not 'buffer-live-p ecc-buffer-registered-buffers))
;;   (unless (and (buffer-live-p ecc-buffer-current-active-buffer)
;;                (member ecc-buffer-current-active-buffer ecc-buffer-registered-buffers))
;;     (setq ecc-buffer-current-active-buffer (car ecc-buffer-registered-buffers))))

;; ;; 2. Buffer management functions
;; ;; ----------------------------------------

;; ;;;###autoload
;; (defun ecc-buffer-register-as-active (buffer-or-name)
;;   "Set BUFFER-OR-NAME as `ecc-buffer-current-active-buffer'."
;;   (interactive
;;    (list (completing-read "Switch active buffer to: "
;;                           (mapcar 'buffer-name ecc-buffer-registered-buffers))))
;;   (let ((buffer (get-buffer buffer-or-name)))
;;     (when (and (buffer-live-p buffer)
;;                (member buffer ecc-buffer-registered-buffers))
;;       (setq ecc-buffer-current-active-buffer buffer)
;;       (ecc-update-mode-line-all-buffers)
;;       (message "Switched active Claude buffer to %s"
;;                (buffer-name buffer)))))

;; ;; 3. Helper functions and variables
;; ;; ----------------------------------------

;; (defvar ecc-buffer-timestamps (make-hash-table :test 'equal)
;;   "Hash table mapping Claude buffers to their creation timestamps.")

;; (defun --ecc-buffer-name-change-hook ()
;;   "Hook to prevent Claude from changing the buffer name."
;;   (let ((current-name (buffer-name))
;;         (original-name
;;          (buffer-local-value 'ecc-original-name
;;                              (current-buffer))))
;;     (when (and original-name
;;                (not (string= current-name original-name)))
;;       (rename-buffer original-name t))))

;; ;; Hooks
;; ;; ----------------------------------------
;; ;;;###autoload
;; (defun ecc-buffer-setup-cleanup-hooks ()
;;   "Setup hooks to handle Claude buffer deletion or session exit."
;;   (add-hook 'kill-buffer-hook 'ecc-buffer-cleanup-hook)
;;   (add-hook 'vterm-exit-functions 'ecc-session-exit-hook))

;; ;;;###autoload
;; (defun ecc-buffer-cleanup-hook ()
;;   "Hook that runs when a buffer is being killed.
;; Performs cleanup if the buffer is a Claude buffer."
;;   (when (member (current-buffer) ecc-buffer-registered-buffers)
;;     (ecc-buffer-unregister-buffer (current-buffer))
;;     (ecc-buffer-cleanup-buffer-registry)))

;; ;;;###autoload
;; (defun ecc-session-exit-hook (buffer exit-code)
;;   "Hook that runs when a vterm process exits.
;; BUFFER is the vterm buffer where the process exited.
;; EXIT-CODE is the exit code of the process."
;;   (when (member buffer ecc-buffer-registered-buffers)
;;     (ecc-buffer-unregister-buffer buffer)
;;     (ecc-buffer-cleanup-buffer-registry)))

;; ;; Initialize the hooks
;; (ecc-buffer-setup-cleanup-hooks)

;; (provide 'ecc-buffer-registry)
;; (when
;;     (not load-file-name)
;;   (message "ecc-buffer-registry.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))


(provide 'ecc-buffer/ecc-buffer-current)

(when
    (not load-file-name)
  (message "ecc-buffer-current.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))