;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-06-04 08:47:04>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-host-switch.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'cl-lib)

;; 1. Variables
;; ----------------------------------------

(defvar ecc-ssh-hosts-cache nil
  "Cache of SSH hosts from config files.")

;; 2. SSH Config Parsing
;; ----------------------------------------

(defun --ecc-parse-ssh-config ()
  "Parse SSH config files for available hosts."
  (let ((hosts '())
        (config-files (append
                       (list "~/.ssh/config")
                       (when (file-directory-p "~/.ssh/conf.d")
                         (directory-files "~/.ssh/conf.d" t "\\.conf$")))))
    (dolist (file config-files)
      (when (file-readable-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward "^Host\\s-+\\(.+\\)$" nil t)
            (let ((host-spec (match-string 1)))
              ;; Skip wildcards and special entries
              (unless (string-match-p "[*?]" host-spec)
                (dolist (host (split-string host-spec))
                  (push host hosts))))))))
    (delete-dups hosts)))

;; 3. Host Selection
;; ----------------------------------------

(defun ecc-switch-host ()
  "Switch to a different host/machine with completion."
  (interactive)
  (unless ecc-ssh-hosts-cache
    (setq ecc-ssh-hosts-cache (--ecc-parse-ssh-config)))
  (let* ((hosts (append '("localhost" "local") ecc-ssh-hosts-cache))
         (current-host (or (--ecc-get-current-host) "local"))
         (selected (completing-read
                    (format "Switch to host (current: %s): "
                            current-host)
                    hosts nil nil nil nil current-host)))
    (--ecc-connect-to-host selected)))

(defun --ecc-get-current-host ()
  "Get current host from vterm or buffer context."
  (cond
   ;; Check if in vterm
   ((derived-mode-p 'vterm-mode)
    (let ((prompt (or (bound-and-true-p vterm--prompt) "")))
      (when (string-match "@\\([^:]+\\)" prompt)
        (match-string 1 prompt))))
   ;; Check default-directory for TRAMP
   ((file-remote-p default-directory)
    (file-remote-p default-directory 'host))
   (t nil)))

(defun --ecc-connect-to-host (host)
  "Connect to HOST in a new vterm."
  (let ((buffer-name (format "*Claude-vterm-%s*" host)))
    (if (string-match-p "^\\(localhost\\|local\\)$" host)
        ;; Local connection
        (progn
          (vterm buffer-name)
          (message "Switched to local machine"))
      ;; SSH connection
      (let ((default-directory "/"))
        (vterm buffer-name)
        (vterm-send-string (format "ssh %s" host))
        (vterm-send-return)
        (--ecc-debug-message "Connecting to %s..." host)))
    ;; Enable auto-response if it was enabled
    (when (bound-and-true-p ecc-auto-response-global-mode)
      (--ecc-auto-response-enable-buffer))))

;; 4. Quick switch commands
;; ----------------------------------------

(defun ecc-switch-to-local ()
  "Quick switch to local machine."
  (interactive)
  (--ecc-connect-to-host "local"))

(defun ecc-refresh-ssh-hosts ()
  "Refresh the SSH hosts cache."
  (interactive)
  (setq ecc-ssh-hosts-cache (--ecc-parse-ssh-config))
  (--ecc-debug-message "SSH hosts cache refreshed: %d hosts found"
                       (length ecc-ssh-hosts-cache)))

(when (not load-file-name)
  (--ecc-debug-message "ecc-host-switch.el loaded."
                       (file-name-nondirectory
                        (or load-file-name buffer-file-name))))


(provide 'ecc-host-switch)

(when
    (not load-file-name)
  (message "ecc-host-switch.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))