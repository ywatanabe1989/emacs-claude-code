;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-07-01 03:22:45>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-remote.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:
;; Remote functionality for emacs-claude-code
;; Provides SSH host selection, remote file transfer, and yank-to-remote functionality
;; Leverages existing SSH configuration parsing infrastructure

(require 'vterm nil t)
(require 'ecc-vterm-yank-as-file)

;; 1. Configuration
;; ----------------------------------------

(defcustom ecc-directory-for-yank-as-file "~/claude-temp/"
  "Default directory for yank-as-file operations, both local and remote.
This directory will be used for storing temporary files from kill ring content."
  :type 'string
  :group 'ecc)

(defcustom ecc-remote-directory "~/claude-temp/"
  "Default directory for remote files.
Files will be transferred here when using remote functionality.
If nil, uses `ecc-directory-for-yank-as-file' instead."
  :type 'string
  :group 'ecc)

(defcustom ecc-remote-use-scp t
  "Whether to use scp for remote file transfer."
  :type 'boolean
  :group 'ecc)

(defcustom ecc-remote-scp-options "-o StrictHostKeyChecking=no"
  "Additional options for scp command."
  :type 'string
  :group 'ecc)

;; 2. Directory management
;; ----------------------------------------

(defun ecc-get-yank-directory (&optional remote-specific)
  "Get the appropriate directory for yank-as-file operations.

If REMOTE-SPECIFIC is non-nil, prioritizes ecc-remote-directory.
Otherwise, uses ecc-directory-for-yank-as-file as fallback."
  (if remote-specific
      (or ecc-remote-directory ecc-directory-for-yank-as-file)
    ecc-directory-for-yank-as-file))

;; 3. SSH host selection and management
;; ----------------------------------------

(defun ecc-select-host ()
  "Select an SSH host from the SSH configuration files in ~/.ssh/conf.d and ~/.ssh/config, with an option for localhost."
  (interactive)
  (let* ((command
          "rg 'Host' ~/.ssh/conf.d/*.conf ~/.ssh/config | grep -v 'HostName' | awk -F' ' '!/^#/ && !seen[$NF]++ {print $NF}' 2> /dev/null")
         (hosts
          (split-string (shell-command-to-string command) "\n" t))
         (host-list (cons "localhost" hosts)))
    (if host-list
        (completing-read "Choose host: " host-list)
      (progn
        (message "No hosts found")
        nil))))

(defun ecc-get-host-info (hostname)
  "Get username and hostname for the given host from SSH config files."
  (interactive)
  (let* ((command
          (format
           "awk '/^Host.*%s$/,/^$/ {if ($1 == \"User\") print $2; if ($1 == \"HostName\") print $2}' ~/.ssh/config ~/.ssh/conf.d/*.conf 2>/dev/null"
           hostname))
         (output (string-trim (shell-command-to-string command)))
         (lines (split-string output "\n" t))
         (username (if (> (length lines) 0) (car lines) "ywatanabe"))
         (real-hostname (if (> (length lines) 1) (cadr lines) hostname)))
    (list username real-hostname)))

;; 3. SSH context detection
;; ----------------------------------------

(defun ecc-detect-ssh-context ()
  "Detect if current vterm session is connected via SSH.

Returns alist with SSH connection info: ((user . \"username\") (host . \"hostname\") (port . \"port\"))
or nil if not in SSH context."
  (when (and (derived-mode-p 'vterm-mode)
             (boundp 'default-directory)
             (stringp default-directory))
    (let ((dir default-directory))
      (cond
       ;; TRAMP SSH format: /ssh:user@host#port:/path/
       ((string-match "^/ssh:\\([^@]+\\)@\\([^#:]+\\)\\(?:#\\([0-9]+\\)\\)?:" dir)
        `((user . ,(match-string 1 dir))
          (host . ,(match-string 2 dir))
          (port . ,(or (match-string 3 dir) "22"))))
       ;; TRAMP SCP format: /scp:user@host#port:/path/
       ((string-match "^/scp:\\([^@]+\\)@\\([^#:]+\\)\\(?:#\\([0-9]+\\)\\)?:" dir)
        `((user . ,(match-string 1 dir))
          (host . ,(match-string 2 dir))
          (port . ,(or (match-string 3 dir) "22"))))
       (t nil)))))

;; 4. Remote yank functionality
;; ----------------------------------------

(defun ecc-yank-to-remote-file (&optional select-host remote-dir)
  "Create a temporary file locally and transfer it to remote server via scp.

With one prefix arg (C-u), prompts for remote host selection.
With two prefix args (C-u C-u), prompts for both host and directory.
Without prefix args, auto-detects SSH context from current vterm session.

Example:
  ;; Copy some text to kill-ring
  ;; M-x ecc-yank-to-remote-file - auto-detect current SSH session
  ;; C-u M-x ecc-yank-to-remote-file - select host manually
  ;; C-u C-u M-x ecc-yank-to-remote-file - select host and directory"
  (interactive "P")
  (if (not (derived-mode-p 'vterm-mode))
      (message "This command only works in vterm-mode")
    (let ((content (--ecc-vterm-get-kill-ring-content)))
      (if (not content)
          (message "Kill ring is empty")
        (let* ((ssh-info (cond
                          ;; Manual host selection
                          ((equal select-host '(4))  ; C-u
                           (let ((hostname (ecc-select-host)))
                             (when (and hostname (not (equal hostname "localhost")))
                               (let ((host-info (ecc-get-host-info hostname)))
                                 `((user . ,(car host-info))
                                   (host . ,(cadr host-info))
                                   (port . "22"))))))
                          ;; Manual host and directory selection
                          ((equal select-host '(16))  ; C-u C-u
                           (let ((hostname (ecc-select-host)))
                             (when (and hostname (not (equal hostname "localhost")))
                               (let ((host-info (ecc-get-host-info hostname)))
                                 `((user . ,(car host-info))
                                   (host . ,(cadr host-info))
                                   (port . "22"))))))
                          ;; Auto-detect from current session
                          (t (ecc-detect-ssh-context))))
               (target-dir (if (equal select-host '(16))  ; C-u C-u
                             (read-string "Remote directory: " (ecc-get-yank-directory t))
                           (ecc-get-yank-directory t))))
          (if (not ssh-info)
              (message "Not in SSH context and no remote server selected")
            (let* ((local-file (--ecc-vterm-create-temp-file t))  ; Always use default dir for local creation
                   (remote-file (ecc-build-remote-file-path ssh-info local-file target-dir)))
              (--ecc-vterm-write-content-to-file content local-file)
              (if (ecc-transfer-file-to-remote local-file ssh-info target-dir)
                  (--ecc-vterm-send-read-command remote-file)
                (message "Failed to transfer file to remote server")))))))))

;; 5. File transfer functionality
;; ----------------------------------------

(defun ecc-build-remote-file-path (ssh-info local-file target-dir)
  "Build remote file path from SSH-INFO, LOCAL-FILE basename, and TARGET-DIR."
  (let* ((filename (file-name-nondirectory local-file))
         (remote-path (concat target-dir filename)))
    remote-path))

(defun ecc-transfer-file-to-remote (local-file ssh-info target-dir)
  "Transfer LOCAL-FILE to remote server using scp.

SSH-INFO contains connection details, TARGET-DIR is the remote directory.
Returns t on success, nil on failure."
  (when (and ecc-remote-use-scp ssh-info)
    (let* ((user (cdr (assoc 'user ssh-info)))
           (host (cdr (assoc 'host ssh-info)))
           (port (cdr (assoc 'port ssh-info)))
           (filename (file-name-nondirectory local-file))
           (remote-target (format "%s@%s:%s" user host target-dir))
           (scp-cmd (format "scp %s -P %s %s %s%s"
                           ecc-remote-scp-options
                           port
                           (shell-quote-argument local-file)
                           remote-target
                           filename)))
      (message "Transferring file to %s@%s:%s..." user host target-dir)
      ;; First ensure remote directory exists
      (let ((mkdir-cmd (format "ssh %s -p %s %s@%s 'mkdir -p %s'"
                              ecc-remote-scp-options
                              port user host
                              (shell-quote-argument target-dir))))
        (shell-command mkdir-cmd))
      ;; Then transfer the file
      (let ((result (shell-command scp-cmd)))
        (if (= result 0)
            (progn
              (message "File transferred successfully to %s" remote-target)
              t)
          (progn
            (message "SCP failed with exit code %d" result)
            nil))))))

;; 6. Cleanup functionality
;; ----------------------------------------

(defun ecc-cleanup-remote-temp-files (&optional hostname)
  "Clean up temporary files on remote server.

If HOSTNAME is provided, cleans up files on that server.
Otherwise prompts for server selection."
  (interactive)
  (let* ((selected-host (or hostname (ecc-select-host)))
         (host-info (when (and selected-host (not (equal selected-host "localhost")))
                      (ecc-get-host-info selected-host))))
    (if (not host-info)
        (message "No remote server selected or localhost selected")
      (let* ((user (car host-info))
             (host (cadr host-info))
             (cleanup-cmd (format "ssh %s -p 22 %s@%s 'find %s -name \"kill-ring-*.tmp\" -mtime +1 -delete'"
                                 ecc-remote-scp-options
                                 user host
                                 (shell-quote-argument (ecc-get-yank-directory t)))))
        (message "Cleaning up temporary files on %s@%s..." user host)
        (let ((result (shell-command cleanup-cmd)))
          (if (= result 0)
              (message "Cleanup completed successfully")
            (message "Cleanup failed with exit code %d" result)))))))

;; 7. Keybinding integration
;; ----------------------------------------

(defun ecc-remote-setup-keybindings ()
  "Set up keybindings for remote functionality in vterm-mode."
  (when (fboundp 'vterm-mode-map)
    (define-key vterm-mode-map (kbd "C-c C-r") 'ecc-yank-to-remote-file)
    (define-key vterm-mode-map (kbd "C-c C-S-r") 'ecc-cleanup-remote-temp-files)))

;; Set up keybindings when vterm is loaded
(with-eval-after-load 'vterm
  (ecc-remote-setup-keybindings))


(provide 'ecc-remote)

(when
    (not load-file-name)
  (message "ecc-remote.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))