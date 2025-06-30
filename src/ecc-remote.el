;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-07-01 05:04:09>
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

(defun --ecc-ensure-directory-exists (directory &optional ssh-info)
  "Create DIRECTORY if it doesn't exist.

If SSH-INFO is provided, creates directory on remote host via SSH.
SSH-INFO should be an alist with keys: user, host, port."
  (if ssh-info
      ;; Remote directory creation
      (let* ((user (cdr (assoc 'user ssh-info)))
             (host (cdr (assoc 'host ssh-info)))
             (port (cdr (assoc 'port ssh-info)))
             (mkdir-cmd (format "ssh %s -p %s %s@%s 'mkdir -p %s'"
                                ecc-remote-scp-options
                                port user host
                                directory))
                                        ; Don't quote - allow tilde expansion
             (check-cmd (format "ssh %s -p %s %s@%s 'test -d %s'"
                                ecc-remote-scp-options
                                port user host
                                (shell-quote-argument directory))))
        (message "Creating remote directory: %s@%s:%s" user host
                 directory)
        (message "SSH command: %s" mkdir-cmd)
        (let ((result (shell-command mkdir-cmd)))
          (if (not (= result 0))
              (message
               "Warning: Failed to create remote directory (exit code %d)"
               result)
            (message "Remote directory created successfully")))))
  ;; Local directory creation
  (unless (file-exists-p directory)
    (make-directory directory t)))

                                        ; Directory configuration unified - using ecc-directory-for-yank-as-file for both local and remote

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

(defun --ecc-get-yank-directory (&optional remote-specific)
  "Get the appropriate directory for yank-as-file operations.

Always uses ecc-directory-for-yank-as-file for both local and remote.
Creates directory if it doesn't exist (for local operations only)."
  (let ((dir ecc-directory-for-yank-as-file))
    (when (not remote-specific)
      (let ((expanded-dir (expand-file-name dir)))
        (--ecc-ensure-directory-exists expanded-dir)
        (setq dir expanded-dir)))
    dir))

;; 3. SSH host selection and management
;; ----------------------------------------

(defun --ecc-select-host ()
  "Select an SSH host from the SSH configuration files in ~/.ssh/conf.d and ~/.ssh/config, with an option for localhost.
Filters out git-specific hosts (github.com, gitlab.com, etc.) that are not suitable for file transfer."
  (interactive)
  (let* ((command
          "rg 'Host' ~/.ssh/conf.d/*.conf ~/.ssh/config 2>/dev/null | grep -v 'HostName' | grep -v '^#' | awk '{for(i=2;i<=NF;i++) print $i}' | grep -v '^[#*]' | grep -v '^Host$' | sort -u")
         (all-hosts
          (split-string (shell-command-to-string command) "\n" t))
         ;; Filter out git-specific hosts
         (filtered-hosts
          (seq-filter (lambda (host)
                        (not (string-match-p
                              "^\\(github\\|gitlab\\|bitbucket\\|codeberg\\)\\(-\\|\\.\\)\\|^git$"
                              host)))
                      all-hosts))
         (host-list (cons "localhost" filtered-hosts)))
    (if host-list
        (completing-read "Choose host: " host-list)
      (progn
        (message "No hosts found")
        nil))))

(defun --ecc-get-host-info (hostname)
  "Get username and hostname for the given host from SSH config files."
  (interactive)
  (let* ((user-command
          (format
           "awk '/^Host.*\\b%s\\b/,/^Host / {if ($1 == \"User\" && !found_user) {print $2; found_user=1}}' ~/.ssh/config ~/.ssh/conf.d/*.conf 2>/dev/null"
           hostname))
         (hostname-command
          (format
           "awk '/^Host.*\\b%s\\b/,/^Host / {if ($1 == \"HostName\" && !found_host) {print $2; found_host=1}}' ~/.ssh/config ~/.ssh/conf.d/*.conf 2>/dev/null"
           hostname))
         (port-command
          (format
           "awk '/^Host.*\\b%s\\b/,/^Host / {if ($1 == \"Port\" && !found_port) {print $2; found_port=1}}' ~/.ssh/config ~/.ssh/conf.d/*.conf 2>/dev/null"
           hostname))
         (username
          (string-trim (shell-command-to-string user-command)))
         (real-hostname
          (string-trim (shell-command-to-string hostname-command)))
         (port (string-trim (shell-command-to-string port-command))))
    (list (if (string-empty-p username) (user-login-name) username)
          (if (string-empty-p real-hostname) hostname real-hostname)
          (if (string-empty-p port) "22" port))))

;; 4. SSH context detection
;; ----------------------------------------

(defun --ecc-get-ssh-info-from-selection ()
  "Get SSH info by prompting user to select host.
Returns nil for localhost/cancelled selection, ssh-info alist for remote hosts."
  (let ((hostname (--ecc-select-host)))
    (cond
     ;; No selection, empty string, or localhost - return nil
     ((or (not hostname) (equal hostname "")
          (equal hostname "localhost"))
      nil)
     ;; Remote host selected
     (t
      (let ((host-info (--ecc-get-host-info hostname)))
        `((user . ,(car host-info))
          (host . ,(cadr host-info))
          (port . ,(caddr host-info))))))))

;; 5. SSH context detection
;; ----------------------------------------

(defun --ecc-detect-ssh-context ()
  "Detect if current vterm session is connected via SSH.

Returns alist with SSH connection info: ((user . \"username\") (host . \"hostname\") (port . \"port\"))
or nil if not in SSH context."
  (when (and (derived-mode-p 'vterm-mode)
             (boundp 'default-directory)
             (stringp default-directory))
    (let ((dir default-directory))
      (cond
       ;; TRAMP SSH format: /ssh:user@host#port:/path/
       ((string-match
         "^/ssh:\\([^@]+\\)@\\([^#:]+\\)\\(?:#\\([0-9]+\\)\\)?:" dir)
        `((user . ,(match-string 1 dir))
          (host . ,(match-string 2 dir))
          (port . ,(or (match-string 3 dir) "22"))))
       ;; TRAMP SCP format: /scp:user@host#port:/path/
       ((string-match
         "^/scp:\\([^@]+\\)@\\([^#:]+\\)\\(?:#\\([0-9]+\\)\\)?:" dir)
        `((user . ,(match-string 1 dir))
          (host . ,(match-string 2 dir))
          (port . ,(or (match-string 3 dir) "22"))))
       (t nil)))))

;; 6. Remote yank functionality
;; ----------------------------------------

(defun --ecc-yank-to-remote-file (&optional select-host remote-dir)
  "Create a temporary file locally and transfer it to remote server via scp.

Always prompts for remote host selection.
With one prefix arg (C-u), prompts for remote host selection only.
With two prefix args (C-u C-u), prompts for both host and directory.
Without prefix args, uses default remote directory.

Example:
  ;; Copy some text to kill-ring
  ;; M-x ecc-yank-to-remote-file - select host, use default directory
  ;; C-u M-x ecc-yank-to-remote-file - select host, use default directory
  ;; C-u C-u M-x ecc-yank-to-remote-file - select host and directory"
  (interactive "P")
  (if (not (derived-mode-p 'vterm-mode))
      (message "This command only works in vterm-mode")
    (let ((content (--ecc-vterm-get-kill-ring-content)))
      (if (not content)
          (message "Kill ring is empty")
        (let* ((ssh-info (--ecc-get-ssh-info-from-selection)) ; Always prompt for host selection
               (target-dir (if (equal select-host '(16))  ; C-u C-u
                               (read-string "Remote directory: "
                                            (--ecc-get-yank-directory t))
                             (--ecc-get-yank-directory t))))
          (if (not ssh-info)
              (message "No remote server selected")
            (let* ((local-file (--ecc-vterm-create-temp-file t))  ; Always use default dir for local creation
                   (remote-file
                    (--ecc-build-remote-file-path ssh-info local-file
                                                target-dir)))
              (--ecc-vterm-write-content-to-file content local-file)
              (if
                  (--ecc-transfer-file-to-remote local-file ssh-info
                                               target-dir)
                  (--ecc-vterm-send-read-command remote-file)
                (message "Failed to transfer file to remote server")))))))))

;; 7. File transfer functionality
;; ----------------------------------------

(defun --ecc-build-remote-file-path (ssh-info local-file target-dir)
  "Build remote file path from SSH-INFO, LOCAL-FILE basename, and TARGET-DIR."
  (let* ((filename (file-name-nondirectory local-file))
         (remote-path (concat target-dir filename)))
    remote-path))

(defun --ecc-transfer-file-to-remote (local-file ssh-info target-dir)
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
      (--ecc-ensure-directory-exists target-dir ssh-info)
      ;; Then transfer the file
      (let ((result (shell-command scp-cmd)))
        (if (= result 0)
            (progn
              (message "File transferred successfully to %s"
                       remote-target)
              t)
          (progn
            (message "SCP failed with exit code %d" result)
            nil))))))

;; 8. Cleanup functionality
;; ----------------------------------------

(defun --ecc-cleanup-remote-temp-files (&optional hostname)
  "Clean up temporary files on remote server.

If HOSTNAME is provided, cleans up files on that server.
Otherwise prompts for server selection."
  (interactive)
  (let* ((selected-host (or hostname (--ecc-select-host)))
         (host-info (when
                        (and selected-host
                             (not (equal selected-host "localhost")))
                      (--ecc-get-host-info selected-host))))
    (if (not host-info)
        (message "No remote server selected or localhost selected")
      (let* ((user (car host-info))
             (host (cadr host-info))
             (cleanup-cmd (format
                           "ssh %s -p 22 %s@%s 'find %s -name \"kill-ring-*.tmp\" -mtime +1 -delete'"
                           ecc-remote-scp-options
                           user host
                           (shell-quote-argument
                            (--ecc-get-yank-directory t)))))
        (message "Cleaning up temporary files on %s@%s..." user host)
        (let ((result (shell-command cleanup-cmd)))
          (if (= result 0)
              (message "Cleanup completed successfully")
            (message "Cleanup failed with exit code %d" result)))))))

;; 9. Keybinding integration
;; ----------------------------------------

(defun --ecc-remote-setup-keybindings ()
  "Set up keybindings for remote functionality in vterm-mode."
  (when (fboundp 'vterm-mode-map)
    (define-key vterm-mode-map (kbd "C-c C-y")
                'ecc-vterm-yank-as-file)
    (define-key vterm-mode-map (kbd "C-c C-S-r")
                '--ecc-cleanup-remote-temp-files)))

;; Set up keybindings when vterm is loaded
(with-eval-after-load 'vterm
  (--ecc-remote-setup-keybindings))

(provide 'ecc-remote)

(when
    (not load-file-name)
  (message "ecc-remote.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))