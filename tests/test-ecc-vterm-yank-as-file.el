;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-06-04 06:42:45>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-vterm-yank-as-file.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'cl-lib)
(require 'vterm nil t)
(require 'ecc-vterm-yank-as-file)
(require 'ecc-remote nil t)

(ert-deftest test-ecc-vterm-yank-as-file-loadable ()
  "Test if ecc-vterm-yank-as-file is loadable."
  (should (featurep 'ecc-vterm-yank-as-file)))

(ert-deftest test-ecc-vterm-create-temp-file-returns-string ()
  "Test temp file creation function returns string type."
  (let ((filepath (--ecc-create-temp-file)))
    (should (stringp filepath))))

(ert-deftest test-ecc-vterm-create-temp-file-follows-naming-pattern ()
  "Test temp file creation function follows kill-ring naming pattern."
  (let ((filepath (--ecc-create-temp-file)))
    (should (string-match-p "kill-ring-[0-9]+-[0-9]+\\.tmp$" filepath))))

(ert-deftest test-ecc-vterm-get-kill-ring-content-empty ()
  "Test getting content from empty kill-ring."
  (let ((kill-ring nil))
    (should-not (--ecc-get-kill-ring-content))))

(ert-deftest test-ecc-vterm-get-kill-ring-content-with-content ()
  "Test getting content from populated kill-ring."
  (let ((kill-ring '("test content" "older content")))
    (should (equal (--ecc-get-kill-ring-content) "test content"))))

(ert-deftest test-ecc-vterm-write-content-to-file-creates-file ()
  "Test writing content to file creates the file."
  (let* ((temp-file (make-temp-file "ecc-test-"))
         (test-content "Hello, World!"))
    (unwind-protect
        (progn
          (--ecc-write-content-to-file test-content temp-file)
          (should (file-exists-p temp-file)))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest test-ecc-vterm-write-content-to-file-writes-correct-content ()
  "Test writing content to file writes the correct content."
  (let* ((temp-file (make-temp-file "ecc-test-"))
         (test-content "Hello, World!"))
    (unwind-protect
        (progn
          (--ecc-write-content-to-file test-content temp-file)
          (should (equal (with-temp-buffer
                          (insert-file-contents temp-file)
                          (buffer-string))
                        test-content)))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest test-ecc-vterm-yank-as-file-interactive ()
  "Test that ecc-vterm-yank-as-file is an interactive command."
  (should (commandp 'ecc-vterm-yank-as-file)))

;; Tests for ecc-yank-to-remote-file functionality
;; ----------------------------------------

(ert-deftest test-ecc-yank-to-remote-file-interactive ()
  "Test that ecc-yank-to-remote-file is an interactive command."
  (should (commandp '--ecc-yank-to-remote-file)))

(ert-deftest test---ecc-detect-ssh-context-tramp-ssh-extracts-user ()
  "Test SSH context detection extracts correct user from TRAMP SSH format."
  (cl-letf (((symbol-function 'derived-mode-p) (lambda (mode) t)))
    (let ((default-directory "/ssh:user@example.com:/home/user/"))
      (let ((result (--ecc-detect-ssh-context)))
        (should (equal (cdr (assoc 'user result)) "user"))))))

(ert-deftest test---ecc-detect-ssh-context-tramp-ssh-extracts-host ()
  "Test SSH context detection extracts correct host from TRAMP SSH format."
  (cl-letf (((symbol-function 'derived-mode-p) (lambda (mode) t)))
    (let ((default-directory "/ssh:user@example.com:/home/user/"))
      (let ((result (--ecc-detect-ssh-context)))
        (should (equal (cdr (assoc 'host result)) "example.com"))))))

(ert-deftest test---ecc-detect-ssh-context-tramp-ssh-sets-default-port ()
  "Test SSH context detection sets default port from TRAMP SSH format."
  (cl-letf (((symbol-function 'derived-mode-p) (lambda (mode) t)))
    (let ((default-directory "/ssh:user@example.com:/home/user/"))
      (let ((result (--ecc-detect-ssh-context)))
        (should (equal (cdr (assoc 'port result)) "22"))))))

(ert-deftest test---ecc-detect-ssh-context-tramp-ssh-with-port-extracts-user ()
  "Test SSH context detection extracts user from TRAMP SSH format with port."
  (cl-letf (((symbol-function 'derived-mode-p) (lambda (mode) t)))
    (let ((default-directory "/ssh:testuser@example.org#2222:/home/testuser/"))
      (let ((result (--ecc-detect-ssh-context)))
        (should (equal (cdr (assoc 'user result)) "testuser"))))))

(ert-deftest test---ecc-detect-ssh-context-tramp-ssh-with-port-extracts-host ()
  "Test SSH context detection extracts host from TRAMP SSH format with port."
  (cl-letf (((symbol-function 'derived-mode-p) (lambda (mode) t)))
    (let ((default-directory "/ssh:testuser@example.org#2222:/home/testuser/"))
      (let ((result (--ecc-detect-ssh-context)))
        (should (equal (cdr (assoc 'host result)) "example.org"))))))

(ert-deftest test---ecc-detect-ssh-context-tramp-ssh-with-port-extracts-port ()
  "Test SSH context detection extracts custom port from TRAMP SSH format."
  (cl-letf (((symbol-function 'derived-mode-p) (lambda (mode) t)))
    (let ((default-directory "/ssh:testuser@example.org#2222:/home/testuser/"))
      (let ((result (--ecc-detect-ssh-context)))
        (should (equal (cdr (assoc 'port result)) "2222"))))))

(ert-deftest test---ecc-detect-ssh-context-tramp-scp-extracts-user ()
  "Test SSH context detection extracts user from TRAMP SCP format."
  (cl-letf (((symbol-function 'derived-mode-p) (lambda (mode) t)))
    (let ((default-directory "/scp:admin@server.example.com:/opt/"))
      (let ((result (--ecc-detect-ssh-context)))
        (should (equal (cdr (assoc 'user result)) "admin"))))))

(ert-deftest test---ecc-detect-ssh-context-tramp-scp-extracts-host ()
  "Test SSH context detection extracts host from TRAMP SCP format."
  (cl-letf (((symbol-function 'derived-mode-p) (lambda (mode) t)))
    (let ((default-directory "/scp:admin@server.example.com:/opt/"))
      (let ((result (--ecc-detect-ssh-context)))
        (should (equal (cdr (assoc 'host result)) "server.example.com"))))))

(ert-deftest test---ecc-detect-ssh-context-tramp-scp-sets-default-port ()
  "Test SSH context detection sets default port from TRAMP SCP format."
  (cl-letf (((symbol-function 'derived-mode-p) (lambda (mode) t)))
    (let ((default-directory "/scp:admin@server.example.com:/opt/"))
      (let ((result (--ecc-detect-ssh-context)))
        (should (equal (cdr (assoc 'port result)) "22"))))))

(ert-deftest test---ecc-detect-ssh-context-local ()
  "Test SSH context detection with local directory."
  (cl-letf (((symbol-function 'derived-mode-p) (lambda (mode) t)))
    (let ((default-directory "/home/user/local-project/"))
      (should-not (--ecc-detect-ssh-context)))))

(ert-deftest test---ecc-build-remote-file-path ()
  "Test building remote file path."
  (let ((ssh-info '((user . "testuser") (host . "example.com") (port . "22")))
        (local-file "/tmp/kill-ring-20250701-123456.tmp")
        (target-dir "~/claude-temp/"))
    (let ((result (--ecc-build-remote-file-path ssh-info local-file target-dir)))
      (should (equal result "~/claude-temp/kill-ring-20250701-123456.tmp")))))

(ert-deftest test---ecc-transfer-file-to-remote-disabled-when-scp-disabled ()
  "Test that file transfer is skipped when ecc-remote-use-scp is nil."
  (let ((ecc-remote-use-scp nil)
        (ssh-info '((user . "testuser") (host . "example.com") (port . "22"))))
    (should-not (--ecc-transfer-file-to-remote "/tmp/test.txt" ssh-info "~/claude-temp/"))))

(ert-deftest test---ecc-transfer-file-to-remote-disabled-when-no-ssh-info ()
  "Test that file transfer is skipped when no SSH info is provided."
  (let ((ecc-remote-use-scp t))
    (should-not (--ecc-transfer-file-to-remote "/tmp/test.txt" nil "~/claude-temp/"))))

(ert-deftest test---ecc-ensure-directory-exists-local ()
  "Test local directory creation without SSH info."
  (let ((test-dir (make-temp-file "ecc-test-dir-" t)))
    (unwind-protect
        (progn
          ;; Remove the directory to test creation
          (delete-directory test-dir)
          (should-not (file-exists-p test-dir))
          ;; Test local directory creation
          (--ecc-ensure-directory-exists test-dir)
          (should (file-exists-p test-dir)))
      (when (file-exists-p test-dir)
        (delete-directory test-dir)))))

(ert-deftest test---ecc-ensure-directory-exists-remote-with-ssh-info ()
  "Test remote directory creation with SSH info provided."
  (let ((ssh-info '((user . "testuser") (host . "example.com") (port . "22")))
        (test-dir "/tmp/remote-test-dir")
        (shell-command-called nil)
        (shell-command-args nil))
    ;; Mock shell-command to avoid actual SSH execution
    (cl-letf (((symbol-function 'shell-command)
               (lambda (command)
                 (setq shell-command-called t)
                 (setq shell-command-args command)
                 0))) ; Return success exit code
      ;; Test that function completes without error
      (should-not (--ecc-ensure-directory-exists test-dir ssh-info))
      ;; Verify shell-command was called with correct SSH command
      (should shell-command-called)
      (should (string-match-p "ssh.*testuser@example.com.*mkdir -p" shell-command-args)))))

(provide 'test-ecc-vterm-yank-as-file)

(when (not load-file-name)
  (message "test-ecc-vterm-yank-as-file.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))