;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-18 00:00:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-remote.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

(require 'ert)
(require 'cl-lib)
(require 'ecc-remote)

;; Tests for module loading
;; ----------------------------------------

(ert-deftest test-ecc-remote-loadable ()
  "Test if ecc-remote is loadable without circular dependency error."
  (should (featurep 'ecc-remote)))

(ert-deftest test-ecc-remote-defines-ssh-info-function ()
  "Test that --ecc-get-ssh-info-from-selection is defined."
  (should (fboundp '--ecc-get-ssh-info-from-selection)))

(ert-deftest test-ecc-remote-defines-directory-variable ()
  "Test that ecc-directory-for-yank-as-file is defined in ecc-remote."
  (should (boundp 'ecc-directory-for-yank-as-file))
  (should (stringp ecc-directory-for-yank-as-file)))

(ert-deftest test-ecc-remote-does-not-require-vterm-yank ()
  "Test that ecc-remote does not have circular dependency on ecc-vterm-yank-as-file.
Loading ecc-remote should not require ecc-vterm-yank-as-file."
  ;; This test verifies the fix: ecc-remote.el should NOT require ecc-vterm-yank-as-file
  ;; If there was still a circular dependency, loading would fail before reaching this test
  (should (featurep 'ecc-remote)))

;; Tests for configuration variables
;; ----------------------------------------

(ert-deftest test-ecc-remote-use-scp-default ()
  "Test default value of ecc-remote-use-scp."
  (should (boundp 'ecc-remote-use-scp))
  (should (eq ecc-remote-use-scp t)))

(ert-deftest test-ecc-remote-ssh-options-defined ()
  "Test that SSH options are defined."
  (should (boundp 'ecc-remote-ssh-options))
  (should (stringp ecc-remote-ssh-options)))

(ert-deftest test-ecc-remote-scp-options-defined ()
  "Test that SCP options are defined."
  (should (boundp 'ecc-remote-scp-options))
  (should (stringp ecc-remote-scp-options)))

;; Tests for SSH host selection functions
;; ----------------------------------------

(ert-deftest test-ecc-select-host-is-interactive ()
  "Test that --ecc-select-host is an interactive command."
  (should (commandp '--ecc-select-host)))

(ert-deftest test-ecc-get-host-info-is-interactive ()
  "Test that --ecc-get-host-info is an interactive command."
  (should (commandp '--ecc-get-host-info)))

;; Tests for SSH context detection
;; ----------------------------------------

(ert-deftest test-ecc-detect-ssh-context-defined ()
  "Test that SSH context detection function is defined."
  (should (fboundp '--ecc-detect-ssh-context)))

(ert-deftest test-ecc-detect-ssh-context-local ()
  "Test SSH context detection with local directory."
  (cl-letf (((symbol-function 'derived-mode-p) (lambda (mode) t)))
    (let ((default-directory "/home/user/local-project/"))
      (should-not (--ecc-detect-ssh-context)))))

(ert-deftest test-ecc-detect-ssh-context-tramp-ssh ()
  "Test SSH context detection with TRAMP SSH format."
  (cl-letf (((symbol-function 'derived-mode-p) (lambda (mode) t)))
    (let ((default-directory "/ssh:user@example.com:/home/user/"))
      (let ((result (--ecc-detect-ssh-context)))
        (should result)
        (should (equal (cdr (assoc 'user result)) "user"))
        (should (equal (cdr (assoc 'host result)) "example.com"))
        (should (equal (cdr (assoc 'port result)) "22"))))))

(ert-deftest test-ecc-detect-ssh-context-tramp-ssh-with-port ()
  "Test SSH context detection with TRAMP SSH format including port."
  (cl-letf (((symbol-function 'derived-mode-p) (lambda (mode) t)))
    (let ((default-directory "/ssh:testuser@example.org#2222:/home/testuser/"))
      (let ((result (--ecc-detect-ssh-context)))
        (should result)
        (should (equal (cdr (assoc 'user result)) "testuser"))
        (should (equal (cdr (assoc 'host result)) "example.org"))
        (should (equal (cdr (assoc 'port result)) "2222"))))))

;; Tests for directory management
;; ----------------------------------------

(ert-deftest test-ecc-get-yank-directory-defined ()
  "Test that --ecc-get-yank-directory is defined."
  (should (fboundp '--ecc-get-yank-directory)))

(ert-deftest test-ecc-ensure-directory-exists-defined ()
  "Test that --ecc-ensure-directory-exists is defined."
  (should (fboundp '--ecc-ensure-directory-exists)))

(ert-deftest test-ecc-ensure-directory-exists-local ()
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

;; Tests for file transfer functionality
;; ----------------------------------------

(ert-deftest test-ecc-transfer-file-to-remote-defined ()
  "Test that --ecc-transfer-file-to-remote is defined."
  (should (fboundp '--ecc-transfer-file-to-remote)))

(ert-deftest test-ecc-transfer-file-to-remote-nil-ssh-info ()
  "Test that file transfer returns nil when SSH info is nil."
  (should-not (--ecc-transfer-file-to-remote "/tmp/test.txt" nil "~/temp/")))

(ert-deftest test-ecc-build-remote-file-path-defined ()
  "Test that --ecc-build-remote-file-path is defined."
  (should (fboundp '--ecc-build-remote-file-path)))

(ert-deftest test-ecc-build-remote-file-path ()
  "Test building remote file path."
  (let ((ssh-info '((user . "testuser") (host . "example.com") (port . "22")))
        (local-file "/tmp/test-file.txt")
        (target-dir "/home/user/temp/"))
    (let ((result (--ecc-build-remote-file-path ssh-info local-file target-dir)))
      (should (equal result "/home/user/temp/test-file.txt")))))

(provide 'test-ecc-remote)

(when (not load-file-name)
  (message "test-ecc-remote.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
