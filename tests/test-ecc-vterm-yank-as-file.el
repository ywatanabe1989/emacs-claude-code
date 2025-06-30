;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-06-04 06:42:45>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-vterm-yank-as-file.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'vterm nil t)
(require 'ecc-vterm-yank-as-file)

(ert-deftest test-ecc-vterm-yank-as-file-loadable ()
  "Test if ecc-vterm-yank-as-file is loadable."
  (should (featurep 'ecc-vterm-yank-as-file)))

(ert-deftest test-ecc-vterm-create-temp-file ()
  "Test temp file creation function."
  (let ((filepath (--ecc-vterm-create-temp-file)))
    (should (stringp filepath))
    (should (string-match-p "kill-ring-[0-9]+-[0-9]+\\.tmp$" filepath))))

(ert-deftest test-ecc-vterm-get-kill-ring-content-empty ()
  "Test getting content from empty kill-ring."
  (let ((kill-ring nil))
    (should-not (--ecc-vterm-get-kill-ring-content))))

(ert-deftest test-ecc-vterm-get-kill-ring-content-with-content ()
  "Test getting content from populated kill-ring."
  (let ((kill-ring '("test content" "older content")))
    (should (equal (--ecc-vterm-get-kill-ring-content) "test content"))))

(ert-deftest test-ecc-vterm-write-content-to-file ()
  "Test writing content to file."
  (let* ((temp-file (make-temp-file "ecc-test-"))
         (test-content "Hello, World!"))
    (unwind-protect
        (progn
          (--ecc-vterm-write-content-to-file test-content temp-file)
          (should (file-exists-p temp-file))
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
  (should (commandp 'ecc-yank-to-remote-file)))

(ert-deftest test-ecc-detect-ssh-context-tramp-ssh ()
  "Test SSH context detection with TRAMP SSH format."
  (let ((default-directory "/ssh:user@example.com:/home/user/"))
    (let ((result (--ecc-detect-ssh-context)))
      (should (equal (cdr (assoc 'user result)) "user"))
      (should (equal (cdr (assoc 'host result)) "example.com"))
      (should (equal (cdr (assoc 'port result)) "22")))))

(ert-deftest test-ecc-detect-ssh-context-tramp-ssh-with-port ()
  "Test SSH context detection with TRAMP SSH format including port."
  (let ((default-directory "/ssh:testuser@example.org#2222:/home/testuser/"))
    (let ((result (--ecc-detect-ssh-context)))
      (should (equal (cdr (assoc 'user result)) "testuser"))
      (should (equal (cdr (assoc 'host result)) "example.org"))
      (should (equal (cdr (assoc 'port result)) "2222")))))

(ert-deftest test-ecc-detect-ssh-context-tramp-scp ()
  "Test SSH context detection with TRAMP SCP format."
  (let ((default-directory "/scp:admin@server.example.com:/opt/"))
    (let ((result (--ecc-detect-ssh-context)))
      (should (equal (cdr (assoc 'user result)) "admin"))
      (should (equal (cdr (assoc 'host result)) "server.example.com"))
      (should (equal (cdr (assoc 'port result)) "22")))))

(ert-deftest test-ecc-detect-ssh-context-local ()
  "Test SSH context detection with local directory."
  (let ((default-directory "/home/user/local-project/"))
    (should-not (--ecc-detect-ssh-context))))

(ert-deftest test-ecc-build-remote-file-path ()
  "Test building remote file path."
  (let ((ssh-info '((user . "testuser") (host . "example.com") (port . "22")))
        (local-file "/tmp/kill-ring-20250701-123456.tmp")
        (target-dir "~/claude-temp/"))
    (let ((result (--ecc-build-remote-file-path ssh-info local-file target-dir)))
      (should (equal result "~/claude-temp/kill-ring-20250701-123456.tmp")))))

(ert-deftest test-ecc-transfer-file-to-remote-disabled ()
  "Test that file transfer is skipped when ecc-vterm-yank-use-scp is nil."
  (let ((ecc-vterm-yank-use-scp nil)
        (ssh-info '((user . "testuser") (host . "example.com") (port . "22"))))
    (should-not (--ecc-transfer-file-to-remote "/tmp/test.txt" ssh-info "~/claude-temp/"))))

(ert-deftest test-ecc-transfer-file-to-remote-no-ssh-info ()
  "Test that file transfer is skipped when no SSH info is provided."
  (let ((ecc-vterm-yank-use-scp t))
    (should-not (--ecc-transfer-file-to-remote "/tmp/test.txt" nil "~/claude-temp/"))))

(provide 'test-ecc-vterm-yank-as-file)

(when (not load-file-name)
  (message "test-ecc-vterm-yank-as-file.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))