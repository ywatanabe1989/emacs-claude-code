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

(provide 'test-ecc-vterm-yank-as-file)

(when (not load-file-name)
  (message "test-ecc-vterm-yank-as-file.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))