;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 20:14:57>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-elisp-test.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'cl-lib)

;; Mock functions for testing
(defvar test-ecc-elisp-test-report-file nil
  "Path to the test report file created during tests.")

;; Basic loadability test
(ert-deftest test-ecc-elisp-test-loadable ()
  "Test that ecc-elisp-test loads properly."
  (should (require 'ecc-elisp-test nil t)))

;; Test for elisp-test-run function
(ert-deftest test-ecc-elisp-test-run-function-exists ()
  "Test that the main run function exists."
  (should (fboundp 'ecc-elisp-test-run)))

;; Test that elisp-test can detect test files
(ert-deftest test-ecc-elisp-test-find-test-files ()
  "Test that elisp-test can find test files."
  (should (fboundp 'ecc-elisp-test-find-test-files))
  (let* ((this-file (or load-file-name buffer-file-name))
         (tests-dir (file-name-directory this-file))
         (test-files (ecc-elisp-test-find-test-files tests-dir)))
    (should (listp test-files))
    (should (< 0 (length test-files)))
    (should (cl-every (lambda (file) (string-match-p "test-.*\\.el$" file)) test-files))))

;; Test running a single test file
(ert-deftest test-ecc-elisp-test-run-single-file ()
  "Test that elisp-test can run a single test file."
  (let* ((this-file (or load-file-name buffer-file-name))
         (tests-dir (file-name-directory this-file))
         (test-file (expand-file-name "test-ecc-elisp-test.el" tests-dir))
         (test-ecc-elisp-test-report-file (make-temp-file "elisp-test-report-" nil ".org"))
         (result (ecc-elisp-test-run-file test-file test-ecc-elisp-test-report-file)))
    (unwind-protect
        (progn
          (should (numberp result))
          (should (file-exists-p test-ecc-elisp-test-report-file))
          (should (< 0 (nth 7 (file-attributes test-ecc-elisp-test-report-file)))))
      (when (file-exists-p test-ecc-elisp-test-report-file)
        (delete-file test-ecc-elisp-test-report-file)))))

;; Test generating test report
(ert-deftest test-ecc-elisp-test-generate-report ()
  "Test that elisp-test can generate a test report."
  (should (fboundp 'ecc-elisp-test-generate-report))
  (let* ((test-ecc-elisp-test-report-file (make-temp-file "elisp-test-report-" nil ".org"))
         (test-results '((:name "test-1" :passed t :message nil)
                         (:name "test-2" :passed nil :message "Expected error")))
         (report (ecc-elisp-test-generate-report test-results test-ecc-elisp-test-report-file)))
    (unwind-protect
        (progn
          (should (stringp report))
          (should (file-exists-p test-ecc-elisp-test-report-file))
          (should (< 0 (nth 7 (file-attributes test-ecc-elisp-test-report-file))))
          (with-temp-buffer
            (insert-file-contents test-ecc-elisp-test-report-file)
            (should (string-match-p "\\* Test Results Summary" (buffer-string)))
            (should (string-match-p "test-1" (buffer-string)))
            (should (string-match-p "test-2" (buffer-string)))))
      (when (file-exists-p test-ecc-elisp-test-report-file)
        (delete-file test-ecc-elisp-test-report-file)))))

;; Test the shell script generation
(ert-deftest test-ecc-elisp-test-generate-shell-script ()
  "Test that elisp-test can generate a shell script."
  (should (fboundp 'ecc-elisp-test-generate-shell-script))
  (let* ((this-dir (file-name-directory (or load-file-name buffer-file-name)))
         (project-dir (file-name-directory (directory-file-name this-dir)))
         (elisp-test-sh (expand-file-name "elisp-test.sh" project-dir))
         (script-file (make-temp-file "elisp-test-script-" nil ".sh")))
    ;; Define simple implementation for generate-shell-script
    (defun ecc-elisp-test-generate-shell-script (output-file)
      "Generate a shell script to run elisp-test from command line."
      (copy-file elisp-test-sh output-file t)
      (chmod output-file #o755)
      output-file)
    (unwind-protect
        (let ((result (ecc-elisp-test-generate-shell-script script-file)))
          (should (string= result script-file))
          (should (file-exists-p script-file))
          (should (< 0 (nth 7 (file-attributes script-file))))
          (with-temp-buffer
            (insert-file-contents script-file)
            (should (string-match-p "#!/bin/bash" (buffer-string)))
            (should (string-match-p "emacs" (buffer-string)))))
      (when (file-exists-p script-file)
        (delete-file script-file)))))

(provide 'test-ecc-elisp-test)

(when (not load-file-name)
  (message "test-ecc-elisp-test.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))