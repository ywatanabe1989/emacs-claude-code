;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-06 01:37:20>
;;; File: /home/ywatanabe/.emacs.d/lisp/ecc/tests/test-ecc-repository-copy-contents.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ert)
(require 'cl-lib)

;; Mock magit for testing
(unless (fboundp 'magit-toplevel)
  (defun magit-toplevel (&optional directory)
    "Mock implementation of magit-toplevel for testing."
    (or directory default-directory)))

(require 'ecc-repository-copy-contents)

(ert-deftest test-ecc-repository-copy-contents-loadable ()
  (should (featurep 'ecc-repository-copy-contents)))

(ert-deftest test-ecc-repository-dir-defined ()
  (should (boundp 'ecc-repository-dir)))

(ert-deftest test-ecc-repository-output-file-defined ()
  (should (boundp 'ecc-repository-output-file))
  (should (stringp ecc-repository-output-file)))

(ert-deftest test-ecc-repository-file-blacklist-defined ()
  (should (boundp 'ecc-repository-file-blacklist))
  (should (listp ecc-repository-file-blacklist)))

(ert-deftest test-ecc-repository-max-file-size-defined ()
  (should (boundp 'ecc-repository-max-file-size))
  (should (integerp ecc-repository-max-file-size)))

(ert-deftest test-ecc-repository-blacklisted-p-functionality ()
  (should (ecc-repository-blacklisted-p "/path/to/.git/config"))
  (should (ecc-repository-blacklisted-p "/path/to/image.png"))
  (should-not (ecc-repository-blacklisted-p "/path/to/code.el")))

(ert-deftest test-ecc-get-file-type-functionality ()
  (should
   (string= (ecc-get-file-type "/path/to/file.el") "elisp"))
  (should
   (string= (ecc-get-file-type "/path/to/file.py") "python"))
  (should
   (string= (ecc-get-file-type "/path/to/file.js")
            "javascript"))
  (should (string= (ecc-get-file-type "/path/to/file.c") "c"))
  (should
   (string= (ecc-get-file-type "/path/to/file.unknown")
            "unknown")))

(ert-deftest test-ecc-repository-get-file-content ()
  (let ((temp-file (make-temp-file "ecc-test-" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "Test file content"))
          (should
           (string= (ecc-repository-get-file-content temp-file)
                    "Test file content")))
      (delete-file temp-file))))

(ert-deftest test-ecc-get-repository-files-with-mock-dir ()
  (let* ((temp-dir (make-temp-file "ecc-test-dir-" t))
         (subdir
          (file-name-as-directory (expand-file-name "subdir" temp-dir)))
         (elisp-file (expand-file-name "test.el" temp-dir))
         (python-file (expand-file-name "test.py" subdir))
         (image-file (expand-file-name "test.png" temp-dir))
         (large-file (expand-file-name "large.txt" temp-dir))
         (orig-max-size ecc-repository-max-file-size))
    (unwind-protect
        (progn
          (make-directory subdir)
          (with-temp-file elisp-file
            (insert "(defun test () (message \"test\"))"))
          (with-temp-file python-file
            (insert "def test():\n    print('test')"))
          (with-temp-file image-file
            (insert "fake image content"))
          (with-temp-file large-file
            (insert (make-string (1+ ecc-repository-max-file-size) ?x)))

          (let ((files (ecc-get-repository-files temp-dir)))
            ;; Should include elisp and python files
            (should (member elisp-file files))
            (should (member python-file files))
            ;; Should exclude image file and large file
            (should-not (member image-file files))
            (should-not (member large-file files))))
      (delete-directory temp-dir t))))

(ert-deftest test-ecc-repository-copy-contents-creates-output-file ()
  (let* ((temp-dir (make-temp-file "ecc-test-repo-" t))
         (test-file (expand-file-name "test.el" temp-dir))
         (expected-output-path
          (expand-file-name ecc-repository-output-file
                            temp-dir))
         (orig-repository-dir ecc-repository-dir)
         (kill-called nil)
         (kill-content nil))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "(defun test () (message \"test\"))"))

          (cl-letf (((symbol-function 'kill-new)
                     (lambda (content)
                       (setq kill-called t
                             kill-content content))))
            (ecc-repository-copy-contents temp-dir)

            ;; Check if output file was created
            (should (file-exists-p expected-output-path))

            ;; Check if content was added to kill ring
            (should kill-called)
            (should (stringp kill-content))
            (should
             (string-match-p "Repository Structure" kill-content))
            (should
             (string-match-p
              (regexp-quote "(defun test () (message \"test\"))")
              kill-content))

            ;; Check if repository dir was remembered
            (should (string= ecc-repository-dir temp-dir))))
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t))
      (setq ecc-repository-dir orig-repository-dir))))


(provide 'test-ecc-repository-copy-contents)

(when
    (not load-file-name)
  (message "test-ecc-repository-copy-contents.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))