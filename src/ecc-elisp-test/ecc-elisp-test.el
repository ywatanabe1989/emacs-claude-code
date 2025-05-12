;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-09 00:49:24>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/ecc-elisp-test.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'cl-lib)
(require 'ert)

;; Basic variables

(defgroup ecc-elisp-test nil
  "Claude-integrated Elisp Test Framework for Emacs."
  :group 'tools
  :prefix "ecc-elisp-test-")

(defvar ecc-elisp-test-timeout-sec 10
  "Default timeout in seconds for running a single test.")

(defvar ecc-elisp-test-default-output-dir nil
  "Default directory for test output files.
If nil, use the current directory when executed.")

;; Core functions

(defun ecc-elisp-test-find-test-files (directory)
  "Find all test files in DIRECTORY."
  (when (file-directory-p directory)
    (let
        ((files
          (directory-files-recursively directory "test-.*\\.el$" t)))
      (cl-remove-if (lambda (file)
                      (or (string-match-p "/\\.[^/]*/" file)
                          (string-match-p "/\\.[^/]*$" file)
                          (string-match-p "/_[^/]*/" file)
                          (string-match-p "/_[^/]*$" file)))
                    files))))

(defun ecc-elisp-test-find-tests-in-file (file)
  "Extract ert-deftest names from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (tests)
      (while
          (re-search-forward "^(ert-deftest\\s-+\\([^[:space:]\n]+\\)"
                             nil t)
        (push (cons file (match-string 1)) tests))
      tests)))

;; Keep track of loaded test files to avoid duplication

(defvar ecc-elisp-test--loaded-files (make-hash-table :test 'equal)
  "Hash table of files that have been loaded.")

(defun ecc-elisp-test-run-single-test
    (file testname &optional timeout)
  "Run a single TEST in FILE with TIMEOUT seconds."
  (let ((timeout-secs (or timeout ecc-elisp-test-timeout-sec))
        (result nil))
    (condition-case err
        (progn
          ;; Load the file only once per run
          (unless (gethash file ecc-elisp-test--loaded-files)
            (load file nil t)
            (puthash file t ecc-elisp-test--loaded-files))

          (let ((test-symbol (intern testname)))
            (message "Running test: %s from %s" testname file)
            (if (ert-test-boundp test-symbol)
                (with-timeout (timeout-secs
                               (list :name testname :passed nil
                                     :message (format
                                               "TIMEOUT: Test exceeded time limit of %s"
                                               timeout-secs)))
                  (let* ((ert-debug-on-error nil) ; Prevent debugger from being triggered
                         (ert-result (ert-run-test test-symbol)))
                    (message "Test %s %s"
                             testname
                             (if (ert-test-passed-p ert-result)
                                 "PASSED"
                               "FAILED"))
                    (list :name testname
                          :passed (ert-test-passed-p ert-result)
                          :message (unless
                                       (ert-test-passed-p ert-result)
                                     (ert-test-result-with-explanation-message
                                      ert-result)))))
              (progn
                (message "Test %s NOT-FOUND" testname)
                (list :name testname :passed nil :message
                      "NOT-FOUND: Test not found")))))
      (error
       (message "Test %s ERROR: %S" testname err)
       (list :name testname :passed nil :message
             (format "ERROR: %S" err))))))

(defun ecc-elisp-test-generate-report
    (test-results output-file &optional total-time-spent)
  "Generate test report for TEST-RESULTS and save to OUTPUT-FILE."
  (let* ((passed 0)
         (failed 0)
         (skipped 0)
         (timeout 0)
         (duplicates 0)
         (test-names (make-hash-table :test 'equal)))

    ;; Count results
    (dolist (result test-results)
      (let ((test-name (plist-get result :name))
            (passed-p (plist-get result :passed))
            (message (plist-get result :message)))
        (puthash test-name (1+ (gethash test-name test-names 0))
                 test-names)
        (cond
         (passed-p (cl-incf passed))
         ((and message (string-prefix-p "TIMEOUT:" message))
          (cl-incf timeout))
         ((and message (string-prefix-p "NOT-FOUND:" message))
          (cl-incf skipped))
         (t (cl-incf failed)))))

    ;; Count duplicates
    (maphash (lambda (k v) (when (> v 1) (cl-incf duplicates)))
             test-names)

    ;; Calculate success rate
    (let* ((total (+ passed failed skipped timeout))
           (success-rate
            (if (> total 0) (* 100.0 (/ (float passed) total)) 0.0))
           (timestamp (format-time-string "%Y%m%d-%H%M%S"))
           (success-rate-str
            (format "%d-PERCENT" (round success-rate)))
           (org-file-with-rate (replace-regexp-in-string
                                "\\.org$"
                                (format "-%s-%s.org" timestamp
                                        success-rate-str)
                                output-file)))

      ;; Create the report content
      (with-temp-buffer
        ;; Insert header
        (insert "#+TITLE: Elisp Test Report\n")
        (insert "#+AUTHOR: ywatanabe\n")
        (insert "#+DATE: ")
        (insert (format-time-string "%Y-%m-%d %H:%M:%S"))
        (insert " Created by ECC-ELISP-TEST\n\n")

        ;; Insert summary
        (insert "* Test Results Summary\n\n")
        (insert (format "- Passed: %d\n" passed))
        (insert (format "- Failed: %d\n" failed))
        (insert (format "- Skipped: %d\n" skipped))
        (insert
         (format "- Timeout (= %d s): %d\n" ecc-elisp-test-timeout-sec
                 timeout))
        (insert (format "- Duplicates: %d\n" duplicates))
        (insert (format "- Total: %d\n" total))
        (when total-time-spent
          (insert
           (format "- Total Time: %.2f seconds\n" total-time-spent)))
        (insert (format "- Success Rate: %.1f%%\n\n" success-rate))

        ;; Write to file
        (write-region (point-min) (point-max) org-file-with-rate))

      org-file-with-rate)))

(defun ecc-elisp-test-run-file (file output-file &optional timeout)
  "Run all tests in FILE and generate a report at OUTPUT-FILE.
Returns number of tests run."
  ;; Clear loaded files cache when running a new file
  (setq ecc-elisp-test--loaded-files (make-hash-table :test 'equal))

  (let* ((tests (ecc-elisp-test-find-tests-in-file file))
         (timeout-value (or timeout ecc-elisp-test-timeout-sec))
         (test-results '())
         (start-time (current-time)))

    ;; Run each test sequentially
    (dolist (test tests)
      (let* ((file-path (car test))
             (test-name (cdr test))
             (result
              (ecc-elisp-test-run-single-test file-path test-name
                                              timeout-value)))
        (push result test-results)))

    ;; Calculate elapsed time
    (let
        ((total-time-spent
          (float-time (time-subtract (current-time) start-time))))
      ;; Generate the report
      (ecc-elisp-test-generate-report test-results output-file
                                      total-time-spent))

    (length tests)))

(defun ecc-elisp-test-run
    (directory &optional output-file timeout pattern)
  "Run all tests in DIRECTORY and generate report.
If OUTPUT-FILE is nil, use default location.
If TIMEOUT is nil, use default timeout.
If PATTERN is provided, only run tests matching the pattern."
  ;; Clear loaded files cache when running a new set of tests
  (interactive)
  (setq ecc-elisp-test--loaded-files (make-hash-table :test 'equal))

  (let* ((test-files (ecc-elisp-test-find-test-files directory))
         (filtered-files (if pattern
                             (cl-remove-if-not
                              (lambda (file)
                                (string-match-p pattern file))
                              test-files)
                           test-files))
         (output-path (or output-file
                          (expand-file-name
                           "ELISP-TEST-REPORT.org"
                           (or ecc-elisp-test-default-output-dir
                               directory))))
         (test-timeout (or timeout ecc-elisp-test-timeout-sec))
         (all-results '())
         (start-time (current-time)))

    ;; Run all test files
    (dolist (file filtered-files)
      (let ((tests (ecc-elisp-test-find-tests-in-file file)))
        (dolist (test tests)
          (let* ((file-path (car test))
                 (test-name (cdr test))
                 (result
                  (ecc-elisp-test-run-single-test file-path test-name
                                                  test-timeout)))
            (push result all-results)))))

    ;; Calculate elapsed time
    (let
        ((total-time-spent
          (float-time (time-subtract (current-time) start-time))))
      ;; Generate the report
      (ecc-elisp-test-generate-report all-results output-path
                                      total-time-spent)

      ;; Return basic stats
      (list :files (length filtered-files)
            :tests (length all-results)
            :report output-path))))

(defun ecc-elisp-test-generate-shell-script (output-file)
  "Generate a shell script to run elisp-test from command line.
Write the script to OUTPUT-FILE and make it executable."
  (let*
      ((this-dir
        (file-name-directory (or load-file-name buffer-file-name)))
       (elisp-test-sh (expand-file-name "elisp-test.sh" this-dir)))
    ;; Use existing elisp-test.sh file as template
    (copy-file elisp-test-sh output-file t)

    ;; Make executable
    (chmod output-file #o755)
    output-file))


(provide 'ecc-elisp-test)

(when
    (not load-file-name)
  (message "ecc-elisp-test.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))