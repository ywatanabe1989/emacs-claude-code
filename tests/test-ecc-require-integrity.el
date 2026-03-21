;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-03-21 12:00:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-require-integrity.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;;; Tests that all (require 'ecc-*) statements reference files that exist.
;;; Prevents issue #20: missing files that break loading for users.

(require 'ert)

(defun test-ecc-require--find-project-root ()
  "Find project root by locating the src/ directory.
Walks up from `load-file-name' or `default-directory'."
  (let ((dir (file-name-directory
              (or load-file-name buffer-file-name default-directory))))
    (while
	(and dir (not (file-directory-p (expand-file-name "src" dir))))
      (let ((parent (file-name-directory (directory-file-name dir))))
        (setq dir (unless (string= parent dir) parent))))
    dir))

(ert-deftest test-ecc-all-requires-have-files ()
  "Every (require 'ecc-*) in src/ must have a corresponding .el file."
  (let* ((project-root (test-ecc-require--find-project-root))
         (src-dir (expand-file-name "src" project-root))
         (src-files (directory-files src-dir t "\\.el\\'"))
         (missing nil))
    (should (file-directory-p src-dir))
    (dolist (file src-files)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward
                "^(require '\\(ecc-[a-zA-Z0-9_-]+\\))" nil t)
          (let* ((feature (match-string 1))
                 (feature-file (expand-file-name
                                (concat feature ".el") src-dir)))
            (unless (file-exists-p feature-file)
              (push (format "%s requires '%s but %s.el not found"
                            (file-name-nondirectory file)
                            feature feature)
                    missing))))))
    (when missing
      (ert-fail (format "Missing files for require statements:\n  %s"
                        (mapconcat #'identity missing "\n  "))))))

(ert-deftest test-ecc-all-provides-match-filenames ()
  "Every .el file's (provide 'foo) should match its filename."
  (let* ((project-root (test-ecc-require--find-project-root))
         (src-dir (expand-file-name "src" project-root))
         (src-files (directory-files src-dir t "\\.el\\'"))
         (mismatches nil))
    (should (file-directory-p src-dir))
    (dolist (file src-files)
      (let ((expected-feature
             (file-name-sans-extension (file-name-nondirectory file))))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (when (re-search-forward
                 "^(provide '\\([a-zA-Z0-9_-]+\\))" nil t)
            (let ((provided (match-string 1)))
              (unless (string= provided expected-feature)
                (push (format "%s provides '%s (expected '%s)"
                              (file-name-nondirectory file)
                              provided expected-feature)
                      mismatches)))))))
    (when mismatches
      (ert-fail (format "Provide/filename mismatches:\n  %s"
                        (mapconcat #'identity mismatches "\n  "))))))

(ert-deftest test-ecc-no-el-files-gitignored ()
  "No .el files in src/ should be gitignored."
  (let* ((project-root (test-ecc-require--find-project-root))
         (default-directory project-root)
         (result (shell-command-to-string
                  "git check-ignore src/*.el 2>/dev/null")))
    (when (and (not (string-empty-p result))
               (= 0 (shell-command
                     "git rev-parse --git-dir 2>/dev/null")))
      (ert-fail (format "These .el files are gitignored:\n%s" result)))))

(provide 'test-ecc-require-integrity)

(when (not load-file-name)
  (message "test-ecc-require-integrity.el loaded."))
