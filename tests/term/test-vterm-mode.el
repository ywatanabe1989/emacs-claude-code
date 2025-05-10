;;; -*- coding: utf-8; lexical-binding: t -*-
;;; test-vterm-mode.el - Custom test script for ecc-claude-vterm-mode

;; Load path setup
(let* ((this-file (or load-file-name buffer-file-name))
       (this-dir (file-name-directory this-file))
       (project-root (expand-file-name "../" this-dir))
       (src-dir (expand-file-name "src" project-root))
       (tests-dir (expand-file-name "tests" project-root)))
  
  ;; Add directories to load-path
  (add-to-list 'load-path project-root)
  (add-to-list 'load-path src-dir)
  (add-to-list 'load-path tests-dir)
  
  ;; Add src subdirectories to load-path
  (dolist (dir (directory-files src-dir t "\\`[^.]"))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))

;; Load test compatibility layer
(require 'ert)
(load (expand-file-name "tests/modules/test-compat.el"))
(load (expand-file-name "tests/modules/fix-names.el"))

;; Load the module and test file
(require 'ecc-term/ecc-claude-vterm-mode)
(load (expand-file-name "tests/test-ecc-claude-vterm-mode.el"))

;; Run the tests
(defun run-vterm-tests ()
  "Run the vterm mode tests."
  (message "\n=== Running ecc-claude-vterm-mode tests ===\n")
  
  ;; Run all tests matching the pattern
  (let ((results (ert-run-tests-batch "^test-ecc-claude-vterm")))
    (message "\n=== Test run complete ===\n")
    
    ;; We don't need to manually report stats since ert-run-tests-batch already does this
    
    ;; Exit with appropriate code
    (kill-emacs (if (zerop (ert--stats-failed-unexpected results)) 0 1))))

(run-vterm-tests)