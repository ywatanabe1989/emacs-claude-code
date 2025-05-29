;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-28 07:22:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-vterm-utils.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'ecc-vterm-utils)

;; ===========================================
;; Feature Loading Tests
;; ===========================================

(ert-deftest test-ecc-vterm-utils-feature-loads-without-error ()
  "Test that ecc-vterm-utils feature loads successfully without errors."
  (should (featurep 'ecc-vterm-utils)))

;; ===========================================
;; Configuration Variables Tests
;; ===========================================
(ert-deftest test-ecc-vterm-yank-extension-patterns-exists ()
  "Test that extension patterns variable exists."
  (should (boundp '--ecc-vterm-yank-extension-patterns)))

(ert-deftest test-ecc-vterm-yank-extension-patterns-is-alist ()
  "Test that extension patterns is an alist."
  (should (listp --ecc-vterm-yank-extension-patterns))
  (should (consp (car --ecc-vterm-yank-extension-patterns))))

(ert-deftest test-ecc-vterm-yank-history-exists ()
  "Test that yank history variable exists."
  (should (boundp '--ecc-vterm-yank-history)))

;; ===========================================
;; File Type Detection Tests
;; ===========================================
(ert-deftest test-ecc-vterm-utils-detect-file-type-python ()
  "Test that Python code is correctly detected."
  (should (string= (--ecc-vterm-utils-detect-file-type "import os\ndef main():")
                   "py")))

(ert-deftest test-ecc-vterm-utils-detect-file-type-javascript ()
  "Test that JavaScript code is correctly detected."
  (should (string= (--ecc-vterm-utils-detect-file-type "const foo = () => {}")
                   "js")))

(ert-deftest test-ecc-vterm-utils-detect-file-type-html ()
  "Test that HTML code is correctly detected."
  (should (string= (--ecc-vterm-utils-detect-file-type "<!DOCTYPE html>\n<html>")
                   "html")))

(ert-deftest test-ecc-vterm-utils-detect-file-type-css ()
  "Test that CSS code is correctly detected."
  (should (string= (--ecc-vterm-utils-detect-file-type ".container { margin: 0; }")
                   "css")))

(ert-deftest test-ecc-vterm-utils-detect-file-type-elisp ()
  "Test that Emacs Lisp code is correctly detected."
  (should (string= (--ecc-vterm-utils-detect-file-type "(defun foo () )")
                   "el")))

(ert-deftest test-ecc-vterm-utils-detect-file-type-shell ()
  "Test that shell script is correctly detected."
  (should (string= (--ecc-vterm-utils-detect-file-type "#!/bin/bash\necho hello")
                   "sh")))

(ert-deftest test-ecc-vterm-utils-detect-file-type-default ()
  "Test that unknown content defaults to txt."
  (should (string= (--ecc-vterm-utils-detect-file-type "Random text content")
                   "txt")))

;; Edge case tests for file type detection
(ert-deftest test-ecc-vterm-utils-detect-file-type-empty-string ()
  "Test file type detection with empty string."
  (should (string= (--ecc-vterm-utils-detect-file-type "")
                   "txt")))

(ert-deftest test-ecc-vterm-utils-detect-file-type-whitespace-only ()
  "Test file type detection with whitespace only."
  (should (string= (--ecc-vterm-utils-detect-file-type "   \n\t  ")
                   "txt")))

(ert-deftest test-ecc-vterm-utils-detect-file-type-mixed-content ()
  "Test file type detection with mixed code content."
  ;; Should detect the first matching pattern (Python in this case)
  (should (string= (--ecc-vterm-utils-detect-file-type "import os\nconst x = 1;")
                   "py")))

(ert-deftest test-ecc-vterm-utils-detect-file-type-case-sensitivity ()
  "Test that file type detection works with capitalized keywords."
  ;; The patterns are not case-sensitive by default in Emacs
  ;; so "IMPORT" will still match the Python pattern
  (should (string= (--ecc-vterm-utils-detect-file-type "IMPORT os")
                   "py"))
  ;; But something that doesn't match any pattern should return txt
  (should (string= (--ecc-vterm-utils-detect-file-type "NOTAKEYWORD")
                   "txt")))

(ert-deftest test-ecc-vterm-utils-detect-file-type-partial-patterns ()
  "Test file type detection with partial pattern matches."
  ;; Should match even with partial keywords
  (should (string= (--ecc-vterm-utils-detect-file-type "def")
                   "py"))
  (should (string= (--ecc-vterm-utils-detect-file-type "function")
                   "js")))

;; ===========================================
;; Interactive Function Tests
;; ===========================================
(ert-deftest test-ecc-vterm-utils-yank-region-to-file-command ()
  "Test that yank-region-to-file exists and is an interactive command."
  (should (fboundp '--ecc-vterm-utils-yank-region-to-file))
  (should (commandp '--ecc-vterm-utils-yank-region-to-file)))

(ert-deftest test-ecc-vterm-utils-yank-buffer-to-file-command ()
  "Test that yank-buffer-to-file exists and is an interactive command."
  (should (fboundp '--ecc-vterm-utils-yank-buffer-to-file))
  (should (commandp '--ecc-vterm-utils-yank-buffer-to-file)))

(ert-deftest test-ecc-vterm-utils-quick-yank-region-command ()
  "Test that quick-yank-region exists and is an interactive command."
  (should (fboundp '--ecc-vterm-utils-quick-yank-region))
  (should (commandp '--ecc-vterm-utils-quick-yank-region)))

;; ===========================================
;; Error Condition Tests
;; ===========================================

(ert-deftest test-ecc-vterm-utils-quick-yank-no-region-error ()
  "Test that quick-yank-region signals error when no region is active."
  (with-temp-buffer
    ;; Ensure no region is active
    (deactivate-mark)
    (should-error (--ecc-vterm-utils-quick-yank-region)
                  :type 'user-error)))

(ert-deftest test-ecc-vterm-utils-detect-file-type-nil-content ()
  "Test file type detection handles nil content gracefully."
  (should-error (--ecc-vterm-utils-detect-file-type nil)))

;; ===========================================
;; Advice-Related Tests
;; ===========================================
(ert-deftest test-ecc-vterm-yank-as-file-enabled-exists ()
  "Test that yank-as-file enabled flag exists."
  (should (boundp '--ecc-vterm-yank-as-file-enabled)))

(ert-deftest test-ecc-vterm-yank-as-file-threshold-exists ()
  "Test that yank-as-file threshold variable exists."
  (should (boundp '--ecc-vterm-yank-as-file-threshold)))

(ert-deftest test-ecc-vterm-yank-as-file-threshold-is-number ()
  "Test that yank-as-file threshold is a number."
  (should (numberp --ecc-vterm-yank-as-file-threshold)))

;; Test advice functions
(ert-deftest test-ecc-vterm-utils-enable-yank-advice-command ()
  "Test that enable-yank-advice exists and is an interactive command."
  (should (fboundp '--ecc-vterm-utils-enable-yank-advice))
  (should (commandp '--ecc-vterm-utils-enable-yank-advice)))

(ert-deftest test-ecc-vterm-utils-disable-yank-advice-exists ()
  "Test that disable-yank-advice function exists."
  (should (fboundp '--ecc-vterm-utils-disable-yank-advice)))

(ert-deftest test-ecc-vterm-utils-toggle-yank-advice-command ()
  "Test that toggle-yank-advice exists and is an interactive command."
  (should (fboundp '--ecc-vterm-utils-toggle-yank-advice))
  (should (commandp '--ecc-vterm-utils-toggle-yank-advice)))

(ert-deftest test-ecc-vterm-utils-setup-yank-advice-command ()
  "Test that setup-yank-advice exists and is an interactive command."
  (should (fboundp '--ecc-vterm-utils-setup-yank-advice))
  (should (commandp '--ecc-vterm-utils-setup-yank-advice)))


(provide 'test-ecc-vterm-utils)

(when (not load-file-name)
  (message "test-ecc-vterm-utils.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))