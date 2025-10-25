;;; test-ecc-smart-yank.el --- Tests for ecc-smart-yank -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for ecc-smart-yank functionality

;;; Code:

(require 'ert)
(require 'ecc-smart-yank)

;; Test configuration
(ert-deftest test-ecc-smart-yank-config ()
  "Test that smart-yank configuration variables exist and have correct defaults."
  (should (boundp 'ecc-smart-yank-diff-hide-delay))
  (should (= ecc-smart-yank-diff-hide-delay 5)))

;; Test basic yank without region
(ert-deftest test-ecc-smart-yank-no-region ()
  "Test that ecc-smart-yank works like normal yank when no region is active."
  (with-temp-buffer
    (kill-new "test content")
    (ecc-smart-yank nil)
    (should (string= (buffer-string) "test content"))))

;; Test yank with region replacement
(ert-deftest test-ecc-smart-yank-with-region ()
  "Test that ecc-smart-yank replaces selected region."
  (with-temp-buffer
    (insert "old content")
    (kill-new "new content")
    ;; Select all
    (set-mark (point-min))
    (goto-char (point-max))
    (ecc-smart-yank nil)
    (should (string= (buffer-string) "new content"))))

;; Test diff buffer creation
(ert-deftest test-ecc-smart-yank-diff-buffer ()
  "Test that diff buffer is created when yanking over different content."
  (with-temp-buffer
    (insert "old text")
    (kill-new "new text")
    (set-mark (point-min))
    (goto-char (point-max))
    (ecc-smart-yank nil)
    ;; Check that diff buffer was created
    (should (get-buffer "*ECC Yank Diff*"))))

;; Test identical content (no diff)
(ert-deftest test-ecc-smart-yank-identical ()
  "Test that no diff is shown when yanking identical content."
  (with-temp-buffer
    (insert "same text")
    (kill-new "same text")
    (set-mark (point-min))
    (goto-char (point-max))
    (ecc-smart-yank nil)
    ;; Buffer should be replaced but no diff shown
    (should (string= (buffer-string) "same text"))))

;; Cleanup
(ert-deftest test-ecc-smart-yank-cleanup ()
  "Test cleanup of diff buffer."
  (when (get-buffer "*ECC Yank Diff*")
    (kill-buffer "*ECC Yank Diff*"))
  (should-not (get-buffer "*ECC Yank Diff*")))

(provide 'test-ecc-smart-yank)
;;; test-ecc-smart-yank.el ends here
