;;; cleanup-after-refactor.el --- Cleanup duplicate definitions and aliases -*- lexical-binding: t -*-

;;; Commentary:
;; This script removes duplicate function definitions and obsolete defalias
;; statements after running refactor-aliases.sh

;;; Code:

(require 'cl-lib)

(defun cleanup-duplicate-definitions ()
  "Find and report duplicate function definitions."
  (interactive)
  (let ((duplicates '(
                      ;; (function-name file1 line1 file2 line2)
                      ("ecc-auto-response-send"
                       "src/ecc-api.el" 148
                       "src/ecc-auto-response.el" 921)
                      ("ecc-buffer-state-get"
                       "src/ecc-buffer-state.el" 184
                       "src/ecc-buffer-state.el" 414)
                      ("ecc-term-claude-check-state"
                       "src/ecc-term-claude-mode.el" 453
                       "src/ecc-term-claude-state.el" 115))))
    (dolist (dup duplicates)
      (message "Duplicate definition: %s in %s:%d and %s:%d"
               (nth 0 dup) (nth 1 dup) (nth 2 dup)
               (nth 3 dup) (nth 4 dup)))))

(defun cleanup-obsolete-aliases ()
  "Find and report obsolete defalias statements."
  (let ((obsolete-aliases '(
                           ;; Auto-response aliases
                           "ecc-auto-start"
                           "ecc-auto-stop"
                           "ecc-auto-toggle"
                           "ecc-yes"
                           "ecc-yes-plus"
                           "ecc-continue"
                           ;; Buffer-local aliases
                           "ecc-buffer-auto-start"
                           "ecc-buffer-auto-stop"
                           "ecc-buffer-auto-toggle"
                           ;; Notification aliases
                           "ecc-auto-notify-toggle"
                           "ecc-auto-notify-toggle-bell"
                           "ecc-auto-notify-check-state"
                           "ecc-auto-notify-prompt"
                           "ecc-auto-notify-ring-bell"
                           "ecc-auto-notify-flash-mode-line"
                           "ecc-auto-notify-setup-for-buffer"
                           ;; Debug aliases
                           "ecc-debug-utils-make-debug-fn"
                           "ecc-debug-utils-message"
                           "ecc-debug-utils-print-state-info"
                           "ecc-toggle-debug"
                           ;; State detection aliases
                           "ecc-detect-simple-state"
                           "ecc-detect-enhanced-state"
                           "ecc-detect-prompt-state")))
    (dolist (alias obsolete-aliases)
      (message "Remove defalias for: %s" alias))))

(defun find-defalias-in-file (file)
  "Find all defalias statements in FILE."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((aliases nil))
        (while (re-search-forward "(defalias\\s-+'\\([^']+\\)" nil t)
          (push (cons (match-string 1) (line-number-at-pos)) aliases))
        (nreverse aliases)))))

(defun cleanup-report ()
  "Generate a cleanup report for manual review."
  (interactive)
  (with-output-to-temp-buffer "*Cleanup Report*"
    (princ "=== Emacs Claude Code Cleanup Report ===\n\n")
    
    (princ "1. DUPLICATE DEFINITIONS TO REMOVE:\n")
    (princ "-----------------------------------\n")
    (cleanup-duplicate-definitions)
    (princ "\n")
    
    (princ "2. OBSOLETE ALIASES TO REMOVE:\n")
    (princ "------------------------------\n")
    (cleanup-obsolete-aliases)
    (princ "\n")
    
    (princ "3. DEFALIAS STATEMENTS BY FILE:\n")
    (princ "-------------------------------\n")
    (dolist (file (directory-files-recursively "src" "\\.el$"))
      (let ((aliases (find-defalias-in-file file)))
        (when aliases
          (princ (format "\n%s:\n" file))
          (dolist (alias aliases)
            (princ (format "  Line %d: %s\n" (cdr alias) (car alias)))))))
    
    (princ "\n4. NEXT STEPS:\n")
    (princ "--------------\n")
    (princ "1. Review the duplicate definitions and keep the most appropriate one\n")
    (princ "2. Remove the defalias statements for renamed functions\n")
    (princ "3. Update any keybindings that use the old function names\n")
    (princ "4. Run all tests to ensure nothing is broken\n")
    (princ "5. Update documentation if needed\n")))

;; Run the report
(cleanup-report)

(provide 'cleanup-after-refactor)
;;; cleanup-after-refactor.el ends here