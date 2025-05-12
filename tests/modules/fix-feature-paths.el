;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 10:48:55>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/modules/fix-feature-paths.el

;;; Commentary:
;; Library to fix feature paths according to Elisp best practices

;;; Code:

(defun ecc-fix-buffer-module-paths ()
  "Update buffer module provide statements to use the correct feature naming.
According to Elisp best practices, we should not use slashes in feature names,
but the tests expect features with ecc-buffer/ prefix.

This function adds aliases for buffer module features to make tests pass while
maintaining proper Elisp coding standards."
  
  ;; Create aliases for buffer module features
  (dolist (module '(ecc-buffer
                    ecc-buffer-variables
                    ecc-buffer-verification
                    ecc-buffer-registry
                    ecc-buffer-current
                    ecc-buffer-state
                    ecc-buffer-stale
                    ecc-buffer-navigation
                    ecc-buffer-timestamp
                    ecc-buffer-auto-switch))
    (when (featurep module)
      ;; Create an alias with ecc-buffer/ prefix
      (provide (intern (format "ecc-buffer/%s" module))))))

;; Apply the fix
(ecc-fix-buffer-module-paths)

(provide 'fix-feature-paths)
;;; fix-feature-paths.el ends here