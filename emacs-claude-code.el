;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-23 00:39:37>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/emacs-claude-code.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; Commentary:
;;; Main entry point for emacs-claude-code package.

(require 'cl-lib)

(defun --ecc-add-all-to-loadpath ()
  "Add src and tests directories recursively to `load-path`."
  (let* ((base-dir (file-name-directory
                    (or load-file-name buffer-file-name)))
         (src-dir (expand-file-name "src" base-dir))
         (tests-dir (expand-file-name "tests" base-dir)))

    ;; Function to add directory and its subdirectories recursively
    (cl-labels ((add-dir-recursively
                  (dir)
                  (when (file-directory-p dir)
                    ;; Add the directory itself
                    (add-to-list 'load-path dir)
                    ;; Add subdirectories
                    (dolist (subdir (directory-files dir t "\\`[^.]"))
                      (when (and (file-directory-p subdir)
                                 (not
                                  (string-match-p "/\\.\\|/\\.\\."
                                                  subdir))
                                 (not
                                  (string-match-p "contrib\\|.old"
                                                  subdir)))
                        (add-dir-recursively subdir))))))

      ;; Add src and tests directories recursively
      (when (file-directory-p src-dir)
        (add-dir-recursively src-dir))
      (when (file-directory-p tests-dir)
        (add-dir-recursively tests-dir)))))

;; Initialize load path
(--ecc-add-all-to-loadpath)

;; Core functionality
;; Core variables
(require 'ecc-variables)

;; Debug utilities
(condition-case nil
    (require 'ecc-debug-utils)
  (error nil))

;; State detection
(condition-case nil
    (require 'ecc-state-detection)
  (error nil))

;; VTerm integration
(condition-case nil
    (progn
      (require 'vterm)
      (require 'ecc-vterm-mode)
      (require 'ecc-vterm-yank-as-file)
      ;; Load Claude terminal mode
      (require 'ecc-term-claude-mode)
      (require 'ecc-term-visual-aid)
      (require 'ecc-vterm-grayscale))
  (error
   (message
    "Claude vterm mode could not be loaded (vterm may not be installed)")))

;; Auto-response functionality
(require 'ecc-auto-response)
(require 'ecc-auto-notify)
(condition-case nil
    (require 'ecc-auto-response-fix)
  (error nil))
(require 'ecc-interaction-tracker)
(require 'ecc-interaction-limiter)
(require 'ecc-color-themes)
(require 'ecc-eye-friendly)

(require 'ecc-convenience-commands)
;; Enable minor mode by default
(ecc-claude-mode 1)


(provide 'emacs-claude-code)

(when
    (not load-file-name)
  (message "emacs-claude-code.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))