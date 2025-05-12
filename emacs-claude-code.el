;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 04:21:25>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/emacs-claude-code.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;; Add all feature directories to the load path
(defun --ecc-add-all-to-loadpath ()
  "Add all necessary directories to `load-path`."
  (let* ((base-dir (file-name-directory
                    (or load-file-name buffer-file-name)))
         (src-dir (expand-file-name "src" base-dir)))
    
    ;; Add src directory
    (add-to-list 'load-path src-dir)
    
    ;; Add all immediate subdirectories of src to load-path
    (dolist (dir (directory-files src-dir t "\\`[^.]"))
      (when (and (file-directory-p dir)
                 (not (string-match-p "/\\.\\|/\\.\\." dir))
                 (not (string-match-p "contrib\\|.old" dir)))
        (add-to-list 'load-path dir)))))

;; Initialize load path
(--ecc-add-all-to-loadpath)

;; ---- Core Modules ----

;; Configuration and variables
(require 'ecc-variables)
(require 'ecc-compat)

;; User Interface
(require 'ecc-update-mode-line)
(require 'ecc-bindings)
(require 'ecc-mode)
(require 'ecc-ui-command)
(require 'ecc-ui-dired)

;; Auto-response functionality
(require 'ecc-auto)

;; Command execution
(require 'ecc-run)

;; Buffer management
(require 'ecc-buffer-manager)
(require 'ecc-buffer)
(require 'ecc-buffer-auto-switch)
(require 'ecc-buffer-current)
(require 'ecc-buffer-navigation)
(require 'ecc-buffer-registry)
(require 'ecc-buffer-stale)
(require 'ecc-buffer-state)
(require 'ecc-buffer-timestamp)
(require 'ecc-buffer-variables)
(require 'ecc-buffer-verification)
(require 'ecc-buffer-large)  ;; Large buffer handling

;; State management 
(require 'ecc-state-engine)
(require 'ecc-state)
(require 'ecc-state-detect)

;; Template system
(require 'ecc-template)
(require 'ecc-template-cache)
(require 'ecc-template-mode)

;; Repository integration
(require 'ecc-repository)
(require 'ecc-repository-view)

;; Integration with other tools
(require 'ecc-integration)

;; Terminal integration (optional)
(condition-case nil
    (progn
      (require 'ecc-term-claude-mode)
      (require 'ecc-term-run)
      (require 'ecc-term-send))
  (error (message "Claude vterm mode could not be loaded (vterm may not be installed)")))

;; History and session management
(require 'ecc-history)


;; ---- Initialization ----

;; Enable modes if auto-enable is set
(when (and (boundp 'ecc-auto-enable) ecc-auto-enable)
  (ecc-mode 1))

(provide 'emacs-claude-code)

(when
    (not load-file-name)
  (message "emacs-claude-code.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))