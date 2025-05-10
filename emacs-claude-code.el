;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 19:58:21>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/emacs-claude-code.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; emacs-claude-code.el --- Claude AI interaction for Emacs -*- coding: utf-8; lexical-binding: t -*-

;; Author: ywatanabe <ywatanabe@alumni.u-tokyo.ac.jp>
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, tools, ai
;; URL: https://github.com/ywatanabe1989/emacs-claude-code
;; Timestamp: <2025-05-10 02:55:29>

;; Copyright (C) 2025 Yusuke Watanabe

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs Claude Code provides a set of tools to interact with Anthropic's
;; Claude AI model directly from Emacs. It offers features like
;; templated prompts, buffer management, auto response, state detection,
;; and integration with vterm for improved interaction.

;;; Code:

;; ---- Load Path Setup ----

;; (defun ecc--add-to-loadpath ()
;;   "Add all necessary directories to `load-path`."
;;   (let* ((parent (file-name-directory
;;                   (or load-file-name buffer-file-name)))
;;          (src-dir (expand-file-name "src" parent)))

;;     ;; Add parent directory to load-path
;;     (add-to-list 'load-path parent)

;;     ;; Add src directory to load-path
;;     (when (file-directory-p src-dir)
;;       (add-to-list 'load-path src-dir)

;;       ;; Add all subdirectories of src to load-path
;;       (dolist (dir (directory-files src-dir t "\\`[^.]"))
;;         (when (file-directory-p dir)
;;           (add-to-list 'load-path dir))))

;;     ;; Add immediate subdirectories to load-path
;;     (dolist (dir (directory-files parent t "\\`[^.]"))
;;       (when (and (file-directory-p dir)
;;                  (not (string= dir src-dir))
;;                  (not (string-match-p "/\\.\\|/\\.\\." dir))
;;                  (not (string-match-p "contrib\\|.old" dir)))
;;         (add-to-list 'load-path dir)))))

(defun --add-to-loadpath ()
  "Add all necessary directories to load-path.
Ensures proper loading of modules from the project.
Special attention is given to the src subdirectories."
  (let* ((project-root (file-name-directory
                        (or load-file-name buffer-file-name))))
    ;; Add project root to load path
    (add-to-list 'load-path project-root)
    
    ;; Add src directory to load path
    (add-to-list 'load-path (expand-file-name "src" project-root))
    
    ;; Explicitly add important subdirectories first
    (dolist (subdir '("src/ecc-state" "src/ecc-template" "src/ecc-buffer" "src/ecc-term"))
      (let ((dir (expand-file-name subdir project-root)))
        (when (file-directory-p dir)
          (add-to-list 'load-path dir))))
    
    ;; Then add all other directories recursively
    (dolist (recursively-acquired-path
             (directory-files-recursively project-root "\\`[^.]" t))
      (when (file-directory-p recursively-acquired-path)
        (add-to-list 'load-path recursively-acquired-path)))))

;; Initialize load path
(--add-to-loadpath)

;; ---- Module Loading ----

;; Core modules
(require 'ecc-variables)        ;; Variables and customization
(require 'ecc-update-mode-line) ;; Mode line updates
;; (require 'ecc-auto)             ;; Auto-response
;; (require 'ecc-run)              ;; CLI invocation
;; (require 'ecc-send)             ;; Input/output
;; (require 'ecc-large-buffer)     ;; Handling large responses
;; (require 'ecc-bindings)         ;; Key bindings
;; (require 'ecc-mode)             ;; Mode definition

;; ;; Buffer management modules (legacy)
;; (require 'ecc-buffer)

;; State management modules
(require 'ecc-state)
(require 'ecc-state-detect)

;; Template system modules
(require 'ecc-template-cache)
(require 'ecc-template)
(require 'ecc-template-mode)

;; ;; Repository integration modules
;; (require 'ecc-repository)
;; (require 'ecc-repository-view)
;; (require 'ecc-dired)

;; ;; Terminal integration module (optional)
;; (condition-case nil
;;     (require 'ecc-claude-vterm-mode)
;;   (error
;;    (message
;;     "Claude vterm mode could not be loaded (vterm may not be installed)")))

;; ;; History and session management
;; (require 'ecc-history)

;; ;; Modern architecture modules
;; (require 'ecc-state-engine)    ;; Modern state machine
;; (require 'ecc-buffer-manager)  ;; Modern buffer management
;; (require 'ecc-command)         ;; Command system

;; ;; Integration layer for backward compatibility
;; (require 'ecc-integration)

;; ;; ---- Initialization ----

;; ;; Enable modes if auto-enable is set
;; (when (and (boundp 'ecc-auto-enable) ecc-auto-enable)
;;   (ecc-mode 1))

;; ;;; emacs-claude-code.el ends here


(provide 'emacs-claude-code)

(when
    (not load-file-name)
  (message "emacs-claude-code.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))