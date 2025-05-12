;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-11 13:18:34>
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
;;
;; For the best experience, consider using the Apptainer integration
;; by running:
;;   ./launch-claude-emacs.sh --run
;;
;; For more information, see the documentation in docs/apptainer.md

;;; Code:

;; ---- Load Path Setup ----

(defvar ecc--module-directories
  '("ecc-state" "ecc-template" "ecc-buffer" "ecc-term")
  "List of module subdirectories that need to be in the load path.")

(defun ecc--add-to-loadpath ()
  "Add all necessary directories to `load-path`.
This function establishes the load paths for the entire package.
CRITICAL: The src subdirectories must be properly mapped for tests to work."
  (let* ((parent (file-name-directory
                  (or load-file-name buffer-file-name)))
         (src-dir (expand-file-name "src" parent))
         (tests-dir (expand-file-name "tests" parent)))

    ;; Add project root to load-path
    (add-to-list 'load-path parent)

    ;; Add src directory to load-path
    (when (file-directory-p src-dir)
      (add-to-list 'load-path src-dir)

      ;; Create module-specific mappings in the symbol table
      ;; This is crucial for tests to find and load modules
      (dolist (module-name ecc--module-directories)
        (let ((module-dir (expand-file-name module-name src-dir)))
          (when (file-directory-p module-dir)
            ;; Add each module directory to load-path
            (add-to-list 'load-path module-dir)
            
            ;; Set up explicit mappings for key modules
            ;; This allows direct require statements without directory prefixes
            ;; For example: (require 'ecc-template-cache) instead of
            ;; (require 'ecc-template-cache) with different path
            (dolist (el-file (directory-files module-dir t "\\.el$"))
              (let* ((base-name (file-name-base el-file))
                     (feature-name (intern base-name)))
                ;; Don't load the modules yet, just set up the mappings
                (unless (featurep feature-name)
                  (message "Set up feature mapping: %s -> %s" base-name el-file)))))))
      
      ;; Add all other src subdirectories
      (dolist (dir (directory-files src-dir t "\\`[^.]"))
        (when (file-directory-p dir)
          (add-to-list 'load-path dir))))
    
    ;; Add tests directory and its subdirectories
    (when (file-directory-p tests-dir)
      (add-to-list 'load-path tests-dir)
      (dolist (dir (directory-files tests-dir t "\\`[^.]"))
        (when (file-directory-p dir)
          (add-to-list 'load-path dir))))
    
    ;; Add other immediate subdirectories
    (dolist (dir (directory-files parent t "\\`[^.]"))
      (when (and (file-directory-p dir)
                 (not (string= dir src-dir))
                 (not (string= dir tests-dir))
                 (not (string-match-p "/\\.\\|/\\.\\." dir)) 
                 (not (string-match-p "contrib\\|.old" dir)))
        (add-to-list 'load-path dir))))
  
  ;; Return load-path for debugging
  load-path)

;; Initialize load path immediately
(ecc--add-to-loadpath)

;; ---- Module Loading ----

;; Core modules (minimal set)
(require 'ecc-variables)        ;; Variables and customization
(condition-case nil
    (require 'ecc-update-mode-line)
  ;; Mode line updates
  (error (message "ecc-update-mode-line could not be loaded")))

;; Try to load key bindings (optional)
(condition-case nil
    (require 'ecc-bindings)
  ;; Key bindings
  (error (message "ecc-bindings could not be loaded")))

;; Try to load mode definition (optional)
(condition-case nil
    (require 'ecc-mode)
  ;; Mode definition
  (error (message "ecc-mode could not be loaded")))

;; Try to load buffer management (optional)
(condition-case nil
    (require 'ecc-buffer)
  ;; Buffer management
  (error (message "ecc-buffer could not be loaded")))

;; Template system modules (optional)
(condition-case nil
    (progn
      (require 'ecc-template-cache) ;; Regular require for ecc-template-cache
      (require 'ecc-template)
      (require 'ecc-template-mode))
  (error (message "Template system could not be loaded")))

;; ;; Repository integration modules
;; (require 'ecc-repository)
;; (require 'ecc-repository-view)
;; (require 'ecc-dired)

;; Terminal integration modules (optional)
(condition-case nil
    (progn
      (require 'ecc-claude-vterm-mode)
      ;; Only require the runner if the mode loaded successfully
      (require 'ecc-run-vterm)
      (message "Claude VTerm mode loaded successfully"))
  (error
   (message
    "Claude vterm mode could not be loaded (vterm may not be installed)")))

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