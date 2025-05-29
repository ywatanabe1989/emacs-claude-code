;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-28 05:36:44>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-variables.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;; 1. Dependencies
;; ----------------------------------------
;; No dependencies for this module


;; 2. Configuration
;; ----------------------------------------
(defgroup ecc nil
  "Emacs Claude Code package."
  :prefix "--ecc-"
  :group 'tools)


;; 3. Variables
;; ----------------------------------------
(defconst --ecc-version "2.1.0"
  "Current version of the emacs-claude-code package.")


;; 4. Main Entry Points
;; ----------------------------------------
;; No main entry points in this file


;; 5. Core Functions
;; ----------------------------------------
;; No core functions in this file


;; 6. Helper/Utility Functions
;; ----------------------------------------
;; No helper functions in this file


(provide 'ecc-variables)

(when
    (not load-file-name)
  (message "ecc-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))