;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 09:30:11>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-ui/ecc-variables.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;; This file provides backward compatibility for ecc-variables module
;; that was renamed to ecc-ui-variables during refactoring.

(require 'ecc-ui-variables)

;; Define group for backward compatibility
(unless (get 'ecc 'custom-group)
  (defgroup ecc nil
    "Customization group for ecc-variables."
    :group 'emacs))

;; Make sure the feature is loaded
(provide 'ecc-variables)

(when (not load-file-name)
  (message "ecc-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))