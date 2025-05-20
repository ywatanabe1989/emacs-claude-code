;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 16:48:30>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-variables.el

;;; Commentary:
;;; Transition module for ecc-variables-refactored.
;;; This file provides backward compatibility with the old ecc-variables module.
;;; It simply requires ecc-variables-refactored and provides the same feature name.
;;; This allows existing code to continue using (require 'ecc-variables) without
;;; breaking functionality.

;;; Code:

(require 'ecc-variables-refactored)

;; Provide the old feature name for backward compatibility
(provide 'ecc-variables)

;;; ecc-variables.el ends here