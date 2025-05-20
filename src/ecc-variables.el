;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 20:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-variables.el

;;; Commentary:
;;; Compatibility wrapper for ecc-variables-consolidated.
;;; 
;;; This module provides backward compatibility with the original variables
;;; module by simply requiring the consolidated version. This ensures that
;;; existing code that imports ecc-variables will continue to work without
;;; modification.
;;;
;;; New code should use ecc-variables-consolidated directly.

;;; Code:

(require 'ecc-variables-consolidated)

;; Provide the same name for backward compatibility
(provide 'ecc-variables)

;;; ecc-variables.el ends here