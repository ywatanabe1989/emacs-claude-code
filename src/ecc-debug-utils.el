;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 20:00:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-debug-utils.el

;;; Commentary:
;;; Compatibility wrapper for ecc-debug-utils-consolidated.
;;; 
;;; This module provides backward compatibility with the original debug-utils
;;; module by simply requiring the consolidated version. This ensures that
;;; existing code that imports ecc-debug-utils will continue to work without
;;; modification.
;;;
;;; New code should use ecc-debug-utils-consolidated directly.

;;; Code:

(require 'ecc-debug-utils-consolidated)

;; Provide the same name for backward compatibility
(provide 'ecc-debug-utils)

;;; ecc-debug-utils.el ends here