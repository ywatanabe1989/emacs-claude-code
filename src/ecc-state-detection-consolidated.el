;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 17:45:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-state-detection-consolidated.el

;;; Commentary:
;;; Wrapper for the consolidated Claude prompt state detection functionality.
;;; This file provides backward compatibility for code that requires the
;;; consolidated module specifically. The actual implementation exists in
;;; ecc-state-detection.el.
;;;
;;; This approach allows for a clean transition path while consolidating 
;;; our codebase to reduce duplication.

(require 'ecc-state-detection)

;;; Code:

;; No additional code needed as ecc-state-detection.el already
;; provides 'ecc-state-detection-consolidated

(provide 'ecc-state-detection-consolidated)

;;; ecc-state-detection-consolidated.el ends here