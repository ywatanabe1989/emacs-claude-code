;;; ecc-auto-response-buffer-local.el --- Compatibility shim for old tests -*- lexical-binding: t -*-

;;; Commentary:
;; This is a compatibility shim to support old tests that use the
;; ecc-auto-response-buffer-local module which has been refactored.

;;; Code:

(require 'ecc-auto-response)
(require 'ecc-buffer-local)

;; Compatibility aliases for old function names
(defalias 'ecc-auto-response-buffer-local-check 'ecc-auto-response--process-buffer-global)
(defalias 'ecc-auto-response-buffer-local-init 'ecc-buffer-local-init)

(defun ecc-auto-response-buffer-local-enable-buffer ()
  "Enable auto-response in current buffer."
  (setq-local ecc-buffer-auto-response-enabled t))

(defun ecc-auto-response-buffer-local-disable-buffer ()
  "Disable auto-response in current buffer."
  (setq-local ecc-buffer-auto-response-enabled nil))

(provide 'ecc-auto-response-buffer-local)
;;; ecc-auto-response-buffer-local.el ends here