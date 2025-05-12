;;; ecc-compat.el --- Compatibility layer for different Emacs versions -*- lexical-binding: t -*-

;; Author: ywatanabe <ywatanabe@alumni.u-tokyo.ac.jp>
;; Maintainer: ywatanabe <ywatanabe@alumni.u-tokyo.ac.jp>
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: ai, convenience, tools
;; URL: https://github.com/ywatanabe1989/emacs-claude-code

;;; Commentary:
;; 
;; This module provides compatibility functions for different Emacs versions.
;; It ensures that newer functions are available even in older Emacs versions
;; by providing fallback implementations.

;;; Code:

(require 'cl-lib)

;; Compatibility for hash-table-values
(unless (fboundp 'hash-table-values)
  (defun hash-table-values (hash-table)
    "Return a list of values in HASH-TABLE."
    (let ((values '()))
      (maphash (lambda (_k v) (push v values)) hash-table)
      values)))

;; Compatibility for seq-position
(unless (fboundp 'seq-position)
  (defun seq-position (sequence elt &optional testfn)
    "Return the index of the first element in SEQUENCE that equals to ELT.
Equality is defined by TESTFN if non-nil or by `equal' if nil."
    (let ((testfn (or testfn #'equal)))
      (catch 'found
        (seq-map-indexed (lambda (e i)
                           (when (funcall testfn e elt)
                             (throw 'found i)))
                         sequence)
        nil))))

;; Compatibility for seq-map-indexed
(unless (fboundp 'seq-map-indexed)
  (defun seq-map-indexed (function sequence)
    "Return the result of applying FUNCTION to each element of SEQUENCE and its index.
FUNCTION takes two arguments: the element and its index within SEQUENCE."
    (let ((index 0)
          (result '()))
      (seq-doseq (elt sequence)
        (push (funcall function elt index) result)
        (setq index (1+ index)))
      (nreverse result))))

;; Compatibility for seq-sort-by
(unless (fboundp 'seq-sort-by)
  (defun seq-sort-by (function pred sequence)
    "Sort SEQUENCE using PRED as a comparison function.
Elements of SEQUENCE are transformed by FUNCTION before being sorted.
FUNCTION must be a function of one argument."
    (seq-sort (lambda (a b)
                (funcall pred
                         (funcall function a)
                         (funcall function b)))
              sequence)))

(provide 'ecc-compat)
;;; ecc-compat.el ends here