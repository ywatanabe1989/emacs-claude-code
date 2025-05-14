;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 17:59:29>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-state/ecc-state.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(let
    ((this-dir
      (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path this-dir))

(require 'ecc-state-variables)
(require 'ecc-state-detect-prompt)
(require 'ecc-state-send)


(provide 'ecc-state)

(when
    (not load-file-name)
  (message "ecc-state.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))