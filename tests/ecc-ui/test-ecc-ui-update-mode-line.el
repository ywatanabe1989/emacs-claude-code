;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 21:20:11>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-ui-update-mode-line.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ert)
(require 'ecc-ui-mode-line)

(ert-deftest test-ecc-ui-update-mode-line-loadable ()
  (should (featurep 'ecc-ui-mode-line)))

(ert-deftest test-ecc-mode-line-indicator-defined ()
  (should (boundp 'ecc-mode-line-indicator))
  (should (stringp ecc-mode-line-indicator)))

(ert-deftest test-ecc-ui-update-mode-line-defined ()
  (should (fboundp 'ecc-ui-update-mode-line)))

(ert-deftest test-ecc-ui-update-mode-line-adds-indicator ()
  (let ((orig-global-mode-string global-mode-string))
    (unwind-protect
        (progn
          (setq global-mode-string nil)
          (ecc-ui-update-mode-line t)
          (should
           (member ecc-mode-line-indicator
                   global-mode-string)))
      (setq global-mode-string orig-global-mode-string))))

(ert-deftest test-ecc-ui-update-mode-line-removes-indicator
    ()
  (let ((orig-global-mode-string global-mode-string))
    (unwind-protect
        (progn
          (setq global-mode-string
                (list ecc-mode-line-indicator))
          (ecc-ui-update-mode-line nil)
          (should-not
           (member ecc-mode-line-indicator
                   global-mode-string)))
      (setq global-mode-string orig-global-mode-string))))

(ert-deftest test-ecc-ui-update-mode-line-adds-overlay ()
  (let ((orig-buffer ecc-buffer-current-active-buffer)
        (mock-buffer (generate-new-buffer "*MOCK-CLAUDE*"))
        (overlay-created nil))
    (unwind-protect
        (progn
          (setq ecc-buffer-current-active-buffer mock-buffer)
          (cl-letf (((symbol-function 'make-overlay)
                     (lambda (&rest _) 
                       (setq overlay-created t)
                       'mock-overlay))
                    ((symbol-function 'overlay-put)
                     (lambda (&rest _) nil))
                    ((symbol-function 'delete-overlay)
                     (lambda (&rest _) nil)))
            (ecc-ui-update-mode-line t)
            (should overlay-created)))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (setq ecc-buffer-current-active-buffer orig-buffer))))

(ert-deftest test-ecc-ui-update-mode-line-removes-overlay ()
  (let ((orig-buffer ecc-buffer-current-active-buffer)
        (mock-buffer (generate-new-buffer "*MOCK-CLAUDE*"))
        (overlay-deleted nil)
        (mock-overlay (make-overlay (point-min) (point-min))))
    (unwind-protect
        (progn
          (setq ecc-buffer-current-active-buffer mock-buffer)
          (with-current-buffer mock-buffer
            (setq-local ecc-buffer-name-overlay mock-overlay))
          (cl-letf (((symbol-function 'delete-overlay)
                     (lambda (overlay)
                       (when (overlay-buffer overlay)
                         (setq overlay-deleted t)))))
            (ecc-ui-update-mode-line nil)
            (should overlay-deleted)))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (setq ecc-buffer-current-active-buffer orig-buffer))))

(ert-deftest test-ecc-ui-update-mode-line-force-updates ()
  (let ((mode-line-updated nil)
        (orig-active-buffer ecc-buffer-current-active-buffer))
    (unwind-protect
        (progn
          ;; Mock active buffer to avoid overlay errors
          (setq ecc-buffer-current-active-buffer nil)
          (cl-letf (((symbol-function 'force-mode-line-update)
                     (lambda (&rest _) (setq mode-line-updated t)))
                    ((symbol-function 'make-overlay)
                     (lambda (&rest _) nil))
                    ((symbol-function 'overlay-put)
                     (lambda (&rest _) nil))
                    ((symbol-function 'delete-overlay)
                     (lambda (&rest _) nil)))
            (ecc-ui-update-mode-line t)
            (should mode-line-updated)))
      (setq ecc-buffer-current-active-buffer orig-active-buffer))))


(provide 'test-ecc-ui-update-mode-line)

(when (not load-file-name)
  (message "test-ecc-ui-update-mode-line.el loaded: %s"
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))