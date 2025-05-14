;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 21:15:23>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-ui/test-ecc-ui-mode-line.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ert)
(require 'ecc-ui-mode-line)

(ert-deftest test-ecc-ui-mode-line-loadable ()
  (should (featurep 'ecc-ui-mode-line)))

(ert-deftest test-ecc-ui-mode-line-indicators-defined ()
  (should (boundp 'ecc-ui--mode-line-indicators))
  (should (hash-table-p ecc-ui--mode-line-indicators)))

(ert-deftest test-ecc-ui-mode-line-update-defined ()
  (should (fboundp 'ecc-ui-mode-line-update)))

(ert-deftest test-ecc-ui-mode-line-update-adds-indicator ()
  (let ((orig-global-mode-string global-mode-string))
    (unwind-protect
        (progn
          (setq global-mode-string nil)
          (ecc-ui-mode-line-update 'test t "TEST" '(:foreground "red"))
          (should
           (member '(:eval (ecc-ui-mode-line-status))
                   global-mode-string)))
      (setq global-mode-string orig-global-mode-string))))

(ert-deftest test-ecc-ui-mode-line-update-removes-indicator
    ()
  (let ((orig-global-mode-string global-mode-string)
        (orig-mode-line-indicators ecc-ui--mode-line-indicators))
    (unwind-protect
        (progn
          (setq global-mode-string 
                (list '(:eval (ecc-ui-mode-line-status))))
          (clrhash ecc-ui--mode-line-indicators)
          (puthash 'test (cons "TEST" '(:foreground "red")) ecc-ui--mode-line-indicators)
          (ecc-ui-mode-line-update 'test nil)
          (should-not
           (member '(:eval (ecc-ui-mode-line-status))
                   global-mode-string))
          (should (= 0 (hash-table-count ecc-ui--mode-line-indicators))))
      (progn
        (setq global-mode-string orig-global-mode-string)
        (setq ecc-ui--mode-line-indicators orig-mode-line-indicators)))))

(ert-deftest test-ecc-ui-highlight-buffer-adds-overlay ()
  (let ((mock-buffer (generate-new-buffer "*MOCK-CLAUDE*"))
        (overlay-created nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'make-overlay)
                     (lambda (&rest _) 
                       (setq overlay-created t)
                       'mock-overlay))
                    ((symbol-function 'overlay-put)
                     (lambda (&rest _) nil))
                    ((symbol-function 'delete-overlay)
                     (lambda (&rest _) nil)))
            (ecc-ui-highlight-buffer mock-buffer)
            (should overlay-created)))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer)))))

(ert-deftest test-ecc-ui-highlight-buffer-removes-overlay ()
  (let ((mock-buffer (generate-new-buffer "*MOCK-CLAUDE*"))
        (overlay-deleted nil)
        (mock-overlay (make-overlay (point-min) (point-min))))
    (unwind-protect
        (progn
          (with-current-buffer mock-buffer
            (setq-local ecc-ui--buffer-overlay mock-overlay))
          (cl-letf (((symbol-function 'delete-overlay)
                     (lambda (overlay)
                       (when (overlay-buffer overlay)
                         (setq overlay-deleted t)))))
            (ecc-ui-highlight-buffer mock-buffer)
            (should overlay-deleted)))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer)))))

(ert-deftest test-ecc-ui-mode-line-update-force-updates ()
  (let ((mode-line-updated nil))
    (cl-letf (((symbol-function 'force-mode-line-update)
               (lambda (&rest _) (setq mode-line-updated t)))
              ((symbol-function 'make-overlay)
               (lambda (&rest _) nil))
              ((symbol-function 'overlay-put)
               (lambda (&rest _) nil))
              ((symbol-function 'delete-overlay)
               (lambda (&rest _) nil)))
      (ecc-ui-mode-line-update 'test t "TEST" nil)
      (should mode-line-updated))))

(ert-deftest test-ecc-ui-mode-line-status ()
  (let ((orig-mode-line-indicators ecc-ui--mode-line-indicators))
    (unwind-protect
        (progn
          (clrhash ecc-ui--mode-line-indicators)
          (puthash 'test1 (cons "TEST1" '(:foreground "red")) ecc-ui--mode-line-indicators)
          (puthash 'test2 (cons "TEST2" '(:foreground "blue")) ecc-ui--mode-line-indicators)
          (let ((status (ecc-ui-mode-line-status)))
            (should (stringp status))
            (should (or (string-match-p "TEST1" status)
                        (string-match-p "TEST2" status)))))
      (setq ecc-ui--mode-line-indicators orig-mode-line-indicators))))


(provide 'test-ecc-ui-mode-line)

(when (not load-file-name)
  (message "test-ecc-ui-mode-line.el loaded: %s"
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))