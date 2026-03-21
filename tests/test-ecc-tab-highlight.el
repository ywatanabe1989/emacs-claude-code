;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-03-21 12:00:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-tab-highlight.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)

(require 'ert)
(require 'ecc-tab-highlight)

(ert-deftest test-ecc-tab-highlight-loadable ()
  "Test that ecc-tab-highlight loads correctly."
  (should (featurep 'ecc-tab-highlight)))

(ert-deftest test-ecc-tab-highlight-enabled-by-default ()
  "Tab highlight should be enabled by default."
  (should ecc-tab-highlight-enabled))

(ert-deftest test-ecc-tab-highlight-default-colors-defined ()
  "Default background and foreground colors should be defined."
  (should (stringp ecc-tab-highlight-default-bg))
  (should (stringp ecc-tab-highlight-default-fg)))

(ert-deftest test-ecc-tab-highlight-compute-face-function ()
  "Compute face function should exist."
  (should (functionp 'ecc-tab-highlight--compute-face)))

(ert-deftest test-ecc-tab-highlight-update-function ()
  "Update function should exist."
  (should (functionp 'ecc-tab-highlight--update)))

(ert-deftest test-ecc-tab-highlight-restore-function ()
  "Restore function should exist."
  (should (functionp 'ecc-tab-highlight--restore)))

(ert-deftest test-ecc-tab-highlight-active-initially-nil ()
  "Active flag should be nil initially."
  (should-not ecc-tab-highlight--active))

(ert-deftest test-ecc-tab-highlight-compute-face-no-buffers ()
  "Compute face should return nil when no registered buffers."
  (cl-letf
      (((symbol-function '--ecc-auto-response-get-registered-buffers)
        (lambda () nil)))
    (should-not (ecc-tab-highlight--compute-face))))

(ert-deftest test-ecc-tab-highlight-compute-face-enabled-pulse ()
  "Compute face should return a plist when buffer has pulse active."
  (let ((test-buf (generate-new-buffer " *test-highlight*")))
    (unwind-protect
        (progn
          (with-current-buffer test-buf
            (setq-local --ecc-auto-response--enabled t)
            (setq-local --ecc-auto-response--pulse-state t)
            (setq-local ecc-speaking--flash-active nil)
            (setq-local --ecc-auto-response--yellow-flash-state nil))
          (cl-letf
	      (((symbol-function
		 '--ecc-auto-response-get-registered-buffers)
                (lambda () (list test-buf))))
            (let ((face (ecc-tab-highlight--compute-face)))
              (should face)
              (should (plist-get face :background)))))
      (kill-buffer test-buf))))

(ert-deftest test-ecc-tab-highlight-compute-face-speaking-priority ()
  "Speaking flash should have higher priority than pulse."
  (let ((test-buf (generate-new-buffer " *test-highlight*")))
    (unwind-protect
        (progn
          (with-current-buffer test-buf
            (setq-local --ecc-auto-response--enabled t)
            (setq-local --ecc-auto-response--pulse-state t)
            (setq-local ecc-speaking--flash-active t)
            (setq-local --ecc-auto-response--yellow-flash-state nil))
          (cl-letf
	      (((symbol-function
		 '--ecc-auto-response-get-registered-buffers)
                (lambda () (list test-buf))))
            (let ((face (ecc-tab-highlight--compute-face)))
              (should (string-match-p "00.*55\\|00.*33"
                                      (plist-get face :background))))))
      (kill-buffer test-buf))))

(provide 'test-ecc-tab-highlight)

(when (not load-file-name)
  (message "test-ecc-tab-highlight.el loaded."))
