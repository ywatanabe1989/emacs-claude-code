;;; test-ehw-prep.el --- Tests for ehw-prep -*- lexical-binding: t; -*-

;; Author: ywatanabe
;; Timestamp: <2025-05-12 22:15:45>

;;; Commentary:
;; Tests for the ehw-prep module

;;; Code:

(require 'ert)
(require 'ehw-prep)
(require 'ehw-prep-variables)
(require 'ehw-prep-add-path)
(require 'ehw-prep-install-deps)

(ert-deftest test-ehw-prep-variables-loadable ()
  "Test if ehw-prep-variables is loadable."
  (should
   (featurep 'ehw-prep-variables)))

(ert-deftest test-ehw-prep-add-path-loadable ()
  "Test if ehw-prep-add-path is loadable."
  (should
   (featurep 'ehw-prep-add-path)))

(ert-deftest test-ehw-prep-install-deps-loadable ()
  "Test if ehw-prep-install-deps is loadable."
  (should
   (featurep 'ehw-prep-install-deps)))

(ert-deftest test-ehw-prep-setup-is-function ()
  "Test if ehw-prep-setup is a function."
  (should
   (functionp 'ehw-prep-setup)))

(ert-deftest test-ehw-prep-install-dependencies-is-function ()
  "Test if ehw-prep-install-dependencies is a function."
  (should
   (functionp 'ehw-prep-install-dependencies)))

(provide 'test-ehw-prep)
;;; test-ehw-prep.el ends here