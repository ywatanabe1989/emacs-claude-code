;;; -*- coding: utf-8; lexical-binding: t -*-
;;; test-basic-state.el --- Basic state detection tests
;;; Commentary:
;;; Test basic functionality of the buffer state detection system.

;;; Code:
(require 'ert)
(require 'ecc-variables)
(require 'ecc-state-detection)

(ert-deftest test-basic-state-detection ()
  "Test basic state detection."
  (let ((temp-buffer (generate-new-buffer "*test-buffer*")))
    (unwind-protect
        (progn
          ;; Initial state should be nil
          (with-current-buffer temp-buffer
            (should-not (ecc-detect-state)))
          
          ;; Test y/n state
          (with-current-buffer temp-buffer
            (erase-buffer)
            (insert "Would you like to continue? [y/n]")
            (should (eq (ecc-detect-state) :y/n)))
          
          ;; Test y/y/n state
          (with-current-buffer temp-buffer
            (erase-buffer)
            (insert "Would you like to see more options? [Y/y/n]")
            (should (eq (ecc-detect-state) :y/y/n)))
          
          ;; Test waiting state
          (with-current-buffer temp-buffer
            (erase-buffer)
            (insert "Type 'continue>' to continue")
            (should (eq (ecc-detect-state) :waiting))))
      
      ;; Clean up
      (kill-buffer temp-buffer))))

(provide 'test-basic-state)
;;; test-basic-state.el ends here