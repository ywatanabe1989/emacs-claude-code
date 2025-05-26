;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 11:15:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-term/test-ecc-term-claude-interaction.el

;;; Commentary:
;;; Tests for the ecc-term-claude-interaction module.

(require 'ert)
(require 'ecc-term-claude-interaction)

;;; Code:

;; Mock functions to avoid actual vterm interaction during tests
(defvar ecc-test-last-string-sent nil
  "Last string sent to vterm in tests.")

(defvar ecc-test-return-pressed nil
  "Whether return was pressed in tests.")

(defvar ecc-test-clear-called nil
  "Whether vterm-clear was called in tests.")

(defmacro with-mock-vterm (&rest body)
  "Execute BODY with mock vterm functions."
  (declare (indent 0))
  `(progn
     (setq ecc-test-last-string-sent nil
           ecc-test-return-pressed nil
           ecc-test-clear-called nil)
     (cl-letf (((symbol-function 'vterm-send-string)
                (lambda (str)
                  (setq ecc-test-last-string-sent str)))
               ((symbol-function 'vterm-send-return)
                (lambda ()
                  (setq ecc-test-return-pressed t)))
               ((symbol-function 'vterm-clear)
                (lambda ()
                  (setq ecc-test-clear-called t))))
       ,@body)))

(ert-deftest test-ecc-term-claude-send-yes ()
  "Test sending 'y' response."
  (with-mock-vterm
    (ecc-term-claude-send-yes)
    (should (string= ecc-test-last-string-sent "y"))
    (should ecc-test-return-pressed)))

(ert-deftest test-ecc-term-claude-send-no ()
  "Test sending 'n' response."
  (with-mock-vterm
    (ecc-term-claude-send-no)
    (should (string= ecc-test-last-string-sent "n"))
    (should ecc-test-return-pressed)))

(ert-deftest test-ecc-term-claude-clear-buffer ()
  "Test clearing buffer."
  (with-mock-vterm
    (ecc-term-claude-clear-buffer)
    (should ecc-test-clear-called)))

(ert-deftest test-ecc-term-claude-send-string ()
  "Test sending a custom string."
  (with-mock-vterm
    (ecc-term-claude-send-string "Hello Claude")
    (should (string= ecc-test-last-string-sent "Hello Claude"))
    (should ecc-test-return-pressed)))

(ert-deftest test-ecc-term-claude-toggle-follow-bottom ()
  "Test toggling follow-bottom functionality."
  ;; Test enabling
  (let ((ecc-vterm-always-follow-bottom nil))
    (ecc-term-claude-toggle-follow-bottom)
    (should ecc-vterm-always-follow-bottom))
  
  ;; Test disabling
  (let ((ecc-vterm-always-follow-bottom t))
    (ecc-term-claude-toggle-follow-bottom)
    (should-not ecc-vterm-always-follow-bottom)))

(ert-deftest test-ecc-term-claude-follow-bottom-after-output ()
  "Test follow-bottom function after output."
  (let ((ecc-vterm-always-follow-bottom t)
        (called-scroll nil))
    (cl-letf (((symbol-function 'ecc-term-claude-scroll-to-bottom)
               (lambda () (setq called-scroll t))))
      (ecc-term-claude-follow-bottom-after-output)
      (should called-scroll))))

(ert-deftest test-ecc-term-claude-follow-bottom-after-output-disabled ()
  "Test follow-bottom function is not called when disabled."
  (let ((ecc-vterm-always-follow-bottom nil)
        (called-scroll nil))
    (cl-letf (((symbol-function 'ecc-term-claude-scroll-to-bottom)
               (lambda () (setq called-scroll t))))
      (ecc-term-claude-follow-bottom-after-output)
      (should-not called-scroll))))

(ert-deftest test-ecc-term-claude-yes-alias ()
  "Test backward compatibility alias for yes function."
  (with-mock-vterm
    (ecc-term-claude-yes)
    (should (string= ecc-test-last-string-sent "y"))
    (should ecc-test-return-pressed)))

(provide 'test-ecc-term-claude-interaction)

;;; test-ecc-term-claude-interaction.el ends here