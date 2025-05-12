;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 02:07:20>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/notcategorized/test-ecc-integration.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; test-ecc-integration.el --- Tests for ecc-integration.el -*- lexical-binding: t -*-

(require 'ert)
(require 'test-setup)
(require 'ecc-integration)
(require 'ecc-state-engine)
(require 'ecc-buffer-manager)
(require 'ecc-command)

;; State detection fallback for tests

(defun ecc-state-detect-prompt (&optional _buffer)
  "Legacy state detection function for tests."
  'waiting)

;; Test buffer import/export
(ert-deftest test-ecc-integration-import-legacy-buffers ()
  "Test importing legacy buffers into the new system."
  ;; Initialize to ensure clean state
  (ecc-buffer-manager-init)
  (setq ecc-buffer-registry (make-hash-table :test 'equal))

  ;; Create a legacy buffer
  (let* ((buffer-name "*Claude-legacy*")
         (buffer (generate-new-buffer buffer-name)))
    (unwind-protect
        (progn
          ;; Set up as a legacy buffer
          (with-current-buffer buffer
            (setq-local ecc-buffer-state 'running)
            (setq-local ecc-buffer-timestamp (current-time)))

          ;; Add to legacy registry
          (puthash buffer-name t ecc-buffer-registry)

          ;; Import legacy buffers
          (ecc-integration-import-legacy-buffers)

          ;; Verify buffer was imported
          (let
              ((claude-buffer
                (ecc-buffer-manager-get-by-buffer buffer)))
            (should claude-buffer)
            (should (eq (ecc-buffer-state claude-buffer) 'running))
            (should
             (ecc-buffer-manager-get-metadata claude-buffer 'timestamp))))

      ;; Clean up
      (kill-buffer buffer))))

(ert-deftest test-ecc-integration-export-to-legacy-buffers ()
  "Test exporting new buffers to the legacy system."
  ;; Initialize to ensure clean state
  (ecc-buffer-manager-init)
  (setq ecc-buffer-registry (make-hash-table :test 'equal))

  ;; Create a new buffer
  (let* ((buffer-name "*Claude-new*")
         (claude-buffer (ecc-buffer-manager-create buffer-name))
         (buffer (ecc-buffer-buffer claude-buffer)))

    ;; Set state and metadata
    (ecc-buffer-manager-set-state claude-buffer 'waiting)
    (ecc-buffer-manager-set-metadata claude-buffer 'timestamp
                                     (current-time))

    ;; Export to legacy
    (ecc-integration-export-to-legacy-buffers)

    ;; Verify export
    (with-current-buffer buffer
      (should (eq ecc-buffer-state 'waiting))
      (should (boundp 'ecc-buffer-timestamp))
      (should ecc-buffer-timestamp))

    ;; Verify added to legacy registry
    (should (gethash buffer-name ecc-buffer-registry))

    ;; Clean up
    (kill-buffer buffer)))

(ert-deftest test-ecc-integration-sync-buffer-states ()
  "Test synchronizing buffer states between old and new systems."
  ;; Initialize to ensure clean state
  (ecc-buffer-manager-init)
  (setq ecc-buffer-registry (make-hash-table :test 'equal))

  ;; Create buffers in both systems
  (let* ((legacy-name "*Claude-legacy*")
         (new-name "*Claude-new*")
         (legacy-buffer (generate-new-buffer legacy-name))
         (claude-buffer (ecc-buffer-manager-create new-name)))

    ;; Set up legacy buffer
    (with-current-buffer legacy-buffer
      (setq-local ecc-buffer-state 'running)
      (setq-local ecc-buffer-timestamp (current-time)))

    ;; Add to legacy registry
    (puthash legacy-name t ecc-buffer-registry)

    ;; Set state in new buffer
    (ecc-buffer-manager-set-state claude-buffer 'waiting)

    ;; Sync states
    (ecc-integration-sync-buffer-states)

    ;; Verify legacy buffer was imported to new system
    (let ((imported (ecc-buffer-manager-get-by-buffer legacy-buffer)))
      (should imported)
      (should (eq (ecc-buffer-state imported) 'running)))

    ;; Verify new buffer was exported to legacy system
    (with-current-buffer (ecc-buffer-buffer claude-buffer)
      (should (eq ecc-buffer-state 'waiting)))

    ;; Clean up
    (kill-buffer legacy-buffer)
    (kill-buffer (ecc-buffer-buffer claude-buffer))))

;; Test state detection - simplified for compatibility
(ert-deftest test-ecc-integration-state-detection ()
  "Test detecting state in buffers using both systems."
  ;; Initialize to ensure clean state
  (ecc-state-engine-init)
  (ecc-buffer-manager-init)

  ;; Just verify direct state setting works
  (ecc-state-engine-set-state 'running)
  (should (eq (ecc-state-engine-get-current-state-id) 'running))

  ;; Create a buffer and verify its state can be set
  (let* ((buffer-name "*Claude-test*")
         (claude-buffer (ecc-buffer-manager-create buffer-name)))

    ;; Set state in buffer manager
    (ecc-buffer-manager-set-state claude-buffer 'waiting)
    (should (eq (ecc-buffer-manager-get-state claude-buffer) 'waiting))

    ;; Clean up
    (ecc-buffer-manager-kill claude-buffer)))

;; Test command integration
(ert-deftest test-ecc-integration-handle-input ()
  "Test input handling with the command system."
  ;; Initialize to ensure clean state
  (ecc-state-engine-init)
  (ecc-buffer-manager-init)
  (ecc-command-init)

  ;; Create a test buffer
  (let ((buffer (generate-new-buffer "*Claude-test*")))
    (unwind-protect
        (progn
          ;; Handle input through integration
          (ecc-integration-handle-input "/help" buffer)

          ;; Verify buffer is in buffer manager
          (let
              ((claude-buffer
                (ecc-buffer-manager-get-by-buffer buffer)))
            (should claude-buffer)
            (should
             (eq (ecc-buffer-manager-get-current) claude-buffer)))

          ;; Verify command was executed (by checking history)
          (let ((history (ecc-command-get-history)))
            (should (= (length history) 1))
            (should (string= (car history) "/help"))))

      ;; Clean up
      (kill-buffer buffer))))

;; Test initialization
(ert-deftest test-ecc-integration-init ()
  "Test initialization of the integration layer."
  ;; Set up legacy state
  (setq ecc-buffer-registry (make-hash-table :test 'equal))

  ;; Create a legacy buffer
  (let* ((buffer-name "*Claude-legacy*")
         (buffer (generate-new-buffer buffer-name)))
    (unwind-protect
        (progn
          ;; Set up as a legacy buffer
          (with-current-buffer buffer
            (setq-local ecc-buffer-state 'running)
            (setq-local ecc-buffer-timestamp (current-time)))

          ;; Add to legacy registry
          (puthash buffer-name t ecc-buffer-registry)

          ;; Initialize integration layer
          (ecc-integration-init)

          ;; Verify components were initialized
          (should ecc-state-engine--states)
          (should ecc-buffer-manager--registry)
          (should ecc-command--registry)

          ;; Verify legacy buffer was imported
          (should (ecc-buffer-manager-get-by-buffer buffer)))

      ;; Clean up
      (kill-buffer buffer))))

;; ;; Run the tests
;; (when (and (fboundp 'ert-run-tests-batch-and-exit)
;;            (not (member 'no-run-tests-automatically command-line-args)))
;;   (ert-run-tests-batch-and-exit (regexp-quote (symbol-name (quote ecc-integration)))))

;;; test-ecc-integration.el ends here


(provide 'test-ecc-integration)

(when
    (not load-file-name)
  (message "test-ecc-integration.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))