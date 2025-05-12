;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 02:07:59>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/notcategorized/test-ecc-buffer-manager.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; test-ecc-buffer-manager.el --- Tests for ecc-buffer-manager.el -*- lexical-binding: t -*-

(require 'ert)
(require 'test-setup)
(require 'ecc-buffer-manager)

;; Test buffer creation and registration
(ert-deftest test-ecc-buffer-manager-creation ()
  "Test buffer creation and properties."
  ;; Initialize to ensure clean state
  (ecc-buffer-manager-init)

  ;; Create a test buffer
  (let* ((buffer-name "test-claude-buffer")
         (claude-buffer (ecc-buffer-manager-create buffer-name)))

    ;; Test basic properties
    (should claude-buffer)
    (should (ecc-buffer-p claude-buffer))
    (should (string= (ecc-buffer-name claude-buffer) buffer-name))
    (should (buffer-live-p (ecc-buffer-buffer claude-buffer)))
    (should (eq (ecc-buffer-state claude-buffer) 'idle))

    ;; Clean up
    (kill-buffer (ecc-buffer-buffer claude-buffer))))

(ert-deftest test-ecc-buffer-manager-get-by-id ()
  "Test retrieving a buffer by ID."
  ;; Initialize to ensure clean state
  (ecc-buffer-manager-init)

  ;; Create a test buffer
  (let* ((buffer-name "test-claude-buffer")
         (claude-buffer (ecc-buffer-manager-create buffer-name))
         (id (ecc-buffer-id claude-buffer)))

    ;; Verify it can be retrieved by ID
    (let ((retrieved (ecc-buffer-manager-get-by-id id)))
      (should retrieved)
      (should (eq retrieved claude-buffer))
      (should (string= (ecc-buffer-name retrieved) buffer-name)))

    ;; Clean up
    (kill-buffer (ecc-buffer-buffer claude-buffer))))

(ert-deftest test-ecc-buffer-manager-get-by-buffer ()
  "Test retrieving a Claude buffer by buffer object or name."
  ;; Initialize to ensure clean state
  (ecc-buffer-manager-init)

  ;; Create a test buffer
  (let* ((buffer-name "test-claude-buffer")
         (claude-buffer (ecc-buffer-manager-create buffer-name))
         (buffer-obj (ecc-buffer-buffer claude-buffer)))

    ;; Verify it can be retrieved by buffer object
    (let ((retrieved (ecc-buffer-manager-get-by-buffer buffer-obj)))
      (should retrieved)
      (should (eq retrieved claude-buffer)))

    ;; Verify it can be retrieved by buffer name
    (let ((retrieved (ecc-buffer-manager-get-by-buffer buffer-name)))
      (should retrieved)
      (should (eq retrieved claude-buffer)))

    ;; Clean up
    (kill-buffer buffer-obj)))

;; Test buffer management functions
(ert-deftest test-ecc-buffer-manager-set-current ()
  "Test setting the current Claude buffer."
  ;; Initialize to ensure clean state
  (ecc-buffer-manager-init)

  ;; Create two test buffers
  (let* ((buffer-name1 "test-claude-buffer-1")
         (buffer-name2 "test-claude-buffer-2")
         (claude-buffer1 (ecc-buffer-manager-create buffer-name1))
         (claude-buffer2 (ecc-buffer-manager-create buffer-name2)))

    ;; Verify first buffer is automatically current
    (should (eq (ecc-buffer-manager-get-current) claude-buffer1))

    ;; Set second buffer as current
    (ecc-buffer-manager-set-current claude-buffer2)
    (should (eq (ecc-buffer-manager-get-current) claude-buffer2))

    ;; Set first buffer as current by buffer object
    (ecc-buffer-manager-set-current (ecc-buffer-buffer claude-buffer1))
    (should (eq (ecc-buffer-manager-get-current) claude-buffer1))

    ;; Clean up
    (kill-buffer (ecc-buffer-buffer claude-buffer1))
    (kill-buffer (ecc-buffer-buffer claude-buffer2))))

(ert-deftest test-ecc-buffer-manager-state ()
  "Test setting and getting buffer state."
  ;; Initialize to ensure clean state
  (ecc-buffer-manager-init)

  ;; Create a test buffer
  (let* ((buffer-name "test-claude-buffer")
         (claude-buffer (ecc-buffer-manager-create buffer-name)))

    ;; Default state should be idle
    (should (eq (ecc-buffer-manager-get-state claude-buffer) 'idle))

    ;; Set state to running
    (ecc-buffer-manager-set-state claude-buffer 'running)
    (should (eq (ecc-buffer-manager-get-state claude-buffer) 'running))

    ;; Set state to waiting
    (ecc-buffer-manager-set-state claude-buffer 'waiting)
    (should (eq (ecc-buffer-manager-get-state claude-buffer) 'waiting))

    ;; Clean up
    (kill-buffer (ecc-buffer-buffer claude-buffer))))

(ert-deftest test-ecc-buffer-manager-metadata ()
  "Test setting and getting buffer metadata."
  ;; Initialize to ensure clean state
  (ecc-buffer-manager-init)

  ;; Create a test buffer
  (let* ((buffer-name "test-claude-buffer")
         (claude-buffer (ecc-buffer-manager-create buffer-name)))

    ;; Set metadata
    (ecc-buffer-manager-set-metadata claude-buffer 'test-key
                                     "test-value")
    (should
     (string=
      (ecc-buffer-manager-get-metadata claude-buffer 'test-key)
      "test-value"))

    ;; Set another metadata key
    (ecc-buffer-manager-set-metadata claude-buffer 'numeric-key 42)
    (should
     (= (ecc-buffer-manager-get-metadata claude-buffer 'numeric-key)
        42))

    ;; Clean up
    (kill-buffer (ecc-buffer-buffer claude-buffer))))

(ert-deftest test-ecc-buffer-manager-rename ()
  "Test renaming a buffer."
  ;; Initialize to ensure clean state
  (ecc-buffer-manager-init)

  ;; Create a test buffer
  (let* ((buffer-name "test-claude-buffer")
         (claude-buffer (ecc-buffer-manager-create buffer-name))
         (new-name "renamed-claude-buffer"))

    ;; Rename the buffer
    (ecc-buffer-manager-rename claude-buffer new-name)

    ;; Verify the name was updated in both the structure and the actual buffer
    (should (string= (ecc-buffer-name claude-buffer) new-name))
    (should
     (string= (buffer-name (ecc-buffer-buffer claude-buffer)) new-name))

    ;; Clean up
    (kill-buffer (ecc-buffer-buffer claude-buffer))))

(ert-deftest test-ecc-buffer-manager-kill ()
  "Test killing a buffer."
  ;; Initialize to ensure clean state
  (ecc-buffer-manager-init)

  ;; Create two test buffers
  (let* ((buffer-name1 "test-claude-buffer-1")
         (buffer-name2 "test-claude-buffer-2")
         (claude-buffer1 (ecc-buffer-manager-create buffer-name1))
         (claude-buffer2 (ecc-buffer-manager-create buffer-name2))
         (buffer-obj1 (ecc-buffer-buffer claude-buffer1)))

    ;; Kill the first buffer
    (ecc-buffer-manager-kill claude-buffer1)

    ;; Verify it's gone from the manager and actual buffer is killed
    (should-not (buffer-live-p buffer-obj1))
    (should-not
     (ecc-buffer-manager-get-by-id (ecc-buffer-id claude-buffer1)))

    ;; Verify second buffer became current
    (should (eq (ecc-buffer-manager-get-current) claude-buffer2))

    ;; Clean up
    (kill-buffer (ecc-buffer-buffer claude-buffer2))))

(ert-deftest test-ecc-buffer-manager-cleanup ()
  "Test cleaning up killed buffers from registry."
  ;; Initialize to ensure clean state
  (ecc-buffer-manager-init)

  ;; Create two test buffers
  (let* ((buffer-name1 "test-claude-buffer-1")
         (buffer-name2 "test-claude-buffer-2")
         (claude-buffer1 (ecc-buffer-manager-create buffer-name1))
         (claude-buffer2 (ecc-buffer-manager-create buffer-name2))
         (buffer-obj1 (ecc-buffer-buffer claude-buffer1)))

    ;; Kill the first buffer directly
    (kill-buffer buffer-obj1)

    ;; Verify it's still in the registry but buffer is killed
    (should-not (buffer-live-p buffer-obj1))
    (should
     (ecc-buffer-manager-get-by-id (ecc-buffer-id claude-buffer1)))

    ;; Run cleanup
    (should (= (ecc-buffer-manager-cleanup) 1))

    ;; Verify it's gone from the registry
    (should-not
     (ecc-buffer-manager-get-by-id (ecc-buffer-id claude-buffer1)))

    ;; Verify second buffer became current
    (should (eq (ecc-buffer-manager-get-current) claude-buffer2))

    ;; Clean up
    (kill-buffer (ecc-buffer-buffer claude-buffer2))))

;; Test navigation functions
(ert-deftest test-ecc-buffer-manager-navigation ()
  "Test navigation between buffers."
  ;; Initialize to ensure clean state
  (ecc-buffer-manager-init)

  ;; Create three test buffers
  (let* ((buffer-name1 "test-claude-buffer-1")
         (buffer-name2 "test-claude-buffer-2")
         (buffer-name3 "test-claude-buffer-3")
         (claude-buffer1 (ecc-buffer-manager-create buffer-name1))
         (claude-buffer2 (ecc-buffer-manager-create buffer-name2))
         (claude-buffer3 (ecc-buffer-manager-create buffer-name3)))

    ;; First buffer should be current
    (should (eq (ecc-buffer-manager-get-current) claude-buffer1))

    ;; Test next navigation - we need to ensure that buffer selection changes
    (ecc-buffer-manager-next)
    (let ((current (ecc-buffer-manager-get-current)))
      (should (not (eq current claude-buffer1))))

    ;; Navigate through all buffers to ensure we have coverage
    (ecc-buffer-manager-next)
    (ecc-buffer-manager-next)

    ;; Test previous navigation - ensure we can navigate backward
    (ecc-buffer-manager-previous)
    (let ((current (ecc-buffer-manager-get-current)))
      (should (buffer-live-p (ecc-buffer-buffer current))))

    ;; Clean up
    (kill-buffer (ecc-buffer-buffer claude-buffer1))
    (kill-buffer (ecc-buffer-buffer claude-buffer2))
    (kill-buffer (ecc-buffer-buffer claude-buffer3))))

;; Test hooks
(ert-deftest test-ecc-buffer-manager-hooks ()
  "Test buffer manager hooks."
  ;; Initialize to ensure clean state
  (ecc-buffer-manager-init)

  ;; Set up test hooks
  (let ((create-hook-called nil)
        (select-hook-called nil)
        (rename-hook-called nil)
        (update-hook-called nil)
        (kill-hook-called nil))

    ;; Add hooks
    (ecc-buffer-manager-add-hook 'create
                                 (lambda (cb)
                                   (setq create-hook-called cb)))
    (ecc-buffer-manager-add-hook 'select
                                 (lambda (cb)
                                   (setq select-hook-called cb)))
    (ecc-buffer-manager-add-hook 'rename
                                 (lambda (cb)
                                   (setq rename-hook-called cb)))
    (ecc-buffer-manager-add-hook 'update
                                 (lambda (cb)
                                   (setq update-hook-called cb)))
    (ecc-buffer-manager-add-hook 'kill
                                 (lambda (cb)
                                   (setq kill-hook-called cb)))

    ;; Create a buffer - should trigger create hook
    (let* ((buffer-name "test-claude-buffer")
           (claude-buffer (ecc-buffer-manager-create buffer-name)))

      ;; Verify create hook was called
      (should create-hook-called)
      (should (eq create-hook-called claude-buffer))
      (setq create-hook-called nil)

      ;; Test select hook
      (ecc-buffer-manager-set-current claude-buffer)
      (should select-hook-called)
      (should (eq select-hook-called claude-buffer))
      (setq select-hook-called nil)

      ;; Test rename hook
      (ecc-buffer-manager-rename claude-buffer "renamed-buffer")
      (should rename-hook-called)
      (should (eq rename-hook-called claude-buffer))
      (setq rename-hook-called nil)

      ;; Test update hook
      (ecc-buffer-manager-set-state claude-buffer 'running)
      (should update-hook-called)
      (should (eq update-hook-called claude-buffer))
      (setq update-hook-called nil)

      ;; Test kill hook
      (ecc-buffer-manager-kill claude-buffer)
      (should kill-hook-called)
      (should (eq kill-hook-called claude-buffer)))))

;; ;; Run the tests
;; (when (and (fboundp 'ert-run-tests-batch-and-exit)
;;            (not (member 'no-run-tests-automatically command-line-args)))
;;   (ert-run-tests-batch-and-exit
;;    (regexp-quote (symbol-name (quote ecc-buffer-manager)))))

;;; test-ecc-buffer-manager.el ends here


(provide 'test-ecc-buffer-manager)

(when
    (not load-file-name)
  (message "test-ecc-buffer-manager.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))