;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 02:08:31>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/notcategorized/test-ecc-command.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; test-ecc-command.el --- Tests for ecc-command.el -*- lexical-binding: t -*-

(require 'ert)
(require 'test-setup)
(require 'ecc-command)
(require 'ecc-buffer-manager)

;; Test command creation and registration
(ert-deftest test-ecc-command-creation ()
  "Test command creation and properties."
  (let ((command (ecc-command--create :id "test"
                                      :name "Test Command"
                                      :handler (lambda (args) args)
                                      :description "Test description"
                                      :usage "/test [arg]"
                                      :aliases '("t" "tst"))))
    ;; Test basic properties
    (should (string= (ecc-command-id command) "test"))
    (should (string= (ecc-command-name command) "Test Command"))
    (should (functionp (ecc-command-handler command)))
    (should
     (string= (ecc-command-description command) "Test description"))
    (should (string= (ecc-command-usage command) "/test [arg]"))
    (should (equal (ecc-command-aliases command) '("t" "tst")))))

(ert-deftest test-ecc-command-registration ()
  "Test command registration and retrieval."
  ;; Initialize to ensure clean state
  (ecc-command-init)

  ;; Register a new test command
  (let ((command (ecc-command--create :id "test-cmd"
                                      :name "Test Command"
                                      :handler (lambda (args) args)
                                      :description "Test description"
                                      :usage "/test-cmd [arg]"
                                      :aliases '("tc" "test"))))
    (ecc-command-register command)

    ;; Verify it can be retrieved by ID
    (let ((retrieved (ecc-command-get "test-cmd")))
      (should retrieved)
      (should (string= (ecc-command-id retrieved) "test-cmd"))
      (should (string= (ecc-command-name retrieved) "Test Command")))

    ;; Verify it can be retrieved by alias
    (should (eq (ecc-command-get "tc") command))
    (should (eq (ecc-command-get "test") command))

    ;; Verify exists check
    (should (ecc-command-exists-p "test-cmd"))
    (should (ecc-command-exists-p "tc"))
    (should-not (ecc-command-exists-p "nonexistent"))))

(ert-deftest test-ecc-command-list-all ()
  "Test listing all commands (excluding aliases)."
  ;; Initialize to ensure clean state
  (ecc-command-init)

  ;; Register two test commands
  (let ((command1 (ecc-command--create :id "test1"
                                       :name "Test 1"
                                       :handler (lambda (args) args)
                                       :description "Test 1"
                                       :aliases '("t1")))
        (command2 (ecc-command--create :id "test2"
                                       :name "Test 2"
                                       :handler (lambda (args) args)
                                       :description "Test 2"
                                       :aliases '("t2"))))
    (ecc-command-register command1)
    (ecc-command-register command2)

    ;; Get all commands
    (let ((all-commands (ecc-command-list-all)))
      ;; Should include our 2 test commands plus built-ins
      (should (member command1 all-commands))
      (should (member command2 all-commands)))))

;; Test command execution and history
(ert-deftest test-ecc-command-execution ()
  "Test command execution with various input formats."
  ;; Initialize to ensure clean state
  (ecc-command-init)
  (ecc-buffer-manager-init)

  ;; Create a test buffer for commands
  (let* ((buffer-name "test-claude-buffer")
         (claude-buffer (ecc-buffer-manager-create buffer-name))
         (executed-command nil)
         (executed-args nil))

    ;; Register a test command
    (ecc-command-register
     (ecc-command--create :id "test"
                          :name "Test"
                          :handler (lambda (args)
                                     (setq executed-command "test"
                                           executed-args args))
                          :aliases '("t")))

    ;; Execute command with no args
    (with-current-buffer (ecc-buffer-buffer claude-buffer)
      (ecc-command-execute "/test"))

    ;; Verify it was executed
    (should (string= executed-command "test"))
    (should (null executed-args))

    ;; Reset tracking
    (setq executed-command nil)
    (setq executed-args nil)

    ;; Execute command with args
    (with-current-buffer (ecc-buffer-buffer claude-buffer)
      (ecc-command-execute "/test with some args"))

    ;; Verify it was executed with args
    (should (string= executed-command "test"))
    (should (string= executed-args "with some args"))

    ;; Reset tracking
    (setq executed-command nil)
    (setq executed-args nil)

    ;; Execute command by alias
    (with-current-buffer (ecc-buffer-buffer claude-buffer)
      (ecc-command-execute "/t alias test"))

    ;; Verify it was executed
    (should (string= executed-command "test"))
    (should (string= executed-args "alias test"))

    ;; Check history
    (let ((history (ecc-command-get-history)))
      (should (= (length history) 3))
      (should (string= (nth 0 history) "/t alias test"))
      (should (string= (nth 1 history) "/test with some args"))
      (should (string= (nth 2 history) "/test")))

    ;; Clean up
    (kill-buffer (ecc-buffer-buffer claude-buffer))))

;; Test built-in commands
(ert-deftest test-ecc-command-help ()
  "Test the help command."
  ;; Initialize to ensure clean state
  (ecc-command-init)

  ;; Verify help command exists
  (should (ecc-command-exists-p "help"))

  ;; Test help command - should not raise errors
  (should-not (condition-case err
                  (progn
                    (ecc-command-execute "/help")
                    nil)
                (error err))))

(ert-deftest test-ecc-command-new-and-kill ()
  "Test the new and kill commands."
  ;; Initialize to ensure clean state
  (ecc-command-init)
  (ecc-buffer-manager-init)

  ;; Verify commands exist
  (should (ecc-command-exists-p "new"))
  (should (ecc-command-exists-p "kill"))

  ;; Create a new buffer
  (ecc-command-execute "/new test-buffer")

  ;; Verify it was created
  (let ((buffers (ecc-buffer-manager-get-all)))
    (should (= (length buffers) 1))
    (should (string= (ecc-buffer-name (car buffers)) "test-buffer")))

  ;; Kill the buffer
  (ecc-command-execute "/kill")

  ;; Verify it was killed
  (should (= (length (ecc-buffer-manager-get-all)) 0)))

(ert-deftest test-ecc-command-rename ()
  "Test the rename command."
  ;; Initialize to ensure clean state
  (ecc-command-init)
  (ecc-buffer-manager-init)

  ;; Create a new buffer
  (ecc-command-execute "/new original-name")

  ;; Verify it was created
  (let ((buffers (ecc-buffer-manager-get-all)))
    (should (= (length buffers) 1))
    (should (string= (ecc-buffer-name (car buffers)) "original-name")))

  ;; Rename the buffer
  (ecc-command-execute "/rename renamed-buffer")

  ;; Verify it was renamed
  (let ((buffers (ecc-buffer-manager-get-all)))
    (should (= (length buffers) 1))
    (should (string= (ecc-buffer-name (car buffers)) "renamed-buffer")))

  ;; Clean up
  (ecc-command-execute "/kill"))

;; ;; Run the tests
;; (when (and (fboundp 'ert-run-tests-batch-and-exit)
;;            (not (member 'no-run-tests-automatically command-line-args)))
;;   (ert-run-tests-batch-and-exit (regexp-quote (symbol-name (quote ecc-command)))))

;;; test-ecc-command.el ends here


(provide 'test-ecc-command)

(when
    (not load-file-name)
  (message "test-ecc-command.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))