;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 07:14:27>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-state/ecc-state-engine.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; Code:

(require 'cl-lib)
(require 'ecc-compat)
(require 'ecc-state-variables)

;; State definitions and constants
;; ------------------------------

(cl-defstruct (ecc-state (:constructor ecc-state--create)
                         (:copier nil))
  "Structure representing a Claude interaction state."
  (id nil :read-only t :documentation
      "Unique identifier for this state")
  (name "" :read-only t :documentation "Human-readable name")
  (prompt-pattern nil :read-only t :documentation
                  "Regex to detect this state")
  (description "" :read-only t :documentation "Detailed description")
  (next-states nil :read-only t :documentation
               "List of possible next states")
  (action nil :read-only t :documentation
          "Function to call when entering state"))

(defvar ecc-state-engine--current-state nil
  "The current state of the Claude AI interaction.")

(defvar ecc-state-engine--states (make-hash-table :test 'eq)
  "Hash table of all defined states, keyed by state ID.")

(defvar ecc-state-engine--buffer nil
  "The buffer currently being monitored for state changes.")

(defvar ecc-state-engine--hooks nil
  "Alist of hooks to run when states change.
Each element is (STATE-ID . HOOK-FUNCTION-LIST).")

(defvar ecc-state-engine--last-action-result nil
  "Stores the result of the last action handler function.
This is used for debugging and tracking state transitions.")

;; Predefined states
;; ------------------------------

(defconst ecc-state-idle
  (ecc-state--create :id 'idle
                     :name "Idle"
                     :description "Claude is idle, waiting for input"
                     :next-states '(running waiting))
  "State representing Claude in an idle state.")

(defconst ecc-state-running
  (ecc-state--create :id 'running
                     :name "Running"
                     :prompt-pattern
                     "\\(?:Generating\\|Thinking\\|Loading\\)\\.\\.\\."
                     :description "Claude is generating a response"
                     :next-states
                     '(idle waiting yes-no yes-yes-no error))
  "State representing Claude actively generating a response.")

(defconst ecc-state-waiting
  (ecc-state--create :id 'waiting
                     :name "Waiting"
                     :prompt-pattern emacs-claude-prompt-waiting
                     :description
                     "Claude is waiting for the user to continue"
                     :next-states '(running idle)
                     :action 'ecc-state-engine--handle-waiting)
  "State representing Claude waiting for the user to continue.")

(defconst ecc-state-yes-no
  (ecc-state--create :id 'yes-no
                     :name "Yes/No"
                     :prompt-pattern emacs-claude-prompt-y/n
                     :description "Claude is asking a yes/no question"
                     :next-states '(running idle)
                     :action 'ecc-state-engine--handle-yes-no)
  "State representing Claude asking a yes/no question.")

(defconst ecc-state-yes-yes-no
  (ecc-state--create :id 'yes-yes-no
                     :name "Yes/Yes/No"
                     :prompt-pattern emacs-claude-prompt-y/y/n
                     :description
                     "Claude is asking a yes/yes/no question"
                     :next-states '(running idle)
                     :action 'ecc-state-engine--handle-yes-yes-no)
  "State representing Claude asking a yes/yes/no question.")

(defconst ecc-state-error
  (ecc-state--create :id 'error
                     :name "Error"
                     :prompt-pattern
                     "\\(?:Error\\|Exception\\|Failed\\)"
                     :description "Claude encountered an error"
                     :next-states '(idle)
                     :action 'ecc-state-engine--handle-error)
  "State representing Claude encountering an error.")

;; Core state engine functions
;; ------------------------------

(defun ecc-state-engine-init ()
  "Initialize the state engine.
Registers all predefined states and sets the initial state to idle."
  (setq ecc-state-engine--states (make-hash-table :test 'eq))
  (setq ecc-state-engine--current-state nil)
  (setq ecc-state-engine--hooks nil)

  ;; Register predefined states
  (dolist (state (list ecc-state-idle
                       ecc-state-running
                       ecc-state-waiting
                       ecc-state-yes-no
                       ecc-state-yes-yes-no
                       ecc-state-error))
    (ecc-state-engine-register-state state))

  ;; Set initial state
  (ecc-state-engine-set-state 'idle))

(defun ecc-state-engine-register-state (state)
  "Register STATE with the state engine."
  (puthash (ecc-state-id state) state ecc-state-engine--states))

(defun ecc-state-engine-get-state (state-id)
  "Get the state with STATE-ID."
  (gethash state-id ecc-state-engine--states))

(defun ecc-state-engine-set-state (state-id)
  "Set the current state to STATE-ID.
Runs any associated hooks and actions."
  (let ((state (ecc-state-engine-get-state state-id)))
    (when state
      ;; Run exit hooks for old state if applicable
      (when ecc-state-engine--current-state
        (ecc-state-engine--run-hooks
         (ecc-state-id ecc-state-engine--current-state) 'exit))

      ;; Update current state
      (setq ecc-state-engine--current-state state)

      ;; Run enter hooks and action for new state
      (ecc-state-engine--run-hooks state-id 'enter)
      (when-let ((action (ecc-state-action state)))
        ;; Reset last action result before calling handler
        (setq ecc-state-engine--last-action-result nil)
        ;; Call action and ignore return value
        (funcall action)
        ;; Always return nil from action
        nil)

      ;; Return the new state
      state)))

(defun ecc-state-engine-get-current-state ()
  "Get the current state."
  ecc-state-engine--current-state)

(defun ecc-state-engine-get-current-state-id ()
  "Get the ID of the current state."
  (when ecc-state-engine--current-state
    (ecc-state-id ecc-state-engine--current-state)))

(defun ecc-state-engine-detect-state (&optional buffer n-lines)
  "Detect the current state from BUFFER content.
If BUFFER is nil, uses the current buffer.
Examines the last N-LINES lines (defaults to 50)."
  (let ((buf (or buffer (current-buffer)))
        (n-lines (or n-lines 50)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (save-excursion
          ;; Start from the end of the buffer
          (goto-char (point-max))
          (let ((current-line (line-number-at-pos))
                (min-line (line-number-at-pos (point-min)))
                (states-to-check
                 (hash-table-values ecc-state-engine--states))
                (detected-state nil))

            ;; Sort states by priority (order registered)
            (setq states-to-check
                  (sort states-to-check
                        (lambda (a b)
                          (<
                           (gethash (ecc-state-id a)
                                    ecc-state-engine--states 0)
                           (gethash (ecc-state-id b)
                                    ecc-state-engine--states 0)))))

            ;; Check each state's pattern against the buffer content
            (dolist (state states-to-check)
              (let ((pattern (ecc-state-prompt-pattern state)))
                (when pattern
                  (let
                      ((search-end-line
                        (max min-line (- current-line n-lines)))
                       (search-end (save-excursion
                                     (goto-char (point-min))
                                     (forward-line
                                      (1- search-end-line))
                                     (point))))
                    (when (re-search-backward pattern search-end t)
                      (setq detected-state (ecc-state-id state))
                      (ecc-state-engine-set-state detected-state)
                      (cl-return))))))

            detected-state))))))

(defun ecc-state-engine-add-hook (state-id hook-type function)
  "Add FUNCTION to run when STATE-ID changes.
HOOK-TYPE should be 'enter or 'exit."
  (let ((hook-key (cons state-id hook-type)))
    (unless (assoc hook-key ecc-state-engine--hooks)
      (push (cons hook-key nil) ecc-state-engine--hooks))
    (let ((hooks (assoc hook-key ecc-state-engine--hooks)))
      (unless (member function (cdr hooks))
        (setcdr hooks (cons function (cdr hooks)))))))

(defun ecc-state-engine-remove-hook (state-id hook-type function)
  "Remove FUNCTION from STATE-ID's HOOK-TYPE hooks."
  (let* ((hook-key (cons state-id hook-type))
         (hooks (assoc hook-key ecc-state-engine--hooks)))
    (when hooks
      (setcdr hooks (delete function (cdr hooks))))))

(defun ecc-state-engine--run-hooks (state-id hook-type)
  "Run all hooks for STATE-ID and HOOK-TYPE."
  (let* ((hook-key (cons state-id hook-type))
         (hooks (cdr (assoc hook-key ecc-state-engine--hooks))))
    (dolist (hook hooks)
      (condition-case err
          (funcall hook)
        (error (message "Error in state hook: %S" err))))))

;; Default state handlers
;; ------------------------------

(defun ecc-state-engine--handle-waiting ()
  "Handle the waiting state."
  (message "Claude is waiting for input to continue")
  (setq ecc-state-engine--last-action-result 'waiting)
  nil)

(defun ecc-state-engine--handle-yes-no ()
  "Handle the yes/no state."
  (message "Claude is asking a yes/no question")
  (setq ecc-state-engine--last-action-result 'yes-no)
  nil)

(defun ecc-state-engine--handle-yes-yes-no ()
  "Handle the yes/yes/no state."
  (message "Claude is asking a yes/yes/no question")
  ;; Explicitly return nil to avoid any potential marker return value
  (setq ecc-state-engine--last-action-result 'yes-yes-no)
  nil)

(defun ecc-state-engine--handle-error ()
  "Handle the error state."
  (message "Claude encountered an error")
  (setq ecc-state-engine--last-action-result 'error)
  nil)

;; Set up the state engine when the package is loaded
(ecc-state-engine-init)

;;; ecc-state-engine.el ends here


(provide 'ecc-state-engine)

(when
    (not load-file-name)
  (message "ecc-state-engine.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))