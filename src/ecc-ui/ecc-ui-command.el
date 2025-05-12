;;; ecc-ui-command.el --- Command system for Claude AI -*- lexical-binding: t -*-

;; Author: ywatanabe <ywatanabe@alumni.u-tokyo.ac.jp>
;; Maintainer: ywatanabe <ywatanabe@alumni.u-tokyo.ac.jp>
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: ai, convenience, tools
;; URL: https://github.com/ywatanabe1989/emacs-claude-code

;;; Commentary:
;; 
;; This module provides a command system for interacting with Claude AI.
;; It centralizes all input/output operations and handles command dispatching.
;;
;; The command system supports command history, built-in commands, and
;; extensibility through custom command registration.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'ecc-compat)
(require 'ecc-buffer-manager)

;; Command structures and registry
;; ------------------------------

(cl-defstruct (ecc-ui-command (:constructor ecc-ui-command--create)
                        (:copier nil))
  "Structure representing a Claude command."
  (id nil :read-only t :documentation "Unique identifier for this command")
  (name "" :read-only t :documentation "Human-readable name")
  (handler nil :read-only t :documentation "Function to handle this command")
  (description "" :read-only t :documentation "Detailed description")
  (usage "" :read-only t :documentation "Usage instructions")
  (aliases nil :read-only t :documentation "Alternative command names"))

(defvar ecc-ui-command--registry (make-hash-table :test 'equal)
  "Registry of all Claude commands, keyed by command ID.")

(defvar ecc-ui-command--history nil
  "History of Claude commands executed.")

(defvar ecc-ui-command--history-size 100
  "Maximum size of the command history.")

;; Core command functions
;; ------------------------------

(defun ecc-ui-command-init ()
  "Initialize the command system."
  (setq ecc-ui-command--registry (make-hash-table :test 'equal))
  (setq ecc-ui-command--history nil)
  
  ;; Register built-in commands
  (ecc-ui-command-register
   (ecc-ui-command--create
    :id "help"
    :name "Help"
    :handler 'ecc-ui-command--handle-help
    :description "Display help information about Claude commands"
    :usage "/help [command]"
    :aliases '("?" "h")))
  
  (ecc-ui-command-register
   (ecc-ui-command--create
    :id "clear"
    :name "Clear"
    :handler 'ecc-ui-command--handle-clear
    :description "Clear the current conversation"
    :usage "/clear"
    :aliases '("clear-history" "reset")))
  
  (ecc-ui-command-register
   (ecc-ui-command--create
    :id "new"
    :name "New"
    :handler 'ecc-ui-command--handle-new
    :description "Start a new Claude conversation"
    :usage "/new [name]"
    :aliases '("create")))
  
  (ecc-ui-command-register
   (ecc-ui-command--create
    :id "switch"
    :name "Switch"
    :handler 'ecc-ui-command--handle-switch
    :description "Switch to another Claude conversation"
    :usage "/switch [name]"
    :aliases '("use" "select")))
  
  (ecc-ui-command-register
   (ecc-ui-command--create
    :id "list"
    :name "List"
    :handler 'ecc-ui-command--handle-list
    :description "List all Claude conversations"
    :usage "/list"
    :aliases '("ls" "buffers")))
  
  (ecc-ui-command-register
   (ecc-ui-command--create
    :id "rename"
    :name "Rename"
    :handler 'ecc-ui-command--handle-rename
    :description "Rename the current Claude conversation"
    :usage "/rename <n>"
    :aliases '("name")))
  
  (ecc-ui-command-register
   (ecc-ui-command--create
    :id "kill"
    :name "Kill"
    :handler 'ecc-ui-command--handle-kill
    :description "Kill a Claude conversation"
    :usage "/kill [name]"
    :aliases '("close" "remove")))
  
  (ecc-ui-command-register
   (ecc-ui-command--create
    :id "compact"
    :name "Compact"
    :handler 'ecc-ui-command--handle-compact
    :description "Compact the conversation history to save tokens"
    :usage "/compact"
    :aliases '("compress")))
  
  (ecc-ui-command-register
   (ecc-ui-command--create
    :id "template"
    :name "Template"
    :handler 'ecc-ui-command--handle-template
    :description "Use a template for a new prompt"
    :usage "/template <n>"
    :aliases '("t" "temp")))
  
  (ecc-ui-command-register
   (ecc-ui-command--create
    :id "retry"
    :name "Retry"
    :handler 'ecc-ui-command--handle-retry
    :description "Retry the last prompt"
    :usage "/retry"
    :aliases '("again"))))

(defun ecc-ui-command-register (command)
  "Register COMMAND with the command system."
  (puthash (ecc-ui-command-id command) command ecc-ui-command--registry)
  
  ;; Also register aliases
  (dolist (alias (ecc-ui-command-aliases command))
    (puthash alias command ecc-ui-command--registry))
  
  command)

(defun ecc-ui-command-get (command-id)
  "Get the command with COMMAND-ID."
  (gethash command-id ecc-ui-command--registry))

(defun ecc-ui-command-exists-p (command-id)
  "Check if a command with COMMAND-ID exists."
  (not (null (ecc-ui-command-get command-id))))

(defun ecc-ui-command-list-all ()
  "Get all registered commands as a list.
Excludes aliases."
  (let ((commands (make-hash-table :test 'equal))
        (result nil))
    ;; Remove duplicates from aliases
    (maphash (lambda (_id command)
               (puthash (ecc-ui-command-id command) command commands))
             ecc-ui-command--registry)
    
    ;; Convert to list
    (maphash (lambda (_id command)
               (push command result))
             commands)
    
    ;; Sort by name
    (seq-sort-by #'ecc-ui-command-name #'string< result)))

(defun ecc-ui-command-execute (input)
  "Execute the command in INPUT.
If INPUT starts with '/', treat it as a command.
Otherwise, send it as a message to Claude."
  (if (string-match "^/\\([^ ]+\\)\\(?: +\\(.*\\)\\)?$" input)
      ;; Command
      (let* ((command-name (match-string 1 input))
             (args (match-string 2 input))
             (command (ecc-ui-command-get command-name)))
        (if command
            (progn
              ;; Add to history
              (ecc-ui-command--add-to-history input)
              
              ;; Execute command
              (funcall (ecc-ui-command-handler command) args))
          
          ;; Unknown command
          (message "Unknown command: /%s" command-name)))
    
    ;; Normal message - send to Claude
    (ecc-ui-command--add-to-history input)
    (ecc-ui-command--send-to-claude input)))

(defun ecc-ui-command--add-to-history (input)
  "Add INPUT to the command history."
  (push input ecc-ui-command--history)
  
  ;; Trim history if needed
  (when (> (length ecc-ui-command--history) ecc-ui-command--history-size)
    (setq ecc-ui-command--history (seq-take ecc-ui-command--history ecc-ui-command--history-size))))

(defun ecc-ui-command-get-history ()
  "Get the command history."
  ecc-ui-command--history)

;; Command handlers
;; ------------------------------

(defun ecc-ui-command--handle-help (args)
  "Handle the help command with ARGS."
  (if args
      ;; Help for specific command
      (let ((command (ecc-ui-command-get args)))
        (if command
            (message "%s: %s\nUsage: %s\nAliases: %s"
                     (ecc-ui-command-name command)
                     (ecc-ui-command-description command)
                     (ecc-ui-command-usage command)
                     (mapconcat #'identity (ecc-ui-command-aliases command) ", "))
          (message "Unknown command: /%s" args)))
    
    ;; General help
    (let ((commands (ecc-ui-command-list-all))
          (help-text "Available Claude commands:\n\n"))
      (dolist (command commands)
        (setq help-text
              (concat help-text
                      (format "/%s - %s\n"
                              (ecc-ui-command-id command)
                              (ecc-ui-command-description command)))))
      (with-output-to-temp-buffer "*Claude Help*"
        (princ help-text)))))

(defun ecc-ui-command--handle-clear (_args)
  "Handle the clear command."
  (let* ((current (ecc-buffer-manager-get-current))
         (buffer (and current (ecc-buffer-buffer current))))
    (if (buffer-live-p buffer)
        (progn
          (with-current-buffer buffer
            (erase-buffer)
            (insert "Conversation cleared.\n\n"))
          (message "Claude conversation cleared"))
      (message "No active Claude conversation"))))

(defun ecc-ui-command--handle-new (args)
  "Handle the new command with ARGS."
  (let ((name (if (and args (not (string-empty-p args)))
                 args
               (format "Claude-%d" (1+ (hash-table-count ecc-buffer-manager--registry))))))
    (let ((claude-buffer (ecc-buffer-manager-create name)))
      (ecc-buffer-manager-set-current claude-buffer)
      (message "Created new Claude conversation: %s" name))))

(defun ecc-ui-command--handle-switch (args)
  "Handle the switch command with ARGS."
  (if (and args (not (string-empty-p args)))
      ;; Switch to named buffer
      (let ((found nil))
        (dolist (cb (ecc-buffer-manager-get-all))
          (when (string= (ecc-buffer-name cb) args)
            (ecc-buffer-manager-set-current cb)
            (setq found t)
            (message "Switched to Claude conversation: %s" args)))
        (unless found
          (message "No Claude conversation named: %s" args)))
    
    ;; Interactive switch
    (let ((buffers (ecc-buffer-manager-get-all)))
      (if buffers
          (let* ((names (mapcar (lambda (cb)
                                  (ecc-buffer-name cb))
                                buffers))
                 (selected (completing-read "Switch to Claude conversation: " names nil t)))
            (dolist (cb buffers)
              (when (string= (ecc-buffer-name cb) selected)
                (ecc-buffer-manager-set-current cb)
                (message "Switched to Claude conversation: %s" selected))))
        (message "No Claude conversations available")))))

(defun ecc-ui-command--handle-list (_args)
  "Handle the list command."
  (let ((buffers (ecc-buffer-manager-get-all))
        (current (ecc-buffer-manager-get-current)))
    (if buffers
        (with-output-to-temp-buffer "*Claude Buffers*"
          (princ "Claude conversations:\n\n")
          (dolist (cb buffers)
            (princ (format "%s %s (%s)\n"
                           (if (eq cb current) "*" " ")
                           (ecc-buffer-name cb)
                           (ecc-buffer-state cb)))))
      (message "No Claude conversations available"))))

(defun ecc-ui-command--handle-rename (args)
  "Handle the rename command with ARGS."
  (if (and args (not (string-empty-p args)))
      (let ((current (ecc-buffer-manager-get-current)))
        (if current
            (progn
              (ecc-buffer-manager-rename current args)
              (message "Renamed to: %s" args))
          (message "No active Claude conversation")))
    (message "Usage: /rename <n>")))

(defun ecc-ui-command--handle-kill (args)
  "Handle the kill command with ARGS."
  (if (and args (not (string-empty-p args)))
      ;; Kill named buffer
      (let ((found nil))
        (dolist (cb (ecc-buffer-manager-get-all))
          (when (string= (ecc-buffer-name cb) args)
            (ecc-buffer-manager-kill cb)
            (setq found t)
            (message "Killed Claude conversation: %s" args)))
        (unless found
          (message "No Claude conversation named: %s" args)))
    
    ;; Kill current buffer
    (let ((current (ecc-buffer-manager-get-current)))
      (if current
          (progn
            (ecc-buffer-manager-kill current)
            (message "Killed current Claude conversation"))
        (message "No active Claude conversation")))))

(defun ecc-ui-command--handle-compact (_args)
  "Handle the compact command."
  (message "Compacting conversation history..."))

(defun ecc-ui-command--handle-template (args)
  "Handle the template command with ARGS."
  (if (and args (not (string-empty-p args)))
      (message "Using template: %s" args)
    (message "Usage: /template <n>")))

(defun ecc-ui-command--handle-retry (_args)
  "Handle the retry command."
  (if (car ecc-ui-command--history)
      (let ((last-input (car ecc-ui-command--history)))
        ;; Don't re-add to history
        (ecc-ui-command--send-to-claude last-input)
        (message "Retrying: %s" last-input))
    (message "No previous input to retry")))

(defun ecc-ui-command--send-to-claude (text)
  "Send TEXT to the current Claude conversation."
  (let* ((current (ecc-buffer-manager-get-current))
         (buffer (and current (ecc-buffer-buffer current))))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert "\n\nYou: " text "\n\nClaude: ")
          ;; In a real implementation, this would send to the Claude API
          (insert "[Response would appear here]"))
      (message "No active Claude conversation"))))

;; Initialize the command system
(ecc-ui-command-init)

;; Backward compatibility for ecc-command functions
(defalias 'ecc-command--create 'ecc-ui-command--create)
(defalias 'ecc-command-id 'ecc-ui-command-id)
(defalias 'ecc-command-name 'ecc-ui-command-name)
(defalias 'ecc-command-handler 'ecc-ui-command-handler)
(defalias 'ecc-command-description 'ecc-ui-command-description)
(defalias 'ecc-command-usage 'ecc-ui-command-usage)
(defalias 'ecc-command-aliases 'ecc-ui-command-aliases)
(defalias 'ecc-command-init 'ecc-ui-command-init)
(defalias 'ecc-command-register 'ecc-ui-command-register)
(defalias 'ecc-command-get 'ecc-ui-command-get)
(defalias 'ecc-command-exists-p 'ecc-ui-command-exists-p)
(defalias 'ecc-command-list-all 'ecc-ui-command-list-all)
(defalias 'ecc-command-execute 'ecc-ui-command-execute)
(defalias 'ecc-command-get-history 'ecc-ui-command-get-history)

;; Provide both new and old feature names for backward compatibility
(provide 'ecc-ui-command)
(provide 'ecc-command)

;;; ecc-ui-command.el ends here