;;; ecc-command.el --- Command system for Claude AI -*- lexical-binding: t -*-

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

(cl-defstruct (ecc-command (:constructor ecc-command--create)
                          (:copier nil))
  "Structure representing a Claude command."
  (id nil :read-only t :documentation "Unique identifier for this command")
  (name "" :read-only t :documentation "Human-readable name")
  (handler nil :read-only t :documentation "Function to handle this command")
  (description "" :read-only t :documentation "Detailed description")
  (usage "" :read-only t :documentation "Usage instructions")
  (aliases nil :read-only t :documentation "Alternative command names"))

(defvar ecc-command--registry (make-hash-table :test 'equal)
  "Registry of all Claude commands, keyed by command ID.")

(defvar ecc-command--history nil
  "History of Claude commands executed.")

(defvar ecc-command--history-size 100
  "Maximum size of the command history.")

;; Core command functions
;; ------------------------------

(defun ecc-command-init ()
  "Initialize the command system."
  (setq ecc-command--registry (make-hash-table :test 'equal))
  (setq ecc-command--history nil)
  
  ;; Register built-in commands
  (ecc-command-register
   (ecc-command--create
    :id "help"
    :name "Help"
    :handler 'ecc-command--handle-help
    :description "Display help information about Claude commands"
    :usage "/help [command]"
    :aliases '("?" "h")))
  
  (ecc-command-register
   (ecc-command--create
    :id "clear"
    :name "Clear"
    :handler 'ecc-command--handle-clear
    :description "Clear the current conversation"
    :usage "/clear"
    :aliases '("clear-history" "reset")))
  
  (ecc-command-register
   (ecc-command--create
    :id "new"
    :name "New"
    :handler 'ecc-command--handle-new
    :description "Start a new Claude conversation"
    :usage "/new [name]"
    :aliases '("create")))
  
  (ecc-command-register
   (ecc-command--create
    :id "switch"
    :name "Switch"
    :handler 'ecc-command--handle-switch
    :description "Switch to another Claude conversation"
    :usage "/switch [name]"
    :aliases '("use" "select")))
  
  (ecc-command-register
   (ecc-command--create
    :id "list"
    :name "List"
    :handler 'ecc-command--handle-list
    :description "List all Claude conversations"
    :usage "/list"
    :aliases '("ls" "buffers")))
  
  (ecc-command-register
   (ecc-command--create
    :id "rename"
    :name "Rename"
    :handler 'ecc-command--handle-rename
    :description "Rename the current Claude conversation"
    :usage "/rename <name>"
    :aliases '("name")))
  
  (ecc-command-register
   (ecc-command--create
    :id "kill"
    :name "Kill"
    :handler 'ecc-command--handle-kill
    :description "Kill a Claude conversation"
    :usage "/kill [name]"
    :aliases '("close" "remove")))
  
  (ecc-command-register
   (ecc-command--create
    :id "compact"
    :name "Compact"
    :handler 'ecc-command--handle-compact
    :description "Compact the conversation history to save tokens"
    :usage "/compact"
    :aliases '("compress")))
  
  (ecc-command-register
   (ecc-command--create
    :id "template"
    :name "Template"
    :handler 'ecc-command--handle-template
    :description "Use a template for a new prompt"
    :usage "/template <name>"
    :aliases '("t" "temp")))
  
  (ecc-command-register
   (ecc-command--create
    :id "retry"
    :name "Retry"
    :handler 'ecc-command--handle-retry
    :description "Retry the last prompt"
    :usage "/retry"
    :aliases '("again"))))

(defun ecc-command-register (command)
  "Register COMMAND with the command system."
  (puthash (ecc-command-id command) command ecc-command--registry)
  
  ;; Also register aliases
  (dolist (alias (ecc-command-aliases command))
    (puthash alias command ecc-command--registry))
  
  command)

(defun ecc-command-get (command-id)
  "Get the command with COMMAND-ID."
  (gethash command-id ecc-command--registry))

(defun ecc-command-exists-p (command-id)
  "Check if a command with COMMAND-ID exists."
  (not (null (ecc-command-get command-id))))

(defun ecc-command-list-all ()
  "Get all registered commands as a list.
Excludes aliases."
  (let ((commands (make-hash-table :test 'equal))
        (result nil))
    ;; Remove duplicates from aliases
    (maphash (lambda (_id command)
               (puthash (ecc-command-id command) command commands))
             ecc-command--registry)
    
    ;; Convert to list
    (maphash (lambda (_id command)
               (push command result))
             commands)
    
    ;; Sort by name
    (seq-sort-by #'ecc-command-name #'string< result)))

(defun ecc-command-execute (input)
  "Execute the command in INPUT.
If INPUT starts with '/', treat it as a command.
Otherwise, send it as a message to Claude."
  (if (string-match "^/\\([^ ]+\\)\\(?: +\\(.*\\)\\)?$" input)
      ;; Command
      (let* ((command-name (match-string 1 input))
             (args (match-string 2 input))
             (command (ecc-command-get command-name)))
        (if command
            (progn
              ;; Add to history
              (ecc-command--add-to-history input)
              
              ;; Execute command
              (funcall (ecc-command-handler command) args))
          
          ;; Unknown command
          (message "Unknown command: /%s" command-name)))
    
    ;; Normal message - send to Claude
    (ecc-command--add-to-history input)
    (ecc-command--send-to-claude input)))

(defun ecc-command--add-to-history (input)
  "Add INPUT to the command history."
  (push input ecc-command--history)
  
  ;; Trim history if needed
  (when (> (length ecc-command--history) ecc-command--history-size)
    (setq ecc-command--history (seq-take ecc-command--history ecc-command--history-size))))

(defun ecc-command-get-history ()
  "Get the command history."
  ecc-command--history)

;; Command handlers
;; ------------------------------

(defun ecc-command--handle-help (args)
  "Handle the help command with ARGS."
  (if args
      ;; Help for specific command
      (let ((command (ecc-command-get args)))
        (if command
            (message "%s: %s\nUsage: %s\nAliases: %s"
                     (ecc-command-name command)
                     (ecc-command-description command)
                     (ecc-command-usage command)
                     (mapconcat #'identity (ecc-command-aliases command) ", "))
          (message "Unknown command: /%s" args)))
    
    ;; General help
    (let ((commands (ecc-command-list-all))
          (help-text "Available Claude commands:\n\n"))
      (dolist (command commands)
        (setq help-text
              (concat help-text
                      (format "/%s - %s\n"
                              (ecc-command-id command)
                              (ecc-command-description command)))))
      (with-output-to-temp-buffer "*Claude Help*"
        (princ help-text)))))

(defun ecc-command--handle-clear (_args)
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

(defun ecc-command--handle-new (args)
  "Handle the new command with ARGS."
  (let ((name (if (and args (not (string-empty-p args)))
                 args
               (format "Claude-%d" (1+ (hash-table-count ecc-buffer-manager--registry))))))
    (let ((claude-buffer (ecc-buffer-manager-create name)))
      (ecc-buffer-manager-set-current claude-buffer)
      (message "Created new Claude conversation: %s" name))))

(defun ecc-command--handle-switch (args)
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

(defun ecc-command--handle-list (_args)
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

(defun ecc-command--handle-rename (args)
  "Handle the rename command with ARGS."
  (if (and args (not (string-empty-p args)))
      (let ((current (ecc-buffer-manager-get-current)))
        (if current
            (progn
              (ecc-buffer-manager-rename current args)
              (message "Renamed to: %s" args))
          (message "No active Claude conversation")))
    (message "Usage: /rename <name>")))

(defun ecc-command--handle-kill (args)
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

(defun ecc-command--handle-compact (_args)
  "Handle the compact command."
  (message "Compacting conversation history..."))

(defun ecc-command--handle-template (args)
  "Handle the template command with ARGS."
  (if (and args (not (string-empty-p args)))
      (message "Using template: %s" args)
    (message "Usage: /template <name>")))

(defun ecc-command--handle-retry (_args)
  "Handle the retry command."
  (if (car ecc-command--history)
      (let ((last-input (car ecc-command--history)))
        ;; Don't re-add to history
        (ecc-command--send-to-claude last-input)
        (message "Retrying: %s" last-input))
    (message "No previous input to retry")))

(defun ecc-command--send-to-claude (text)
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
(ecc-command-init)

(provide 'ecc-command)
;;; ecc-command.el ends here