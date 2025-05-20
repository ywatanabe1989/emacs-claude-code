;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 02:58:16>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/examples/basic-usage.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; Commentary:
;; Basic usage example for emacs-claude-code.
;; This example demonstrates how to:
;; 1. Initialize the package
;; 2. Create a new Claude buffer
;; 3. Send a basic prompt to Claude
;; 4. Handle the response

;;; Code:

;; Load the package - make sure emacs-claude-code.el is in your load-path
(require 'emacs-claude-code)

;; Make sure all required components are loaded
(require 'vterm)
(require 'ecc-vterm)

;; Define a simple function to create a new Claude vterm buffer

;; EMACS_SERVER_FILE=/tmp/emacs-new-server /opt/emacs-30.1/bin/emacs --daemon
;; EMACS_SERVER_FILE=/tmp/emacs-new-server /opt/emacs-30.1/bin/emacsclient -cn
;; emacsclient
;; /opt/emacs-30.1/bin/emacsclient -cn -f ~/.emacs.d/server/server

;; BUG: example-claude-vterm: Symbol’s function definition is void: ecc-vterm-create

;; (defun example-claude-vterm ()
;;   "Create a new Claude vterm buffer for interaction."
;;   (interactive)
;;   ;; Use the built-in vterm creation function
;;   (ecc-vterm-create))
;; Error reading undo-tree history from "/home/ywatanabe/.emacs.d/undo-tree-history/.!home!ywatanabe!.emacs.d!lisp!emacs-claude-code!examples!basic-usage.el.~undo-tree~"
;; emacs-claude-code
;; vterm
;; ecc-vterm
;; consp: Wrong type argument: listp, t
;; Error in buffer hook: (wrong-type-argument number-or-marker-p #s(ecc-state yes-yes-no "Yes/Yes/No" "\\(?:❯ 1\\. Yes.*\\)\\(?:❯ 2\\. Yes, but.*\\)\\(?:❯ 3\\. No.*\\)" "Claude is asking a yes/yes/no question" (running idle) ecc-state-engine--handle-yes-yes-no))
;; #<buffer *Claude vterm 1*>
;; Error running timer ‘ecc-vterm--detect-state’: (wrong-type-argument number-or-marker-p #s(ecc-state yes-yes-no "Yes/Yes/No" "\\(?:❯ 1\\. Yes.*\\)\\(?:❯ 2\\. Yes, but.*\\)\\(?:❯ 3\\. No.*\\)" "Claude is asking a yes/yes/no question" (running idle) ecc-state-engine--handle-yes-yes-no)) [2 times]
;; Error running timer: (wrong-type-argument number-or-marker-p #s(ecc-state yes-yes-no "Yes/Yes/No" "\\(?:❯ 1\\. Yes.*\\)\\(?:❯ 2\\. Yes, but.*\\)\\(?:❯ 3\\. No.*\\)" "Claude is asking a yes/yes/no question" (running idle) ecc-state-engine--handle-yes-yes-no))
;; Error running timer ‘ecc-vterm--detect-state’: (wrong-type-argument number-or-marker-p #s(ecc-state yes-yes-no "Yes/Yes/No" "\\(?:❯ 1\\. Yes.*\\)\\(?:❯ 2\\. Yes, but.*\\)\\(?:❯ 3\\. No.*\\)" "Claude is asking a yes/yes/no question" (running idle) ecc-state-engine--handle-yes-yes-no)) [17 times]
;; Error running timer: (wrong-type-argument number-or-marker-p #s(ecc-state yes-yes-no "Yes/Yes/No" "\\(?:❯ 1\\. Yes.*\\)\\(?:❯ 2\\. Yes, but.*\\)\\(?:❯ 3\\. No.*\\)" "Claude is asking a yes/yes/no question" (running idle) ecc-state-engine--handle-yes-yes-no))

;; Define a simple f222unction to send a prompt to Claude

(defun example-claude-send-prompt (prompt)
  "Send PROMPT to the active Claude buffer."
  (interactive "sEnter your prompt for Claude: ")
  (let ((buffer (or (ecc-buffer-get-current-buffer)
                    (call-interactively 'example-claude-vterm))))
    ;; Send the prompt using the vterm send function
    (ecc-vterm-send-command buffer prompt)
    buffer))

;; Example usage in non-interactive mode

(defun example-run-claude-demo ()
  "Run a demo of Claude with a sample prompt."
  (interactive)
  ;; Sample prompt for demonstration
  (let ((sample-prompt
         "Write a short Emacs Lisp function that counts words in the current buffer."))
    ;; Create a Claude buffer and send the prompt
    (example-claude-send-prompt sample-prompt)
    ;; Message to inform the user
    (message "Sent prompt to Claude: %s" sample-prompt)))

;; Additional examples for handling Claude responses

(defun example-claude-response-handler (response)
  "Example handler for Claude RESPONSE."
  (message "Received response from Claude: %s"
           (if (> (length response) 50)
               (concat (substring response 0 50) "...")
             response)))

;; Register the response handler if needed

(defun example-register-response-handler ()
  "Register the example response handler."
  (interactive)
  (add-hook 'ecc-response-received-hook
            'example-claude-response-handler))

;; Remove the response handler when done

(defun example-unregister-response-handler ()
  "Unregister the example response handler."
  (interactive)
  (remove-hook 'ecc-response-received-hook
               'example-claude-response-handler))

;;; basic-usage.el ends here


(provide 'basic-usage)

(when
    (not load-file-name)
  (message "basic-usage.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))