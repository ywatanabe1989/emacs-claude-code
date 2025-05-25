;;; -*- coding: utf-8; lexical-binding: t -*-
;;; ecc-term-claude-mode.el --- Optimized vterm mode for Claude interaction
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 18:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-term-claude-mode.el

;;; Commentary:
;;; Optimized vterm mode for Claude interaction.
;;;
;;; This module is the consolidated implementation for Claude terminal interaction,
;;; providing optimized performance and comprehensive functionality.
;;;
;;; It provides a specialized major mode derived from vterm-mode that is
;;; tailored for interacting with the Claude AI assistant, offering:
;;; - Performance optimizations for high-volume streaming output
;;; - Auto-response functionality for Claude prompts (Y/N, continue, etc.)
;;; - Visual indicators for Claude state in the mode line
;;; - Follow-bottom scrolling for real-time interaction
;;; - Integration with state detection and auto-response systems
;;; - Yank-as-file functionality for saving Claude output
;;; - Customizable appearance and behavior through user options
;;; - Comprehensive debugging support
;;;
;;; Both a dedicated major mode and lightweight enhancement functions for
;;; existing vterm buffers are provided.

;;; Code:

(require 'vterm)

;; Prefer consolidated modules when available
(if (featurep 'ecc-variables-consolidated)
    (require 'ecc-variables)
  (require 'ecc-variables))

(if (featurep 'ecc-state-detection)
    (require 'ecc-state-detection)
  (require 'ecc-state-detection))

(if (featurep 'ecc-auto-response-consolidated)
    (require 'ecc-auto-response)
  (require 'ecc-auto-response))

(if (featurep 'ecc-debug-utils-consolidated)
    (require 'ecc-debug-utils)
  (require 'ecc-debug-utils nil t))

(require 'ecc-vterm-yank-as-file)

;;;; Customization

(defgroup ecc-term-claude nil
  "Optimized vterm mode for Claude interaction."
  :group 'ecc
  :prefix "ecc-term-claude-")

(defcustom ecc-term-claude-line-numbers nil
  "Whether to display line numbers in Claude vterm buffers."
  :type 'boolean
  :group 'ecc-term-claude)

(defcustom ecc-term-claude-scroll-conservatively 10000
  "Value for scroll-conservatively in Claude vterm buffers.
Higher values make scrolling smoother but may use more CPU."
  :type 'integer
  :group 'ecc-term-claude)

(defcustom ecc-term-claude-truncate-lines t
  "Whether to truncate lines in Claude vterm buffers.
When enabled, long lines will not wrap to the next line."
  :type 'boolean
  :group 'ecc-term-claude)

(defcustom ecc-term-claude-state-update-interval 1.0
  "Interval in seconds for updating Claude state.
Lower values make the UI more responsive but use more CPU."
  :type 'number
  :group 'ecc-term-claude)

(defcustom ecc-term-claude-auto-mode nil
  "When non-nil, automatically respond to Claude prompts.
This enables the auto-response system to handle yes/no prompts
and continuation prompts without manual intervention."
  :type 'boolean
  :group 'ecc-term-claude)

(defcustom ecc-term-claude-buffer-name "*CLAUDE-VTERM*"
  "Default buffer name for Claude vterm buffers."
  :type 'string
  :group 'ecc-term-claude)

(defcustom ecc-term-claude-auto-mode-modeline-color "#4a5d23"
  "Background color for modeline when auto-mode is enabled.
This provides visual feedback that auto-mode is active."
  :type 'color
  :group 'ecc-term-claude)

(defcustom ecc-term-claude-auto-mode-modeline-box-color "#7fa339"
  "Border color for modeline when auto-mode is enabled.
This creates a highlighted border around the modeline."
  :type 'color
  :group 'ecc-term-claude)

(defcustom ecc-term-claude-show-state-in-mode-line t
  "Whether to show Claude state in the mode line.
When enabled, the current state (Y/N, Waiting, etc.) is shown
in the mode line to provide immediate feedback."
  :type 'boolean
  :group 'ecc-term-claude)

(defcustom ecc-term-claude-update-frame-title t
  "Whether to update the frame title with Claude state.
When enabled, the frame title will be updated to show
the current Claude state, making it easier to track
status even when the buffer is not visible."
  :type 'boolean
  :group 'ecc-term-claude)

(defcustom ecc-term-claude-debug nil
  "When non-nil, enable debug messages for Claude vterm mode."
  :type 'boolean
  :group 'ecc-term-claude)

(defcustom ecc-term-claude-debug-to-buffer nil
  "When non-nil, send debug messages to a buffer instead of *Messages*."
  :type 'boolean
  :group 'ecc-term-claude)

(defcustom ecc-term-claude-disable-bold nil
  "When non-nil, disable bold font in Claude vterm buffers."
  :type 'boolean
  :group 'ecc-term-claude)

(defcustom ecc-term-claude-disable-underline nil
  "When non-nil, disable underline in Claude vterm buffers."
  :type 'boolean
  :group 'ecc-term-claude)

;;;; Internal Variables

(defvar ecc-term-claude--update-functions nil
  "Hook functions to run after vterm output is updated.
Each function should take no arguments and will be run
whenever vterm produces new output.")

(defvar-local ecc-term-claude--state-timer nil
  "Timer for updating the Claude state.
This is used to periodically check for Claude prompts.")

(defvar-local ecc-term-claude--last-state nil
  "Last detected Claude state for the buffer.
Used to track state changes for updates.")

(defvar-local ecc-term-claude--auto-mode-active nil
  "Buffer-local indicator that auto-mode is active.
Used to track face remapping state.")

(defvar ecc-term-claude--registered-buffers (make-hash-table :test 'eq)
  "Hash table of registered Claude vterm buffers.")

(defvar ecc-term-claude--debug-buffer "*Claude-VTerm-Debug*"
  "Buffer name for Claude vterm debug messages.")

;;;; Mode Menu

(defvar ecc-term-claude--menu
  (let ((menu (make-sparse-keymap "Claude-VTerm")))
    ;; Basic commands
    (define-key menu [ecc-term-claude-clear]
      '(menu-item "Clear buffer" ecc-term-claude-clear
                  :help "Clear the vterm buffer"))
    (define-key menu [ecc-term-claude-separator-1]
      '(menu-item "--"))
    
    ;; Yank-as-file commands
    (define-key menu [ecc-vterm-yank-buffer-as-file]
      '(menu-item "Yank Buffer to File" ecc-vterm-yank-buffer-as-file
                  :help "Save entire buffer content to a file"))
    (define-key menu [ecc-vterm-yank-as-file]
      '(menu-item "Yank Region to File" ecc-vterm-yank-as-file
                  :help "Save selected region to a file"))
    (define-key menu [ecc-vterm-quick-yank-region]
      '(menu-item "Quick Yank Region" ecc-vterm-quick-yank-region
                  :help "Quickly save selected region with auto-generated filename"))
    (define-key menu [ecc-term-claude-separator-2]
      '(menu-item "--"))
    
    ;; Auto mode & responses
    (define-key menu [ecc-term-claude-auto-mode-toggle]
      '(menu-item "Toggle Auto-mode" ecc-term-claude-auto-mode-toggle
                  :help "Toggle automatic response to Claude prompts"
                  :button (:toggle . ecc-term-claude-auto-mode)))
    (define-key menu [ecc-term-claude-separator-3]
      '(menu-item "--"))
    (define-key menu [ecc-term-claude-yes]
      '(menu-item "Yes (y)" ecc-term-claude-yes
                  :help "Send 'y' to respond affirmatively"))
    (define-key menu [ecc-term-claude-no]
      '(menu-item "No (n)" ecc-term-claude-no
                  :help "Send 'n' to respond negatively"))
    
    ;; Debug menu
    (define-key menu [ecc-term-claude-separator-4]
      '(menu-item "--"))
    (define-key menu [ecc-term-claude-debug-toggle]
      '(menu-item "Toggle Debug Mode" ecc-term-claude-debug-toggle
                  :help "Toggle debug output for Claude vterm mode"
                  :button (:toggle . ecc-term-claude-debug)))
    (define-key menu [ecc-term-claude-debug-show-buffer]
      '(menu-item "Show Debug Buffer" ecc-term-claude-debug-show-buffer
                  :help "Show the Claude vterm debug buffer"))
    menu)
  "Menu for Claude-VTerm mode.")

;;;; Keybindings

(defvar ecc-term-claude-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map vterm-mode-map)
    
    ;; Claude-specific keybindings
    (define-key map (kbd "C-c C-y") 'ecc-term-claude-yes)
    (define-key map (kbd "C-c C-n") 'ecc-term-claude-no)
    (define-key map (kbd "C-c C-l") 'ecc-term-claude-clear)
    (define-key map (kbd "C-c C-a") 'ecc-term-claude-auto-mode-toggle)
    (define-key map (kbd "C-c C-v") 'ecc-term-claude-toggle-follow-bottom)
    (define-key map (kbd "C-c C-d") 'ecc-term-claude-debug-toggle)
    
    ;; Yank-as-file keybindings
    (define-key map (kbd "C-c C-f") 'ecc-vterm-yank-as-file)
    (define-key map (kbd "C-c C-b") 'ecc-vterm-yank-buffer-as-file)
    (define-key map (kbd "C-c C-q") 'ecc-vterm-quick-yank-region)
    
    ;; Add menu
    (define-key map [menu-bar claude-vterm] (cons "Claude" ecc-term-claude--menu))
    
    map)
  "Keymap for `ecc-term-claude-mode'.")

;;;; Debugging Functions

(defun ecc-term-claude--debug (format-string &rest args)
  "Output a debug message with FORMAT-STRING and ARGS if debug is enabled.
Messages go to debug buffer if `ecc-term-claude-debug-to-buffer' is set,
otherwise they go to *Messages*."
  (when ecc-term-claude-debug
    (if (fboundp 'ecc-debug-message)
        (apply #'ecc-debug-message (concat "[Claude-VTerm] " format-string) args)
      (let ((msg (apply #'format (concat "[Claude-VTerm] " format-string) args)))
        (if ecc-term-claude-debug-to-buffer
            (with-current-buffer (get-buffer-create ecc-term-claude--debug-buffer)
              (goto-char (point-max))
              (let ((inhibit-read-only t)
                    (timestamp (format-time-string "[%H:%M:%S] ")))
                (insert timestamp msg "\n")
                (when (> (buffer-size) 100000)
                  (goto-char (point-min))
                  (forward-line 50000)
                  (delete-region (point-min) (point)))))
          (message "%s" msg))))))

(defun ecc-term-claude-debug-toggle ()
  "Toggle debugging for Claude vterm mode."
  (interactive)
  (setq ecc-term-claude-debug (not ecc-term-claude-debug))
  (message "Claude vterm debug mode %s"
           (if ecc-term-claude-debug "enabled" "disabled")))

(defun ecc-term-claude-debug-show-buffer ()
  "Show the Claude vterm debug buffer."
  (interactive)
  (let ((buf (get-buffer-create ecc-term-claude--debug-buffer)))
    (with-current-buffer buf
      (unless (eq major-mode 'special-mode)
        (special-mode)
        (buffer-disable-undo)))
    (display-buffer buf)))

;;;; Mode Definition

(define-derived-mode ecc-term-claude-mode vterm-mode "Claude-VTerm"
  "Major mode for optimized Claude interaction in vterm.
This mode is optimized for high-performance streaming output.
You can also use `ecc-term-claude-enable` in an existing vterm buffer
to apply Claude features without changing the major mode.

Key bindings:
\\{ecc-term-claude-mode-map}"
  ;; Apply the mode setup
  (ecc-term-claude--debug "Setting up ecc-term-claude-mode")
  (ecc-term-claude--setup-buffer))

;;;; Buffer Setup Functions

(defun ecc-term-claude--setup-buffer ()
  "Set up the current buffer for Claude vterm mode."
  (ecc-term-claude--debug "Setting up buffer: %s" (buffer-name))
  
  ;; Disable line numbers for performance
  (ecc-term-claude--setup-display-options)
  
  ;; Apply performance optimizations
  (ecc-term-claude--setup-performance-options)
  
  ;; Register buffer
  (ecc-term-claude--register-buffer (current-buffer))
  
  ;; Set up visual indicators
  (ecc-term-claude--setup-mode-line)
  
  ;; Set up state detection timer
  (ecc-term-claude--setup-state-timer)
  
  ;; Connect to vterm hooks
  (ecc-term-claude--setup-hooks)
  
  ;; Apply appearance customizations
  (ecc-term-claude--setup-appearance)
  
  ;; Add hook to clean up when buffer is killed
  (add-hook 'kill-buffer-hook 'ecc-term-claude--cleanup-buffer nil t))

(defun ecc-term-claude--setup-display-options ()
  "Set up display options for Claude vterm buffer."
  (ecc-term-claude--debug "Setting up display options")
  (when (and (boundp 'display-line-numbers-mode)
             (not ecc-term-claude-line-numbers))
    (display-line-numbers-mode -1)))

(defun ecc-term-claude--setup-performance-options ()
  "Set up performance-related options for Claude vterm buffer."
  (ecc-term-claude--debug "Setting up performance options")
  (setq-local scroll-conservatively ecc-term-claude-scroll-conservatively
              scroll-margin 0
              scroll-step 1
              fast-but-imprecise-scrolling t
              truncate-lines ecc-term-claude-truncate-lines))

(defun ecc-term-claude--setup-appearance ()
  "Set up visual appearance for Claude vterm buffer."
  (ecc-term-claude--debug "Setting up appearance")
  (when ecc-term-claude-disable-bold
    (setq-local vterm-disable-bold-font t))
  (when ecc-term-claude-disable-underline
    (setq-local vterm-disable-underline t)))

(defun ecc-term-claude--setup-mode-line ()
  "Set up mode line indicator for Claude state."
  (ecc-term-claude--debug "Setting up mode line (show-state: %s, auto-mode: %s)"
                         ecc-term-claude-show-state-in-mode-line
                         ecc-term-claude-auto-mode)
  (when ecc-term-claude-show-state-in-mode-line
    (ecc-term-claude--debug "Setting mode-line-process for state indicator")
    (setq mode-line-process
          '(:eval (ecc-term-claude--mode-line-state-indicator)))
    (ecc-term-claude--debug "Mode-line-process set: %s" mode-line-process))
  ;; Force initial update
  (ecc-term-claude--debug "Forcing initial mode-line update")
  (force-mode-line-update))

(defun ecc-term-claude--setup-state-timer ()
  "Set up the timer for checking Claude state."
  (ecc-term-claude--debug "Setting up state timer")
  (when ecc-term-claude--state-timer
    (cancel-timer ecc-term-claude--state-timer)
    (ecc-term-claude--debug "Cancelled existing state timer"))
  
  ;; Store current buffer for timer
  (let ((buffer (current-buffer)))
    (setq ecc-term-claude--state-timer
          (run-with-timer 0 ecc-term-claude-state-update-interval
                         (lambda ()
                           (when (buffer-live-p buffer)
                             (with-current-buffer buffer
                               (ecc-term-claude-check-state)))))))
  (ecc-term-claude--debug "State timer initialized with interval %s seconds" 
                          ecc-term-claude-state-update-interval))

(defun ecc-term-claude--setup-hooks ()
  "Set up hooks for vterm output and other events."
  (ecc-term-claude--debug "Setting up hooks")
  ;; Connect to vterm update functions
  (add-hook 'vterm-update-functions
            (lambda (&rest _)
              (ecc-term-claude--debug "vterm update triggered")
              (run-hooks 'ecc-term-claude--update-functions))
            nil t)
  
  ;; Enable follow bottom by default
  (add-hook 'ecc-term-claude--update-functions
            'ecc-term-claude-follow-bottom-after-output))

(defun ecc-term-claude--setup-keys ()
  "Set up Claude-specific keybindings for a vterm buffer."
  (ecc-term-claude--debug "Setting up keybindings")
  (when (eq major-mode 'vterm-mode)
    ;; Basic functions
    (local-set-key (kbd "C-c C-y") 'ecc-term-claude-yes)
    (local-set-key (kbd "C-c C-n") 'ecc-term-claude-no)
    (local-set-key (kbd "C-c C-l") 'ecc-term-claude-clear)
    (local-set-key (kbd "C-c C-a") 'ecc-term-claude-auto-mode-toggle)
    (local-set-key (kbd "C-c C-v") 'ecc-term-claude-toggle-follow-bottom)
    (local-set-key (kbd "C-c C-d") 'ecc-term-claude-debug-toggle)
    
    ;; Yank-as-file functions
    (local-set-key (kbd "C-c C-f") 'ecc-vterm-yank-as-file)
    (local-set-key (kbd "C-c C-b") 'ecc-vterm-yank-buffer-as-file)
    (local-set-key (kbd "C-c C-q") 'ecc-vterm-quick-yank-region)))

;;;; State Detection

(defun ecc-term-claude--mode-line-state-indicator ()
  "Return mode line indicator for current Claude state."
  (let ((state (ecc-detect-state))
        (auto-indicator (if ecc-term-claude-auto-mode
                           (propertize " ◉ AUTO-MODE ON " 
                                     'face '(:background "#7fa339" 
                                            :foreground "#ffffff" 
                                            :weight bold
                                            :box (:line-width 1 :color "#4a5d23")))
                         "")))
    (ecc-term-claude--debug "Mode-line indicator - Auto-mode: %s, State: %s" 
                           (if ecc-term-claude-auto-mode "ON" "OFF") state)
    (ecc-term-claude--update-frame-title state)
    (let ((result (concat
                   auto-indicator
                   (cond
                    ((eq state :waiting) " [Waiting]")
                    ((eq state :y/n) " [Y/N]")
                    ((eq state :y/y/n) " [Y/Y/N]")
                    ((eq state :initial-waiting) " [Init]")
                    (t "")))))
      (ecc-term-claude--debug "Mode-line indicator returning: %s" result)
      result)))

(defun ecc-term-claude--update-frame-title (state)
  "Update frame title with STATE if enabled."
  (when (and ecc-term-claude-update-frame-title state)
    (let ((state-str (cond
                      ((eq state :waiting) "Waiting")
                      ((eq state :y/n) "Y/N")
                      ((eq state :y/y/n) "Y/Y/N")
                      ((eq state :initial-waiting) "Init")
                      (t nil))))
      (when state-str
        (let ((current-title (frame-parameter nil 'name)))
          (when current-title
            (unless (string-match-p (concat "\\[" state-str "\\]") current-title)
              (set-frame-parameter nil 'name 
                                   (concat "[" state-str "] " 
                                           (replace-regexp-in-string "\\[.*?\\] " "" current-title))))))))))


;;;; Claude Interaction Commands

(defun ecc-term-claude-yes ()
  "Send 'y' response to Claude prompt."
  (interactive)
  (ecc-term-claude--debug "Sending 'y' response")
  (vterm-send-string "y")
  (vterm-send-return))

(defun ecc-term-claude-no ()
  "Send 'n' response to Claude prompt."
  (interactive)
  (ecc-term-claude--debug "Sending 'n' response")
  (vterm-send-string "n")
  (vterm-send-return))

(defun ecc-term-claude-clear ()
  "Clear the vterm buffer."
  (interactive)
  (ecc-term-claude--debug "Clearing buffer")
  (vterm-clear))

;;;; Auto-Response Functions

(defun ecc-term-claude-auto-mode-toggle ()
  "Toggle automatic response to Claude prompts."
  (interactive)
  
  ;; Ensure we're in ecc-term-claude-mode when enabling auto-mode
  (when (and (not ecc-term-claude-auto-mode)  ; We're about to enable auto-mode
             (eq major-mode 'vterm-mode)       ; We're in vterm-mode
             (not (eq major-mode 'ecc-term-claude-mode))) ; But not in claude mode
    (ecc-term-claude-mode))  ; Switch to claude mode
  
  (setq ecc-term-claude-auto-mode (not ecc-term-claude-auto-mode))
  (ecc-term-claude--debug "Auto-mode toggled to: %s" (if ecc-term-claude-auto-mode "ON" "OFF"))
  (message "Claude auto-mode %s"
           (if ecc-term-claude-auto-mode "enabled" "disabled"))
  
  ;; Force mode-line update to show auto-mode indicator
  (ecc-term-claude--debug "Forcing mode-line update")
  (force-mode-line-update)
  (ecc-term-claude--debug "Calling redisplay to ensure visual update")
  (redisplay t)
  
  ;; Ensure mode-line-process is set if not already
  (when (and ecc-term-claude-show-state-in-mode-line
             (not mode-line-process))
    (ecc-term-claude--debug "Setting mode-line-process for state indicator")
    (setq mode-line-process
          '(:eval (ecc-term-claude--mode-line-state-indicator))))
  
  ;; Update mode name to reflect auto-mode state
  (let ((new-mode-name (if ecc-term-claude-auto-mode
                           (propertize "Claude-VTerm[AUTO]" 'face 'bold)
                         "Claude-VTerm")))
    (ecc-term-claude--debug "Updating mode-name from '%s' to '%s'" mode-name new-mode-name)
    (setq mode-name new-mode-name))
  
  ;; Update buffer name to reflect auto-mode state
  (when (string-match "\\*CLAUDE.*\\*" (buffer-name))
    (let ((base-name (replace-regexp-in-string " \\[AUTO\\]" "" (buffer-name))))
      (if ecc-term-claude-auto-mode
          (rename-buffer (concat base-name " [AUTO]") t)
        (rename-buffer base-name t))))
  
  ;; Update modeline color for visual feedback
  (if ecc-term-claude-auto-mode
      (progn
        ;; Use face-remap-add-relative which is more reliable
        (face-remap-add-relative 'mode-line
                                 :background ecc-term-claude-auto-mode-modeline-color
                                 :foreground "#ffffff"
                                 :box `(:line-width 2 :color ,ecc-term-claude-auto-mode-modeline-box-color))
        (face-remap-add-relative 'mode-line-inactive
                                 :background "#3a4d13"
                                 :foreground "#cccccc"
                                 :box '(:line-width 1 :color "#2a3d03"))
        ;; Also add a more visible indicator in mode-line-buffer-id
        (face-remap-add-relative 'mode-line-buffer-id
                                 :foreground "#7fa339"
                                 :weight 'bold)
        ;; Track state
        (setq-local ecc-term-claude--auto-mode-active t))
    ;; Reset face remappings when auto-mode is off
    (face-remap-reset-base 'mode-line)
    (face-remap-reset-base 'mode-line-inactive)
    (face-remap-reset-base 'mode-line-buffer-id)
    (setq-local ecc-term-claude--auto-mode-active nil))
  
  ;; Force modeline update
  (force-mode-line-update)
  
  ;; Set up hooks for auto-responses
  (if ecc-term-claude-auto-mode
      (progn
        (ecc-term-claude--debug "Adding auto-response hook")
        (add-to-list 'ecc-term-claude--update-functions
                     'ecc-term-claude--auto-send-respond)
        ;; Ensure buffer is registered with auto-response system
        (ecc-auto-response-register-buffer (current-buffer))
        ;; Start buffer-local auto-response
        (ecc-auto-response-buffer-start (current-buffer)))
    (ecc-term-claude--debug "Removing auto-response hook")
    (setq ecc-term-claude--update-functions
          (remove 'ecc-term-claude--auto-send-respond
                  ecc-term-claude--update-functions))
    ;; Stop buffer-local auto-response
    (ecc-auto-response-buffer-stop (current-buffer))))

(defun ecc-term-claude--auto-send-respond ()
  "Automatically respond to Claude prompts in vterm mode."
  (when ecc-term-claude-auto-mode
    (let ((state (ecc-detect-state)))
      (ecc-term-claude--debug "Auto-response checking state: %s" state)
      (cond
       ((eq state :y/y/n)
        (ecc-term-claude--auto-send-y/y/n))
       ((eq state :y/n)
        (ecc-term-claude--auto-send-y/n))
       ((eq state :initial-waiting)
        (ecc-term-claude--auto-send-initial-waiting))
       ((eq state :waiting)
        (ecc-term-claude--auto-send-continue))))))

(defun ecc-term-claude--auto-send-y/n ()
  "Automatically respond with 'y' to Y/N prompts."
  (ecc-term-claude--debug "Auto-responding to Y/N with '%s'" ecc-auto-response-yes)
  (vterm-send-string ecc-auto-response-yes)
  (vterm-send-return)
  (message "Auto-responded: %s" ecc-auto-response-yes))

(defun ecc-term-claude--auto-send-y/y/n ()
  "Automatically respond with 'y' to Y/Y/N prompts."
  (ecc-term-claude--debug "Auto-responding to Y/Y/N with '%s'" ecc-auto-response-yes-plus)
  (vterm-send-string ecc-auto-response-yes-plus)
  (vterm-send-return)
  (message "Auto-responded: %s" ecc-auto-response-yes-plus))

(defun ecc-term-claude--auto-send-continue ()
  "Automatically respond to continue prompts."
  (ecc-term-claude--debug "Auto-responding to continue with '%s'" ecc-auto-response-continue)
  (vterm-send-string ecc-auto-response-continue)
  (vterm-send-return)
  (message "Auto-responded: %s" ecc-auto-response-continue))

(defun ecc-term-claude--auto-send-initial-waiting ()
  "Automatically respond to initial waiting prompts."
  (ecc-term-claude--debug "Auto-responding to initial waiting with '%s'"
                          ecc-auto-response-initial-waiting)
  (vterm-send-string ecc-auto-response-initial-waiting)
  (vterm-send-return)
  (message "Auto-responded: %s" ecc-auto-response-initial-waiting))

;;;; Buffer Management

(defun ecc-term-claude--cleanup-buffer ()
  "Clean up when buffer is killed."
  (when (memq major-mode '(ecc-term-claude-mode vterm-mode))
    (ecc-term-claude--debug "Cleaning up buffer: %s" (buffer-name))
    ;; Cancel any timers
    (when ecc-term-claude--state-timer
      (ecc-term-claude--debug "Cancelling state timer")
      (cancel-timer ecc-term-claude--state-timer)
      (setq ecc-term-claude--state-timer nil))
    
    ;; Remove from registered buffers
    (let ((buf (current-buffer)))
      (remhash buf ecc-term-claude--registered-buffers)
      (ecc-term-claude--debug "Removed buffer from registered list"))))

(defun ecc-term-claude--register-buffer (&optional buffer)
  "Register BUFFER as a Claude buffer.
If BUFFER is nil, use the current buffer."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (unless (buffer-live-p buf)
      (ecc-term-claude--debug "Error: Buffer is not alive: %s" buf)
      (user-error "Buffer is not alive"))
    
    ;; Add to registered buffers
    (puthash buf t ecc-term-claude--registered-buffers)
    (ecc-term-claude--debug "Registered buffer: %s" (buffer-name buf))
    
    ;; Register the buffer with auto-response system
    (ecc-auto-response-register-buffer buf)
    (ecc-term-claude--debug "Registered with auto-response system")
    
    ;; Return the buffer
    buf))

;;;; Follow Bottom Functionality

(defun ecc-term-claude-follow-bottom-after-output ()
  "Scroll to bottom after vterm produces new output.
Only has effect if `ecc-vterm-always-follow-bottom` is non-nil."
  (when ecc-vterm-always-follow-bottom
    (ecc-term-claude--debug "Following bottom after output")
    (ecc-term-claude-scroll-to-bottom)))

(defun ecc-term-claude-scroll-to-bottom ()
  "Scroll to show the bottom of the buffer."
  (when (and ecc-vterm-always-follow-bottom
             (buffer-live-p (current-buffer)))
    (let ((window (get-buffer-window (current-buffer))))
      (when window
        (with-selected-window window
          (recenter (- (window-height) 
                     ecc-vterm-follow-bottom-margin
                     1)))))))

(defun ecc-term-claude-toggle-follow-bottom ()
  "Toggle the always-follow-bottom feature."
  (interactive)
  (setq ecc-vterm-always-follow-bottom 
        (not ecc-vterm-always-follow-bottom))
  (ecc-term-claude--debug "Follow bottom %s" 
                          (if ecc-vterm-always-follow-bottom "enabled" "disabled"))
  (message "Always follow bottom %s"
           (if ecc-vterm-always-follow-bottom "enabled" "disabled"))
  (when ecc-vterm-always-follow-bottom
    (ecc-term-claude-scroll-to-bottom)))

;;;; Public APIs

(defun ecc-term-claude-enable ()
  "Enable Claude features in the current vterm buffer.
This applies Claude state detection and auto-response functionality
without changing the major mode."
  (interactive)
  (if (eq major-mode 'vterm-mode)
      (progn
        (ecc-term-claude--debug "Enabling Claude features in existing vterm buffer")
        (ecc-term-claude--setup-existing-buffer))
    (ecc-term-claude--debug "Error: Not a vterm buffer: %s" (buffer-name))
    (user-error "This command can only be used in vterm buffers")))

(defun ecc-term-claude--setup-existing-buffer ()
  "Set up current vterm buffer with Claude enhancements without changing major mode."
  (unless (eq major-mode 'vterm-mode)
    (ecc-term-claude--debug "Error: Not a vterm buffer: %s" (buffer-name))
    (user-error "Current buffer is not in vterm-mode"))
  
  ;; Apply the individual setup functions
  (ecc-term-claude--debug "Setting up existing vterm buffer: %s" (buffer-name))
  (ecc-term-claude--register-buffer)
  (ecc-term-claude--setup-mode-line)
  (ecc-term-claude--setup-state-timer)
  (ecc-term-claude--setup-hooks)
  (ecc-term-claude--setup-keys)
  (ecc-term-claude--setup-appearance)
  (add-hook 'kill-buffer-hook 'ecc-term-claude--cleanup-buffer nil t)
  
  (message "Claude features applied to current vterm buffer"))

(defun ecc-term-claude ()
  "Create a new Claude vterm buffer with optimized settings.
If called from an existing vterm buffer, applies Claude settings to current buffer.
Otherwise, creates a new buffer with Claude vterm mode."
  (interactive)
  (cond
   ;; If we're already in a vterm buffer, apply Claude settings without changing the mode
   ((and (eq major-mode 'vterm-mode) 
         (not (eq major-mode 'ecc-term-claude-mode)))
    (ecc-term-claude--debug "Applying Claude settings to existing vterm buffer")
    (ecc-term-claude--setup-existing-buffer)
    (current-buffer))
   
   ;; Default case: create a new Claude vterm buffer
   (t
    (ecc-term-claude--debug "Creating new Claude vterm buffer")
    (let* ((buffer-name ecc-term-claude-buffer-name)
           (existing-buffer (get-buffer buffer-name))
           (new-buffer (or existing-buffer (get-buffer-create buffer-name))))
      (with-current-buffer new-buffer
        (unless (eq major-mode 'ecc-term-claude-mode)
          (ecc-term-claude-mode)))
      (switch-to-buffer new-buffer)
      new-buffer))))

;;;; Backward Compatibility

;; Function aliases for backward compatibility
(defalias 'ecc-register-buffer 'ecc-term-claude--register-buffer)
(defalias 'ecc-term-claude-auto-send-accept 'ecc-term-claude--auto-send-respond)
(defalias 'ecc-term-claude-auto-send-y/n 'ecc-term-claude--auto-send-y/n)
(defalias 'ecc-term-claude-auto-send-y/y/n 'ecc-term-claude--auto-send-y/y/n)
(defalias 'ecc-term-claude-auto-send-continue 'ecc-term-claude--auto-send-continue)
(defalias 'ecc-term-claude-auto-send-initial-waiting 'ecc-term-claude--auto-send-initial-waiting)
(defalias 'ecc-term-claude-setup-existing-buffer 'ecc-term-claude--setup-existing-buffer)
(defalias 'ecc-term-claude-setup-mode-line 'ecc-term-claude--setup-mode-line)
(defalias 'ecc-term-claude-mode-line-state-indicator 'ecc-term-claude--mode-line-state-indicator)
(defalias 'ecc-term-claude-setup-keys 'ecc-term-claude--setup-keys)
(defalias 'ecc-term-claude-cleanup-buffer 'ecc-term-claude--cleanup-buffer)

;; Compatibility with old variables
(defvaralias 'ecc-term-claude-update-functions 'ecc-term-claude--update-functions)
(defvaralias 'ecc-term-claude-state-timer 'ecc-term-claude--state-timer)
(defvaralias 'ecc-term-claude-menu 'ecc-term-claude--menu)

;; New aliases for consistent naming with other consolidated modules
(defalias 'ecc-term-claude-consolidated-debug 'ecc-term-claude--debug)
(defalias 'ecc-term-claude-consolidated-register-buffer 'ecc-term-claude--register-buffer)
(defalias 'ecc-term-claude-consolidated-mode-line-indicator 'ecc-term-claude--mode-line-state-indicator)

;; For compatibility with original module
(defalias 'ecc-term-claude-mode-original 'ecc-term-claude-mode
  "Compatibility alias for original implementation.")

;; Provide the standard name and compatibility aliases
(provide 'ecc-term-claude-mode)
(provide 'ecc-term-claude-mode) ;; For backward compatibility

;;; ecc-term-claude-mode.el ends here