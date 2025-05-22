;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-23 00:39:21>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-convenience-commands.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;; Define convenience commands

(defun ecc-claude-vterm ()
  "Start a new Claude vterm session."
  (interactive)
  (cond
   ;; First try the ecc-term-claude function from ecc-term-claude-mode
   ((fboundp 'ecc-term-claude)
    (ecc-term-claude))
   ;; Fall back to a simple vterm setup if available
   ((fboundp 'vterm)
    (let ((buf (vterm)))
      (message
       "Starting basic vterm (Claude optimizations not available)")
      buf))
   ;; No vterm available
   (t
    (message "Claude vterm mode not available"))))

(defun ecc-claude-optimize-vterm ()
  "Optimize the current vterm buffer for Claude."
  (interactive)
  (if (fboundp 'ecc-vterm-mode)
      (progn
        (unless (derived-mode-p 'vterm-mode)
          (user-error "Not in a vterm buffer"))
        (ecc-vterm-mode 1)
        (message "Vterm buffer optimized for Claude"))
    (message "ecc-vterm-mode not available")))

(defun ecc-claude-auto-respond ()
  "Enable auto-response for Claude in the current buffer."
  (interactive)
  (if (fboundp 'ecc-start-auto-response)
      (progn
        (ecc-register-buffer)
        (ecc-start-auto-response)
        (message "Auto-response enabled for this buffer"))
    (message "Auto-response functionality not available")))

(defun ecc-claude-quick-auto-response ()
  "Quickly enable auto-response with the '/auto' continue response.
This is a convenience function that enables auto-response with
default settings but using '/auto' for the continue prompt."
  (interactive)
  (if (fboundp 'ecc-start-auto-response)
      (progn
        (ecc-register-buffer)
        (ecc-start-auto-response "1" "2" "/auto")
        (message "Quick auto-response enabled with /auto continue"))
    (message "Auto-response functionality not available")))

(defun ecc-claude-visual-aid-toggle ()
  "Toggle visual aids for Claude interaction."
  (interactive)
  (if (fboundp 'ecc-term-visual-aid-toggle)
      (ecc-term-visual-aid-toggle)
    (message "Visual aid functionality not available")))

(defun ecc-notify-toggle ()
  "Toggle notifications for Claude prompts."
  (interactive)
  (if (fboundp 'ecc-auto-notify-toggle)
      (ecc-auto-notify-toggle)
    (message "Notification functionality not available")))

(defun ecc-bell-toggle ()
  "Toggle bell notifications for Claude prompts."
  (interactive)
  (if (fboundp 'ecc-auto-notify-toggle-bell)
      (ecc-auto-notify-toggle-bell)
    (message "Notification bell functionality not available")))

(defun ecc-claude-interaction-stats ()
  "Display Claude interaction statistics."
  (interactive)
  (if (fboundp 'ecc-display-interaction-stats)
      (ecc-display-interaction-stats)
    (message "Interaction tracking functionality not available")))

(defun ecc-toggle-periodic-cmds ()
  "Toggle sending periodic commands to Claude."
  (interactive)
  (if (fboundp 'ecc-toggle-periodic-commands)
      (ecc-toggle-periodic-commands)
    (message "Periodic commands functionality not available")))

(defun ecc-add-periodic-cmd (command interval)
  "Add a new periodic COMMAND with INTERVAL."
  (interactive "sCommand to send: \nnInterval (interactions): ")
  (if (fboundp 'ecc-add-periodic-command)
      (ecc-add-periodic-command command interval)
    (message "Periodic commands functionality not available")))

(defun ecc-toggle-colors ()
  "Toggle between color themes for Claude vterm buffers."
  (interactive)
  (if (fboundp 'ecc-colors-toggle-theme)
      (ecc-colors-toggle-theme)
    (message "Color theme functionality not available")))

(defun ecc-toggle-grayscale ()
  "Toggle grayscale mode for Claude vterm buffers."
  (interactive)
  (if (fboundp 'ecc-vterm-grayscale-toggle)
      (ecc-vterm-grayscale-toggle)
    (message "Grayscale mode functionality not available")))

(defun ecc-toggle-eye-friendly ()
  "Toggle eye-friendly mode for buffer updates and scrolling."
  (interactive)
  (if (fboundp 'ecc-eye-friendly-toggle)
      (ecc-eye-friendly-toggle)
    (message "Eye-friendly functionality not available")))

(defun ecc-adjust-scroll-speed (speed)
  "Adjust maximum scroll speed to SPEED lines per second."
  (interactive "nMaximum scroll speed (lines/sec): ")
  (if (fboundp 'ecc-eye-friendly-adjust-speed)
      (ecc-eye-friendly-adjust-speed speed)
    (message "Eye-friendly functionality not available")))

;; Disable auto-follow by default to prevent scrolling issues
(setq ecc-vterm-always-follow-bottom nil)

(defun ecc-claude-stop-auto ()
  "Immediately stop auto-response and show a clear notification."
  (interactive)
  (if (fboundp 'ecc-stop-auto-response)
      (progn
        (ecc-stop-auto-response)
        (let ((msg "Auto-response STOPPED (C-g equivalent)"))
          (message msg)
          (when (fboundp 'display-message-or-buffer)
            (display-message-or-buffer msg "*Claude Auto Control*"))))
    (message "Auto-response functionality not available")))

(defun ecc-claude-help ()
  "Display help about Claude mode commands and keybindings."
  (interactive)
  (with-help-window "*Claude Help*"
    (princ "Claude Mode Commands\n")
    (princ "==================\n\n")
    (princ "Keybinding   Command                 Description\n")
    (princ "----------   -------                 -----------\n")

    ;; Basic operations
    (princ
     "C-c c v      ecc-claude-vterm        Start a new Claude vterm session\n")
    (princ
     "C-c c o      ecc-claude-optimize     Optimize current vterm for Claude\n")

    ;; Auto-response
    (princ
     "C-c c a      ecc-auto-response-toggle Toggle auto-response on/off\n")
    (princ
     "C-c c q      ecc-quick-auto          Quick enable auto-response with /auto\n")
    (princ
     "C-c c s      ecc-claude-stop-auto    Emergency stop auto-response\n")

    ;; Visual aids
    (princ "C-c c t      ecc-visual-aid-toggle   Toggle visual aids\n")
    (princ
     "C-c c m      ecc-toggle-colors       Toggle between color themes\n")
    (princ
     "C-c c g      ecc-toggle-grayscale    Toggle grayscale mode (disable colors)\n")
    (princ
     "C-c c e      ecc-toggle-eye-friendly Toggle eye-friendly scrolling mode\n")
    (princ
     "C-c c r      ecc-adjust-scroll-speed Set maximum scroll speed\n")

    ;; Notifications
    (princ
     "C-c c n      ecc-notify-toggle       Toggle prompt notifications\n")
    (princ
     "C-c c b      ecc-bell-toggle         Toggle notification bell\n")

    ;; Interaction tracking and periodic commands
    (princ
     "C-c c i      ecc-interaction-stats   Show interaction statistics\n")
    (princ
     "C-c c p      ecc-toggle-periodic     Toggle periodic commands\n")
    (princ
     "C-c c c      ecc-add-periodic-cmd    Add a new periodic command\n")

    ;; Help
    (princ "C-c c h      ecc-claude-help         Show this help\n\n")

    ;; Auto-Response System
    (princ "Auto-Response System\n")
    (princ "==================\n\n")
    (princ
     "The auto-response system automatically responds to Claude prompts:\n")
    (princ "- Y/N prompts: Sends \"1\" (Yes) by default\n")
    (princ "- Y/Y/N prompts: Sends \"2\" (Yes, and) by default\n")
    (princ "- Continue prompts: Sends \"/auto\" by default\n\n")

    ;; Notification System
    (princ "Notification System\n")
    (princ "==================\n\n")
    (princ
     "The notification system alerts you when Claude prompts are detected:\n")
    (princ "- Audio bell: Enabled by default (C-c c b to toggle)\n")
    (princ "- Visual mode-line flash: Enabled by default\n")
    (princ "- Messages: Show detected prompt types\n\n")

    ;; Visual Customization
    (princ "Visual Customization\n")
    (princ "===================\n\n")
    (princ "Customize Claude vterm buffer appearance:\n")
    (princ "- Toggle line numbers with C-c C-l in any vterm buffer\n")
    (princ
     "- Line numbers can be hidden by default with ecc-vterm-hide-line-numbers\n")
    (princ
     "- Cycle between dark, light, and gray themes with C-c c m\n")
    (princ "- Toggle grayscale mode (disable colors) with C-c c g\n")
    (princ "- Customize colors through ecc-colors-* variables\n")
    (princ
     "- Adjust font size with C-c C-+ and C-c C-- (also Ctrl+mouse wheel)\n")
    (princ "- Reset font size with C-c C-0\n")
    (princ
     "- Default and step sizes configurable through ecc-vterm-* variables\n\n")

    ;; Yank-as-file Feature
    (princ "Yank-as-File Feature\n")
    (princ "===================\n\n")
    (princ
     "Save Claude's output to files with automatic file type detection:\n")
    (princ
     "- Yank region to file with C-c C-y (prompts for filename)\n")
    (princ "- Yank entire buffer to file with C-c C-w\n")
    (princ "- Quick yank region to auto-named file with C-c C-q\n")
    (princ
     "- Auto-detects appropriate file extension based on content\n")
    (princ
     "- Supports many languages including Python, JavaScript, HTML, CSS, etc.\n")
    (princ
     "- Customizable default directory and extensions through ecc-vterm-yank-* variables\n\n")

    ;; Eye-Friendly Features
    (princ "Eye-Friendly Features\n")
    (princ "====================\n\n")
    (princ
     "Reduce eye strain with these features (C-c c e to toggle):\n")
    (princ
     "- Smooth scrolling: Buffer moves smoothly instead of jumping\n")
    (princ
     "- Scroll speed control: Limit maximum scrolling speed (C-c c r)\n")
    (princ
     "- Visual indicator: Shows scroll speed with color feedback\n")
    (princ "- Update throttling: Limits buffer update frequency\n")
    (princ "- Enabled by default in Claude vterm buffers\n\n")

    ;; Interaction Tracking System
    (princ "Interaction Tracking System\n")
    (princ "=========================\n\n")
    (princ "Tracks your interactions with Claude:\n")
    (princ "- Counts each user input\n")
    (princ "- Can display statistics (C-c c i)\n")
    (princ
     "- Supports periodic commands based on interaction count\n\n")

    ;; Periodic Commands System
    (princ "Periodic Commands System\n")
    (princ "======================\n\n")
    (princ "Automatically sends commands at specified intervals:\n")
    (princ
     "- Default: /compact every 10 interactions, /git every 20\n")
    (princ "- Add custom commands with C-c c c\n")
    (princ "- Toggle system on/off with C-c c p\n")
    (princ "- Customize through ecc-periodic-commands-alist\n\n")

    (princ
     "Smart scrolling is enabled - your position will be preserved when reading\n")
    (princ "earlier content in the buffer.\n\n")
    (princ
     "If you need to stop auto-response immediately, use C-c c s.\n")))

;; Define a minor mode with dedicated keymap instead of using global keybindings

(define-minor-mode ecc-claude-mode
  "Minor mode for emacs-claude-code global features.
When enabled, provides keybindings for Claude interaction commands."
  :lighter " Claude"
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            ;; Basic operations
            (define-key map (kbd "C-c c v") 'ecc-claude-vterm)
            (define-key map (kbd "C-c c o") 'ecc-claude-optimize-vterm)

            ;; Auto-response
            (define-key map (kbd "C-c c a") 'ecc-auto-response-toggle)
            (define-key map (kbd "C-c c q")
                        'ecc-claude-quick-auto-response)
            (define-key map (kbd "C-c c s") 'ecc-claude-stop-auto)

            ;; Visual aids
            (define-key map (kbd "C-c c t")
                        'ecc-claude-visual-aid-toggle)
            (define-key map (kbd "C-c c m") 'ecc-toggle-colors)
            (define-key map (kbd "C-c c g") 'ecc-toggle-grayscale)
            (define-key map (kbd "C-c c e") 'ecc-toggle-eye-friendly)
            (define-key map (kbd "C-c c r") 'ecc-adjust-scroll-speed)

            ;; Notifications
            (define-key map (kbd "C-c c n") 'ecc-notify-toggle)
            (define-key map (kbd "C-c c b") 'ecc-bell-toggle)

            ;; Interaction tracking
            (define-key map (kbd "C-c c i")
                        'ecc-claude-interaction-stats)

            ;; Periodic commands
            (define-key map (kbd "C-c c p") 'ecc-toggle-periodic-cmds)
            (define-key map (kbd "C-c c c") 'ecc-add-periodic-cmd)

            ;; Help
            (define-key map (kbd "C-c c h") 'ecc-claude-help)
            map))


(provide 'ecc-convenience-commands)

(when
    (not load-file-name)
  (message "ecc-convenience-commands.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))