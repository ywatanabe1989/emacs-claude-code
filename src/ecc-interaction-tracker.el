;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 09:15:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-interaction-tracker.el

;;; Commentary:
;;; Tracks Claude interactions and provides periodic command functionality.

(require 'ecc-variables)

;; Customization group
(defgroup ecc-interaction nil
  "Interaction tracking settings for Claude."
  :group 'ecc
  :prefix "ecc-interaction-")

(defcustom ecc-interaction-display-count t
  "Whether to display the interaction count in messages."
  :type 'boolean
  :group 'ecc-interaction)

(defcustom ecc-interaction-log-timestamps nil
  "Whether to log timestamp of each interaction."
  :type 'boolean
  :group 'ecc-interaction)

(defcustom ecc-interaction-max-history 100
  "Maximum number of interaction timestamps to keep."
  :type 'number
  :group 'ecc-interaction)

(defcustom ecc-periodic-commands-alist 
  '(("/compact" . 10) 
    ("/git" . 20))
  "Alist of periodic commands and their intervals.
Each entry is a cons cell (COMMAND . INTERVAL) where:
- COMMAND is the string to send to Claude
- INTERVAL is the number of interactions after which to send the command."
  :type '(alist :key-type string :value-type integer)
  :group 'ecc-interaction)

(defcustom ecc-periodic-commands-enabled nil
  "Whether to automatically send periodic commands."
  :type 'boolean
  :group 'ecc-interaction)

;;;###autoload
(defun ecc-track-interaction ()
  "Track a new interaction with Claude.
Increments the counter and handles periodic commands if enabled."
  (interactive)
  ;; Increment counter
  (setq ecc-interaction-counter (1+ ecc-interaction-counter))
  
  ;; Add timestamp if logging is enabled
  (when ecc-interaction-log-timestamps
    (push (float-time) ecc-interaction-timestamps)
    ;; Trim history if needed
    (when (> (length ecc-interaction-timestamps) ecc-interaction-max-history)
      (setq ecc-interaction-timestamps 
            (seq-take ecc-interaction-timestamps ecc-interaction-max-history))))
  
  ;; Display count if enabled
  (when ecc-interaction-display-count
    (ecc-debug-message "Claude interaction #%d" ecc-interaction-counter))
  
  ;; Handle periodic commands if enabled
  (when ecc-periodic-commands-enabled
    (ecc-check-and-send-periodic-commands)))

(defun ecc-check-and-send-periodic-commands ()
  "Check all configured periodic commands and send if interval reached."
  (when (and ecc-periodic-commands-enabled
             (buffer-live-p ecc-buffer-current-buffer)
             (> ecc-interaction-counter 0))
    (dolist (cmd-pair ecc-periodic-commands-alist)
      (let ((command (car cmd-pair))
            (interval (cdr cmd-pair)))
        (when (and (> interval 0)
                   (= (mod ecc-interaction-counter interval) 0))
          (ecc-send-command command))))))

(defun ecc-send-command (command)
  "Send COMMAND to the current Claude buffer."
  (when (and (buffer-live-p ecc-buffer-current-buffer)
             (stringp command)
             (not (string-empty-p command)))
    (with-current-buffer ecc-buffer-current-buffer
      (when (fboundp 'ecc-auto--send-vterm-response)
        (ecc-auto--send-vterm-response command)
        (ecc-debug-message "Sent command: %s (interaction #%d)" 
                 command 
                 ecc-interaction-counter)))))

;;;###autoload
(defun ecc-reset-interaction-counter ()
  "Reset the Claude interaction counter to zero."
  (interactive)
  (setq ecc-interaction-counter 0)
  (setq ecc-interaction-timestamps nil)
  (ecc-debug-message "Claude interaction counter reset to 0"))

;;;###autoload
(defun ecc-display-interaction-stats ()
  "Display statistics about Claude interactions."
  (interactive)
  (let ((count ecc-interaction-counter)
        (timestamps ecc-interaction-timestamps)
        (buffer-name "*Claude Interaction Stats*"))
    
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (when (fboundp 'org-mode)
          (org-mode)
          (insert "#+TITLE: Claude Interaction Statistics\n\n")
          (insert (format "* Total Interactions: %d\n\n" count))
          
          (when ecc-periodic-commands-enabled
            (insert "* Periodic Commands\n\n")
            (insert "| Command | Interval | Next At |\n")
            (insert "|---------|----------|--------|\n")
            (dolist (cmd-pair ecc-periodic-commands-alist)
              (let* ((cmd (car cmd-pair))
                     (interval (cdr cmd-pair))
                     (next-at (if (<= interval 0) 
                                  "disabled"
                                (number-to-string 
                                 (* (1+ (/ count interval)) interval)))))
                (insert (format "| %s | %d | #%s |\n" 
                                cmd interval next-at)))))
          
          (when timestamps
            (insert "\n* Interaction Timestamps\n\n")
            (insert "| # | Time | Relative |\n")
            (insert "|---+------+----------|\n")
            (let* ((timestamps-rev (reverse timestamps))
                   (now (float-time))
                   (first-time (car timestamps-rev)))
              (cl-loop for time in timestamps-rev
                       for i from 1 to (length timestamps-rev)
                       do (insert (format "| %d | %s | %s |\n" 
                                          i
                                          (format-time-string "%Y-%m-%d %H:%M:%S" time)
                                          (if (= i 1) 
                                              "First"
                                            (format "%.1f min" 
                                                    (/ (- time first-time) 60.0))))))))
          
          (goto-char (point-min))
          (when (fboundp 'org-display-inline-images)
            (org-display-inline-images))
          (read-only-mode 1))))
    
    (display-buffer buffer-name)))

;;;###autoload
(defun ecc-toggle-periodic-commands ()
  "Toggle automatic periodic commands on/off."
  (interactive)
  (setq ecc-periodic-commands-enabled (not ecc-periodic-commands-enabled))
  (ecc-debug-message "Periodic commands %s (%d commands configured)"
           (if ecc-periodic-commands-enabled "enabled" "disabled")
           (length ecc-periodic-commands-alist)))

;;;###autoload
(defun ecc-add-periodic-command (command interval)
  "Add or update a periodic COMMAND with INTERVAL."
  (interactive "sCommand to send: \nnInterval (interactions): ")
  (setf (alist-get command ecc-periodic-commands-alist nil nil #'string=) interval)
  (ecc-debug-message "Added periodic command: %s every %d interactions" 
           command interval))

;;;###autoload
(defun ecc-remove-periodic-command (command)
  "Remove COMMAND from periodic commands."
  (interactive
   (list (completing-read "Remove command: " 
                          (mapcar #'car ecc-periodic-commands-alist)
                          nil t)))
  (setq ecc-periodic-commands-alist
        (delq (assoc command ecc-periodic-commands-alist)
              ecc-periodic-commands-alist))
  (ecc-debug-message "Removed periodic command: %s" command))

;;;###autoload
(defun ecc-disable-periodic-command (command)
  "Disable COMMAND by setting its interval to 0."
  (interactive
   (list (completing-read "Disable command: " 
                          (mapcar #'car ecc-periodic-commands-alist)
                          nil t)))
  (setf (alist-get command ecc-periodic-commands-alist nil nil #'string=) 0)
  (ecc-debug-message "Disabled periodic command: %s (interval set to 0)" command))

;; Hook into vterm to track user input
(defun ecc-track-vterm-input-hook (input)
  "Track user input in vterm.
INPUT is the string being sent to the terminal."
  (when (and (derived-mode-p 'vterm-mode)
             (string-match-p ".*\\S-.*" input)  ; Non-empty, non-whitespace input
             (not (string-prefix-p "^" input))) ; Not a control character
    ;; This appears to be a real interaction, track it
    (ecc-track-interaction))
  ;; Return input unchanged
  input)

;; Add hook to track interactions when sending to vterm
(with-eval-after-load 'vterm
  (advice-add 'vterm-send-string :filter-args
              (lambda (args)
                (let ((orig-string (car args)))
                  (cons (ecc-track-vterm-input-hook orig-string) (cdr args))))))

(provide 'ecc-interaction-tracker)

;;; ecc-interaction-tracker.el ends here