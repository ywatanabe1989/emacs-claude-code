;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-10-26 10:14:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-smart-yank.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:
;; Smart yank with visual diff when replacing regions
;; Shows what changed when you yank over selected text

;; 1. Configuration
;; ----------------------------------------

(defcustom ecc-smart-yank-diff-hide-delay 5
  "Seconds of idle time before hiding the *ECC Yank Diff* buffer.
Set to nil or 0 to disable auto-hiding."
  :type '(choice (const :tag "Disable" nil) integer)
  :group 'ecc)

(defvar --ecc-smart-yank-diff-hide-timer nil
  "Timer used to hide the *ECC Yank Diff* buffer.")

;; 2. Helper functions
;; ----------------------------------------

(defun --ecc-hide-yank-diff-buffer ()
  "Hide the *ECC Yank Diff* buffer by killing it."
  (interactive)
  (let ((diff-buffer (get-buffer "*ECC Yank Diff*")))
    (when (and diff-buffer (buffer-live-p diff-buffer))
      (kill-buffer diff-buffer)
      (message "*ECC Yank Diff* buffer closed.")))
  ;; Timer has done its job or was cancelled, clear the variable
  (when (timerp --ecc-smart-yank-diff-hide-timer)
    (cancel-timer --ecc-smart-yank-diff-hide-timer)
    (setq --ecc-smart-yank-diff-hide-timer nil)))

(defun --ecc-show-diff-in-buffer (original-text yanked-text)
  "Compare ORIGINAL-TEXT and YANKED-TEXT using diff and display in *ECC Yank Diff*.
The diff buffer is read-only and can be closed with 'q'."
  (let ((diff-buffer-name "*ECC Yank Diff*")
        (old-file (make-temp-file "ecc-yank-diff-old-"))
        (new-file (make-temp-file "ecc-yank-diff-new-"))
        diff-buffer)
    (unwind-protect
        (progn
          ;; Write texts to temporary files for diff command
          (with-temp-file old-file (insert original-text))
          (with-temp-file new-file (insert yanked-text))

          ;; Get or create the target diff buffer, ensure it's empty
          (setq diff-buffer (get-buffer-create diff-buffer-name))
          (with-current-buffer diff-buffer
            (let ((inhibit-read-only t))
              ;; Allow modification temporarily
              (erase-buffer)
              ;; Call diff command
              (call-process shell-file-name nil t nil
                            shell-command-switch
                            (format "diff -u %s %s"
                                    (shell-quote-argument old-file)
                                    (shell-quote-argument new-file)))
              ;; Set up buffer properties
              (goto-char (point-min))
              (diff-mode)
              (setq buffer-read-only t) ; Make it read-only
              ;; Define a local keymap for 'q' to quit
              (let ((map (make-sparse-keymap)))
                (set-keymap-parent map diff-mode-map) ; Inherit diff-mode keys
                (define-key map (kbd "q") #'--ecc-hide-yank-diff-buffer) ; Bind q
                (use-local-map map))))

          ;; Display the diff buffer
          (display-buffer diff-buffer '(display-buffer-pop-up-window))

          ;; Cancel any existing hide timer
          (when (timerp --ecc-smart-yank-diff-hide-timer)
            (cancel-timer --ecc-smart-yank-diff-hide-timer)
            (setq --ecc-smart-yank-diff-hide-timer nil))

          ;; Start a new timer to hide the buffer after idle delay, if delay is set
          (when (and (integerp ecc-smart-yank-diff-hide-delay)
                     (> ecc-smart-yank-diff-hide-delay 0))
            (setq --ecc-smart-yank-diff-hide-timer
                  (run-with-idle-timer ecc-smart-yank-diff-hide-delay nil
                                       #'--ecc-hide-yank-diff-buffer))))
      ;; Cleanup: Delete temporary files
      (when (file-exists-p old-file) (delete-file old-file))
      (when (file-exists-p new-file) (delete-file new-file)))))

;; 3. Main function
;; ----------------------------------------

;;;###autoload
(defun ecc-smart-yank (arg)
  "Yank text. If region is active, replace it and show diff if changed.
The diff buffer *ECC Yank Diff* is read-only, closes with 'q',
and auto-hides after `ecc-smart-yank-diff-hide-delay` idle seconds.
With prefix ARG, yank the ARG-th kill in the kill ring."
  (interactive "*P")
  (let ((region-is-active (region-active-p)))
    (if region-is-active
        ;; Region is active: capture, delete, yank, then show diff
        (let* ((orig-beg (region-beginning))
               (orig-end (region-end))
               (region-had-content (> orig-end orig-beg))
               (orig-text (if region-had-content
                              (buffer-substring-no-properties orig-beg orig-end)
                            ""))
               yank-succeeded
               yanked-text)
          ;; Delete the region *before* yanking
          (when region-had-content
            (delete-region orig-beg orig-end))
          ;; Ensure point is at the start position for yank insertion
          (goto-char orig-beg)
          ;; Perform the yank operation
          (condition-case err
              (progn
                (yank arg) ; Call the original yank command
                (setq yanked-text (current-kill 0)) ; Get what was just yanked
                (setq yank-succeeded t))
            (user-error ; Catch "Kill ring is empty" etc.
             (message "%s" (error-message-string err))
             (setq yank-succeeded nil))
            (error ; Catch other potential errors during yank
             (message "Error during yank: %s" (error-message-string err))
             (setq yank-succeeded nil)))

          ;; If yank happened and region originally had content, check if text changed
          (when (and yank-succeeded region-had-content)
            (let ((text-changed (not (string= orig-text yanked-text))))
              ;; Show diff only if the text actually changed
              (if text-changed
                  (progn
                    (--ecc-show-diff-in-buffer orig-text yanked-text)
                    (message "Region replaced. Diff shown in *ECC Yank Diff*. Press 'q' in diff buffer to close."))
                (message "Region content was identical to yanked text.")))))
      ;; Else: No active region, just perform the original yank
      (yank arg))))

(provide 'ecc-smart-yank)

(when (not load-file-name)
  (message "ecc-smart-yank.el loaded."
           (file-name-nondirectory (or load-file-name buffer-file-name))))
