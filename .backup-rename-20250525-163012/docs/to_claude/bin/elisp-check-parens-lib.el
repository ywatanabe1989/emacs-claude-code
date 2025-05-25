;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-24 20:05:32>
;;; File: /home/ywatanabe/.dotfiles/.claude/to_claude/bin/elisp-check-parens-lib.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;; Check if parentheses are balanced in a string

(defun check-parens-in-string (str)
  "Check if parentheses are balanced in STR. Return t if balanced, nil otherwise."
  (let ((depth 0))
    (catch 'unbalanced
      (mapc (lambda (char)
              (cond ((eq char ?\() (setq depth (1+ depth)))
                    ((eq char ?\)) (setq depth (1- depth))))
              (when (< depth 0)
                (throw 'unbalanced nil)))
            str)
      (zerop depth))))

;; Interactive function to check current buffer

(defun check-buffer-parens ()
  "Check if parentheses are balanced in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (condition-case err
        (progn
          (check-parens)
          (message "Parentheses are balanced!"))
      (error
       (message "Unbalanced parentheses: %s"
                (error-message-string err))))))

;; Function to highlight unmatched parentheses

(defun highlight-unmatched-parens ()
  "Highlight unmatched parentheses in the current buffer."
  (interactive)
  (let ((overlays nil))
    ;; Remove existing overlays
    (remove-overlays (point-min) (point-max) 'paren-check t)

    (save-excursion
      (goto-char (point-min))
      (let ((stack nil))
        (while (re-search-forward "[()]" nil t)
          (let ((char (char-before)))
            (cond
             ;; Opening paren
             ((eq char ?\()
              (push (1- (point)) stack))
             ;; Closing paren
             ((eq char ?\))
              (if stack
                  (pop stack)
                ;; Unmatched closing paren
                (let ((ov (make-overlay (1- (point)) (point))))
                  (overlay-put ov 'face '(:background "red"))
                  (overlay-put ov 'paren-check t)))))))
        ;; Highlight remaining unmatched opening parens
        (dolist (pos stack)
          (let ((ov (make-overlay pos (1+ pos))))
            (overlay-put ov 'face '(:background "red"))
            (overlay-put ov 'paren-check t)))))))

;; Function to auto-fix common parentheses issues

(defun fix-obvious-paren-errors ()
  "Attempt to fix obvious parentheses errors in Elisp code."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; Add missing closing parens at end of defun forms
    (while (re-search-forward "^(defun\\|^(defvar\\|^(defmacro" nil t)
      (let ((start (match-beginning 0)))
        (goto-char start)
        (condition-case nil
            (forward-sexp)
          (error
           ;; If forward-sexp fails, try to add closing parens
           (goto-char (point-max))
           (unless (looking-back ")" (1- (point)))
             (insert ")"))))))
    (goto-char (point-min))
    ;; Remove extra closing parens at end of buffer
    (goto-char (point-max))
    (while (and (> (point) (point-min))
                (looking-back ")\\s-*" (line-beginning-position)))
      (backward-char)
      (when (condition-case nil
                (progn (check-parens) nil)
              (error t))
        (delete-char 1)))))

;; Validate Elisp code from Claude

(defun validate-elisp-from-claude (code-string)
  "Validate CODE-STRING for balanced parentheses and basic Elisp syntax."
  (with-temp-buffer
    (insert code-string)
    (emacs-lisp-mode)
    (let ((balanced (condition-case nil
                        (progn (check-parens) t)
                      (error nil)))
          (syntax-ok (condition-case err
                         (progn
                           (goto-char (point-min))
                           (while (not (eobp))
                             (forward-sexp))
                           t)
                       (error nil))))
      (list :balanced balanced
            :syntax-ok syntax-ok
            :message (if (and balanced syntax-ok)
                         "Code looks good!"
                       "Issues found with parentheses or syntax")))))

;; Hook to check parens on save

(defun enable-auto-paren-check ()
  "Enable automatic parentheses checking when saving Elisp files."
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'check-buffer-parens nil t))))


(provide 'elisp-check-parens-lib)

(when
    (not load-file-name)
  (message "elisp-check-parens-lib.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))