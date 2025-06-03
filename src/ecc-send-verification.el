;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-06-04 07:10:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-send-verification.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ecc-state-detection)
(require 'ecc-auto-response-logging)
(require 'cl-lib)

;; 1. Configuration
;; ----------------------------------------
(defcustom ecc-send-verification-timeout 2.0
  "Timeout in seconds for verifying command was sent."
  :type 'float
  :group 'ecc)

(defcustom ecc-send-verification-retry-count 3
  "Maximum number of retries for failed sends."
  :type 'integer
  :group 'ecc)

(defcustom ecc-send-verification-check-interval 0.1
  "Interval in seconds between verification checks."
  :type 'float
  :group 'ecc)

;; 2. Variables
;; ----------------------------------------
(defvar-local --ecc-send-verification-last-content nil
  "Buffer content before sending command.")

(defvar-local --ecc-send-verification-expected-text nil
  "Expected text that should appear after send.")

;; 3. Main verification functions
;; ----------------------------------------
(defun ecc-send-verify-command (text send-func)
  "Send TEXT using SEND-FUNC and verify it was sent.

Returns t if verified, nil otherwise.

Example:
  (ecc-send-verify-command \"y\"
    (lambda () (vterm-send-string \"y\")))"
  (let ((initial-content (ecc--get-buffer-end-content))
        (start-time (float-time))
        (verified nil)
        (attempt 1))
    
    ;; Store verification state
    (setq-local --ecc-send-verification-last-content initial-content)
    (setq-local --ecc-send-verification-expected-text text)
    
    ;; Try sending with retries
    (while (and (not verified)
                (<= attempt ecc-send-verification-retry-count))
      (ecc-auto-response-log 'info "Send attempt %d for: %s" attempt text)
      
      ;; Execute send function
      (funcall send-func)
      
      ;; Verify send
      (setq verified (ecc--verify-text-sent text initial-content start-time))
      
      (if verified
          (progn
            (ecc-auto-response-log 'info "Send verified after %d attempts" attempt)
            (ecc-auto-response-log-send-success nil text))
        (progn
          (ecc-auto-response-log 'warn "Send attempt %d failed for: %s" attempt text)
          (when (< attempt ecc-send-verification-retry-count)
            (sit-for 0.5))))
      
      (cl-incf attempt))
    
    (unless verified
      (ecc-auto-response-log-send-failure nil "Verification failed after all retries"))
    
    verified))

;; 4. Verification helpers
;; ----------------------------------------
(defun ecc--verify-text-sent (text initial-content start-time)
  "Verify TEXT was sent by checking buffer changes."
  (let ((deadline (+ start-time ecc-send-verification-timeout))
        (found nil))
    (while (and (not found)
                (< (float-time) deadline))
      (let ((current-content (ecc--get-buffer-end-content)))
        ;; Check if content changed and contains our text
        (when (and (not (string= initial-content current-content))
                   (or (string-match-p (regexp-quote text) current-content)
                       (ecc--buffer-content-advanced-p initial-content current-content)))
          (setq found t)))
      (unless found
        (sit-for ecc-send-verification-check-interval)))
    found))

(defun ecc--get-buffer-end-content ()
  "Get content from end of buffer for comparison."
  (let ((size (min 500 (- (point-max) (point-min)))))
    (buffer-substring-no-properties
     (max (point-min) (- (point-max) size))
     (point-max))))

(defun ecc--buffer-content-advanced-p (old-content new-content)
  "Check if buffer content has advanced from OLD-CONTENT to NEW-CONTENT."
  (and (> (length new-content) (length old-content))
       (not (string= old-content new-content))))

;; 5. Prompt state verification
;; ----------------------------------------
(defun ecc-verify-prompt-ready ()
  "Verify the prompt is ready to receive input.

Returns t if prompt is empty and ready."
  (let ((state (--ecc-state-detection-detect))
        (prompt-area (ecc--get-prompt-area)))
    (ecc-auto-response-log 'debug "Prompt verification - State: %s, Prompt: %s" 
                          state (if prompt-area (substring prompt-area 0 (min 50 (length prompt-area))) "nil"))
    ;; Check if we're in a waiting state with empty prompt
    (and (memq state '(:waiting :initial-waiting :y/n :y/y/n))
         (or (not prompt-area)
             (string-match-p "^[[:space:]]*$" prompt-area)))))

(defun ecc--get-prompt-area ()
  "Extract the current prompt area from vterm."
  (when (derived-mode-p 'vterm-mode)
    ;; Look for common prompt patterns
    (save-excursion
      (goto-char (point-max))
      (beginning-of-line)
      (let ((line (buffer-substring-no-properties (point) (point-max))))
        ;; Extract text after prompt markers
        (when (string-match "\\(?:> \\|❯ \\|\\$ \\|# \\)\\(.*\\)$" line)
          (match-string 1 line))))))

;; 6. Integration helpers
;; ----------------------------------------
(defun ecc-send-with-verification (text mode)
  "Send TEXT in MODE with verification.

MODE can be 'vterm or 'comint."
  (let ((send-func
         (pcase mode
           ('vterm (lambda () 
                    (vterm-send-string text)
                    (sit-for 0.1)
                    (vterm-send-return)))
           ('comint (lambda ()
                     (goto-char (point-max))
                     (insert text)
                     (comint-send-input)))
           (_ (lambda ()
                (goto-char (point-max))
                (insert text "\n"))))))
    (ecc-send-verify-command text send-func)))

(provide 'ecc-send-verification)

(when (not load-file-name)
  (message "ecc-send-verification.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))