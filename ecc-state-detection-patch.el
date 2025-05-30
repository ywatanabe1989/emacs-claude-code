;;; ecc-state-detection-patch.el --- Temporary patch for state detection

;; This patch adds more flexible pattern detection for Claude prompts
;; Load this file to override the default detection function

(require 'ecc-state-detection)

(defun --ecc-state-detection--analyze-text (text)
  "Analyze TEXT to detect Claude prompt state - PATCHED VERSION."
  (catch 'found
    ;; Check for running state first (highest priority)
    (when (string-match-p "esc to interrupt" text)
      (--ecc-debug-message "Matched state :running")
      (throw 'found :running))
    
    ;; More aggressive detection for waiting states
    ;; Check if we're at the end of a conversation with Human: prompt
    (when (and (string-match-p "Human:" text)
               (not (string-match-p "Assistant:" (substring text (max 0 (- (length text) 200))))))
      (--ecc-debug-message "Matched Human: without following Assistant:")
      (throw 'found :waiting))
    
    ;; Check for any prompt-like pattern at the end
    (when (string-match-p "\\(?:│\\|>\\|:\\)[[:space:]]*$" text)
      (--ecc-debug-message "Matched generic prompt pattern at end")
      (throw 'found :waiting))
    
    ;; Check for /user: commands that indicate we're waiting
    (when (string-match-p "/user:[[:alnum:]]*[[:space:]]*$" text)
      (--ecc-debug-message "Matched /user: command at end")
      (throw 'found :waiting))
    
    ;; Check for specific patterns
    (when (string-match-p "\\[Y/y/n\\]" text)
      (throw 'found :y/y/n))
    (when (string-match-p "\\[y/n\\]\\|\\[Y/n\\]" text)
      (throw 'found :y/n))
    (when (string-match-p "continue>\\|Continue>" text)
      (throw 'found :waiting))
    
    ;; Check for Y/Y/N pattern first (must come before Y/N check)
    (let ((yyn-pattern (cdr (assq :y/y/n --ecc-state-detection-patterns))))
      (when (and yyn-pattern (string-match-p (regexp-quote yyn-pattern) text))
        (--ecc-debug-message "Matched state :y/y/n")
        (throw 'found :y/y/n)))
    
    ;; Fallback: if buffer ends with multiple newlines, assume waiting
    (when (string-match-p "\n\n+$" text)
      (--ecc-debug-message "Matched multiple newlines at end - assuming waiting")
      (throw 'found :waiting))
    
    ;; Check for exact pattern matches
    (dolist (pattern-pair --ecc-state-detection-patterns)
      (let ((state (car pattern-pair))
            (pattern (cdr pattern-pair)))
        ;; Skip initial-waiting check if there's previous content
        (unless (and (eq state :initial-waiting)
                     (--ecc-state-detection--has-previous-messages-p))
          (when (string-match-p (regexp-quote pattern) text)
            (--ecc-debug-message "Matched state %s" state)
            (throw 'found state)))))
    nil))

(message "State detection patch loaded - more aggressive pattern matching enabled")

(provide 'ecc-state-detection-patch)