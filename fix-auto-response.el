;;; fix-auto-response.el --- Quick fix for auto-response issues

;; Load this file to apply fixes and diagnostics

(require 'ecc-auto-response)
(require 'ecc-state-detection)

;; Load the patch for better detection
(load-file "/home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/ecc-state-detection-patch.el")

(defun ecc-fix-auto-response ()
  "Apply fixes for auto-response issues."
  (interactive)
  
  ;; 1. Enable debug mode
  (setq --ecc-debug-enabled t)
  
  ;; 2. Refresh all timers
  (when (fboundp 'ecc-refresh-timers)
    (ecc-refresh-timers))
  
  ;; 3. Show current state
  (let ((state (--ecc-state-detection-detect)))
    (message "Current detected state: %s" state)
    
    ;; 4. If in waiting state but auto-response not enabled, enable it
    (when (and state
               (not (bound-and-true-p --ecc-auto-response--enabled)))
      (message "Auto-response not enabled - enabling now...")
      (--ecc-auto-response-enable-buffer))
    
    ;; 5. Force a response if in waiting state
    (when (memq state '(:waiting :initial-waiting))
      (message "Forcing auto-response for waiting state...")
      (let ((response (cdr (assq state --ecc-auto-response-responses))))
        (when response
          (--ecc-auto-response--send-to-buffer (current-buffer) response))))
    
    ;; 6. Show diagnostics
    (message "\n=== Diagnostics ===")
    (message "Buffer: %s" (buffer-name))
    (message "Auto-response enabled: %s" (bound-and-true-p --ecc-auto-response--enabled))
    (message "Main timer active: %s" (bound-and-true-p --ecc-auto-response--timer))
    (message "Detected state: %s" state)
    (message "Last 100 chars: %S" 
             (buffer-substring-no-properties
              (max (- (point-max) 100) (point-min))
              (point-max)))))

;; Run the fix automatically
(ecc-fix-auto-response)

(provide 'fix-auto-response)