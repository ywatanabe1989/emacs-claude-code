;;; test-auto-response-vterm.el --- Test auto-response with vterm

;;; Commentary:
;;; This test shows how to properly set up and test auto-response functionality

(require 'ecc-variables)
(require 'ecc-state-detection)
(require 'ecc-auto-response)
(require 'vterm nil t)

(defun test-auto-response-setup ()
  "Set up and test auto-response functionality."
  (interactive)
  
  ;; Create a test buffer
  (let ((buffer (get-buffer-create "*Claude Auto-Response Test*")))
    (switch-to-buffer buffer)
    
    ;; Insert some example prompts (with proper non-breaking spaces)
    (erase-buffer)
    (insert "Claude response here...\n")
    (insert "❯ 1. Yes / 2. No\n")
    (insert "> ")
    
    ;; Register the buffer for auto-response
    (message "\n=== Setting up Auto-Response ===")
    (ecc-auto-response-register-buffer buffer)
    
    ;; Start auto-response with debug enabled
    (setq ecc-auto-response-debug t)
    (ecc-auto-response-start)
    
    ;; Show current state
    (message "\nAuto-response is now enabled.")
    (message "Current buffer: %s" (buffer-name))
    (message "Registered buffers: %s" (hash-table-count ecc-auto-response--registered-buffers))
    
    ;; Detect state manually
    (let ((state (ecc-detect-state)))
      (message "Current detected state: %s" state))
    
    ;; Instructions
    (message "\nTo test:")
    (message "1. The auto-response timer is running and will check for prompts every %s seconds" 
             ecc-auto-response-check-interval)
    (message "2. When a Y/N prompt is detected, it will send: %s" ecc-auto-response-yes)
    (message "3. To stop auto-response: M-x ecc-auto-response-stop")
    (message "4. To see debug output, check *Messages* buffer")
    
    buffer))

(defun test-manual-response ()
  "Test manual response sending."
  (interactive)
  (message "\n=== Testing Manual Response ===")
  
  ;; Send a manual response
  (ecc-auto-response-yes)
  (message "Sent manual Y/N response")
  
  ;; Check buffer content
  (message "Buffer now contains:")
  (message "%s" (buffer-substring-no-properties (point-min) (point-max))))

;; Instructions for use
(message "Auto-Response Test Instructions:")
(message "1. Run: M-x test-auto-response-setup")
(message "2. This will create a test buffer and enable auto-response")
(message "3. Watch the *Messages* buffer for debug output")
(message "4. Run: M-x test-manual-response to test manual sending")
(message "5. Run: M-x ecc-auto-response-status to see current status")

;;; test-auto-response-vterm.el ends here