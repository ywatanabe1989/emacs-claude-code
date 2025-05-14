;; Manual test script for dashboard buffer visibility
;; Run with: emacs -Q -l manual-test-buffer-visibility.el

;; Load dependencies
(add-to-list 'load-path ".")
(add-to-list 'load-path "./src")
(add-to-list 'load-path "./src/ecc-buffer")
(add-to-list 'load-path "./src/ecc-dashboard")
(add-to-list 'load-path "./src/ecc-state")
(add-to-list 'load-path "./src/ecc-ui")
(add-to-list 'load-path "./src/ecc-term")

;; Load required files
(require 'ecc-buffer)
(require 'ecc-dashboard)

;; Define test steps
(defun test-dashboard-buffer-visibility ()
  "Test the fix for dashboard buffer visibility."
  (interactive)
  (message "Starting manual test for dashboard buffer visibility...")
  
  ;; Create a new buffer
  (let ((test-buf (ecc-buffer-create)))
    (message "Created buffer: %s" test-buf)
    
    ;; Show the dashboard
    (message "Opening dashboard...")
    (ecc-dashboard-show)
    
    ;; Check if the buffer appears in the dashboard
    (message "Checking if buffer appears in dashboard...")
    (let* ((uid (ecc-buffer-get-uid test-buf))
           (agents (ecc-dashboard--get-filtered-agents)))
      (if (member uid agents)
          (message "SUCCESS: Buffer with UID %s found in dashboard agents: %s" uid agents)
        (message "FAILURE: Buffer with UID %s not found in dashboard agents: %s" uid agents)))))

;; Run the test
(message "Running manual test...")
(test-dashboard-buffer-visibility)