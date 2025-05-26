;;; test-pattern-debug.el --- Debug pattern matching issues

(require 'ecc-variables)
(require 'ecc-state-detection)

;; Test the exact patterns
(defun test-exact-patterns ()
  "Test exact pattern matching."
  (message "\n=== Testing Exact Pattern Matching ===")
  
  ;; Show the patterns with their character codes
  (message "Waiting pattern: %S" ecc-state-prompt-waiting)
  (message "Waiting pattern chars:")
  (let ((i 0))
    (while (< i (length ecc-state-prompt-waiting))
      (message "  Char %d: %c (code %d)" i 
               (aref ecc-state-prompt-waiting i)
               (aref ecc-state-prompt-waiting i))
      (setq i (1+ i))))
  
  (message "\nInitial waiting pattern: %S" ecc-state-prompt-initial-waiting)
  (message "Initial waiting pattern chars:")
  (let ((i 0))
    (while (< i (length ecc-state-prompt-initial-waiting))
      (message "  Char %d: %c (code %d)" i 
               (aref ecc-state-prompt-initial-waiting i)
               (aref ecc-state-prompt-initial-waiting i))
      (setq i (1+ i))))
  
  ;; Test matching
  (let ((test-waiting "│ >                            ")
        (test-initial "│ > Try "))
    
    (message "\nTest waiting string chars:")
    (let ((i 0))
      (while (< i (length test-waiting))
        (message "  Char %d: %c (code %d)" i 
                 (aref test-waiting i)
                 (aref test-waiting i))
        (setq i (1+ i))))
    
    (message "\nDirect string comparison:")
    (message "  Waiting equal? %s" (string= ecc-state-prompt-waiting test-waiting))
    (message "  Initial equal? %s" (string= ecc-state-prompt-initial-waiting test-initial))
    
    (message "\nRegexp-quote matching:")
    (message "  Waiting match? %s" (string-match-p (regexp-quote ecc-state-prompt-waiting) test-waiting))
    (message "  Initial match? %s" (string-match-p (regexp-quote ecc-state-prompt-initial-waiting) test-initial))))

;; Test with buffer content
(defun test-buffer-detection ()
  "Test detection in buffer."
  (message "\n=== Testing Buffer Detection ===")
  
  (with-temp-buffer
    (insert "│ >                            ")
    (message "Buffer contains: %S" (buffer-string))
    (let ((state (ecc-detect-state)))
      (message "Detected state: %s" state)))
  
  (with-temp-buffer
    (insert "│ > Try ")
    (message "Buffer contains: %S" (buffer-string))
    (let ((state (ecc-detect-state)))
      (message "Detected state: %s" state))))

;; Run tests
(test-exact-patterns)
(test-buffer-detection)

;;; test-pattern-debug.el ends here