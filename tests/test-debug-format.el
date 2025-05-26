;;; test-debug-format.el --- Test debug message format -*- lexical-binding: t -*-

;;; Commentary:
;;; Test that debug messages show [ECC DEBUG BUFFERNAME] without timestamp

(require 'ert)
(require 'ecc-debug-utils)
(require 'ecc-variables)

(ert-deftest test-debug-format-no-timestamp ()
  "Test that debug messages have correct format without timestamp."
  (let ((test-buffer (get-buffer-create "*test-debug-format*")))
    (unwind-protect
        (with-current-buffer test-buffer
          ;; Setup: disable timestamp, set custom prefix
          (setq-local ecc-debug-timestamp nil)
          (setq-local ecc-debug-prefix (format "[ECC DEBUG %s]" (buffer-name)))
          (setq-local ecc-debug-buffer-enabled t)
          
          ;; Capture debug output
          (let ((captured-output nil))
            (cl-letf (((symbol-function 'insert)
                       (lambda (&rest args)
                         (setq captured-output (concat captured-output (apply #'concat args))))))
              
              ;; Test 1: Basic debug message
              (ecc-debug-buffer-message test-buffer "Test message")
              
              ;; Verify format
              (should (string-match-p "^\\[ECC DEBUG \\*test-debug-format\\*\\]" captured-output))
              (should-not (string-match-p "\\[[0-9]+:[0-9]+:[0-9]+\\.[0-9]+\\]" captured-output)))))
      (kill-buffer test-buffer))))

(provide 'test-debug-format)
;;; test-debug-format.el ends here