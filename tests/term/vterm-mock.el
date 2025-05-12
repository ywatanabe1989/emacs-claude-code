;;; vterm-mock.el --- Mock vterm functions for testing

;; Copyright (C) 2025 Yusuke Watanabe

;;; Commentary:
;; This file provides mock implementations of vterm functions for testing

;;; Code:

;; Define our tracking variables
(defvar vterm-mock-last-string nil
  "Last string sent via vterm-send-string.")

(defvar vterm-mock-history nil
  "History of strings sent via vterm-send-string.")

(defvar vterm-mock-return-count 0
  "Count of vterm-send-return calls.")

(defvar vterm-mock-clear-count 0
  "Count of vterm-clear calls.")

(defvar vterm-mock-copy-mode-calls nil
  "History of vterm-copy-mode calls with arguments.")

(defun vterm-mock-reset ()
  "Reset all mock tracking variables."
  (setq vterm-mock-last-string nil
        vterm-mock-history nil
        vterm-mock-return-count 0
        vterm-mock-clear-count 0
        vterm-mock-copy-mode-calls nil))

;; Define mock vterm functions
(defun vterm-send-string (string &optional paste-p)
  "Mock vterm-send-string to capture sent STRINGS.
Optional PASTE-P parameter is ignored in the mock."
  (setq vterm-mock-last-string string)
  (push string vterm-mock-history)
  (message "Mock vterm-send-string called with: %s" string))

(defun vterm-send-return ()
  "Mock vterm-send-return to track calls."
  (setq vterm-mock-return-count (1+ vterm-mock-return-count))
  (message "Mock vterm-send-return called"))

(defun vterm-clear ()
  "Mock vterm-clear to track calls."
  (setq vterm-mock-clear-count (1+ vterm-mock-clear-count))
  (message "Mock vterm-clear called"))

(defun vterm-copy-mode (arg)
  "Mock vterm-copy-mode to track ARG value."
  (push arg vterm-mock-copy-mode-calls)
  (message "Mock vterm-copy-mode called with: %s" arg))

(defun vterm-send-key (key &optional times)
  "Mock vterm-send-key with KEY and optional TIMES."
  (message "Mock vterm-send-key called with key: %s, times: %s" 
           key (or times 1)))

;; Define test buffer setup function
(defun vterm-mock-setup-test-buffer ()
  "Create a test buffer for vterm operations."
  (let ((test-buffer (generate-new-buffer "*TEST-VTERM*")))
    (with-current-buffer test-buffer
      (setq-local major-mode 'ecc-claude-vterm-mode))
    ;; Set all the necessary buffer variables
    (setq ecc-buffer test-buffer
          ecc-buffer-current-buffer test-buffer
          ecc-buffer-current-active-buffer test-buffer
          ecc-active-buffer test-buffer)
    test-buffer))

(provide 'vterm-mock)
;;; vterm-mock.el ends here