;;; variable-fix.el --- Fix variables for testing

;; Define the essential variables needed for compatibility
(defvar ecc-buffer-name "*Claude*"
  "Name for the Claude interaction buffer.")

(defvar ecc-buffer nil
  "Buffer for Claude interaction.")

(defvar ecc-active-buffer nil
  "Current active buffer for Claude interaction.")

(defvar ecc-buffers nil
  "List of registered Claude buffers.")

(defvar ecc-buffer-current-buffer nil
  "Current buffer being used for Claude.")

(defvar ecc-buffer-current-active-buffer nil
  "Current active buffer for compatibility.")

(defvar ecc-buffer-registered-buffers-alist nil
  "Registry of all buffers for compatibility.")

;; Provide the feature
(provide 'variable-fix)
