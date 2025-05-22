;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 09:40:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/ecc-vterm/test-ecc-vterm-yank-as-file.el

;;; Commentary:
;;; Tests for the vterm yank-as-file functionality

(require 'ert)

;; Mock vterm if not available
(unless (featurep 'vterm)
  (defvar vterm-timer-delay 0.01)
  (defvar vterm-max-scrollback 1000)
  (defvar display-line-numbers-mode nil)
  (defvar vterm-disable-bold-font nil)
  (defvar vterm-disable-underline nil)
  (defun vterm-send-string (_string) nil)
  (defun vterm-send-return () nil)
  (defvar font-lock-mode nil)
  (defun font-lock-mode (_) nil)
  (defun display-line-numbers-mode (_) nil)
  (provide 'vterm))

(require 'ecc-variables)

;; Load the file under test
(when (file-exists-p "/home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-vterm-yank-as-file.el")
  (load-file "/home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-vterm-yank-as-file.el"))

;; Mock the selected region in buffer
(defvar ecc-test-region-content 
  "def hello_world():
    print('Hello, World!')

hello_world()
")

(defun ecc-test-buffer-substring (start end)
  "Mock `buffer-substring-no-properties' to return test content."
  ecc-test-region-content)

;; Test the yank-as-file function requirements
(ert-deftest test-ecc-vterm-yank-as-file-requirements ()
  "Test that the required functions and variables exist."
  ;; Test that the function exists
  (should (fboundp 'ecc-vterm-yank-as-file))
  
  ;; Test that the customization variables exist
  (should (boundp 'ecc-vterm-yank-default-dir))
  (should (boundp 'ecc-vterm-yank-extension-alist)))

;; Test the extension detection function
(ert-deftest test-ecc-vterm-detect-file-type ()
  "Test the file type detection based on content."
  (when (fboundp 'ecc-vterm-detect-file-type)
    ;; Python detection
    (let ((python-code "def hello():\n    print('hello')\n"))
      (should (string= "py" (ecc-vterm-detect-file-type python-code))))
    
    ;; JavaScript detection
    (let ((js-code "function hello() {\n  console.log('hello');\n}"))
      (should (string= "js" (ecc-vterm-detect-file-type js-code))))
    
    ;; HTML detection
    (let ((html-code "<html>\n<body>\n<h1>Hello</h1>\n</body>\n</html>"))
      (should (string= "html" (ecc-vterm-detect-file-type html-code))))
    
    ;; CSS detection
    (let ((css-code "body {\n  background-color: #ffffff;\n}"))
      (should (string= "css" (ecc-vterm-detect-file-type css-code))))
    
    ;; Elisp detection
    (let ((elisp-code "(defun hello ()\n  (message \"Hello\"))"))
      (should (string= "el" (ecc-vterm-detect-file-type elisp-code))))
    
    ;; Plain text fallback
    (let ((text "This is plain text with no specific markers"))
      (should (string= "txt" (ecc-vterm-detect-file-type text))))))

;; Test the file generation function
(ert-deftest test-ecc-vterm-generate-file-from-region ()
  "Test generating a file from region content."
  ;; Skip if function doesn't exist yet
  (skip-unless (fboundp 'ecc-vterm-generate-file-from-region))
  
  ;; Temp directory for test files
  (let* ((temp-dir (make-temp-file "ecc-test-" t))
         (default-directory temp-dir)
         (test-filename "test_file.py")
         (full-path (expand-file-name test-filename temp-dir)))
    
    ;; Mock region selection
    (cl-letf (((symbol-function 'buffer-substring-no-properties) 
               #'ecc-test-buffer-substring))
      
      ;; Call function with mock data
      (ecc-vterm-generate-file-from-region 1 10 test-filename)
      
      ;; Test that file was created
      (should (file-exists-p full-path))
      
      ;; Test that content was written correctly
      (with-temp-buffer
        (insert-file-contents full-path)
        (should (string= ecc-test-region-content (buffer-string)))))
    
    ;; Cleanup
    (when (file-exists-p full-path)
      (delete-file full-path))
    (when (file-exists-p temp-dir)
      (delete-directory temp-dir t))))

;; Test the interactive function
(ert-deftest test-ecc-vterm-yank-as-file-interactive ()
  "Test the interactive yank-as-file function."
  ;; Skip if function doesn't exist yet
  (skip-unless (fboundp 'ecc-vterm-yank-as-file))
  
  ;; Mock interactive components
  (cl-letf (((symbol-function 'buffer-substring-no-properties) 
             #'ecc-test-buffer-substring)
            ((symbol-function 'region-active-p) (lambda () t))
            ((symbol-function 'region-beginning) (lambda () 1))
            ((symbol-function 'region-end) (lambda () 10))
            ((symbol-function 'read-file-name) 
             (lambda (&rest _) (expand-file-name "test_output.py" temporary-file-directory)))
            ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
    
    ;; Call the interactive function
    (call-interactively 'ecc-vterm-yank-as-file)
    
    ;; Verify file was created
    (let ((expected-file (expand-file-name "test_output.py" temporary-file-directory)))
      (unwind-protect
          (progn
            (should (file-exists-p expected-file))
            ;; Check content
            (with-temp-buffer
              (insert-file-contents expected-file)
              (should (string= ecc-test-region-content (buffer-string)))))
        ;; Cleanup
        (when (file-exists-p expected-file)
          (delete-file expected-file))))))

(provide 'test-ecc-vterm-yank-as-file)

;;; test-ecc-vterm-yank-as-file.el ends here