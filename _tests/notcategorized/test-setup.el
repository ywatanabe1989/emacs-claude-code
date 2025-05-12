;;; test-setup.el --- Test setup for Emacs Claude Code -*- lexical-binding: t -*-

;; Add all necessary paths for testing
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (project-dir (directory-file-name (file-name-directory test-dir)))
       (src-dir (expand-file-name "src" project-dir)))
  
  ;; Add project root to load path
  (add-to-list 'load-path project-dir)
  
  ;; Add src directory to load path
  (when (file-directory-p src-dir)
    (add-to-list 'load-path src-dir)
    
    ;; Add all subdirectories of src to load path
    (dolist (dir (directory-files src-dir t "\\`[^.]"))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)))))

(provide 'test-setup)
;;; test-setup.el ends here