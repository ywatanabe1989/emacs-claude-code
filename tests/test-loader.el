;;; test-loader.el --- Test loader for emacs-claude-code

;; Set up load-path for tests
(let* ((this-file (or load-file-name buffer-file-name))
       (tests-dir (file-name-directory this-file))
       (root-dir (expand-file-name ".." tests-dir))
       (src-dir (expand-file-name "src" root-dir))
       (modules-dir (expand-file-name "modules" tests-dir))
       (buffer-subdir (expand-file-name "ecc-buffer" src-dir))
       (state-subdir (expand-file-name "ecc-state" src-dir))
       (template-subdir (expand-file-name "ecc-template" src-dir))
       (term-subdir (expand-file-name "ecc-term" src-dir)))
  
  ;; Add all necessary directories to load-path
  (add-to-list 'load-path root-dir)
  (add-to-list 'load-path src-dir)
  (add-to-list 'load-path tests-dir)
  (add-to-list 'load-path modules-dir)
  
  ;; Add known subdirectories to load-path
  (add-to-list 'load-path buffer-subdir)
  (add-to-list 'load-path state-subdir)
  (add-to-list 'load-path template-subdir)
  (add-to-list 'load-path term-subdir)
  
  ;; Add all src subdirectories to load-path
  (dolist (dir (directory-files src-dir t "\\`[^.]"))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir)))
  
  ;; Print debug info about load paths
  (message "Added package root to load-path: %s" root-dir)
  (message "Added subdir to load-path: %s" modules-dir))

;; Load compatibility layers
(require 'test-compat)
(require 'fix-names)

;; Set up compatibility for direct loading of root modules
(dolist (name '("ecc-auto" "ecc-bindings" "ecc-dired" "ecc-elisp-test"
                "ecc-history" "ecc-large-buffer" "ecc-mode"
                "ecc-repository" "ecc-repository-view" "ecc-run"
                "ecc-send" "ecc-update-mode-line" "ecc-variables"))
  (let ((src-file (expand-file-name (concat "src/" name ".el") 
                                    (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))))
    (when (file-exists-p src-file)
      (load src-file nil t))))

(provide 'test-loader)