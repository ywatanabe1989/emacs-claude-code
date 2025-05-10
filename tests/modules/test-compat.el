;;; test-compat.el --- Compatibility layer for tests

;; Add core directories to load-path
(let* ((this-file (or load-file-name buffer-file-name))
       (test-dir (file-name-directory this-file))
       (root-dir (expand-file-name "../.." test-dir))
       (src-dir (expand-file-name "src" root-dir)))
  
  ;; Add project root and src to load-path
  (add-to-list 'load-path root-dir)
  (add-to-list 'load-path src-dir)
  
  ;; Add all src subdirectories to load-path
  (dolist (dir (directory-files src-dir t "\\`[^.]"))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))

;; Provide backward compatibility mappings - our old feature names to new module requirements
(defvar test-compat-mappings
  '(
    ;; Core modules - map from root to src/ directory
    ("ecc-variables" . ecc-variables)
    ("ecc-auto" . ecc-auto)
    ("ecc-bindings" . ecc-bindings)
    ("ecc-dired" . ecc-dired)
    ("ecc-elisp-test" . ecc-elisp-test)
    ("ecc-history" . ecc-history)
    ("ecc-large-buffer" . ecc-large-buffer)
    ("ecc-mode" . ecc-mode)
    ("ecc-repository" . ecc-repository)
    ("ecc-repository-view" . ecc-repository-view)
    ("ecc-run" . ecc-run)
    ("ecc-send" . ecc-send)
    ("ecc-update-mode-line" . ecc-update-mode-line)
    
    ;; Buffer modules
    ("ecc-buffer/ecc-buffer" . ecc-buffer/ecc-buffer)
    ("ecc-buffer/ecc-buffer-auto-switch" . ecc-buffer/ecc-buffer-auto-switch)
    ("ecc-buffer/ecc-buffer-current" . ecc-buffer/ecc-buffer-current)
    ("ecc-buffer/ecc-buffer-navigation" . ecc-buffer/ecc-buffer-navigation)
    ("ecc-buffer/ecc-buffer-registry" . ecc-buffer/ecc-buffer-registry)
    ("ecc-buffer/ecc-buffer-stale" . ecc-buffer/ecc-buffer-stale)
    ("ecc-buffer/ecc-buffer-state" . ecc-buffer/ecc-buffer-state)
    ("ecc-buffer/ecc-buffer-timestamp" . ecc-buffer/ecc-buffer-timestamp)
    ("ecc-buffer/ecc-buffer-variables" . ecc-buffer/ecc-buffer-variables)
    ("ecc-buffer/ecc-buffer-verification" . ecc-buffer/ecc-buffer-verification)
    
    ;; State modules
    ("ecc-state/ecc-state" . ecc-state/ecc-state)
    ("ecc-state/ecc-state-detect" . ecc-state/ecc-state-detect)
    
    ;; Template modules
    ("ecc-template/ecc-template" . ecc-template/ecc-template)
    ("ecc-template/ecc-template-cache" . ecc-template/ecc-template-cache)
    ("ecc-template/ecc-template-mode" . ecc-template/ecc-template-mode)
    
    ;; VTERM modules
    ("ecc-term/ecc-claude-vterm-mode" . ecc-term/ecc-claude-vterm-mode)
    
    ;; Legacy names mapping to new modules
    ("emacs-claude-code" . ecc-variables) ;; Main module typically loaded first
    
    ;; Add compatibility for old test module names
    ("test-ecc-auto" . ecc-auto)
    ("test-ecc-buffer" . ecc-buffer/ecc-buffer)
    ("test-ecc-buffer-registry" . ecc-buffer/ecc-buffer-registry)
    ("test-ecc-send" . ecc-send)
    ("test-ecc-variables" . ecc-variables)
    )
  "Mapping from legacy module names to new module paths.")

;; Check if a feature appears to be one of our own modules
(defun test-compat-our-module-p (feature-name)
  "Check if FEATURE-NAME is likely one of our own modules."
  (or (string-prefix-p "ecc-" feature-name)
      (string-prefix-p "ecc/" feature-name)
      (string-match-p "^ecc-\\(buffer\\|state\\|template\\|term\\)" feature-name)
      (string-prefix-p "test-ecc-" feature-name)))

;; Set up advice to provide backward compatibility
(defun test-compat-require-advice (orig-fun feature &rest args)
  "Advice function to map old feature names to new feature names."
  (let ((mapped-feature feature))
    ;; Only try to map our own features, not standard Emacs modules
    (when (symbolp feature)
      (let ((feature-name (symbol-name feature)))
        (when (test-compat-our-module-p feature-name)
          (let ((mapping (assoc feature-name test-compat-mappings)))
            (when mapping
              (setq mapped-feature (cdr mapping))))))) 
    
    ;; For our test modules, first try to load from src directory
    (if (and (symbolp mapped-feature)
             (test-compat-our-module-p (symbol-name mapped-feature)))
        (condition-case nil
            (apply orig-fun mapped-feature args)
          (error
           ;; If error, try loading from src/ directory with explicit path
           (let* ((feature-name (symbol-name mapped-feature))
                  (src-file (expand-file-name 
                             (concat "src/" 
                                     (if (string-match-p "/" feature-name)
                                         feature-name
                                       (concat feature-name ".el"))))))
             (if (file-exists-p src-file)
                 (load src-file nil t)
               (apply orig-fun mapped-feature args)))))
      ;; For non-ECC modules, just apply the original function
      (apply orig-fun mapped-feature args))))

;; Install the advice
(advice-add 'require :around #'test-compat-require-advice)

;; Make sure all the module directories are properly provided
(dolist (mapping test-compat-mappings)
  (let ((old-name (car mapping))
        (new-name (cdr mapping)))
    (when (and (not (featurep new-name))
               (test-compat-our-module-p old-name))
      ;; Load the module if it exists
      (condition-case nil
          (require new-name)
        (error nil)))))

;; Handle duplicate test definitions
(defadvice ert-set-test (around suppress-duplicate-test-errors activate)
  "Suppress errors when a test is redefined or loaded twice."
  (condition-case err
      ad-do-it
    (error
     (unless (string-match-p "redefined" (error-message-string err))
       (signal (car err) (cdr err))))))

;; Preload core modules that are commonly needed
(dolist (module '(ecc-variables ecc-auto ecc-send ecc-update-mode-line))
  (condition-case nil
      (require module)
    (error nil)))

(provide 'test-compat)