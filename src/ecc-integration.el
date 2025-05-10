;;; ecc-integration.el --- Integration of modern architecture components -*- lexical-binding: t -*-

;; Author: ywatanabe <ywatanabe@alumni.u-tokyo.ac.jp>
;; Maintainer: ywatanabe <ywatanabe@alumni.u-tokyo.ac.jp>
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: ai, convenience, tools
;; URL: https://github.com/ywatanabe1989/emacs-claude-code

;;; Commentary:
;; 
;; This module integrates the modern architecture components (state engine,
;; buffer manager, and command system) with the rest of the Emacs Claude Code
;; package. It serves as a bridge between the old and new systems during the
;; architectural transition.
;;
;; The integration layer provides compatibility functions and conversions
;; between the old and new data structures.

;;; Code:

(require 'cl-lib)
(require 'ecc-state-engine)
(require 'ecc-buffer-manager)
(require 'ecc-command)

;; Legacy module requirements for integration
;; Use safe require to handle potential missing modules
(require 'ecc-buffer/ecc-buffer-variables nil t)
(require 'ecc-buffer/ecc-buffer-state nil t)

;; Create stubs for missing modules if needed
(unless (boundp 'ecc-buffer-registry)
  (defvar ecc-buffer-registry (make-hash-table :test 'equal)))

;; Don't require legacy state detection, we'll use the new state engine

;; ---- Buffer Integration ----

(defun ecc-integration-import-legacy-buffers ()
  "Import existing Claude buffers into the new buffer manager.
This converts any existing buffers from the old system to the new one."
  (interactive)
  (when (boundp 'ecc-buffer-registry)
    (maphash (lambda (buffer-name _value)
               (let ((buf (get-buffer buffer-name)))
                 (when (buffer-live-p buf)
                   ;; Check if it's a Claude buffer by looking for markers
                   (with-current-buffer buf
                     (when (or (eq major-mode 'ecc-mode)
                               (local-variable-p 'ecc-buffer-state))
                       ;; Import this buffer
                       (let* ((state (if (local-variable-p 'ecc-buffer-state)
                                        (buffer-local-value 'ecc-buffer-state buf)
                                      'idle))
                              (claude-buffer (ecc-buffer-manager-create buffer-name buf)))
                         ;; Set state
                         (ecc-buffer-manager-set-state claude-buffer state)
                         ;; Import metadata if available
                         (when (local-variable-p 'ecc-buffer-timestamp)
                           (ecc-buffer-manager-set-metadata
                            claude-buffer
                            'timestamp
                            (buffer-local-value 'ecc-buffer-timestamp buf)))
                         ;; Update legacy buffer
                         (with-current-buffer buf
                           (setq-local ecc-buffer-imported-to-new t))))))))
             ecc-buffer-registry)))

(defun ecc-integration-export-to-legacy-buffers ()
  "Export buffers from the new system to the legacy system.
This makes new buffers compatible with old code that expects the old structure."
  (interactive)
  (dolist (claude-buffer (ecc-buffer-manager-get-all))
    (let* ((buf (ecc-buffer-buffer claude-buffer))
           (state (ecc-buffer-state claude-buffer)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          ;; Set legacy variables
          (setq-local ecc-buffer-state state)
          (setq-local ecc-buffer-timestamp 
                      (or (ecc-buffer-manager-get-metadata claude-buffer 'timestamp)
                          (current-time)))
          ;; Add to legacy registry
          (when (boundp 'ecc-buffer-registry)
            (puthash (buffer-name buf) t ecc-buffer-registry)))))))

(defun ecc-integration-sync-buffer-states ()
  "Synchronize buffer states between the old and new systems.
This ensures both systems have consistent state information."
  (interactive)
  
  ;; First sync from new to old
  (ecc-integration-export-to-legacy-buffers)
  
  ;; Then sync from old to new for anything missed
  (ecc-integration-import-legacy-buffers))

;; ---- State Integration ----

(defun ecc-integration-detect-state-legacy (buffer)
  "Detect Claude state in BUFFER using legacy detection.
Returns a state ID symbol compatible with the new state engine."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((legacy-state (cond
                           ;; Try using the legacy detector if available
                           ((fboundp 'ecc-state-detect-prompt)
                            (funcall 'ecc-state-detect-prompt))
                           ;; Fall back to buffer-local state if available
                           ((and (boundp 'ecc-buffer-state)
                                 (local-variable-p 'ecc-buffer-state))
                            ecc-buffer-state)
                           ;; Default
                           (t 'unknown))))
        ;; Convert legacy state to new state engine ID
        (pcase legacy-state
          ('waiting 'waiting)
          ('running 'running)
          ('yes-no 'yes-no)
          ('yes-yes-no 'yes-yes-no)
          ('error 'error)
          (_ 'idle))))))

(defun ecc-integration-detect-and-update-state (buffer)
  "Detect Claude state in BUFFER and update both new and legacy systems."
  (interactive (list (current-buffer)))
  (when (buffer-live-p buffer)
    ;; Try detecting with the new engine first
    (let ((new-state (ecc-state-engine-detect-state buffer)))
      (unless new-state
        ;; Fall back to legacy detection
        (setq new-state (ecc-integration-detect-state-legacy buffer)))
      
      (when new-state
        ;; Update new state engine
        (ecc-state-engine-set-state new-state)
        
        ;; Update legacy state
        (with-current-buffer buffer
          (when (boundp 'ecc-buffer-state)
            (setq-local ecc-buffer-state new-state)))
        
        ;; Update buffer manager state for this buffer
        (let ((claude-buffer (ecc-buffer-manager-get-by-buffer buffer)))
          (when claude-buffer
            (ecc-buffer-manager-set-state claude-buffer new-state)))))))

;; ---- Command Integration ----

(defun ecc-integration-handle-input (input &optional buffer)
  "Handle INPUT in BUFFER using the new command system.
If BUFFER is nil, uses the current buffer."
  (interactive "sInput: ")
  (let ((buf (or buffer (current-buffer))))
    (when (buffer-live-p buf)
      ;; Make sure the buffer is in the new system
      (let ((claude-buffer (ecc-buffer-manager-get-by-buffer buf)))
        (unless claude-buffer
          ;; Import this buffer if it's not yet in the new system
          (setq claude-buffer (ecc-buffer-manager-create (buffer-name buf) buf)))
        
        ;; Set as current in the buffer manager
        (ecc-buffer-manager-set-current claude-buffer)
        
        ;; Process the input through the command system
        (ecc-command-execute input)))))

;; ---- Initialization ----

(defun ecc-integration-init ()
  "Initialize the integration layer.
This sets up the new components and imports existing buffers."
  (interactive)
  
  ;; Initialize the new components
  (ecc-state-engine-init)
  (ecc-buffer-manager-init)
  (ecc-command-init)
  
  ;; Import existing buffers
  (ecc-integration-import-legacy-buffers)
  
  ;; Set up state detection hook
  (let ((state-detector
         (lambda (claude-buffer)
           (let ((buf (ecc-buffer-buffer claude-buffer)))
             (when (buffer-live-p buf)
               (ecc-integration-detect-and-update-state buf))))))
    
    ;; Run state detection when a buffer is selected
    (ecc-buffer-manager-add-hook 'select state-detector)
    
    ;; Run state detection periodically
    (run-with-idle-timer 1 t (lambda ()
                               (when-let ((current (ecc-buffer-manager-get-current)))
                                 (funcall state-detector current))))))

;; Initialize when the package is loaded
(ecc-integration-init)

(provide 'ecc-integration)
;;; ecc-integration.el ends here