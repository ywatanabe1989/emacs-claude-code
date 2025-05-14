;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 19:50:30>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-buffer/ecc-buffer-registry-unified.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'cl-lib)
(require 'seq)
(require 'ecc-buffer-variables)
(require 'ecc-buffer-timestamp)

;; Buffer structures and registry
;; ------------------------------

(cl-defstruct (ecc-buffer (:constructor ecc-buffer--create)
                          (:copier nil))
  "Structure representing a Claude buffer."
  (id nil :read-only t :documentation
      "Unique identifier for this buffer")
  (name "" :read-only nil :documentation "Buffer name")
  (buffer nil :read-only nil :documentation "Actual buffer object")
  (state nil :read-only nil :documentation
         "Current state of the buffer")
  (metadata (make-hash-table :test 'equal) :read-only nil
            :documentation "Buffer metadata")
  (created-time (current-time) :read-only t :documentation
                "When this buffer was created")
  (last-used-time (current-time) :read-only nil :documentation
                  "When this buffer was last used"))

;; Main registry variables
(defvar ecc-buffer--registry (make-hash-table :test 'equal)
  "Registry of all Claude buffers, keyed by buffer ID.")

(defvar ecc-buffer--count 0
  "Counter for generating unique buffer IDs.")

(defvar ecc-buffer--hooks nil
  "Alist of hooks to run when buffer events occur.
Each element is (EVENT-TYPE . HOOK-FUNCTION-LIST) where EVENT-TYPE
is one of: create, select, rename, update, kill.")

;; Initialize the registry system
(defun ecc-buffer-registry-init ()
  "Initialize the buffer registry system."
  (setq ecc-buffer--registry (make-hash-table :test 'equal))
  (setq ecc-buffer-current-buffer nil)
  (setq ecc-buffer--count 0)
  (setq ecc-buffer-registered-buffers-alist nil)
  (setq ecc-buffer--hooks nil))

;; Core buffer registration functions
;; ------------------------------

;;;###autoload
(defun ecc-buffer-register (buffer)
  "Register BUFFER for Claude integration.
Records buffer in both the legacy alist and the structured registry.
Return the registered buffer or buffer structure."
  (when buffer
    (let ((buf (if (bufferp buffer) buffer (get-buffer buffer))))
      (when (and buf (buffer-live-p buf))
        ;; Register in legacy alist for backward compatibility
        (unless (assoc buf ecc-buffer-registered-buffers-alist)
          (push (cons buf nil) ecc-buffer-registered-buffers-alist)
          ;; Record a timestamp for the buffer
          (ecc-buffer-record-timestamp buf))
        
        ;; Register in struct registry if not already there
        (unless (ecc-buffer--get-by-buffer buf)
          (let* ((id (format "claude-%d" (cl-incf ecc-buffer--count)))
                 (name (buffer-name buf))
                 (struct (ecc-buffer--create :id id
                                            :name name
                                            :buffer buf
                                            :state nil)))
            (puthash id struct ecc-buffer--registry)
            (ecc-buffer--run-hooks 'create struct)))
        
        ;; Update current buffer if needed
        (when (null ecc-buffer-current-buffer)
          (ecc-buffer-set-current buf))
        
        ;; Return the buffer for backward compatibility
        buf))))

;;;###autoload
(defun ecc-buffer-unregister (buffer)
  "Unregister BUFFER from Claude integration.
Removes buffer from both legacy alist and structured registry.
Return t if buffer was removed, nil otherwise."
  (when buffer
    (let ((buf (if (bufferp buffer) buffer (get-buffer buffer)))
          (removed nil))
      
      ;; Remove from legacy system
      (when (assoc buf ecc-buffer-registered-buffers-alist)
        (setq ecc-buffer-registered-buffers-alist
              (assoc-delete-all buf ecc-buffer-registered-buffers-alist))
        (setq removed t))
      
      ;; Remove from struct registry
      (when-let ((struct (ecc-buffer--get-by-buffer buf)))
        (ecc-buffer--run-hooks 'kill struct)
        (remhash (ecc-buffer-id struct) ecc-buffer--registry)
        (setq removed t))
      
      ;; Update current buffer if needed
      (when (eq ecc-buffer-current-buffer buf)
        (setq ecc-buffer-current-buffer nil)
        (ecc-buffer--set-default-current))
      
      removed)))

;; Helper functions for buffer registry
;; ------------------------------

(defun ecc-buffer--get-by-id (id)
  "Get the buffer structure with ID from registry."
  (gethash id ecc-buffer--registry))

(defun ecc-buffer--get-by-buffer (buffer)
  "Get the buffer structure for BUFFER."
  (let ((buf (if (bufferp buffer) buffer (get-buffer buffer))))
    (seq-find (lambda (struct)
                (eq (ecc-buffer-buffer struct) buf))
              (hash-table-values ecc-buffer--registry))))

(defun ecc-buffer--set-default-current ()
  "Set the first available buffer as current if none is set."
  (unless ecc-buffer-current-buffer
    (let ((buffers (ecc-buffer-get-all-sorted)))
      (when buffers
        (ecc-buffer-set-current (car buffers))))))

(defun ecc-buffer--run-hooks (event-type buffer-struct)
  "Run all hooks for EVENT-TYPE with BUFFER-STRUCT."
  (let ((hooks (cdr (assoc event-type ecc-buffer--hooks))))
    (dolist (hook hooks)
      (condition-case err
          (funcall hook buffer-struct)
        (error (message "Error in buffer hook: %S" err))))))

;; Public interface for buffer management
;; ------------------------------

;;;###autoload
(defun ecc-buffer-create (&optional name)
  "Create a new Claude buffer with NAME.
If NAME is nil, a default name is generated.
Returns the new buffer."
  (interactive)
  (let* ((buffer-name (or name
                          (format "*CLAUDE-CODE-%02d*"
                                 (+ (length
                                     (ecc-buffer-get-registered-buffers))
                                    1))))
         (new-buffer (get-buffer-create buffer-name)))
    
    ;; Setup buffer if needed
    (with-current-buffer new-buffer
      (unless (derived-mode-p 'vterm-mode)
        (when (fboundp 'vterm-mode)
          (vterm-mode)))
      (set (make-local-variable 'ecc-original-name) buffer-name))
    
    ;; Register the buffer
    (ecc-buffer-register new-buffer)
    
    ;; Display and return
    (switch-to-buffer new-buffer)
    new-buffer))

;;;###autoload
(defun ecc-buffer-get-all-sorted ()
  "Get all registered Claude buffers sorted by timestamp.
Return a list of buffer objects."
  (let* ((all-buffers (ecc-buffer-get-registered-buffers))
         (live-buffers (seq-filter #'buffer-live-p all-buffers))
         (sorted-buffers
          (sort live-buffers
                (lambda (buf1 buf2)
                  (let ((time1 (ecc-buffer-get-timestamp buf1))
                        (time2 (ecc-buffer-get-timestamp buf2)))
                    (time-less-p time2 time1))))))
    sorted-buffers))

;;;###autoload
(defun ecc-buffer-set-current (buffer)
  "Set BUFFER as the current Claude buffer.
Update timestamps and display the buffer."
  (when buffer
    (let ((buf (if (bufferp buffer) buffer (get-buffer buffer))))
      (when (and buf (buffer-live-p buf))
        ;; Record timestamp
        (ecc-buffer-record-timestamp buf)
        
        ;; Set as current
        (setq ecc-buffer-current-buffer buf)
        
        ;; Run hook if struct exists
        (when-let ((struct (ecc-buffer--get-by-buffer buf)))
          (setf (ecc-buffer-last-used-time struct) (current-time))
          (ecc-buffer--run-hooks 'select struct))
        
        ;; Display the buffer
        (switch-to-buffer buf)
        
        buf))))

;;;###autoload
(defun ecc-buffer-get-registered-buffers ()
  "Get list of all registered Claude buffers."
  (mapcar #'car ecc-buffer-registered-buffers-alist))

;;;###autoload
(defun ecc-buffer-set-state (buffer state)
  "Set the state of BUFFER to STATE."
  (when buffer
    (let ((buf (if (bufferp buffer) buffer (get-buffer buffer))))
      (when (and buf (buffer-live-p buf))
        ;; Update in alist
        (let ((entry (assoc buf ecc-buffer-registered-buffers-alist)))
          (when entry
            (setcdr entry state)))
        
        ;; Update in struct registry
        (when-let ((struct (ecc-buffer--get-by-buffer buf)))
          (setf (ecc-buffer-state struct) state)
          (ecc-buffer--run-hooks 'update struct))
        
        state))))

;;;###autoload
(defun ecc-buffer-get-state (buffer)
  "Get the state of BUFFER."
  (when buffer
    (let ((buf (if (bufferp buffer) buffer (get-buffer buffer))))
      (when (and buf (buffer-live-p buf))
        (or 
         ;; Try struct registry first
         (when-let ((struct (ecc-buffer--get-by-buffer buf)))
           (ecc-buffer-state struct))
         
         ;; Fall back to alist
         (cdr (assoc buf ecc-buffer-registered-buffers-alist)))))))

;;;###autoload
(defun ecc-buffer-set-metadata (buffer key value)
  "Set metadata KEY to VALUE for BUFFER."
  (when buffer
    (let ((buf (if (bufferp buffer) buffer (get-buffer buffer))))
      (when (and buf (buffer-live-p buf))
        (when-let ((struct (ecc-buffer--get-by-buffer buf)))
          (puthash key value (ecc-buffer-metadata struct))
          (ecc-buffer--run-hooks 'update struct)
          value)))))

;;;###autoload
(defun ecc-buffer-get-metadata (buffer key)
  "Get metadata value for KEY from BUFFER."
  (when buffer
    (let ((buf (if (bufferp buffer) buffer (get-buffer buffer))))
      (when (and buf (buffer-live-p buf))
        (when-let ((struct (ecc-buffer--get-by-buffer buf)))
          (gethash key (ecc-buffer-metadata struct)))))))

;;;###autoload
(defun ecc-buffer-cleanup ()
  "Clean up buffer registry by removing entries for killed buffers."
  (interactive)
  (let ((removed 0))
    ;; Check legacy alist
    (let ((cleaned-list nil))
      (dolist (entry ecc-buffer-registered-buffers-alist)
        (if (buffer-live-p (car entry))
            (push entry cleaned-list)
          (cl-incf removed)))
      (setq ecc-buffer-registered-buffers-alist (nreverse cleaned-list)))
    
    ;; Check struct registry
    (let ((to-remove nil))
      (maphash (lambda (id struct)
                 (unless (buffer-live-p (ecc-buffer-buffer struct))
                   (push id to-remove)
                   (cl-incf removed)))
               ecc-buffer--registry)
      
      ;; Remove from registry
      (dolist (id to-remove)
        (let ((struct (ecc-buffer--get-by-id id)))
          (ecc-buffer--run-hooks 'kill struct)
          (remhash id ecc-buffer--registry))))
    
    ;; Update current buffer if needed
    (when (or (null ecc-buffer-current-buffer)
              (not (buffer-live-p ecc-buffer-current-buffer)))
      (setq ecc-buffer-current-buffer nil)
      (ecc-buffer--set-default-current))
    
    removed))

;; Hook management
;; ------------------------------

;;;###autoload
(defun ecc-buffer-add-hook (event-type function)
  "Add FUNCTION to run when EVENT-TYPE occurs.
EVENT-TYPE should be one of: create, select, rename, update, kill."
  (unless (assoc event-type ecc-buffer--hooks)
    (push (cons event-type nil) ecc-buffer--hooks))
  (let ((hooks (assoc event-type ecc-buffer--hooks)))
    (unless (member function (cdr hooks))
      (setcdr hooks (cons function (cdr hooks))))))

;;;###autoload
(defun ecc-buffer-remove-hook (event-type function)
  "Remove FUNCTION from EVENT-TYPE hooks."
  (let ((hooks (assoc event-type ecc-buffer--hooks)))
    (when hooks
      (setcdr hooks (delete function (cdr hooks))))))

;; Utility functions
;; ------------------------------

;;;###autoload
(defun ecc-buffer-register-vterm (buffer)
  "Register BUFFER for Claude integration if it's a vterm buffer."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (when (derived-mode-p 'vterm-mode)
        (ecc-buffer-register buffer)
        t))))

;; Initialize the buffer registry
(ecc-buffer-registry-init)

(provide 'ecc-buffer-registry-unified)

(when
    (not load-file-name)
  (message "ecc-buffer-registry-unified.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))