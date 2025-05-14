;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-13 11:21:12>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-buffer/ecc-buffer-manager.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'cl-lib)
(require 'seq)
(require 'ecc-compat)
(require 'ecc-buffer-variables)

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

(defvar ecc-buffer-manager--registry (make-hash-table :test 'equal)
  "Registry of all Claude buffers, keyed by buffer ID.")

(defvar ecc-buffer-manager--current-buffer nil
  "The current active Claude buffer.")

(defvar ecc-buffer-manager--buffer-count 0
  "Counter for generating unique buffer IDs.")

(defvar ecc-buffer-manager--hooks nil
  "Alist of hooks to run when buffer events occur.
Each element is (EVENT-TYPE . HOOK-FUNCTION-LIST) where EVENT-TYPE
is one of: create, select, rename, update, kill.")

;; Core buffer management functions
;; ------------------------------

(defun ecc-buffer-manager-init ()
  "Initialize the buffer manager."
  (setq ecc-buffer-manager--registry (make-hash-table :test 'equal))
  (setq ecc-buffer-manager--current-buffer nil)
  (setq ecc-buffer-manager--buffer-count 0)
  (setq ecc-buffer-manager--hooks nil))

(defun ecc-buffer-manager-create (name &optional buffer)
  "Create a new Claude buffer with NAME.
If BUFFER is provided, use that as the actual buffer.
Otherwise, create a new buffer with NAME.
Returns the created ecc-buffer structure."
  ;; Generate a unique ID
  (let*
      ((id
        (format "claude-%d" (cl-incf ecc-buffer-manager--buffer-count)))
       (buf (or buffer (generate-new-buffer name)))
       (claude-buffer (ecc-buffer--create :id id
                                          :name name
                                          :buffer buf
                                          :state 'idle)))

    ;; Store in registry
    (puthash id claude-buffer ecc-buffer-manager--registry)

    ;; Run hooks
    (ecc-buffer-manager--run-hooks 'create claude-buffer)

    ;; Make current if no current buffer
    (unless ecc-buffer-manager--current-buffer
      (ecc-buffer-manager-set-current claude-buffer))

    claude-buffer))

(defun ecc-buffer-manager-get-by-id (id)
  "Get the Claude buffer with ID."
  (gethash id ecc-buffer-manager--registry))

(defun ecc-buffer-manager-get-by-buffer (buffer)
  "Get the Claude buffer structure for BUFFER.
BUFFER can be a buffer object or buffer name."
  (let ((buf (if (bufferp buffer) buffer (get-buffer buffer))))
    (seq-find (lambda (cb)
                (eq (ecc-buffer-buffer cb) buf))
              (hash-table-values ecc-buffer-manager--registry))))

(defun ecc-buffer-manager-get-all ()
  "Get all Claude buffers as a list."
  (hash-table-values ecc-buffer-manager--registry))

(defun ecc-buffer-manager-get-current ()
  "Get the current Claude buffer."
  ecc-buffer-manager--current-buffer)

(defun ecc-buffer-manager-set-current (claude-buffer)
  "Set CLAUDE-BUFFER as the current buffer.
CLAUDE-BUFFER can be a buffer object, buffer name, or Claude buffer structure."
  (let ((cb (cond
             ((ecc-buffer-p claude-buffer) claude-buffer)
             (t (ecc-buffer-manager-get-by-buffer claude-buffer)))))
    (when cb
      ;; Update last used time
      (setf (ecc-buffer-last-used-time cb) (current-time))

      ;; Set as current
      (setq ecc-buffer-manager--current-buffer cb)

      ;; Run hooks
      (ecc-buffer-manager--run-hooks 'select cb)

      ;; Display the buffer
      (let ((buf (ecc-buffer-buffer cb)))
        (when (buffer-live-p buf)
          (switch-to-buffer buf)))

      cb)))

(defun ecc-buffer-manager-set-state (claude-buffer state)
  "Set the state of CLAUDE-BUFFER to STATE.
CLAUDE-BUFFER can be a buffer object, buffer name, or Claude buffer structure."
  (let ((cb (cond
             ((ecc-buffer-p claude-buffer) claude-buffer)
             (t (ecc-buffer-manager-get-by-buffer claude-buffer)))))
    (when cb
      (setf (ecc-buffer-state cb) state)
      (ecc-buffer-manager--run-hooks 'update cb)
      state)))

(defun ecc-buffer-manager-get-state (claude-buffer)
  "Get the state of CLAUDE-BUFFER.
CLAUDE-BUFFER can be a buffer object, buffer name, or Claude buffer structure."
  (let ((cb (cond
             ((ecc-buffer-p claude-buffer) claude-buffer)
             (t (ecc-buffer-manager-get-by-buffer claude-buffer)))))
    (when cb
      (ecc-buffer-state cb))))

(defun ecc-buffer-manager-set-metadata (claude-buffer key value)
  "Set metadata KEY to VALUE for CLAUDE-BUFFER.
CLAUDE-BUFFER can be a buffer object, buffer name, or Claude buffer structure."
  (let ((cb (cond
             ((ecc-buffer-p claude-buffer) claude-buffer)
             (t (ecc-buffer-manager-get-by-buffer claude-buffer)))))
    (when cb
      (puthash key value (ecc-buffer-metadata cb))
      (ecc-buffer-manager--run-hooks 'update cb)
      value)))

(defun ecc-buffer-manager-get-metadata (claude-buffer key)
  "Get metadata value for KEY from CLAUDE-BUFFER.
CLAUDE-BUFFER can be a buffer object, buffer name, or Claude buffer structure."
  (let ((cb (cond
             ((ecc-buffer-p claude-buffer) claude-buffer)
             (t (ecc-buffer-manager-get-by-buffer claude-buffer)))))
    (when cb
      (gethash key (ecc-buffer-metadata cb)))))

(defun ecc-buffer-manager-rename (claude-buffer new-name)
  "Rename CLAUDE-BUFFER to NEW-NAME.
CLAUDE-BUFFER can be a buffer object, buffer name, or Claude buffer structure.
Updates both the structure and the actual buffer."
  (let ((cb (cond
             ((ecc-buffer-p claude-buffer) claude-buffer)
             (t (ecc-buffer-manager-get-by-buffer claude-buffer)))))
    (when cb
      (setf (ecc-buffer-name cb) new-name)
      (let ((buf (ecc-buffer-buffer cb)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (rename-buffer new-name t))))
      (ecc-buffer-manager--run-hooks 'rename cb)
      cb)))

(defun ecc-buffer-manager-kill (claude-buffer)
  "Kill CLAUDE-BUFFER.
CLAUDE-BUFFER can be a buffer object, buffer name, or Claude buffer structure.
Removes from registry and kills the actual buffer."
  (let ((cb (cond
             ((ecc-buffer-p claude-buffer) claude-buffer)
             (t (ecc-buffer-manager-get-by-buffer claude-buffer)))))
    (when cb
      ;; Run hooks before removing
      (ecc-buffer-manager--run-hooks 'kill cb)

      ;; Remove from registry
      (remhash (ecc-buffer-id cb) ecc-buffer-manager--registry)

      ;; Update current buffer if needed
      (when (eq ecc-buffer-manager--current-buffer cb)
        (setq ecc-buffer-manager--current-buffer nil)
        (let ((next-buffer (car (ecc-buffer-manager-get-all))))
          (when next-buffer
            (ecc-buffer-manager-set-current next-buffer))))

      ;; Kill the actual buffer
      (let ((buf (ecc-buffer-buffer cb)))
        (when (buffer-live-p buf)
          (kill-buffer buf)))

      t)))

(defun ecc-buffer-manager-cleanup ()
  "Clean up the buffer registry by removing entries for killed buffers."
  (let ((to-remove nil))
    ;; Find killed buffers
    (maphash (lambda (id cb)
               (let ((buf (ecc-buffer-buffer cb)))
                 (unless (buffer-live-p buf)
                   (push id to-remove))))
             ecc-buffer-manager--registry)

    ;; Remove from registry
    (dolist (id to-remove)
      (let ((cb (ecc-buffer-manager-get-by-id id)))
        (ecc-buffer-manager--run-hooks 'kill cb)
        (remhash id ecc-buffer-manager--registry)))

    ;; Update current buffer if needed
    (when (and ecc-buffer-manager--current-buffer
               (not
                (buffer-live-p
                 (ecc-buffer-buffer ecc-buffer-manager--current-buffer))))
      (setq ecc-buffer-manager--current-buffer nil)
      (let ((next-buffer (car (ecc-buffer-manager-get-all))))
        (when next-buffer
          (ecc-buffer-manager-set-current next-buffer))))

    (length to-remove)))

;; ;; Navigation functions
;; ;; ------------------------------

;; (defun ecc-buffer-manager-next ()
;;   "Switch to the next Claude buffer in the list.
;; Buffers are sorted by last used time, most recently used first."
;;   (interactive)
;;   (let* ((all-buffers (ecc-buffer-manager-get-all))
;;          (live-buffers (seq-filter (lambda (cb)
;;                                      (buffer-live-p
;;                                       (ecc-buffer-buffer cb)))
;;                                    all-buffers))
;;          (sorted-buffers (seq-sort-by (lambda (cb)
;;                                         (float-time
;;                                          (ecc-buffer-last-used-time cb)))
;;                                       #'>
;;                                       live-buffers))
;;          (current (ecc-buffer-manager-get-current))
;;          next-buffer)

;;     (if (null sorted-buffers)
;;         (message "No Claude buffers available")

;;       (if (null current)
;;           ;; If no current buffer, use the first one
;;           (setq next-buffer (car sorted-buffers))

;;         ;; Find the current buffer's position
;;         (let* ((pos (seq-position sorted-buffers current))
;;                (next-pos (if
;;                              (or (null pos)
;;                                  (= pos (1- (length sorted-buffers))))
;;                              0
;;                                         ; wrap around
;;                            (1+ pos))))
;;           (setq next-buffer (nth next-pos sorted-buffers))))

;;       (ecc-buffer-manager-set-current next-buffer))))

;; (defun ecc-buffer-manager-previous ()
;;   "Switch to the previous Claude buffer in the list.
;; Buffers are sorted by last used time, most recently used first."
;;   (interactive)
;;   (let* ((all-buffers (ecc-buffer-manager-get-all))
;;          (live-buffers (seq-filter (lambda (cb)
;;                                      (buffer-live-p
;;                                       (ecc-buffer-buffer cb)))
;;                                    all-buffers))
;;          (sorted-buffers (seq-sort-by (lambda (cb)
;;                                         (float-time
;;                                          (ecc-buffer-last-used-time cb)))
;;                                       #'>
;;                                       live-buffers))
;;          (current (ecc-buffer-manager-get-current))
;;          prev-buffer)

;;     (if (null sorted-buffers)
;;         (message "No Claude buffers available")

;;       (if (null current)
;;           ;; If no current buffer, use the last one
;;           (setq prev-buffer (car (last sorted-buffers)))

;;         ;; Find the current buffer's position
;;         (let* ((pos (seq-position sorted-buffers current))
;;                (prev-pos (if (or (null pos) (= pos 0))
;;                              (1- (length sorted-buffers))
;;                                         ; wrap around
;;                            (1- pos))))
;;           (setq prev-buffer (nth prev-pos sorted-buffers))))

;;       (ecc-buffer-manager-set-current prev-buffer))))

;; Hook management
;; ------------------------------

(defun ecc-buffer-manager-add-hook (event-type function)
  "Add FUNCTION to run when EVENT-TYPE occurs.
EVENT-TYPE should be one of: create, select, rename, update, kill."
  (unless (assoc event-type ecc-buffer-manager--hooks)
    (push (cons event-type nil) ecc-buffer-manager--hooks))
  (let ((hooks (assoc event-type ecc-buffer-manager--hooks)))
    (unless (member function (cdr hooks))
      (setcdr hooks (cons function (cdr hooks))))))

(defun ecc-buffer-manager-remove-hook (event-type function)
  "Remove FUNCTION from EVENT-TYPE hooks."
  (let ((hooks (assoc event-type ecc-buffer-manager--hooks)))
    (when hooks
      (setcdr hooks (delete function (cdr hooks))))))

(defun ecc-buffer-manager--run-hooks (event-type claude-buffer)
  "Run all hooks for EVENT-TYPE with CLAUDE-BUFFER."
  (let ((hooks (cdr (assoc event-type ecc-buffer-manager--hooks))))
    (dolist (hook hooks)
      (condition-case err
          (funcall hook claude-buffer)
        (error (message "Error in buffer hook: %S" err))))))

;; Initialize the buffer manager
(ecc-buffer-manager-init)

;;; ecc-buffer-manager.el ends here


(provide 'ecc-buffer-manager)

(when
    (not load-file-name)
  (message "ecc-buffer-manager.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))