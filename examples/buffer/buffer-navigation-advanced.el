;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-12 07:50:30>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/examples/buffer/buffer-navigation-advanced.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; Commentary:
;; Advanced buffer navigation examples for emacs-claude-code.
;; This example demonstrates how to:
;; 1. Navigate between buffers with custom criteria
;; 2. Create buffer groups and categories
;; 3. Implement custom navigation flows
;; 4. Build interactive buffer selection interfaces

;;; Code:

(require 'emacs-claude-code)
(require 'ecc-buffer)
(require 'ecc-buffer-navigation)
(require 'ecc-buffer-current)
(require 'ecc-buffer-registry)

;; Navigate buffers by timestamp
(defun example-navigate-buffers-by-timestamp (direction)
  "Navigate to the next Claude buffer in specified DIRECTION based on timestamp.
DIRECTION can be 'newest, 'oldest, 'next-newer, or 'next-older."
  (interactive
   (list (intern (completing-read "Direction: "
                                 '("newest" "oldest" "next-newer" "next-older")
                                 nil t))))
  
  ;; Get all registered buffers
  (let* ((all-buffers (ecc-buffer-get-registered-buffers))
         (live-buffers (seq-filter #'buffer-live-p all-buffers))
         (current-buffer (ecc-buffer-current-get-buffer))
         (sorted-buffers nil)
         (target-buffer nil))
    
    ;; Sort buffers by timestamp
    (setq sorted-buffers
          (sort live-buffers
                (lambda (a b)
                  (time-less-p (ecc-buffer-get-timestamp a)
                              (ecc-buffer-get-timestamp b)))))
    
    ;; Find the target buffer based on direction
    (cond
     ((eq direction 'newest)
      (setq target-buffer (car (last sorted-buffers))))
     
     ((eq direction 'oldest)
      (setq target-buffer (car sorted-buffers)))
     
     ((eq direction 'next-newer)
      (when current-buffer
        (let ((current-pos (seq-position sorted-buffers current-buffer)))
          (when current-pos
            (setq target-buffer (nth (min (1+ current-pos)
                                         (1- (length sorted-buffers)))
                                    sorted-buffers))))))
     
     ((eq direction 'next-older)
      (when current-buffer
        (let ((current-pos (seq-position sorted-buffers current-buffer)))
          (when current-pos
            (setq target-buffer (nth (max 0 (1- current-pos))
                                    sorted-buffers)))))))
    
    ;; Switch to the target buffer
    (if target-buffer
        (progn
          (ecc-buffer-set-current-buffer target-buffer)
          (switch-to-buffer target-buffer)
          (message "Switched to %s buffer: %s"
                   (symbol-name direction)
                   (buffer-name target-buffer)))
      
      (message "No suitable buffer found in direction: %s"
               (symbol-name direction)))))

;; Group buffers by category and navigate within groups
(defun example-buffer-categorize-and-navigate ()
  "Categorize Claude buffers and navigate within categories."
  (interactive)
  
  ;; Define categories based on buffer name patterns
  (let* ((categories '(("Project" . "PROJ-")
                      ("Question" . "Q-")
                      ("Coding" . "CODE-")
                      ("Documentation" . "DOC-")
                      ("General" . "")))
         
         ;; Get all registered buffers
         (all-buffers (ecc-buffer-get-registered-buffers))
         (live-buffers (seq-filter #'buffer-live-p all-buffers))
         
         ;; Group buffers by category
         (grouped-buffers (make-hash-table :test 'equal)))
    
    ;; Assign buffers to categories
    (dolist (buf live-buffers)
      (let ((buf-name (buffer-name buf))
            (category-found nil))
        
        ;; Find matching category
        (catch 'found
          (dolist (category categories)
            (when (and (not (string-empty-p (cdr category)))
                      (string-match-p (cdr category) buf-name))
              (setq category-found (car category))
              (throw 'found t)))
          
          ;; Default to "General" if no match
          (setq category-found "General"))
        
        ;; Add buffer to its category
        (let ((existing (gethash category-found grouped-buffers '())))
          (puthash category-found (cons buf existing) grouped-buffers))))
    
    ;; Create a selection buffer
    (with-current-buffer (get-buffer-create "*Claude Buffer Categories*")
      (erase-buffer)
      (insert "Claude Buffer Categories\n")
      (insert "=======================\n\n")
      
      ;; Display buffers by category
      (maphash (lambda (category buffers)
                (when buffers
                  (insert (format "## %s (%d buffers)\n\n" 
                                 category (length buffers)))
                  
                  (dolist (buf buffers)
                    (let ((name (buffer-name buf))
                          (state (ecc-buffer-get-state buf))
                          (time (format-time-string 
                                "%H:%M:%S" 
                                (ecc-buffer-get-timestamp buf))))
                      
                      (insert-button
                       (format "  %s [%s] %s\n" name state time)
                       'action (lambda (_) 
                                (switch-to-buffer buf)
                                (ecc-buffer-set-current-buffer buf))
                       'follow-link t
                       'help-echo (format "Switch to buffer %s" name))))
                  
                  (insert "\n")))
              grouped-buffers)
      
      ;; Add navigation controls
      (insert "\n")
      (insert "Navigate by category:\n\n")
      
      (dolist (category categories)
        (when (gethash (car category) grouped-buffers)
          (insert-button
           (format "[%s] " (car category))
           'action (lambda (_)
                    (example-navigate-within-category (car category)))
           'follow-link t
           'help-echo (format "Navigate within %s category" (car category)))))
      
      (special-mode)
      (switch-to-buffer (current-buffer))
      (goto-char (point-min)))))

;; Navigate within a specific category of buffers
(defun example-navigate-within-category (category)
  "Navigate through Claude buffers in a specific CATEGORY."
  (interactive
   (list (completing-read "Category: " 
                         '("Project" "Question" "Coding" "Documentation" "General")
                         nil t)))
  
  ;; Get all registered buffers
  (let* ((all-buffers (ecc-buffer-get-registered-buffers))
         (live-buffers (seq-filter #'buffer-live-p all-buffers))
         (category-pattern (cond
                           ((string= category "Project") "PROJ-")
                           ((string= category "Question") "Q-")
                           ((string= category "Coding") "CODE-")
                           ((string= category "Documentation") "DOC-")
                           (t "")))
         (category-buffers '()))
    
    ;; Find buffers matching the category
    (dolist (buf live-buffers)
      (let ((buf-name (buffer-name buf)))
        (if (string-empty-p category-pattern)
            ;; For "General" category, include buffers not matching other patterns
            (unless (or (string-match-p "PROJ-" buf-name)
                       (string-match-p "Q-" buf-name)
                       (string-match-p "CODE-" buf-name)
                       (string-match-p "DOC-" buf-name))
              (push buf category-buffers))
          
          ;; For named categories, match the pattern
          (when (string-match-p category-pattern buf-name)
            (push buf category-buffers)))))
    
    ;; Check if any buffers found
    (if (null category-buffers)
        (message "No buffers found in category: %s" category)
      
      ;; Create a selection buffer
      (with-current-buffer (get-buffer-create (format "*Claude %s Buffers*" category))
        (erase-buffer)
        (insert (format "Claude %s Buffers\n" category))
        (insert (make-string (+ 8 (length category)) ?=) "\n\n")
        
        ;; Display buffers
        (dolist (buf category-buffers)
          (let ((name (buffer-name buf))
                (state (ecc-buffer-get-state buf))
                (time (format-time-string "%Y-%m-%d %H:%M:%S" 
                                         (ecc-buffer-get-timestamp buf))))
            
            (insert-button
             (format "[%s] %s (%s)\n" state name time)
             'action (lambda (_) 
                      (switch-to-buffer buf)
                      (ecc-buffer-set-current-buffer buf))
             'follow-link t
             'help-echo (format "Switch to buffer %s" name))))
        
        ;; Add navigation controls
        (insert "\n")
        (insert-button 
         "[Next in Category]"
         'action (lambda (_)
                  (example-next-buffer-in-category category))
         'follow-link t
         'help-echo "Navigate to next buffer in this category")
        
        (insert "  ")
        
        (insert-button 
         "[Previous in Category]"
         'action (lambda (_)
                  (example-prev-buffer-in-category category))
         'follow-link t
         'help-echo "Navigate to previous buffer in this category")
        
        (insert "  ")
        
        (insert-button 
         "[All Categories]"
         'action (lambda (_)
                  (example-buffer-categorize-and-navigate))
         'follow-link t
         'help-echo "Return to all categories view")
        
        (special-mode)
        (switch-to-buffer (current-buffer))
        (goto-char (point-min))))))

;; Navigate to next buffer in category
(defun example-next-buffer-in-category (category)
  "Navigate to the next Claude buffer in CATEGORY."
  (interactive
   (list (completing-read "Category: " 
                         '("Project" "Question" "Coding" "Documentation" "General")
                         nil t)))
  
  ;; Get all registered buffers
  (let* ((all-buffers (ecc-buffer-get-registered-buffers))
         (live-buffers (seq-filter #'buffer-live-p all-buffers))
         (category-pattern (cond
                           ((string= category "Project") "PROJ-")
                           ((string= category "Question") "Q-")
                           ((string= category "Coding") "CODE-")
                           ((string= category "Documentation") "DOC-")
                           (t "")))
         (category-buffers '())
         (current-buffer (ecc-buffer-current-get-buffer)))
    
    ;; Find buffers matching the category
    (dolist (buf live-buffers)
      (let ((buf-name (buffer-name buf)))
        (if (string-empty-p category-pattern)
            ;; For "General" category
            (unless (or (string-match-p "PROJ-" buf-name)
                       (string-match-p "Q-" buf-name)
                       (string-match-p "CODE-" buf-name)
                       (string-match-p "DOC-" buf-name))
              (push buf category-buffers))
          ;; For named categories
          (when (string-match-p category-pattern buf-name)
            (push buf category-buffers)))))
    
    ;; Sort by timestamp
    (setq category-buffers
          (sort category-buffers
                (lambda (a b)
                  (time-less-p (ecc-buffer-get-timestamp a)
                              (ecc-buffer-get-timestamp b)))))
    
    ;; Find next buffer in cycle
    (if (null category-buffers)
        (message "No buffers found in category: %s" category)
      
      (let ((next-buffer nil))
        ;; Find current buffer position
        (if (null current-buffer)
            ;; If no current buffer, start from the first one
            (setq next-buffer (car category-buffers))
          
          (let ((pos (seq-position category-buffers current-buffer)))
            (if (null pos)
                ;; Current buffer not in category, start from the first one
                (setq next-buffer (car category-buffers))
              
              ;; Move to next buffer, wrapping around if needed
              (setq next-buffer 
                    (nth (mod (1+ pos) (length category-buffers))
                        category-buffers)))))
        
        ;; Switch to the next buffer
        (when next-buffer
          (ecc-buffer-set-current-buffer next-buffer)
          (switch-to-buffer next-buffer)
          (message "Switched to next buffer in %s category: %s"
                   category (buffer-name next-buffer)))))))

;; Navigate to previous buffer in category
(defun example-prev-buffer-in-category (category)
  "Navigate to the previous Claude buffer in CATEGORY."
  (interactive
   (list (completing-read "Category: " 
                         '("Project" "Question" "Coding" "Documentation" "General")
                         nil t)))
  
  ;; Get all registered buffers
  (let* ((all-buffers (ecc-buffer-get-registered-buffers))
         (live-buffers (seq-filter #'buffer-live-p all-buffers))
         (category-pattern (cond
                           ((string= category "Project") "PROJ-")
                           ((string= category "Question") "Q-")
                           ((string= category "Coding") "CODE-")
                           ((string= category "Documentation") "DOC-")
                           (t "")))
         (category-buffers '())
         (current-buffer (ecc-buffer-current-get-buffer)))
    
    ;; Find buffers matching the category
    (dolist (buf live-buffers)
      (let ((buf-name (buffer-name buf)))
        (if (string-empty-p category-pattern)
            ;; For "General" category
            (unless (or (string-match-p "PROJ-" buf-name)
                       (string-match-p "Q-" buf-name)
                       (string-match-p "CODE-" buf-name)
                       (string-match-p "DOC-" buf-name))
              (push buf category-buffers))
          ;; For named categories
          (when (string-match-p category-pattern buf-name)
            (push buf category-buffers)))))
    
    ;; Sort by timestamp
    (setq category-buffers
          (sort category-buffers
                (lambda (a b)
                  (time-less-p (ecc-buffer-get-timestamp a)
                              (ecc-buffer-get-timestamp b)))))
    
    ;; Find previous buffer in cycle
    (if (null category-buffers)
        (message "No buffers found in category: %s" category)
      
      (let ((prev-buffer nil))
        ;; Find current buffer position
        (if (null current-buffer)
            ;; If no current buffer, start from the last one
            (setq prev-buffer (car (last category-buffers)))
          
          (let ((pos (seq-position category-buffers current-buffer)))
            (if (null pos)
                ;; Current buffer not in category, start from the last one
                (setq prev-buffer (car (last category-buffers)))
              
              ;; Move to previous buffer, wrapping around if needed
              (setq prev-buffer 
                    (nth (mod (1- pos) (length category-buffers))
                        category-buffers)))))
        
        ;; Switch to the previous buffer
        (when prev-buffer
          (ecc-buffer-set-current-buffer prev-buffer)
          (switch-to-buffer prev-buffer)
          (message "Switched to previous buffer in %s category: %s"
                   category (buffer-name prev-buffer)))))))

;; Create an interactive buffer selector
(defun example-interactive-buffer-selector ()
  "Create an interactive buffer selector for Claude buffers."
  (interactive)
  
  ;; Get all registered buffers
  (let* ((all-buffers (ecc-buffer-get-registered-buffers))
         (live-buffers (seq-filter #'buffer-live-p all-buffers)))
    
    ;; Create a selector buffer
    (with-current-buffer (get-buffer-create "*Claude Buffer Selector*")
      (erase-buffer)
      (insert "Claude Buffer Selector\n")
      (insert "=====================\n\n")
      
      ;; Add filtering options
      (insert "## Filter by:\n\n")
      
      (insert-button 
       "[All]"
       'action (lambda (_)
                (example-interactive-buffer-selector))
       'follow-link t
       'help-echo "Show all buffers")
      
      (insert " ")
      
      (insert-button 
       "[By Category]"
       'action (lambda (_)
                (example-buffer-categorize-and-navigate))
       'follow-link t
       'help-echo "Filter buffers by category")
      
      (insert " ")
      
      (insert-button 
       "[By Time]"
       'action (lambda (_)
                (example-filter-buffers-by-time))
       'follow-link t
       'help-echo "Filter buffers by time")
      
      (insert " ")
      
      (insert-button 
       "[By State]"
       'action (lambda (_)
                (example-filter-buffers-by-state))
       'follow-link t
       'help-echo "Filter buffers by state")
      
      (insert "\n\n")
      
      ;; Sort options
      (insert "## Sort by:\n\n")
      
      (insert-button 
       "[Name]"
       'action (lambda (_)
                (example-sort-buffers-by 'name))
       'follow-link t
       'help-echo "Sort buffers by name")
      
      (insert " ")
      
      (insert-button 
       "[Newest]"
       'action (lambda (_)
                (example-sort-buffers-by 'newest))
       'follow-link t
       'help-echo "Sort buffers by newest first")
      
      (insert " ")
      
      (insert-button 
       "[Oldest]"
       'action (lambda (_)
                (example-sort-buffers-by 'oldest))
       'follow-link t
       'help-echo "Sort buffers by oldest first")
      
      (insert " ")
      
      (insert-button 
       "[State]"
       'action (lambda (_)
                (example-sort-buffers-by 'state))
       'follow-link t
       'help-echo "Sort buffers by state")
      
      (insert "\n\n")
      
      ;; Display all buffers
      (insert "## Available Buffers:\n\n")
      
      (if (null live-buffers)
          (insert "No Claude buffers found.\n")
        
        ;; Display each buffer
        (dolist (buf live-buffers)
          (let ((name (buffer-name buf))
                (state (ecc-buffer-get-state buf))
                (time (format-time-string "%H:%M:%S" 
                                         (ecc-buffer-get-timestamp buf))))
            
            (insert-button
             (format "[%s] %s (%s)\n" state name time)
             'action (lambda (_) 
                      (switch-to-buffer buf)
                      (ecc-buffer-set-current-buffer buf))
             'follow-link t
             'help-echo (format "Switch to buffer %s" name)))))
      
      ;; Buffer actions
      (insert "\n## Buffer Actions:\n\n")
      
      (insert-button 
       "[New Buffer]"
       'action (lambda (_)
                (let ((buf (ecc-buffer-current-get-or-create)))
                  (switch-to-buffer buf)
                  (example-interactive-buffer-selector)))
       'follow-link t
       'help-echo "Create a new Claude buffer")
      
      (insert " ")
      
      (insert-button 
       "[Kill Selected]"
       'action (lambda (_)
                (let ((buf (ecc-buffer-current-get-buffer)))
                  (when (and buf
                            (yes-or-no-p (format "Kill buffer %s? " (buffer-name buf))))
                    (kill-buffer buf)
                    (example-interactive-buffer-selector))))
       'follow-link t
       'help-echo "Kill the currently selected buffer")
      
      (insert " ")
      
      (insert-button 
       "[Refresh]"
       'action (lambda (_)
                (example-interactive-buffer-selector))
       'follow-link t
       'help-echo "Refresh the buffer list")
      
      (special-mode)
      (switch-to-buffer (current-buffer))
      (goto-char (point-min)))))

;; Filter buffers by time (helper for interactive selector)
(defun example-filter-buffers-by-time ()
  "Filter Claude buffers by time range."
  (interactive)
  
  ;; Time range options
  (let* ((options '(("Last hour" . 3600)
                   ("Last 8 hours" . 28800)
                   ("Today" . 86400)
                   ("Last 3 days" . 259200)
                   ("All time" . nil)))
         (choice (completing-read "Time range: " 
                                 (mapcar #'car options)
                                 nil t))
         (seconds (cdr (assoc choice options)))
         (now (current-time))
         (all-buffers (ecc-buffer-get-registered-buffers))
         (live-buffers (seq-filter #'buffer-live-p all-buffers))
         (filtered-buffers '()))
    
    ;; Apply time filter
    (if (null seconds)
        ;; "All time" - no filtering
        (setq filtered-buffers live-buffers)
      
      ;; Filter by time range
      (dolist (buf live-buffers)
        (let* ((timestamp (ecc-buffer-get-timestamp buf))
               (diff (float-time (time-subtract now timestamp))))
          (when (<= diff seconds)
            (push buf filtered-buffers)))))
    
    ;; Show results
    (with-current-buffer (get-buffer-create "*Claude Time-Filtered Buffers*")
      (erase-buffer)
      (insert (format "Claude Buffers - %s\n" choice))
      (insert "=======================\n\n")
      
      (if (null filtered-buffers)
          (insert "No buffers found in this time range.\n")
        
        ;; Display filtered buffers
        (dolist (buf filtered-buffers)
          (let ((name (buffer-name buf))
                (state (ecc-buffer-get-state buf))
                (time (format-time-string "%Y-%m-%d %H:%M:%S" 
                                         (ecc-buffer-get-timestamp buf))))
            
            (insert-button
             (format "[%s] %s (%s)\n" state name time)
             'action (lambda (_) 
                      (switch-to-buffer buf)
                      (ecc-buffer-set-current-buffer buf))
             'follow-link t
             'help-echo (format "Switch to buffer %s" name)))))
      
      ;; Add back button
      (insert "\n")
      (insert-button 
       "[Back to Selector]"
       'action (lambda (_)
                (example-interactive-buffer-selector))
       'follow-link t
       'help-echo "Return to buffer selector")
      
      (special-mode)
      (switch-to-buffer (current-buffer))
      (goto-char (point-min)))))

;; Filter buffers by state (helper for interactive selector)
(defun example-filter-buffers-by-state ()
  "Filter Claude buffers by state."
  (interactive)
  
  ;; Get all possible states
  (let* ((all-buffers (ecc-buffer-get-registered-buffers))
         (live-buffers (seq-filter #'buffer-live-p all-buffers))
         (states '())
         (state-buffers (make-hash-table :test 'eq)))
    
    ;; Collect all states
    (dolist (buf live-buffers)
      (let ((state (ecc-buffer-get-state buf)))
        (when state
          (unless (memq state states)
            (push state states))
          
          ;; Group buffers by state
          (let ((existing (gethash state state-buffers '())))
            (puthash state (cons buf existing) state-buffers)))))
    
    ;; If no states found, add nil state
    (when (null states)
      (push nil states))
    
    ;; Show state selection
    (with-current-buffer (get-buffer-create "*Claude State Filter*")
      (erase-buffer)
      (insert "Filter Claude Buffers by State\n")
      (insert "============================\n\n")
      
      ;; Display each state with its buffer count
      (dolist (state states)
        (let* ((state-name (if state (symbol-name state) "unknown"))
               (buffers (gethash state state-buffers '()))
               (count (length buffers)))
          
          (insert-button
           (format "[%s] (%d buffers)\n" state-name count)
           'action (lambda (_) 
                    (example-show-buffers-in-state state))
           'follow-link t
           'help-echo (format "Show buffers in %s state" state-name))))
      
      ;; Add back button
      (insert "\n")
      (insert-button 
       "[Back to Selector]"
       'action (lambda (_)
                (example-interactive-buffer-selector))
       'follow-link t
       'help-echo "Return to buffer selector")
      
      (special-mode)
      (switch-to-buffer (current-buffer))
      (goto-char (point-min)))))

;; Show buffers in a specific state
(defun example-show-buffers-in-state (state)
  "Show Claude buffers in specified STATE."
  (interactive
   (list (intern (completing-read "Buffer state: " 
                                 '("y/n" "y/y/n" "waiting" "initial-waiting" "running")
                                 nil t))))
  
  ;; Get all matching buffers
  (let* ((all-buffers (ecc-buffer-get-registered-buffers))
         (live-buffers (seq-filter #'buffer-live-p all-buffers))
         (state-buffers '()))
    
    ;; Find buffers in the specified state
    (dolist (buf live-buffers)
      (when (eq (ecc-buffer-get-state buf) state)
        (push buf state-buffers)))
    
    ;; Sort by timestamp (newest first)
    (setq state-buffers
          (sort state-buffers
                (lambda (a b)
                  (time-less-p (ecc-buffer-get-timestamp b)
                              (ecc-buffer-get-timestamp a)))))
    
    ;; Show results
    (with-current-buffer (get-buffer-create (format "*Claude Buffers in %s state*" 
                                                  (symbol-name state)))
      (erase-buffer)
      (insert (format "Claude Buffers in '%s' state\n" (symbol-name state)))
      (insert (make-string (+ 20 (length (symbol-name state))) ?=) "\n\n")
      
      (if (null state-buffers)
          (insert "No buffers found in this state.\n")
        
        ;; Display buffers
        (dolist (buf state-buffers)
          (let ((name (buffer-name buf))
                (time (format-time-string "%Y-%m-%d %H:%M:%S" 
                                         (ecc-buffer-get-timestamp buf))))
            
            (insert-button
             (format "%s (%s)\n" name time)
             'action (lambda (_) 
                      (switch-to-buffer buf)
                      (ecc-buffer-set-current-buffer buf))
             'follow-link t
             'help-echo (format "Switch to buffer %s" name)))))
      
      ;; Add back buttons
      (insert "\n")
      (insert-button 
       "[Back to States]"
       'action (lambda (_)
                (example-filter-buffers-by-state))
       'follow-link t
       'help-echo "Return to state filter")
      
      (insert " ")
      
      (insert-button 
       "[Back to Selector]"
       'action (lambda (_)
                (example-interactive-buffer-selector))
       'follow-link t
       'help-echo "Return to buffer selector")
      
      (special-mode)
      (switch-to-buffer (current-buffer))
      (goto-char (point-min)))))

;; Sort buffers by different criteria (helper for interactive selector)
(defun example-sort-buffers-by (criterion)
  "Sort Claude buffers by CRITERION.
CRITERION can be 'name, 'newest, 'oldest, or 'state."
  (interactive
   (list (intern (completing-read "Sort by: " 
                                 '("name" "newest" "oldest" "state")
                                 nil t))))
  
  ;; Get all buffers
  (let* ((all-buffers (ecc-buffer-get-registered-buffers))
         (live-buffers (seq-filter #'buffer-live-p all-buffers))
         (sorted-buffers '()))
    
    ;; Apply sorting
    (setq sorted-buffers
          (cond
           ;; Sort by name
           ((eq criterion 'name)
            (sort live-buffers
                  (lambda (a b)
                    (string< (buffer-name a) (buffer-name b)))))
           
           ;; Sort by newest first
           ((eq criterion 'newest)
            (sort live-buffers
                  (lambda (a b)
                    (time-less-p (ecc-buffer-get-timestamp b)
                                (ecc-buffer-get-timestamp a)))))
           
           ;; Sort by oldest first
           ((eq criterion 'oldest)
            (sort live-buffers
                  (lambda (a b)
                    (time-less-p (ecc-buffer-get-timestamp a)
                                (ecc-buffer-get-timestamp b)))))
           
           ;; Sort by state
           ((eq criterion 'state)
            (sort live-buffers
                  (lambda (a b)
                    (let ((state-a (ecc-buffer-get-state a))
                          (state-b (ecc-buffer-get-state b)))
                      (if (and state-a state-b)
                          (string< (symbol-name state-a) (symbol-name state-b))
                        (and state-a (not state-b)))))))
           
           ;; Default - no sorting
           (t live-buffers)))
    
    ;; Show results
    (with-current-buffer (get-buffer-create (format "*Claude Buffers - Sorted by %s*" 
                                                  (symbol-name criterion)))
      (erase-buffer)
      (insert (format "Claude Buffers - Sorted by %s\n" (symbol-name criterion)))
      (insert (make-string (+ 22 (length (symbol-name criterion))) ?=) "\n\n")
      
      (if (null sorted-buffers)
          (insert "No Claude buffers found.\n")
        
        ;; Display buffers
        (dolist (buf sorted-buffers)
          (let ((name (buffer-name buf))
                (state (ecc-buffer-get-state buf))
                (time (format-time-string "%H:%M:%S" 
                                         (ecc-buffer-get-timestamp buf))))
            
            (insert-button
             (format "[%s] %s (%s)\n" state name time)
             'action (lambda (_) 
                      (switch-to-buffer buf)
                      (ecc-buffer-set-current-buffer buf))
             'follow-link t
             'help-echo (format "Switch to buffer %s" name)))))
      
      ;; Add back button
      (insert "\n")
      (insert-button 
       "[Back to Selector]"
       'action (lambda (_)
                (example-interactive-buffer-selector))
       'follow-link t
       'help-echo "Return to buffer selector")
      
      (special-mode)
      (switch-to-buffer (current-buffer))
      (goto-char (point-min)))))

(provide 'example-buffer-navigation-advanced)

(when (not load-file-name)
  (message "buffer-navigation-advanced.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

;;; buffer-navigation-advanced.el ends here