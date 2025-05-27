<!-- ---
!-- Timestamp: 2025-05-28 04:55:17
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/FROM_CLAUDE.md
!-- --- -->

#### Structure
**Recommended Structure:**

```
emacs-claude-code/
├── ecc.el                    # Main entry point
├── --ecc-debug.el             # Debug utilities
├── --ecc-state-detection.el   # State detection
├── --ecc-auto-response.el     # Auto-response system
├── --ecc-notification.el      # Notifications
├── --ecc-vterm-utils.el       # VTerm utilities & yank-as-file
└── --ecc-vterm-mode.el        # VTerm mode enhancement
```

**Reasoning:**

1. **7 core files** - essential functionality only
2. **Flat structure** - no subdirectories
3. **Single responsibility** - each file has one clear purpose
4. **Consolidated features** - related functionality grouped together

**Remove entirely:**
- `/--ecc-vterm/` - merge into main files
- `/--ecc-utils/` - just use --ecc-debug.el
- `/--ecc-state/` - use single --ecc-state-detection.el
- `/--ecc-buffer-registry/` - integrate into --ecc-auto-response.el
- `/--ecc-auto-*/` - all subdirectories, consolidate features
- All complex wrapper/abstraction layers
- Performance modules (premature optimization)
- Buffer management complexity
- Interaction tracking
- Visual aids and themes

**Keep consolidated:**
- Debug functions in --ecc-debug.el
- State detection patterns in one file
- Auto-response with built-in buffer registry
- Simple notifications
- VTerm utilities including file yank
- Single mode file for VTerm enhancement

This reduces from 30+ files to 7 focused modules.


---


| CLAUDE-SONNET-4-20250514

**--ecc-variables.el**
```elisp
;;; -*- coding: utf-8; lexical-binding: t -*-

(defgroup ecc nil
  "Emacs Claude Code package."
  :prefix "--ecc-"
  :group 'tools)

(defconst --ecc-version "1.0.0"
  "Current version of the emacs-claude-code package.")

(provide '--ecc-variables)
```

**--ecc-debug.el**
```elisp
;;; -*- coding: utf-8; lexical-binding: t -*-

(defcustom --ecc-debug-enabled nil
  "Whether debugging is globally enabled."
  :type 'boolean
  :group 'ecc)

(defvar --ecc-debug-prefix "[ECC] "
  "Prefix for debug messages.")

(defun --ecc-debug-toggle ()
  "Toggle global debug output."
  (interactive)
  (setq --ecc-debug-enabled (not --ecc-debug-enabled))
  (message "ECC debug %s" (if --ecc-debug-enabled "enabled" "disabled")))

(defun --ecc-debug-message (format-string &rest args)
  "Output a debug message if debugging is enabled."
  (when --ecc-debug-enabled
    (apply #'message (concat --ecc-debug-prefix format-string) args)))

(provide '--ecc-debug)
```

**--ecc-state-detection.el**
```elisp
;;; -*- coding: utf-8; lexical-binding: t -*-

(require 'ecc-debug)

(defcustom --ecc-state-detection-buffer-size 2000
  "Number of characters to check from end of buffer for prompt detection."
  :type 'integer
  :group 'ecc)

(defcustom --ecc-state-detection-patterns
  '((:initial-waiting . "│ > Try ")
    (:waiting . "│ >                            ")
    (:y/n . "❯ 1. Yes")
    (:y/y/n . " 2. Yes, and"))
  "Alist mapping state symbols to detection patterns."
  :type '(alist :key-type symbol :value-type string)
  :group 'ecc)

(defun --ecc-state-detection-detect (&optional buffer)
  "Detect Claude prompt state in BUFFER or current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (--ecc-debug-message "Detecting state in buffer: %s" (buffer-name))
    (let ((buffer-text (buffer-substring-no-properties
                        (max (- (point-max) --ecc-state-detection-buffer-size)
                             (point-min))
                        (point-max))))
      (--ecc-state-detection--analyze-text buffer-text))))

(defun --ecc-state-detection--analyze-text (text)
  "Analyze TEXT to detect Claude prompt state."
  (catch 'found
    (dolist (pattern-pair --ecc-state-detection-patterns)
      (let ((state (car pattern-pair))
            (pattern (cdr pattern-pair)))
        (when (string-match-p (regexp-quote pattern) text)
          (--ecc-debug-message "Matched state %s" state)
          (throw 'found state))))
    (cond
     ((string-match-p "\\[Y/y/n\\]" text) :y/y/n)
     ((string-match-p "\\[y/n\\]\\|\\[Y/n\\]" text) :y/n)
     ((string-match-p "continue>\\|Continue>" text) :waiting)
     (t nil))))

(defun --ecc-state-detection-get-name (state)
  "Convert STATE symbol to human-readable name."
  (cond
   ((eq state :y/y/n) "Y/Y/N")
   ((eq state :y/n) "Y/N")
   ((eq state :waiting) "Continue")
   ((eq state :initial-waiting) "Initial-Waiting")
   (t (format "%s" state))))

(provide '--ecc-state-detection)
```

**--ecc-buffer-registry.el**
```elisp
;;; -*- coding: utf-8; lexical-binding: t -*-

(require 'ecc-debug)

(defvar --ecc-buffer-registry--registered-buffers (make-hash-table :test 'eq)
  "Hash table of registered buffers for auto-response.")

(defun --ecc-buffer-registry-register (buffer)
  "Register BUFFER for auto-response."
  (interactive (list (current-buffer)))
  (unless (buffer-live-p buffer)
    (user-error "Buffer is not alive"))
  (puthash buffer t --ecc-buffer-registry--registered-buffers)
  (--ecc-debug-message "Registered buffer: %s" (buffer-name buffer))
  buffer)

(defun --ecc-buffer-registry-unregister (buffer)
  "Unregister BUFFER from auto-response."
  (interactive (list (current-buffer)))
  (remhash buffer --ecc-buffer-registry--registered-buffers)
  (--ecc-debug-message "Unregistered buffer: %s" (buffer-name buffer))
  buffer)

(defun --ecc-buffer-registry-get-registered ()
  "Get list of all registered buffers."
  (let ((buffers nil))
    (maphash (lambda (buffer _)
               (when (buffer-live-p buffer)
                 (push buffer buffers)))
             --ecc-buffer-registry--registered-buffers)
    buffers))

(defun --ecc-buffer-registry-cleanup ()
  "Remove dead buffers from registry."
  (let ((dead-buffers nil))
    (maphash (lambda (buffer _)
               (unless (buffer-live-p buffer)
                 (push buffer dead-buffers)))
             --ecc-buffer-registry--registered-buffers)
    (dolist (buffer dead-buffers)
      (remhash buffer --ecc-buffer-registry--registered-buffers))
    (when dead-buffers
      (--ecc-debug-message "Cleaned up %d dead buffers" (length dead-buffers)))))

(provide '--ecc-buffer-registry)
```

**--ecc-notification.el**
```elisp
;;; -*- coding: utf-8; lexical-binding: t -*-

(require 'ecc-debug)
(require 'ecc-state-detection)

(defcustom --ecc-notification-enabled t
  "Whether notifications are enabled globally."
  :type 'boolean
  :group 'ecc)

(defcustom --ecc-notification-methods '(bell flash message)
  "List of notification methods to use."
  :type '(set (const :tag "Audible bell" bell)
              (const :tag "Mode line flash" flash)
              (const :tag "Echo area message" message))
  :group 'ecc)

(defcustom --ecc-notification-throttle-duration 2.0
  "Minimum interval between notifications in seconds."
  :type 'number
  :group 'ecc)

(defvar --ecc-notification--last-time 0
  "Time of the last notification in float seconds.")

(defvar --ecc-notification--last-state nil
  "Last Claude state that triggered a notification.")

(defvar --ecc-notification--flash-timer nil
  "Timer for mode line flashing.")

(defun --ecc-notification-notify (state &optional buffer)
  "Notify about STATE using configured methods."
  (when (--ecc-notification--should-notify-p state)
    (when (memq 'bell --ecc-notification-methods)
      (--ecc-notification--ring-bell))
    (when (memq 'flash --ecc-notification-methods)
      (--ecc-notification--flash-mode-line buffer))
    (when (memq 'message --ecc-notification-methods)
      (--ecc-notification--display-message state buffer))
    (--ecc-notification--update-state state)))

(defun --ecc-notification--should-notify-p (state)
  "Check if we should notify about STATE now."
  (and --ecc-notification-enabled
       state
       (or (not (eq state --ecc-notification--last-state))
           (> (- (float-time) --ecc-notification--last-time)
              --ecc-notification-throttle-duration))))

(defun --ecc-notification--update-state (state)
  "Update tracking variables for STATE."
  (setq --ecc-notification--last-state state)
  (setq --ecc-notification--last-time (float-time)))

(defun --ecc-notification--ring-bell ()
  "Ring an audible bell."
  (let ((ring-bell-function nil))
    (ding t)))

(defun --ecc-notification--flash-mode-line (&optional buffer)
  "Flash the mode line in BUFFER."
  (let ((target-buffer (or buffer (current-buffer))))
    (when --ecc-notification--flash-timer
      (cancel-timer --ecc-notification--flash-timer))
    (with-current-buffer target-buffer
      (invert-face 'mode-line)
      (force-mode-line-update))
    (setq --ecc-notification--flash-timer
          (run-with-timer 0.5 nil
                          (lambda ()
                            (when (buffer-live-p target-buffer)
                              (with-current-buffer target-buffer
                                (invert-face 'mode-line)
                                (force-mode-line-update))))))))

(defun --ecc-notification--display-message (state &optional buffer)
  "Display a message about STATE."
  (let ((state-name (--ecc-state-detection-get-name state)))
    (if buffer
        (message "[%s] Claude prompt detected: %s"
                 (buffer-name buffer) state-name)
      (message "Claude prompt detected: %s" state-name))))

(defun --ecc-notification-toggle ()
  "Toggle notifications for Claude prompts."
  (interactive)
  (setq --ecc-notification-enabled (not --ecc-notification-enabled))
  (message "Claude notifications %s"
           (if --ecc-notification-enabled "enabled" "disabled")))

(provide '--ecc-notification)
```

**--ecc-auto-response.el**
```elisp
;;; -*- coding: utf-8; lexical-binding: t -*-

(require 'ecc-debug)
(require 'ecc-state-detection)
(require 'ecc-buffer-registry)
(require 'ecc-notification)

(defcustom --ecc-auto-response-interval 0.5
  "Interval in seconds for auto-response timer checks."
  :type 'float
  :group 'ecc)

(defcustom --ecc-auto-response-throttle-duration 5.0
  "Minimum seconds between auto-responses to the same state."
  :type 'float
  :group 'ecc)

(defcustom --ecc-auto-response-accumulation-threshold 5
  "Maximum number of responses allowed within accumulation window."
  :type 'number
  :group 'ecc)

(defcustom --ecc-auto-response-accumulation-window 3.0
  "Time window in seconds for accumulation detection."
  :type 'number
  :group 'ecc)

(defcustom --ecc-auto-response-responses
  '((:y/n . "1")
    (:y/y/n . "2")
    (:waiting . "/user:auto")
    (:initial-waiting . "/user:understand-guidelines"))
  "Alist of auto-responses for different Claude states."
  :type '(alist :key-type symbol :value-type string)
  :group 'ecc)

(defvar --ecc-auto-response--timer nil
  "Timer for checking and responding to Claude prompts.")

(defvar-local --ecc-auto-response--enabled nil
  "Whether auto-response is enabled for this buffer.")

(defvar-local --ecc-auto-response--last-state nil
  "Last Claude state that received an auto-response in this buffer.")

(defvar-local --ecc-auto-response--last-time 0
  "Timestamp of last auto-response in this buffer.")

(defvar-local --ecc-auto-response--accumulation-count 0
  "Counter for responses sent within current accumulation window.")

(defvar-local --ecc-auto-response--accumulation-start-time 0
  "Start time of current accumulation tracking window.")

(defun --ecc-auto-response-start ()
  "Start the global auto-response system."
  (interactive)
  (when --ecc-auto-response--timer
    (cancel-timer --ecc-auto-response--timer))
  (setq --ecc-auto-response--timer
        (run-with-timer 0 --ecc-auto-response-interval
                        '--ecc-auto-response--process-all-buffers))
  (--ecc-debug-message "Auto-response system started"))

(defun --ecc-auto-response-stop ()
  "Stop the global auto-response system."
  (interactive)
  (when --ecc-auto-response--timer
    (cancel-timer --ecc-auto-response--timer)
    (setq --ecc-auto-response--timer nil))
  (--ecc-debug-message "Auto-response system stopped"))

(defun --ecc-auto-response-enable-buffer (&optional buffer)
  "Enable auto-response for BUFFER."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (--ecc-buffer-registry-register buf)
    (with-current-buffer buf
      (setq-local --ecc-auto-response--enabled t))
    (unless --ecc-auto-response--timer
      (--ecc-auto-response-start))
    (--ecc-debug-message "Auto-response enabled for buffer: %s" (buffer-name buf))))

(defun --ecc-auto-response-disable-buffer (&optional buffer)
  "Disable auto-response for BUFFER."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      (setq-local --ecc-auto-response--enabled nil))
    (--ecc-debug-message "Auto-response disabled for buffer: %s" (buffer-name buf))))

(defun --ecc-auto-response-toggle-buffer (&optional buffer)
  "Toggle auto-response for BUFFER."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      (if --ecc-auto-response--enabled
          (--ecc-auto-response-disable-buffer buf)
        (--ecc-auto-response-enable-buffer buf)))))

(defun --ecc-auto-response--process-all-buffers ()
  "Process all registered buffers for auto-response."
  (--ecc-buffer-registry-cleanup)
  (dolist (buffer (--ecc-buffer-registry-get-registered))
    (when (buffer-live-p buffer)
      (--ecc-auto-response--process-buffer buffer))))

(defun --ecc-auto-response--process-buffer (buffer)
  "Process BUFFER for auto-response."
  (with-current-buffer buffer
    (when --ecc-auto-response--enabled
      (let ((state (--ecc-state-detection-detect)))
        (when state
          (unless (--ecc-auto-response--should-throttle-p state)
            (--ecc-auto-response--send-response state buffer)))))))

(defun --ecc-auto-response--should-throttle-p (state)
  "Check if auto-response for STATE should be throttled."
  (let ((current-time (float-time)))
    (or
     (and (eq state --ecc-auto-response--last-state)
          (< (- current-time --ecc-auto-response--last-time)
             --ecc-auto-response-throttle-duration))
     (--ecc-auto-response--accumulation-detected-p))))

(defun --ecc-auto-response--accumulation-detected-p ()
  "Check if auto-response accumulation has been detected."
  (let ((current-time (float-time)))
    (when (> (- current-time --ecc-auto-response--accumulation-start-time)
             --ecc-auto-response-accumulation-window)
      (setq-local --ecc-auto-response--accumulation-count 0)
      (setq-local --ecc-auto-response--accumulation-start-time 0))
    (>= --ecc-auto-response--accumulation-count
        --ecc-auto-response-accumulation-threshold)))

(defun --ecc-auto-response--send-response (state buffer)
  "Send appropriate response for STATE in BUFFER."
  (let ((response (cdr (assq state --ecc-auto-response-responses))))
    (when response
      (--ecc-auto-response--update-tracking state)
      (--ecc-auto-response--send-to-buffer buffer response)
      (--ecc-notification-notify state buffer))))

(defun --ecc-auto-response--update-tracking (state)
  "Update tracking variables for STATE."
  (let ((current-time (float-time)))
    (setq-local --ecc-auto-response--last-state state)
    (setq-local --ecc-auto-response--last-time current-time)
    (when (= --ecc-auto-response--accumulation-count 0)
      (setq-local --ecc-auto-response--accumulation-start-time current-time))
    (setq-local --ecc-auto-response--accumulation-count
                (1+ --ecc-auto-response--accumulation-count))))

(defun --ecc-auto-response--send-to-buffer (buffer text)
  "Send TEXT to BUFFER."
  (with-current-buffer buffer
    (cond
     ((derived-mode-p 'vterm-mode)
      (when (fboundp 'vterm-send-string)
        (vterm-send-string text)
        (vterm-send-return)))
     ((derived-mode-p 'comint-mode)
      (goto-char (point-max))
      (insert text)
      (comint-send-input))
     (t
      (goto-char (point-max))
      (insert text "\n"))))
  (--ecc-debug-message "Sent response to %s: %s" (buffer-name buffer) text))

(provide '--ecc-auto-response)
```

**--ecc-vterm-utils.el**
```elisp
;;; -*- coding: utf-8; lexical-binding: t -*-

(require 'ecc-debug)

(defcustom --ecc-vterm-yank-extension-patterns
  '(("py" . "^\\(import\\|from\\|def\\|class\\|if __name__ ==\\)")
    ("js" . "\\(function\\|const\\|let\\|var\\|=>\\|import\\|export\\)")
    ("html" . "\\(<html\\|<!DOCTYPE\\|<body\\|<div\\|<script\\)")
    ("css" . "\\([.#]?[a-zA-Z0-9_-]+\\s-*{\\)")
    ("el" . "\\((defun\\|(defvar\\|(defcustom\\|(require\\|(provide\\)")
    ("sh" . "\\(^#!.*sh\\|function\\s-+[a-zA-Z0-9_-]+\\s-*(\\)")
    ("txt" . ".*"))
  "Alist mapping file extensions to regex patterns for content detection."
  :type '(alist :key-type string :value-type regexp)
  :group 'ecc)

(defvar --ecc-vterm-yank-history nil
  "History of filenames used for yanking vterm content.")

(defun --ecc-vterm-utils-detect-file-type (content)
  "Detect file type based on CONTENT."
  (catch 'found
    (dolist (entry --ecc-vterm-yank-extension-patterns)
      (when (string-match-p (cdr entry) content)
        (throw 'found (car entry))))
    "txt"))

(defun --ecc-vterm-utils-yank-region-to-file (start end filename)
  "Yank region between START and END to file named FILENAME."
  (interactive
   (if (region-active-p)
       (let* ((content (buffer-substring-no-properties
                        (region-beginning) (region-end)))
              (file-type (--ecc-vterm-utils-detect-file-type content))
              (default-name (format "claude_output.%s" file-type))
              (filename (read-file-name "Save to file: " nil nil nil
                                        default-name
                                        '--ecc-vterm-yank-history)))
         (list (region-beginning) (region-end) filename))
     (user-error "No active region")))
  (let* ((content (buffer-substring-no-properties start end))
         (file-type (--ecc-vterm-utils-detect-file-type content))
         (filename (if (and filename (not (string-empty-p filename)))
                       filename
                     (format "claude_output_%s.%s"
                             (format-time-string "%Y%m%d_%H%M%S")
                             file-type)))
         (full-path (expand-file-name filename)))
    (when (and (file-exists-p full-path)
               (not (y-or-n-p (format "File %s exists. Overwrite? " full-path))))
      (user-error "Aborted"))
    (with-temp-file full-path
      (insert content))
    (--ecc-debug-message "Wrote %s (%d bytes)" full-path (length content))
    (when (y-or-n-p "Open file in a new buffer? ")
      (find-file full-path))
    full-path))

(defun --ecc-vterm-utils-yank-buffer-to-file (filename)
  "Yank entire vterm buffer content to file named FILENAME."
  (interactive
   (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
          (file-type (--ecc-vterm-utils-detect-file-type content))
          (default-name (format "claude_buffer.%s" file-type))
          (filename (read-file-name "Save buffer to file: " nil nil nil
                                    default-name '--ecc-vterm-yank-history)))
     (list filename)))
  (--ecc-vterm-utils-yank-region-to-file (point-min) (point-max) filename))

(defun --ecc-vterm-utils-quick-yank-region ()
  "Quickly yank active region to auto-named file based on content."
  (interactive)
  (if (region-active-p)
      (let* ((content (buffer-substring-no-properties
                       (region-beginning) (region-end)))
             (file-type (--ecc-vterm-utils-detect-file-type content))
             (filename (format "claude_output_%s.%s"
                               (format-time-string "%Y%m%d_%H%M%S")
                               file-type)))
        (--ecc-vterm-utils-yank-region-to-file
         (region-beginning) (region-end) filename))
    (user-error "No active region")))

(provide '--ecc-vterm-utils)
```


---





---


| YOU

| None

| continue


---


| CLAUDE-SONNET-4-20250514

**--ecc-performance.el**
```elisp
;;; -*- coding: utf-8; lexical-binding: t -*-

(require 'ecc-debug)

(defcustom --ecc-performance-gc-threshold (* 64 1024 1024)
  "GC threshold to use during high-output operations."
  :type 'integer
  :group 'ecc)

(defcustom --ecc-performance-max-search-size 2000
  "Maximum buffer size in characters to search for state detection."
  :type 'integer
  :group 'ecc)

(defcustom --ecc-performance-jit-lock-defer-time 0.05
  "Time to defer fontification during rapid updates."
  :type 'float
  :group 'ecc)

(defun --ecc-performance-optimize-buffer (&optional buffer)
  "Apply performance optimizations to BUFFER or current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (setq-local scroll-conservatively 101
                scroll-margin 0
                scroll-step 1
                fast-but-imprecise-scrolling t
                auto-window-vscroll nil
                truncate-lines t
                line-move-visual t
                inhibit-field-text-motion t
                jit-lock-defer-time --ecc-performance-jit-lock-defer-time
                redisplay-skip-fontification-on-input t
                redisplay-dont-pause t)
    (when (and (boundp 'display-line-numbers-mode)
               display-line-numbers-mode)
      (display-line-numbers-mode -1))
    (buffer-disable-undo)))

(defun --ecc-performance-with-gc-optimization (func &rest args)
  "Execute FUNC with ARGS under optimized garbage collection settings."
  (let ((old-gc-threshold gc-cons-threshold))
    (setq gc-cons-threshold --ecc-performance-gc-threshold)
    (unwind-protect
        (apply func args)
      (setq gc-cons-threshold old-gc-threshold))))

(provide '--ecc-performance)
```

**--ecc-vterm-mode.el**
```elisp
;;; -*- coding: utf-8; lexical-binding: t -*-

(require 'ecc-debug)
(require 'ecc-state-detection)
(require 'ecc-auto-response)
(require 'ecc-notification)
(require 'ecc-vterm-utils)
(require 'ecc-performance)
(require 'vterm nil t)

(defcustom --ecc-vterm-mode-buffer-name "*CLAUDE-VTERM*"
  "Default buffer name for Claude vterm buffers."
  :type 'string
  :group 'ecc)

(defcustom --ecc-vterm-mode-show-state-in-mode-line t
  "Whether to show Claude state in the mode line."
  :type 'boolean
  :group 'ecc)

(defcustom --ecc-vterm-mode-auto-mode-color "#4a5d23"
  "Background color for modeline when auto-mode is enabled."
  :type 'color
  :group 'ecc)

(defvar-local --ecc-vterm-mode--auto-mode-active nil
  "Whether auto-mode is active for this buffer.")

(defvar-local --ecc-vterm-mode--state-timer nil
  "Timer for updating Claude state.")

(defvar --ecc-vterm-mode-map
  (let ((map (make-sparse-keymap)))
    (when (boundp 'vterm-mode-map)
      (set-keymap-parent map vterm-mode-map))
    (define-key map (kbd "C-c C-y") '--ecc-vterm-mode-yes)
    (define-key map (kbd "C-c C-n") '--ecc-vterm-mode-no)
    (define-key map (kbd "C-c C-a") '--ecc-vterm-mode-toggle-auto)
    (define-key map (kbd "C-c C-f") '--ecc-vterm-utils-yank-region-to-file)
    (define-key map (kbd "C-c C-b") '--ecc-vterm-utils-yank-buffer-to-file)
    (define-key map (kbd "C-c C-q") '--ecc-vterm-utils-quick-yank-region)
    map)
  "Keymap for Claude vterm mode.")

(when (featurep 'vterm)
  (define-derived-mode --ecc-vterm-mode vterm-mode "Claude-VTerm"
    "Major mode for optimized Claude interaction in vterm."
    (--ecc-vterm-mode--setup-buffer)))

(defun --ecc-vterm-mode--setup-buffer ()
  "Set up current buffer for Claude vterm mode."
  (--ecc-debug-message "Setting up Claude vterm buffer: %s" (buffer-name))
  (--ecc-performance-optimize-buffer)
  (--ecc-auto-response-enable-buffer)
  (when --ecc-vterm-mode-show-state-in-mode-line
    (setq mode-line-process
          '(:eval (--ecc-vterm-mode--mode-line-indicator))))
  (--ecc-vterm-mode--setup-state-timer)
  (add-hook 'kill-buffer-hook '--ecc-vterm-mode--cleanup-buffer nil t))

(defun --ecc-vterm-mode--setup-state-timer ()
  "Set up timer for checking Claude state."
  (when --ecc-vterm-mode--state-timer
    (cancel-timer --ecc-vterm-mode--state-timer))
  (let ((buffer (current-buffer)))
    (setq --ecc-vterm-mode--state-timer
          (run-with-timer 0 1.0
                         (lambda ()
                           (when (buffer-live-p buffer)
                             (with-current-buffer buffer
                               (force-mode-line-update))))))))

(defun --ecc-vterm-mode--cleanup-buffer ()
  "Clean up when buffer is killed."
  (when --ecc-vterm-mode--state-timer
    (cancel-timer --ecc-vterm-mode--state-timer)
    (setq --ecc-vterm-mode--state-timer nil)))

(defun --ecc-vterm-mode--mode-line-indicator ()
  "Return mode line indicator for current Claude state."
  (let ((state (--ecc-state-detection-detect))
        (auto-indicator (if --ecc-vterm-mode--auto-mode-active
                           (propertize " [AUTO] "
                                     'face `(:background ,--ecc-vterm-mode-auto-mode-color
                                            :foreground "#ffffff"
                                            :weight bold))
                         "")))
    (concat auto-indicator
            (cond
             ((eq state :waiting) " [Waiting]")
             ((eq state :y/n) " [Y/N]")
             ((eq state :y/y/n) " [Y/Y/N]")
             ((eq state :initial-waiting) " [Init]")
             (t "")))))

(defun --ecc-vterm-mode-yes ()
  "Send 'y' response to Claude prompt."
  (interactive)
  (when (fboundp 'vterm-send-string)
    (vterm-send-string "y")
    (vterm-send-return)))

(defun --ecc-vterm-mode-no ()
  "Send 'n' response to Claude prompt."
  (interactive)
  (when (fboundp 'vterm-send-string)
    (vterm-send-string "n")
    (vterm-send-return)))

(defun --ecc-vterm-mode-toggle-auto ()
  "Toggle auto-response mode."
  (interactive)
  (setq-local --ecc-vterm-mode--auto-mode-active
              (not --ecc-vterm-mode--auto-mode-active))
  (if --ecc-vterm-mode--auto-mode-active
      (--ecc-auto-response-enable-buffer)
    (--ecc-auto-response-disable-buffer))
  (--ecc-debug-message "Auto-mode %s"
                     (if --ecc-vterm-mode--auto-mode-active "enabled" "disabled"))
  (force-mode-line-update))

(defun --ecc-vterm-mode-create ()
  "Create a new Claude vterm buffer."
  (interactive)
  (let ((buffer (get-buffer-create --ecc-vterm-mode-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode '--ecc-vterm-mode)
        (--ecc-vterm-mode)))
    (switch-to-buffer buffer)
    buffer))

(provide '--ecc-vterm-mode)
```

**ecc.el**
```elisp
;;; -*- coding: utf-8; lexical-binding: t -*-

(require 'ecc-variables)
(require 'ecc-debug)
(require 'ecc-state-detection)
(require 'ecc-buffer-registry)
(require 'ecc-notification)
(require 'ecc-auto-response)
(require 'ecc-vterm-utils)
(require 'ecc-performance)
(require 'ecc-vterm-mode)

;;;###autoload
(defun --ecc-start ()
  "Start the ECC system."
  (interactive)
  (--ecc-auto-response-start)
  (message "ECC system started"))

;;;###autoload
(defun --ecc-stop ()
  "Stop the ECC system."
  (interactive)
  (--ecc-auto-response-stop)
  (message "ECC system stopped"))

;;;###autoload
(defun --ecc-create-vterm ()
  "Create a new Claude vterm buffer."
  (interactive)
  (--ecc-vterm-mode-create))

(provide 'ecc)
```

<!-- EOF -->