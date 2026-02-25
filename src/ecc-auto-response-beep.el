;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-02-20 16:40:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-auto-response-beep.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;;; Periodic beep/audio alert when auto mode is enabled and a buffer is running.
;;; One global timer fires every `ecc-auto-response-running-beep-interval' seconds.
;;;
;;; Audio modes:
;;;   1. Plain beep: (ding t), always available
;;;   2. Pre-recorded TTS: Bundled mp3 files in src/audio/ (no network needed).
;;;      Custom files can be regenerated via `ecc-auto-response-tts-regenerate-audio'.
;;;      Played via mpg123/paplay.
;;;
;;; Two distinct notification events:
;;;   - running: periodic, when any buffer is in :running state
;;;   - sent: each time a response is sent to Claude

;; 1. Dependencies
;; ----------------------------------------

(require 'cl-lib)
(require 'ecc-debug)
(require 'ecc-state-detection)

;; Function stubs (defined in ecc-auto-response.el)
(declare-function --ecc-auto-response-get-registered-buffers
		  "ecc-auto-response" ())

;; Variable stubs (defined in ecc-auto-response.el)

(defvar-local --ecc-auto-response--enabled nil)

;; 2. Configuration
;; ----------------------------------------

(defcustom ecc-auto-response-running-beep-enabled t
  "Whether to enable audio notifications for auto-response.
Plays a sound at regular intervals when any auto-enabled buffer is
in the :running state, and also when a response is sent."
  :type 'boolean
  :group 'ecc)

(defcustom ecc-auto-response-running-beep-interval 10.0
  "Interval in seconds between running-state audio notifications."
  :type 'float
  :group 'ecc)

(defcustom ecc-auto-response-tts-enabled nil
  "Whether to use pre-recorded TTS sounds instead of plain beeps.
Requires `scitex audio speak' CLI to generate files on first use.
When nil, falls back to standard `ding'."
  :type 'boolean
  :group 'ecc)

(defcustom ecc-auto-response-tts-backend "gtts"
  "TTS backend used to pre-generate audio files.
Passed as --backend to `scitex audio speak --no-play'."
  :type '(choice (const "gtts")
                 (const "elevenlabs")
                 (const "pyttsx3"))
  :group 'ecc)

(defcustom ecc-auto-response-audio-dir nil
  "Directory with custom pre-recorded ECC audio files.
When nil, uses bundled files from the package's src/audio/ directory.
Set to a custom directory to override the bundled sounds."
  :type '(choice (const :tag "Use bundled files" nil)
                 (directory :tag "Custom directory"))
  :group 'ecc)

(defcustom ecc-auto-response-tts-running-text "Claude is running"
  "Text for the periodic running-state notification."
  :type 'string
  :group 'ecc)

(defcustom ecc-auto-response-tts-sent-text "Response sent"
  "Text for the notification when a response is sent."
  :type 'string
  :group 'ecc)

;; 3. Variables
;; ----------------------------------------

(defvar --ecc-auto-response--running-beep-timer nil
  "Single global timer for periodic running-beep checks.")

(defvar --ecc-auto-response--last-notify-time 0.0
  "Timestamp of the last audio notification, used for debouncing.")

(defcustom ecc-auto-response-beep-cooldown 5.0
  "Minimum seconds between consecutive audio notifications.
Prevents chattering when multiple events fire close together."
  :type 'float
  :group 'ecc)

(defconst --ecc-auto-response--audio-filenames
  '(("running" . "ecc-running.mp3")
    ("sent"    . "ecc-sent.mp3"))
  "Alist mapping event names to audio filenames.")

;; 4. Core Beep (always-available fallback)
;; ----------------------------------------

(defcustom ecc-auto-response-beep-running-hz 800
  "Frequency (Hz) for the periodic running-state beep."
  :type 'integer
  :group 'ecc)

(defcustom ecc-auto-response-beep-sent-hz 1200
  "Frequency (Hz) for the response-sent beep."
  :type 'integer
  :group 'ecc)

(defcustom ecc-auto-response-beep-duration-ms 100
  "Duration in milliseconds for beep tones."
  :type 'integer
  :group 'ecc)

(defun --ecc-auto-response--force-beep ()
  "Ring the bell unconditionally, bypassing `ring-bell-function' if ignore."
  (let ((ring-bell-function nil)
        (visible-bell nil))
    (ding t)))

(defun --ecc-auto-response--tone-beep (hz &optional duration-ms)
  "Play a tone at HZ frequency for DURATION-MS milliseconds.
Uses Linux `beep' command if available, falls back to `ding'."
  (let ((dur (or duration-ms ecc-auto-response-beep-duration-ms)))
    (cond
     ((executable-find "beep")
      (start-process "ecc-tone" nil "beep" "-f" (number-to-string hz)
                     "-l" (number-to-string dur)))
     (t
      (--ecc-auto-response--force-beep)))))

;; 5. Pre-recorded Audio
;; ----------------------------------------

(defun --ecc-auto-response--audio-dir ()
  "Return the effective audio directory (custom or bundled)."
  (or ecc-auto-response-audio-dir
      (expand-file-name
       "audio"
       (file-name-directory
        (or (locate-library "ecc-auto-response-beep")
            load-file-name
            buffer-file-name)))))

(defun --ecc-auto-response--audio-path (event)
  "Return full path to pre-recorded audio file for EVENT."
  (expand-file-name
   (cdr (assoc event --ecc-auto-response--audio-filenames))
   (--ecc-auto-response--audio-dir)))

(defun --ecc-auto-response--generate-audio (event text)
  "Generate pre-recorded audio for EVENT (TEXT) via scitex CLI asynchronously."
  (let ((path (--ecc-auto-response--audio-path event)))
    (make-directory (file-name-directory path) t)
    (apply #'start-process
           (format "ecc-tts-gen-%s" event) nil "scitex"
           (list "audio" "speak" text
                 "--backend" ecc-auto-response-tts-backend
                 "--no-play"
                 "--output" path))
    (--ecc-debug-message "Generating ECC audio: %s -> %s" text path)))

(defun --ecc-auto-response--ensure-audio-files ()
  "Generate audio files if they don't exist yet."
  (when (executable-find "scitex")
    (let ((pairs `(("running" . ,ecc-auto-response-tts-running-text)
                   ("sent"    . ,ecc-auto-response-tts-sent-text))))
      (dolist (pair pairs)
        (unless (file-exists-p
                 (--ecc-auto-response--audio-path (car pair)))
          (--ecc-auto-response--generate-audio
           (car pair) (cdr pair)))))))

(defun --ecc-auto-response--play-audio (event)
  "Play pre-recorded audio for EVENT asynchronously.
Falls back to beep if file missing or no player found."
  (let* ((path (--ecc-auto-response--audio-path event))
         (player (or (executable-find "mpg123")
                     (executable-find "mpg321")
                     (executable-find "paplay")
                     (executable-find "aplay"))))
    (cond
     ((and (file-exists-p path) player)
      (start-process "ecc-audio" nil player "-q" path))
     ((not (file-exists-p path))
      (--ecc-auto-response--ensure-audio-files)
      (--ecc-auto-response--force-beep))
     (t
      (--ecc-auto-response--force-beep)))))

;; 6. Notification Dispatchers (with cooldown)
;; ----------------------------------------

(defun --ecc-auto-response--can-notify-p ()
  "Return t if enough time has passed since the last notification."
  (> (- (float-time) --ecc-auto-response--last-notify-time)
     ecc-auto-response-beep-cooldown))

(defun --ecc-auto-response--do-notify (event)
  "Fire notification for EVENT and record the timestamp.
Uses TTS if enabled, then tone beep with per-event frequency, then plain ding."
  (setq --ecc-auto-response--last-notify-time (float-time))
  (cond
   (ecc-auto-response-tts-enabled
    (--ecc-auto-response--play-audio event))
   ((string= event "running")
    (--ecc-auto-response--tone-beep ecc-auto-response-beep-running-hz))
   ((string= event "sent")
    (--ecc-auto-response--tone-beep ecc-auto-response-beep-sent-hz))
   (t
    (--ecc-auto-response--force-beep))))

(defun --ecc-auto-response--notify-running ()
  "Notify that Claude is running (called by periodic timer).
Suppressed if within `ecc-auto-response-beep-cooldown' of last notify."
  (when (--ecc-auto-response--can-notify-p)
    (--ecc-auto-response--do-notify "running")))

(defun --ecc-auto-response--notify-sent ()
  "Notify that a response was sent to Claude.
Suppressed if within `ecc-auto-response-beep-cooldown' of last notify."
  (when (--ecc-auto-response--can-notify-p)
    (--ecc-auto-response--do-notify "sent")))

;; 7. Running-Beep Timer
;; ----------------------------------------

(defun --ecc-auto-response--running-beep-check ()
  "Notify once if any auto-enabled buffer is currently in the :running state.
Suppressed when a send is in progress."
  (when (and ecc-auto-response-running-beep-enabled
             (not (bound-and-true-p --ecc-auto-response--sending-p)))
    (catch 'notified
      (dolist (buffer (--ecc-auto-response-get-registered-buffers))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (and --ecc-auto-response--enabled
                       (eq (--ecc-state-detection-detect) :running))
              (--ecc-auto-response--notify-running)
              (throw 'notified t))))))))

(defun --ecc-auto-response--start-running-beep-timer ()
  "Start the single global running-beep timer.
Cancels any existing timer first to prevent duplicates."
  (when --ecc-auto-response--running-beep-timer
    (cancel-timer --ecc-auto-response--running-beep-timer))
  (setq --ecc-auto-response--running-beep-timer
        (run-with-timer ecc-auto-response-running-beep-interval
                        ecc-auto-response-running-beep-interval
                        '--ecc-auto-response--running-beep-check))
  (--ecc-debug-message "Running-beep timer started (every %s s)"
                       ecc-auto-response-running-beep-interval))

(defun --ecc-auto-response--stop-running-beep-timer ()
  "Stop the running-beep timer if no auto-enabled buffers remain."
  (let ((any-enabled nil))
    (dolist (buffer (--ecc-auto-response-get-registered-buffers))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when --ecc-auto-response--enabled
            (setq any-enabled t)))))
    (unless any-enabled
      (when --ecc-auto-response--running-beep-timer
        (cancel-timer --ecc-auto-response--running-beep-timer)
        (setq --ecc-auto-response--running-beep-timer nil)
        (--ecc-debug-message "Running-beep timer stopped")))))

;; 8. Interactive
;; ----------------------------------------

(defun ecc-auto-response-running-beep-toggle ()
  "Toggle audio notifications on or off."
  (interactive)
  (setq ecc-auto-response-running-beep-enabled
        (not ecc-auto-response-running-beep-enabled))
  (if ecc-auto-response-running-beep-enabled
      (progn
        (--ecc-auto-response--start-running-beep-timer)
        (message "Running-beep ON (every %.0f s)"
                 ecc-auto-response-running-beep-interval))
    (when --ecc-auto-response--running-beep-timer
      (cancel-timer --ecc-auto-response--running-beep-timer)
      (setq --ecc-auto-response--running-beep-timer nil))
    (message "Running-beep OFF")))

(defun ecc-auto-response-tts-toggle ()
  "Toggle pre-recorded TTS sounds on or off."
  (interactive)
  (setq ecc-auto-response-tts-enabled
        (not ecc-auto-response-tts-enabled))
  (when ecc-auto-response-tts-enabled
    (--ecc-auto-response--ensure-audio-files))
  (message "TTS %s (backend: %s)"
           (if ecc-auto-response-tts-enabled "ON" "OFF")
           ecc-auto-response-tts-backend))

(defun ecc-auto-response-tts-regenerate-audio ()
  "Re-generate all pre-recorded ECC audio files."
  (interactive)
  (when (executable-find "scitex")
    (let ((pairs `(("running" . ,ecc-auto-response-tts-running-text)
                   ("sent"    . ,ecc-auto-response-tts-sent-text))))
      (dolist (pair pairs)
        (--ecc-auto-response--generate-audio (car pair) (cdr pair))))
    (message "Regenerating ECC audio files in %s"
             ecc-auto-response-audio-dir)))

;; 9. Timer Lifecycle Management
;; ----------------------------------------

(defvar --ecc-auto-response--all-timer-vars
  '(--ecc-auto-response--running-beep-timer
    --ecc-auto-response--timer
    --ecc-auto-response--periodic-timer
    --ecc-auto-response--pulse-timer)
  "All global timer variables managed by ECC auto-response.
Used for lifecycle cleanup to prevent orphaned timers.")

(defun ecc-auto-response-cleanup-timers ()
  "Cancel ALL ECC auto-response timers (tracked and orphaned).
Prevents timer accumulation that can hang Emacs."
  (interactive)
  (let ((cancelled 0))
    ;; 1. Cancel all tracked timer variables
    (dolist (var --ecc-auto-response--all-timer-vars)
      (when (and (boundp var) (symbol-value var))
        (cancel-timer (symbol-value var))
        (set var nil)
        (cl-incf cancelled)))
    ;; 2. Scan timer-list for orphaned ECC timers (named functions)
    (dolist (timer timer-list)
      (let ((fn (timer--function timer)))
        (when (and (symbolp fn)
                   (string-match-p
		    "ecc-auto-response\\|ecc-state-detection"
                    (symbol-name fn)))
          (cancel-timer timer)
          (cl-incf cancelled))))
    (when (called-interactively-p 'interactive)
      (message "ECC: cancelled %d timer(s)" cancelled))
    cancelled))

;; Clean up before starting fresh on load
(ecc-auto-response-cleanup-timers)
(run-with-timer
 0.5 nil
 (lambda ()
   (when ecc-auto-response-running-beep-enabled
     (--ecc-auto-response--start-running-beep-timer))))

(provide 'ecc-auto-response-beep)

(when
    (not load-file-name)
  (message "ecc-auto-response-beep.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
