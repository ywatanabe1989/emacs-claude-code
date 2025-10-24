<!-- ---
!-- Timestamp: 2025-10-24 16:07:19
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/TODO.md
!-- --- -->

## Performance Issue: Emacs Heavy with Multiple Claude Code Instances

### Problem
When running multiple Claude Code instances, Emacs gets really heavy.
The main timer `--ecc-auto-response--process-all-buffers` runs every 1.5 seconds
and processes ALL registered buffers each time.

### Optimizations Implemented (2025-10-24)

1. **Reduced buffer check size** (ecc-state-detection.el:17)
   - Changed from 2048 to 512 characters
   - Claude prompts typically appear in the last few hundred chars
   - ~75% reduction in text analysis per buffer

2. **Buffer rotation** (ecc-auto-response.el)
   - Added `--ecc-auto-response-max-buffers-per-cycle` (default: 3)
   - Processes only N buffers per cycle in round-robin fashion
   - With 10 buffers, each buffer checked every ~5 seconds instead of 1.5s
   - Set to nil to process all buffers (old behavior)

3. **Conditional logging** (ecc-auto-response.el)
   - Added `--ecc-auto-response-verbose-logging` (default: nil)
   - Debug messages only logged when needed
   - Reduces overhead from string formatting and logging calls

4. **Idle timer option** (ecc-auto-response.el)
   - Added `--ecc-auto-response-use-idle-timer` (default: nil)
   - When enabled, processing only happens when Emacs is idle
   - Reduces interference with typing and other activities

### Configuration for Best Performance

```elisp
;; Recommended settings for 5+ Claude buffers:
(setq --ecc-auto-response-max-buffers-per-cycle 3)       ; Process 3 buffers per cycle
(setq --ecc-auto-response-verbose-logging nil)           ; Disable verbose logging
(setq --ecc-auto-response-use-idle-timer t)              ; Use idle timer
(setq --ecc-state-detection-buffer-size 512)             ; Check last 512 chars only
```

### Expected Impact
- **With 10 buffers**: ~70% reduction in per-cycle processing
- **CPU usage**: Lower peak usage, better distributed over time
- **Responsiveness**: Improved with idle timer, no interference during typing


Active Timers:
=============

Press 'c' or 'd' to cancel timer at point
Press 'g' to refresh the list
Press 'q' to quit

Timer: [t 26874 65511 428614 1.5 --ecc-auto-response--process-all-buffers nil nil 676000 nil]
  Function: --ecc-auto-response--process-all-buffers
  Args: nil
  Idle: nil
  Repeat: 1.5

Timer: [nil 26875 64 294811 2 #[nil ((if (buffer-live-p buffer) (progn (save-current-buffer (set-buffer buffer) (my/vterm-truncate-if-needed))))) ((buffer . #<buffer cloud-24:50-80-01>))] nil nil 875000 nil]
  Function: #[nil ((if (buffer-live-p buffer) (progn (save-current-buffer (set-buffer buffer) (my/vterm-truncate-if-needed))))) ((buffer . #<buffer cloud-24:50-80-01>))]
  Args: nil
  Idle: nil
  Repeat: 2

Timer: [nil 26875 64 329815 2 #[nil ((if (buffer-live-p buffer) (progn (save-current-buffer (set-buffer buffer) (my/vterm-truncate-if-needed))))) ((buffer . #<buffer cloud-24:50-160-01>))] nil nil 877000 nil]
  Function: #[nil ((if (buffer-live-p buffer) (progn (save-current-buffer (set-buffer buffer) (my/vterm-truncate-if-needed))))) ((buffer . #<buffer cloud-24:50-160-01>))]
  Args: nil
  Idle: nil
  Repeat: 2

Timer: [nil 26875 64 541472 2 #[nil ((if (buffer-live-p buffer) (progn (save-current-buffer (set-buffer buffer) (my/vterm-truncate-if-needed))))) ((buffer . #<buffer -15:03:04>))] nil nil 495000 nil]
  Function: #[nil ((if (buffer-live-p buffer) (progn (save-current-buffer (set-buffer buffer) (my/vterm-truncate-if-needed))))) ((buffer . #<buffer -15:03:04>))]
  Args: nil
  Idle: nil
  Repeat: 2

Timer: [nil 26875 64 897879 60 etm-numeric-cleanup-dead-buffers nil nil 76000 nil]
  Function: etm-numeric-cleanup-dead-buffers
  Args: nil
  Idle: nil
  Repeat: 60

Timer: [nil 26875 65 36967 2 #[nil ((if (buffer-live-p buffer) (progn (save-current-buffer (set-buffer buffer) (my/vterm-truncate-if-needed))))) ((buffer . #<buffer -15:25:00>))] nil nil 673000 nil]
  Function: #[nil ((if (buffer-live-p buffer) (progn (save-current-buffer (set-buffer buffer) (my/vterm-truncate-if-needed))))) ((buffer . #<buffer -15:25:00>))]
  Args: nil
  Idle: nil
  Repeat: 2

Timer: [nil 26875 65 200362 2 #[nil ((if (buffer-live-p buffer) (progn (save-current-buffer (set-buffer buffer) (my/vterm-truncate-if-needed))))) ((buffer . #<buffer -15:03:11>))] nil nil 163000 nil]
  Function: #[nil ((if (buffer-live-p buffer) (progn (save-current-buffer (set-buffer buffer) (my/vterm-truncate-if-needed))))) ((buffer . #<buffer -15:03:11>))]
  Args: nil
  Idle: nil
  Repeat: 2

Timer: [nil 26875 65 922964 nil undo-auto--boundary-timer nil nil 550000 nil]
  Function: undo-auto--boundary-timer
  Args: nil
  Idle: nil
  Repeat: nil

Timer: [nil 26875 66 83595 2 #[nil ((if (buffer-live-p buffer) (progn (save-current-buffer (set-buffer buffer) (my/vterm-truncate-if-needed))))) ((buffer . #<buffer -15:03:08>))] nil nil 834000 nil]
  Function: #[nil ((if (buffer-live-p buffer) (progn (save-current-buffer (set-buffer buffer) (my/vterm-truncate-if-needed))))) ((buffer . #<buffer -15:03:08>))]
  Args: nil
  Idle: nil
  Repeat: 2

Timer: [nil 26875 66 85997 2 #[nil ((if (buffer-live-p buffer) (progn (save-current-buffer (set-buffer buffer) (my/vterm-truncate-if-needed))))) ((buffer . #<buffer -15:26:45>))] nil nil 704000 nil]
  Function: #[nil ((if (buffer-live-p buffer) (progn (save-current-buffer (set-buffer buffer) (my/vterm-truncate-if-needed))))) ((buffer . #<buffer -15:26:45>))]
  Args: nil
  Idle: nil
  Repeat: 2

Timer: [nil 26875 68 181713 5 auto-revert-buffers nil nil 159000 nil]
  Function: auto-revert-buffers
  Args: nil
  Idle: nil
  Repeat: 5

Timer: [nil 26875 80 0 60 display-time-event-handler nil nil 0 t]
  Function: display-time-event-handler
  Args: nil
  Idle: nil
  Repeat: 60

Timer: [nil 26875 346 100726 300 savehist-autosave nil nil 239000 nil]
  Function: savehist-autosave
  Args: nil
  Idle: nil
  Repeat: 300

<!-- EOF -->