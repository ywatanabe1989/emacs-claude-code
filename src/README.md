<!-- ---
!-- Timestamp: 2025-10-24 12:54:56
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/README.md
!-- --- -->

# emacs-claude-code Source Documentation

## Auto-Response Throttling

The auto-response system includes throttling mechanisms to prevent excessive rapid responses while maintaining responsiveness.

### Throttling Configuration

Three key parameters control the throttling behavior:

| Parameter | Default Value | Description |
|-----------|--------------|-------------|
| `--ecc-auto-response-same-state-delay` | 1.5 seconds | Minimum time between responses to the same state |
| `--ecc-auto-response-burst-limit` | 10 responses | Maximum responses allowed within the time window |
| `--ecc-auto-response-burst-window` | 3 seconds | Time window for counting burst responses |

### Visual Explanation

#### 1. **Throttle Duration** - Same State Blocking

This prevents responding to the **same state** too quickly:

```
Time:  0.0s   0.3s   0.6s   0.9s   1.2s   1.5s
State: :y/n   :y/n   :y/n   :y/n   :y/n   :y/n
       │      │      │      │      │      │
Current: ✓    ✓      ✓      ✓      ✓      ✓
       └0.3s┘ └0.3s┘ └0.3s┘ └0.3s┘ └0.3s┘

✓ = Response sent
✗ = Blocked by throttle
```

**Current (0.3s):** Only wait 0.3 seconds between responses to the same state

---

#### 2. **Accumulation Threshold** - Rapid Response Limiting

This counts how many responses occur within a time window:

```
Window: [←────────── 3 seconds ──────────→]
Time:    0.0   0.3   0.6   0.9   1.2   1.5   1.8   2.1   2.4   2.7   3.0
State:   :y/n :wait :y/n :wait :y/n :wait :y/n :wait :y/n :wait :y/n
         │    │     │    │     │    │     │    │     │    │     │
Current: ✓────✓─────✓────✓─────✓────✓─────✓────✓─────✓────✓─────✗
         └─1──┴──2──┴─3──┴──4──┴─5──┴──6──┴─7──┴─8──┴─9──┴─10→ BLOCKED
                                                                    (>10 in 3s)
```

**Current:** Max 10 responses in 3 second window → blocks after 10th response

---

#### 3. **Accumulation Window** - Sliding Time Frame

Shows how the window "slides" forward in time:

```
Time:    0.0   0.5   1.0   1.5   2.0   2.5   3.0   3.5   4.0   4.5   5.0
         │     │     │     │     │     │     │     │     │     │     │
Resp:    ✓     ✓     ✓     ✓     ✓     ✓     ✓     ✓     ✓     ✓     ✓

Current (3 sec window):
At 3.0s: [0.0───────3.0]  Count: 7 → OK! (< 10)
         └─────3s─────┘

At 4.0s: [1.0───────4.0]  Count: 7 → OK! (< 10)
         └─────3s─────┘

At 5.0s: [2.0───────5.0]  Count: 7 → OK! (< 10)
         └─────3s─────┘
```

**Current:** Counts responses in the last 3 seconds (sliding window approach)

---

#### 4. **Real-World Example** - Interactive Session

A typical rapid interaction session with Claude:

```
Scenario: Claude asks → you answer → Claude responds → asks again

Time:    0.0    0.2    0.4    0.6    0.8    1.0    1.2    1.4    1.6
Event:   Q1     A1     Q2     A2     Q3     A3     Q4     A4     Q5
         :y/n   →      :wait  →      :y/n   →      :wait  →      :y/n
         │             │             │             │             │
Current: ✓      │      ✓      │      ✓      │      ✓      │      ✓
         └0.3s┘ │      └0.3s┘ │      └0.3s┘ │      └0.3s┘ │
         "Fast!"│      "Fast!"│      "Fast!"│      "Fast!"│
                │             │             │             │
         Still within 10 responses per 3 seconds → ALL ALLOWED!
```

---

### Customization

You can adjust these settings in your Emacs configuration:

```elisp
;; More aggressive (faster, more responses allowed)
(with-eval-after-load 'ecc-auto-response
  (setq --ecc-auto-response-same-state-delay 0.1           ; Very fast
        --ecc-auto-response-burst-limit 20                 ; Many responses
        --ecc-auto-response-burst-window 5))               ; Wide window

;; More conservative (slower, fewer responses)
(with-eval-after-load 'ecc-auto-response
  (setq --ecc-auto-response-same-state-delay 1.0           ; Slower
        --ecc-auto-response-burst-limit 5                  ; Fewer responses
        --ecc-auto-response-burst-window 2))               ; Narrow window
```

### Summary Table

| Concept | What it controls | Current Setting | Effect |
|---------|-----------------|-----------------|--------|
| **Same-State Delay** | Cool-down between same responses | 0.3s ⚡ | Fast response to same state |
| **Burst Limit** | Max responses allowed | 10 🟢 | Generous limit |
| **Burst Window** | Time frame for counting | 3s ⏰ | Wide time frame |

**Result:** Responsive auto mode that prevents infinite loops while allowing rapid legitimate interactions.

---

## Timing Flow

The auto-response system uses multiple timers and delays. This section shows the complete flow from timer tick to keystroke delivery, with all timing variables.

### Timer Architecture

```
Main Timer (1.5s)          Periodic Timer (300s)     Beep Timer (3.0s)     Pulse Timer (1.0s)
│ --ecc-auto-response-     │ --ecc-auto-response-    │ ecc-auto-response-  │ mode-line
│   interval               │   periodic-interval     │   running-beep-     │ indicator
│                          │                         │   interval          │ animation
▼                          ▼                         ▼                     ▼
process-all-buffers        send-periodic-return      beep if :running      toggle pulse
```

### Main Processing Flow

```
Timer tick (every 1.5s)
│
▼
process-all-buffers ─────────────────────────────────────────────────────
│
├─ GUARD: sending-p? ──yes──▶ SKIP (watchdog clears after 30.0s)
│                              --ecc-auto-response-sending-timeout
│
├─ max-buffers-per-cycle: 3 (round-robin rotation)
│
▼
process-buffer (per buffer)
│
├─ GUARD: --ecc-auto-response--enabled? ──no──▶ SKIP
│
├─ Detect state ──▶ :y/y/n :y/n :suggestion :running :user-typing :waiting nil
│
├─ nil state? ──▶ retry with wider window after 5.0s
│                  --ecc-auto-response-nil-state-retry-interval
│                  (buffer-size × 4)
│
├─ Stuck state watchdog: same state for 15.0s? ──▶ force re-send
│                        --ecc-auto-response-stuck-state-threshold
│
▼
dispatch-state
│
├─ :running ─────────▶ SKIP (Claude is working)
├─ :user-typing ─────▶ SKIP (user is actively typing)
│
├─ already-sent? ────▶ SKIP (same position within 100 chars)
│
├─ throttle?
│  ├─ same state within 1.5s? ──▶ SKIP
│  │  --ecc-auto-response-same-state-delay
│  └─ 10+ responses in 3s? ────▶ SKIP
│     --ecc-auto-response-burst-limit / burst-window
│
▼
send-response
```

### Send Response Detail

```
send-response(state, buffer)
│
├─ IF state = :y/n:
│  ├─ sit-for 1.0s  ◄── wait for CLI to render full options
│  └─ re-detect ──▶ upgrade to :y/y/n if Y/Y/N now visible
│
├─ Lookup response:
│  ├─ :waiting ──▶ encouragement system ──▶ "/speak"
│  ├─ :y/n ─────▶ "1"
│  ├─ :y/y/n ───▶ "2"
│  └─ :suggestion ──▶ (from responses alist)
│
├─ SET sending-p = t  (blocks all other processing)
│
▼
send-to-buffer(buffer, text, state) ─── wrapped in catch 'abort-send
│
├─ sit-for 1.0s ◄── --ecc-auto-response-safe-interval (pre-send delay)
│
├─ RECHECK: detect state again
│  └─ :user-typing? ──▶ throw 'abort-send (user started typing)
│
├─ vterm-send-string TEXT        ◄── actual keystroke delivery
├─ sit-for 0.5s                  ◄── gap between text and return
├─ vterm-send-return             ◄── press Enter
│
├─ sit-for 1.0s ◄── --ecc-auto-response-safe-interval (post-send delay)
└─ show-encouragement overlay (2.0s visual highlight)
│
▼
verify-send(buffer, original-state)
│
├─ Permission prompts (:y/y/n :y/n :suggestion):
│  ├─ sit-for 1.5s ◄── --ecc-auto-response-permission-retry-delay
│  ├─ re-detect state
│  ├─ state changed? ──▶ DONE (send succeeded)
│  └─ retry: re-send text+return (max 1 retry)
│            --ecc-auto-response-permission-retry-max
│
└─ CLEAR sending-p
```

### Timing Variables Reference

| Variable | Default | Location | Purpose |
|----------|---------|----------|---------|
| `--ecc-auto-response-interval` | 1.5s | core | Main timer tick interval |
| `--ecc-auto-response-safe-interval` | 1.0s | retry | Pre/post send delay |
| `--ecc-auto-response-same-state-delay` | 1.5s | core | Throttle: min gap between same-state responses |
| `--ecc-auto-response-burst-limit` | 10 | core | Throttle: max responses per window |
| `--ecc-auto-response-burst-window` | 3s | core | Throttle: sliding window size |
| `--ecc-auto-response-sending-timeout` | 30.0s | core | Watchdog: force-clear sending-p |
| `--ecc-auto-response-stuck-state-threshold` | 15.0s | core | Watchdog: force re-send if stuck |
| `--ecc-auto-response-nil-state-retry-interval` | 5.0s | core | Nil-state: wider detection delay |
| `--ecc-auto-response-nil-state-wide-multiplier` | 4 | core | Nil-state: buffer size multiplier |
| `--ecc-auto-response-send-verify-delay` | 2.0s | retry | Verify: wait before re-check |
| `--ecc-auto-response-permission-retry-delay` | 1.5s | retry | Permission: wait between retries |
| `--ecc-auto-response-permission-retry-max` | 1 | retry | Permission: max retries |
| `--ecc-auto-response-send-retry-max` | 8 | retry | Waiting: max return retries |
| `--ecc-auto-response-periodic-interval` | 300s | core | Periodic fallback return |
| `--ecc-auto-response-vterm-return-delay` | 1.0s | main | Gap between text and return in vterm |
| `ecc-auto-response-running-beep-interval` | 3.0s | beep | Heartbeat beep interval |
| `ecc-encouragement-speak-max-count` | 3 | encouragement | Idle-loop suppression threshold |
| `ecc-encouragement-min-work-duration` | 30.0s | encouragement | Min elapsed time to count as real work |

### Worst-Case Latency

From state detection to keystroke delivery for a Y/Y/N prompt:

```
Timer fires ──▶ detect :y/n ──▶ sit-for 1.0s (Y/N recheck)
                                 ──▶ re-detect :y/y/n
                                 ──▶ sit-for 1.0s (safe-interval)
                                 ──▶ recheck (user-typing guard)
                                 ──▶ vterm-send-string "2"
                                 ──▶ sit-for 0.5s
                                 ──▶ vterm-send-return
                                 ──▶ sit-for 1.0s (safe-interval)
                                 ──▶ verify: sit-for 1.5s
                                 Total: ~5.0s from detection
```

For a :waiting prompt (direct, no recheck):

```
Timer fires ──▶ detect :waiting
                ──▶ sit-for 1.0s (safe-interval)
                ──▶ recheck (user-typing guard)
                ──▶ vterm-send-string "/speak"
                ──▶ sit-for 0.5s
                ──▶ vterm-send-return
                ──▶ sit-for 1.0s (safe-interval)
                Total: ~2.5s from detection
```

---

## Audio Notifications

The auto-response system provides audio feedback via async subprocess calls (never blocks Emacs).

| Parameter | Default | Description |
|-----------|---------|-------------|
| `ecc-auto-response-running-beep-enabled` | t | Enable/disable audio notifications |
| `ecc-auto-response-running-beep-interval` | 3.0s | Heartbeat interval when Claude is running |
| `ecc-auto-response-beep-running-hz` | 400 Hz | Low tone for periodic heartbeat |
| `ecc-auto-response-beep-sent-hz` | 1400 Hz | High tone when response is sent |
| `ecc-auto-response-beep-cooldown` | 2.0s | Min seconds between consecutive beeps |
| `ecc-auto-response-tts-enabled` | nil | Use pre-recorded TTS audio files |

Audio priority chain: `play` (sox) > `beep` > `paplay` > idle-timer `ding`

---

## Watchdog & Reliability

| Parameter | Default | Description |
|-----------|---------|-------------|
| `--ecc-auto-response-stuck-state-threshold` | 15.0s | Force re-send if actionable state persists this long |
| `--ecc-auto-response-sending-timeout` | 30.0s | Force-clear sending guard if stuck |
| `--ecc-auto-response-send-retry-max` | 8 | Max retries for verify-send loop |

The buffer list dashboard (`M-x ecc-list-buffers`) shows all timer statuses, state durations, and a live event log for debugging.

<!-- EOF -->