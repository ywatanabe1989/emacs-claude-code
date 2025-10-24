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
| `--ecc-auto-response-same-state-delay` | 0.3 seconds | Minimum time between responses to the same state |
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

<!-- EOF -->