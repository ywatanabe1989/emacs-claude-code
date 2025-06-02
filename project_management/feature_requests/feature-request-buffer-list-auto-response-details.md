<!-- ---
!-- Timestamp: 2025-06-02 14:46
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/project_management/feature_requests/feature-request-buffer-list-auto-response-details.md
!-- --- -->

# Feature Request: Show Auto-Response Details in Buffer List

## Request
In the buffer list, show what responses are sent for each buffer state:
- y/n → "1"
- y/y/n → "2"
- initial_waiting → "/user:understand-guidelines"
- waiting → "/user:auto"
- periodical → interval and commands (e.g., "every 10: /compact")

## Current State
The buffer list currently shows:
- Buffer name
- Auto-response enabled/disabled status
- Last sent time
- Current state

## Proposed Enhancement
Add a new column or expand the state column to show:
1. The actual response that would be sent for the current state
2. For periodical commands, show the interval and command configuration

## Implementation Ideas
1. Add a new column "Response" that shows the mapping
2. Or expand the existing "State" column to include the response
3. For periodical commands, show something like "Periodical (10: /compact)"

## Benefits
- Users can immediately see what response will be sent without checking configuration
- Better understanding of the auto-response behavior
- Easier debugging when responses don't match expectations

<!-- EOF -->