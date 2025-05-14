# Bug Report: Dashboard Not Showing Newly Created Buffers

## Description
When a new buffer is created using `ecc-buffer-create` and then the dashboard is opened with `ecc-dashboard`, the newly created buffer does not appear in the dashboard view.

## Steps to Reproduce
1. Call `(ecc-buffer-create)` to create a new Claude Code buffer
2. Call `(ecc-dashboard)` to open the dashboard
3. Observe that the newly created buffer is not visible in the dashboard

## Expected Behavior
The dashboard should display all created Claude Code buffers, including those created immediately before opening the dashboard.

## Current Behavior
The dashboard does not show buffers that were created in the same session before opening the dashboard.

## Possible Causes
- The dashboard might not be refreshing its buffer list when opened
- There might be a disconnection between buffer creation and registration with the dashboard
- The buffer registry might not be properly updated when new buffers are created
- The dashboard view might not be reading from the up-to-date buffer registry

## Impact
Users cannot see or manage newly created buffers through the dashboard interface, limiting the dashboard's utility as a buffer management tool.