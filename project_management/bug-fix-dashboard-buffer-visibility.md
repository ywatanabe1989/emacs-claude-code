# Dashboard Buffer Visibility Bug Fix

## Root Cause Analysis

After analyzing the code, I've identified the root cause of the issue:

1. When `ecc-buffer-create` is called, it properly creates and registers a buffer in both the old registry (`ecc-buffer-registered-buffers-alist`) and the new UID-based registry (`ecc-buffer-registry-by-uid`).

2. However, when the dashboard's `ecc-dashboard-show` function is called, it doesn't refresh the list of buffers from the registry first before displaying them. It simply calls `ecc-dashboard-refresh` which depends on the current state of `ecc-buffer-registry-by-uid`.

3. The dashboard uses `ecc-dashboard--get-filtered-agents()` to retrieve agents, which uses the `ecc-buffer-registry-by-uid` hash table directly. This means if buffers are created and registered in the same Emacs session, the dashboard will show them.

4. The key issue is that the registration of buffers happens correctly but there is no explicit call to refresh/reload the registry when the dashboard is first shown, especially if both operations happen in close succession.

## Solution Approach

The fix will involve making the following changes:

1. Modify `ecc-dashboard-show` to ensure it has the most up-to-date list of buffers before displaying the dashboard.

2. This can be achieved by:
   - Adding a new function `ecc-dashboard-ensure-registry-updated` that checks all registered buffers against the UID registry and ensures they are properly registered
   - Calling this function at the beginning of `ecc-dashboard-show`

3. This ensures that any buffers created with `ecc-buffer-create` are properly reflected in the dashboard, even if they were created immediately before opening the dashboard.

## Implementation Plan

1. Create a new function `ecc-dashboard-ensure-registry-updated` in `ecc-dashboard-core.el` that:
   - Iterates through all buffers registered in `ecc-buffer-registered-buffers-alist` 
   - Ensures each is also registered in the UID-based registry
   - Could be implemented as a lightweight version of `ecc-buffer-registry-migrate-from-old-registry`

2. Modify `ecc-dashboard-show` in `ecc-dashboard-ui.el` to call this function before refreshing the dashboard

3. Update related documentation to clarify the relationship between buffer creation and dashboard display

## Testing Plan

1. Test creating a buffer with `ecc-buffer-create` and immediately opening the dashboard
2. Test creating multiple buffers and verifying all appear in the dashboard
3. Test with both direct function calls and through the UI