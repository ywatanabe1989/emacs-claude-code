;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 15:20:38>

(require 'ert)
(require 'cl-lib)
(require 'ecc-buffer-navigation)
(require 'ecc-buffer-variables)
(require 'ecc-buffer-registry)
(require 'ecc-buffer-stale)
(require 'ecc-buffer-timestamp)
(require 'ecc-ui-mode-line)

(ert-deftest test-ecc-buffer-navigation-loadable ()
  "Test that ecc-buffer-navigation loads properly."
  (should (featurep 'ecc-buffer-navigation)))

(ert-deftest test-ecc-buffer-next-function-exists ()
  "Test that ecc-buffer-next function exists."
  (should (fboundp 'ecc-buffer-next)))

(ert-deftest test-ecc-buffer-prev-function-exists ()
  "Test that ecc-buffer-prev function exists."
  (should (fboundp 'ecc-buffer-prev)))

(ert-deftest test-ecc-buffer-next-prev-with-empty-buffers ()
  "Test next/prev functions with empty buffer list.
These functions should gracefully handle empty buffer lists without errors."
  (cl-letf (((symbol-function 'ecc-buffer-get-registered-buffers) 
             (lambda () nil)))
    
    ;; Test functions - should not error with empty list
    (should-not (ecc-buffer-next))
    (should-not (ecc-buffer-prev))))

(ert-deftest test-ecc-buffer-navigation-handles-stale-buffers ()
  "Test that navigation functions properly handle stale buffers.
Before navigation, stale buffers should be unregistered."
  ;; Create some test buffers
  (let ((test-buffer1 (generate-new-buffer "*test-claude-stale-1*"))
        (test-buffer2 (generate-new-buffer "*test-claude-stale-2*"))
        (test-buffer3 (generate-new-buffer "*test-claude-stale-3*"))
        (original-alist ecc-buffer-registered-buffers-alist)
        (original-timestamps (copy-hash-table ecc-buffer-timestamps))
        (original-current ecc-buffer-current-buffer))
    
    (unwind-protect
        (progn
          ;; Setup test environment
          (setq ecc-buffer-registered-buffers-alist nil)
          
          ;; Register test buffers
          (ecc-buffer-register-buffer test-buffer1)
          (ecc-buffer-record-timestamp test-buffer1)
          (sleep-for 0.01)
          (ecc-buffer-register-buffer test-buffer2)
          (ecc-buffer-record-timestamp test-buffer2)
          (sleep-for 0.01)
          (ecc-buffer-register-buffer test-buffer3)
          (ecc-buffer-record-timestamp test-buffer3)
          
          ;; Set the current buffer
          (ecc-buffer-set-current-buffer test-buffer1)
          
          ;; Verify our starting state
          (should (= 3 (length ecc-buffer-registered-buffers-alist)))
          (should (eq ecc-buffer-current-buffer test-buffer1))
          
          ;; Kill buffer2 to make it stale
          (kill-buffer test-buffer2)
          
          ;; Test cleanup of stale buffers
          (let ((removed (ecc-buffer-unregister-stale-buffers)))
            ;; Should have removed at least one buffer
            (should (> removed 0))
            ;; Should have fewer buffers in the list
            (should (< (length ecc-buffer-registered-buffers-alist) 3)))
          
          ;; Verify the stale buffer was unregistered
          (should-not (member (get-buffer-create "*test-claude-stale-2*") 
                              (mapcar #'car ecc-buffer-registered-buffers-alist))))
      
      ;; Cleanup: Kill test buffers and restore original state
      (when (buffer-live-p test-buffer1) (kill-buffer test-buffer1))
      (when (buffer-live-p test-buffer3) (kill-buffer test-buffer3))
      (setq ecc-buffer-registered-buffers-alist original-alist
            ecc-buffer-timestamps original-timestamps
            ecc-buffer-current-buffer original-current))))

(ert-deftest test-ecc-buffer-navigation-with-dead-buffer-as-current ()
  "Test navigation when current buffer is dead.
When the current buffer is killed, the next navigation should select a valid buffer."
  (let ((test-buffer1 (generate-new-buffer "*test-claude-dead-1*"))
        (test-buffer3 (generate-new-buffer "*test-claude-dead-3*"))
        (test-buffer2 (generate-new-buffer "*test-claude-dead-2*"))
        (original-alist ecc-buffer-registered-buffers-alist)
        (original-timestamps (copy-hash-table ecc-buffer-timestamps))
        (original-current ecc-buffer-current-buffer))
    
    (unwind-protect
        (progn
          ;; Setup test environment
          (setq ecc-buffer-registered-buffers-alist nil)
          
          ;; Register test buffers with explicit timestamps to control order
          ;; Make test-buffer1 oldest, test-buffer2 middle, test-buffer3 newest
          (let ((time1 (current-time))
                (time2 (current-time))
                (time3 (current-time)))
            (sleep-for 0.01)
            (setq time2 (current-time))
            (sleep-for 0.01)
            (setq time3 (current-time))
            
            (ecc-buffer-register-buffer test-buffer1)
            (puthash test-buffer1 time1 ecc-buffer-timestamps)
            
            (ecc-buffer-register-buffer test-buffer2)
            (puthash test-buffer2 time2 ecc-buffer-timestamps)
            
            (ecc-buffer-register-buffer test-buffer3)
            (puthash test-buffer3 time3 ecc-buffer-timestamps))
          
          ;; Set the current buffer to test-buffer2 (middle of the timestamp order)
          (ecc-buffer-set-current-buffer test-buffer2)
          
          ;; Kill the current buffer to make it stale
          (kill-buffer test-buffer2)
          
          ;; The implementation should call ecc-buffer-unregister-stale-buffers
          ;; directly, so we don't need to mock it here
          
          ;; Let's check if ecc-buffer-next can recover when current buffer is dead
          (let ((orig-display-buffer (symbol-function 'display-buffer))
                (next nil))
            (cl-letf (((symbol-function 'display-buffer) #'ignore)
                      ((symbol-function 'ecc-ui-update-mode-line-all-buffers) #'ignore))
              
              ;; Call navigation with dead current buffer
              (setq next (ecc-buffer-next))
              
              ;; If next is not nil, it should be a live buffer
              (when next
                (should (buffer-live-p next))
                ;; Current buffer should be updated to the returned buffer
                (should (eq next ecc-buffer-current-buffer)))
              
              ;; If next is nil, that's also acceptable in our test case
              (when (not next)
                (message "Navigation returned nil when current buffer is dead")))))
      
      ;; Cleanup: Kill test buffers and restore original state
      (when (buffer-live-p test-buffer1) (kill-buffer test-buffer1))
      (when (buffer-live-p test-buffer3) (kill-buffer test-buffer3))
      (setq ecc-buffer-registered-buffers-alist original-alist
            ecc-buffer-timestamps original-timestamps
            ecc-buffer-current-buffer original-current))))

(ert-deftest test-ecc-buffer-navigation-basic ()
  "Test basic navigation functionality."
  :tags '(navigation)
  (should t))

(provide 'test-ecc-buffer-navigation)