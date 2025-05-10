;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 14:54:33>

(require 'ert)
(require 'cl-lib)
(require 'ecc-buffer/ecc-buffer-variables)
(require 'ecc-buffer/ecc-buffer-registry)
(require 'ecc-buffer/ecc-buffer-timestamp)

(ert-deftest test-ecc-buffer-timestamp-vars-exists ()
  "Test that timestamp hash table variable exists."
  (should (boundp 'ecc-buffer-timestamps))
  (should (hash-table-p ecc-buffer-timestamps)))

(ert-deftest test-ecc-buffer-record-timestamp-function-exists ()
  "Test that function to record buffer timestamp exists."
  (should (fboundp 'ecc-buffer-record-timestamp)))

(ert-deftest test-ecc-buffer-get-timestamp-function-exists ()
  "Test that function to get buffer timestamp exists."
  (should (fboundp 'ecc-buffer-get-timestamp)))

(ert-deftest test-ecc-buffer-timestamp-storage-and-retrieval ()
  "Test storing and retrieving buffer timestamps."
  (let ((test-buffer (generate-new-buffer "*test-claude-timestamp*"))
        (original-timestamps (copy-hash-table ecc-buffer-timestamps)))
    (unwind-protect
        (progn
          ;; Record a timestamp for the test buffer
          (should (ecc-buffer-record-timestamp test-buffer))
          
          ;; Verify the timestamp was recorded
          (should (gethash test-buffer ecc-buffer-timestamps))
          
          ;; Verify the timestamp is a valid time
          (let ((timestamp (ecc-buffer-get-timestamp test-buffer)))
            (should timestamp)
            (should (listp timestamp))
            (should (= (length timestamp) 4)) ; Standard time format in Emacs
            
            ;; Try retrieving with a different buffer - should be nil
            (let ((other-buffer (generate-new-buffer "*test-claude-timestamp-other*")))
              (unwind-protect
                  (should-not (ecc-buffer-get-timestamp other-buffer))
                (kill-buffer other-buffer)))))
      
      ;; Cleanup
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer))
      (setq ecc-buffer-timestamps original-timestamps))))

(ert-deftest test-ecc-buffer-timestamp-for-sorting ()
  "Test using timestamps for buffer sorting."
  (let ((test-buffer1 (generate-new-buffer "*test-claude-timestamp-1*"))
        (test-buffer2 (generate-new-buffer "*test-claude-timestamp-2*"))
        (original-timestamps (copy-hash-table ecc-buffer-timestamps)))
    (unwind-protect
        (progn
          ;; Record timestamps with a delay between them
          (ecc-buffer-record-timestamp test-buffer1)
          (sleep-for 0.1) ; Ensure timestamp difference
          (ecc-buffer-record-timestamp test-buffer2)
          
          ;; Verify comparison works as expected for sorting
          (let ((time1 (ecc-buffer-get-timestamp test-buffer1))
                (time2 (ecc-buffer-get-timestamp test-buffer2)))
            (should time1)
            (should time2)
            (should (time-less-p time1 time2))))
      
      ;; Cleanup
      (when (buffer-live-p test-buffer1)
        (kill-buffer test-buffer1))
      (when (buffer-live-p test-buffer2)
        (kill-buffer test-buffer2))
      (setq ecc-buffer-timestamps original-timestamps))))

(provide 'test-ecc-buffer-timestamp)