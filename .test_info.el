(defun ecc-list-all-tests ()
  "List all available ERT tests."
  (message "\nAVAILABLE TESTS:\n=============")
  (let ((tests (ert--list-all-tests)))
    (message "Found %d total tests" (length tests))
    (dolist (test tests)
      (message "  * %s" (ert-test-name test)))))
