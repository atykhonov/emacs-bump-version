(ert-deftest test-bump-version--major-zero ()
  (should (equal
           0
           (bump-version--major-version "0.20.44"))))

(ert-deftest test-bump-version--major-none-zero ()
  (should (equal
           27
           (bump-version--major-version "27.20.44"))))

(ert-deftest test-bump-version--minor-zero ()
  (should (equal
           0
           (bump-version--minor-version "27.0.44"))))

(ert-deftest test-bump-version--minor-none-zero ()
  (should (equal
           61
           (bump-version--minor-version "27.61.44"))))

(ert-deftest test-bump-version--patch-zero ()
  (should (equal
           0
           (bump-version--patch-level "27.61.0"))))

(ert-deftest test-bump-version--patch-none-zero ()
  (should (equal
           129
           (bump-version--patch-level "27.61.129"))))

(ert-deftest test-bump-version--patch-1 ()
  (should (string-equal
           "0.0.2"
           (bump-version--patch "0.0.1"))))

(ert-deftest test-bump-version--patch-2 ()
  (should (string-equal
           "0.0.204"
           (bump-version--patch "0.0.203"))))

(ert-deftest test-bump-version--minor-1 ()
  (should (string-equal
           "0.1.0"
           (bump-version--minor "0.0.20"))))

(ert-deftest test-bump-version--minor-2 ()
  (should (string-equal
           "20.221.0"
           (bump-version--minor "20.220.20"))))

(ert-deftest test-bump-version--major-1 ()
  (should (string-equal
           "21.0.0"
           (bump-version--major "20.221.20"))))

(ert-deftest test-bump-version--major-2 ()
  (should (string-equal
           "1.0.0"
           (bump-version--major "0.221.20"))))

(ert-deftest test-bump-version--version-to-list ()
  (should (equal
           (list 1 2 3)
           (bump-version--version-to-list "1.2.3"))))

(ert-deftest test-bump-version--read-config ()
  (with-temp-buffer
    (insert-file-contents (concat default-directory "/" bump-version-config-file))
    (read (buffer-string))))

;; (ert-deftest test-bump-version-bump-patch-2 ()
;;   (should (string-equal
;;            "0.0.2"
;;            (bump-version--patch
;;            "0.0.1"))))
